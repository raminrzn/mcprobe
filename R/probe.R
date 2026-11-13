#' Run one diagnostic on the executor
#'
#' @param action `"info"` (environment), `"echo"` (size of the input received),
#'   `"sleep"` (wait `seconds`), `"inla"` (install and run INLA in a temporary
#'   library), `"jags"` (look for the JAGS library) or `"dynamo"` (run the
#'   hiaR DYNAMO-HIA tutorial; call asynchronously).
#' @param seconds Seconds to wait for `action = "sleep"`.
#' @param x Any data, for `action = "echo"`.
#' @param ... Ignored.
#' @return A list of results.
#' @export
model_run <- function(action = "info", seconds = 1, x = NULL, ...) {
  switch(action,
         info = probe_info(),
         echo = probe_echo(x),
         sleep = probe_sleep(seconds),
         inla = probe_inla(),
         jags = probe_jags(),
         dynamo = probe_dynamo(),
         dynamo_shim = probe_dynamo(case_shim = TRUE),
         stop("Unknown action: ", action, call. = FALSE))
}

read_lines <- function(path) {
  if (file.exists(path)) paste(readLines(path, warn = FALSE), collapse = "\n") else NA
}

reachable <- function(url) {
  t0 <- Sys.time()
  ok <- tryCatch({
    h <- curlGetHeaders(url, timeout = 10)
    as.character(attr(h, "status"))
  }, error = function(e) paste("error:", conditionMessage(e)))
  list(url = url, status = ok, seconds = as.numeric(Sys.time() - t0, units = "secs"))
}

sh <- function(cmd) {
  tryCatch(paste(system(cmd, intern = TRUE, ignore.stderr = TRUE), collapse = "\n"),
           error = function(e) NA, warning = function(w) NA)
}

probe_info <- function() {
  ip <- utils::installed.packages()
  libs <- .libPaths()
  env <- Sys.getenv(c("R_LIBS", "R_LIBS_USER", "R_LIBS_SITE", "OMP_NUM_THREADS",
                      "HOME", "TZ", "LANG"))
  list(
    r_version = R.version.string,
    platform = R.version$platform,
    os = sh("grep '^PRETTY_NAME' /etc/os-release"),
    glibc = sh("ldd --version | head -1"),
    cores = parallel::detectCores(),
    meminfo = sh("grep -E 'MemTotal|MemAvailable' /proc/meminfo"),
    cgroup_memory_max = read_lines("/sys/fs/cgroup/memory.max"),
    cgroup_memory_v1 = read_lines("/sys/fs/cgroup/memory/memory.limit_in_bytes"),
    cgroup_cpu_max = read_lines("/sys/fs/cgroup/cpu.max"),
    disk_tmp = sh("df -h /tmp | tail -1"),
    lib_paths = libs,
    lib_writable = vapply(libs, function(p) file.access(p, 2) == 0, logical(1)),
    env = as.list(env),
    repos = as.list(getOption("repos")),
    n_packages = nrow(ip),
    has = as.list(vapply(c("remotes", "pak", "devtools", "INLA", "fmesher", "splancs", "sf",
                   "rjags", "voi", "mgcv", "earth", "dbarts", "mvtnorm", "posterior",
                   "ggplot2", "Matrix", "BCEA", "heemod"),
                 function(p) p %in% rownames(ip), logical(1))),
    packages = paste(ip[, "Package"], ip[, "Version"], collapse = ", "),
    jags_binary = unname(Sys.which("jags")),
    network = list(reachable("https://cloud.r-project.org/src/contrib/PACKAGES"),
                   reachable("https://inla.r-inla-download.org/R/stable/src/contrib/PACKAGES"),
                   reachable("https://github.com")))
}

probe_echo <- function(x) {
  list(received_values = length(unlist(x)),
       object_bytes = as.numeric(utils::object.size(x)),
       class = class(x))
}

probe_sleep <- function(seconds) {
  t0 <- Sys.time()
  Sys.sleep(as.numeric(seconds))
  list(slept = as.numeric(Sys.time() - t0, units = "secs"))
}

# Installs INLA into a temporary library for this one container, then fits a
# tiny model, timing each step. Nothing is kept after the job ends.
probe_inla <- function() {
  lib <- file.path(tempdir(), "lib")
  dir.create(lib)
  .libPaths(c(lib, .libPaths()))
  repos <- c(INLA = "https://inla.r-inla-download.org/R/stable",
             CRAN = "https://cloud.r-project.org")
  timed <- function(expr) {
    t0 <- Sys.time()
    warns <- character(0)
    msg <- tryCatch(withCallingHandlers({ force(expr); "ok" }, warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }), error = function(e) conditionMessage(e))
    list(result = msg, warnings = warns,
         seconds = as.numeric(Sys.time() - t0, units = "secs"))
  }
  out <- list()
  out$install <- timed(utils::install.packages(c("INLA", "splancs"), lib = lib,
                                               repos = repos, quiet = TRUE,
                                               dependencies = c("Depends", "Imports", "LinkingTo")))
  pk <- c("INLA", "fmesher", "splancs", "sf", "units", "s2", "wk", "classInt", "sp")
  out$installed <- as.list(vapply(pk, function(p) dir.exists(file.path(lib, p)) ||
                                    p %in% rownames(utils::installed.packages()), logical(1)))
  out$system_libraries <- sh(paste("dpkg -l 2>/dev/null | awk '{print $2, $3}' |",
                                   "grep -Ei 'gdal|geos|proj|udunits|abseil|cmake|sqlite3|jags|gfortran'"))
  out$load <- timed(loadNamespace("INLA", lib.loc = c(lib, .libPaths())))
  out$fit <- timed({
    d <- data.frame(y = stats::rnorm(50), x = stats::rnorm(50))
    fit <- INLA::inla(y ~ x, data = d)
    out$fixed <- fit$summary.fixed[, "mean"]
  })
  out$inla_version <- tryCatch(as.character(utils::packageVersion("INLA", lib.loc = lib)),
                               error = function(e) NA)
  out
}

probe_jags <- function() {
  list(jags_binary = unname(Sys.which("jags")),
       libjags = sh("ldconfig -p | grep -i jags"),
       pkg_config = sh("pkg-config --modversion jags"))
}

# Runs the hiaR tutorial ("Introduction to hiaR", hiaR commit 6ba62ff) on the
# executor: installs hiaR into a temporary library, downloads RIVM's Linux
# build of the DYNAMO-HIA engine with hiaR's own download function, runs the
# Dutch smoking / lung cancer example and returns the first rows of the
# results with each step's timing and error. Nothing is kept after the job.
probe_dynamo <- function(release_tag = "v3.0.0-beta.1",
                         hiar_ref = "6ba62ff56308486c457ed53019cfd2c280e087be",
                         case_shim = FALSE) {
  lib <- file.path(tempdir(), "dynamo_lib")
  dir.create(lib, showWarnings = FALSE)
  .libPaths(c(lib, .libPaths()))
  steps <- list()
  step <- function(name, expr) {
    t0 <- Sys.time()
    res <- tryCatch(list(ok = TRUE, value = force(expr)),
                    error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    res$seconds <- as.numeric(Sys.time() - t0, units = "secs")
    steps[[name]] <<- res[setdiff(names(res), "value")]
    res
  }
  step("install_hiaR", remotes::install_github("Dynamo-HIA/hiaR", ref = hiar_ref, lib = lib,
                                               upgrade = "never", build_vignettes = FALSE,
                                               quiet = TRUE, repos = "https://cloud.r-project.org"))
  if (!steps$install_hiaR$ok) return(list(steps = steps))
  hia <- asNamespace("hiaR")
  temp_dir <- tempfile("dynamo")
  dir.create(temp_dir)
  sim <- "Netherlands_Smoking_Intervention"
  step("configure", {
    fs::dir_copy(system.file("extdata", "example-nl", package = "hiaR", lib.loc = lib),
                 temp_dir, overwrite = TRUE)
    smoking <- hia$configure_risk_factor(name = "Smoking_cat3",
      transition_filename = "NL_RF_smoking_Transitions_Netto",
      prevalence_filename = "NL_RF_smoking_cat3_Prev_V1")
    lung <- hia$configure_disease(name = "Lung_Cancer",
      prevalence_filename = "NL_disease_LungCa_Prev_V2",
      incidence_filename = "NL_disease_LungCa_Inc_V2",
      excess_mortality_filename = "NL_disease_LungCa_ExecMor_V2",
      disability_weights_filename = "NL_disease_LungCa_DALY_V1")
    rr <- hia$configure_relative_risk(index = 0, from = "Smoking_cat3", to = "Lung_Cancer",
      relative_risk_filename = "RR_to_LungCa-Smoking_cat3")
    scen <- hia$configure_scenario(name = "All_Never_Smokers",
      transition_filename = "NL_RF_smoking_Transitions_Netto",
      prevalence_filename = "RF_smoking_cat_All_Never_Smokers_Prev")
    hia$create_simulation_dir(simulation_name = fs::path(temp_dir, "Simulations", sim),
      population_name = "Netherlands", has_newborns = TRUE, starting_year = 2010,
      number_of_years = 10, population_size = 10, min_age = 0, max_age = 95, time_step = 1,
      ref_scenario_name = "Netherlands_Reference_Scenario", random_seed = 42,
      scenarios = list(scen), diseases = list(lung), risk_factors = smoking,
      relative_risks = list(rr))
    write(sim, fs::path(temp_dir, "simulation_names.txt"))
  })
  step("download_engine", hia$download_github_release(
    repo_url = "https://github.com/Dynamo-HIA/dynamo-hia", release_tag = release_tag,
    dest_dir = temp_dir))
  exe <- file.path(temp_dir, Sys.getenv("DYNAMO_HIA_PATH"))
  engine <- list(path = Sys.getenv("DYNAMO_HIA_PATH"), exists = file.exists(exe),
                 executable = file.access(exe, 1) == 0,
                 runtime_release = read_lines(file.path(dirname(dirname(exe)), "lib", "runtime", "release")))
  # Optional: the engine opens some folders in lower case (e.g. "prevalences")
  # while the data use capitals ("Prevalences"); Windows and macOS ignore
  # case, Linux does not. Add a lower-case symbolic link next to every folder
  # and file with capitals in its name (data and simulation folders only).
  shims <- 0L
  if (case_shim) step("case_shim", {
    for (d in c("Reference_Data", "Simulations")) {
      paths <- c(file.path(temp_dir, d), list.files(file.path(temp_dir, d), recursive = TRUE,
                                                    full.names = TRUE, include.dirs = TRUE))
      for (p in rev(paths)) {
        b <- basename(p); lb <- tolower(b); target <- file.path(dirname(p), lb)
        if (b != lb && !file.exists(target)) { file.symlink(b, target); shims <- shims + 1L }
      }
    }
    shims
  })
  step("run_engine", withr::with_dir(temp_dir, hia$run_dynamo_hia("simulation_names.txt", exe)))
  sim_dir <- file.path(temp_dir, "Simulations", sim)
  files <- list.files(temp_dir, recursive = TRUE, include.dirs = FALSE)
  files <- files[!grepl("^DYNAMO-HIA/|Reference_Data/", files)]
  pop <- step("load_batchoutput", hia$load_simulation_results(temp_dir, sim, type = "batchoutput"))
  le <- step("load_cohortLE", hia$load_simulation_results(temp_dir, sim, type = "cohortLE"))
  log_file <- file.path(temp_dir, "run_dynamo_hia.log")
  list(steps = steps, engine = engine, case_links = shims, result_files = utils::head(files, 60),
       batchoutput_head = if (pop$ok) utils::head(pop$value, 6) else NULL,
       batchoutput_rows = if (pop$ok) nrow(pop$value) else NULL,
       cohortLE_head = if (le$ok) utils::head(le$value, 6) else NULL,
       log_tail = if (file.exists(log_file)) utils::tail(readLines(log_file, warn = FALSE), 30) else NULL,
       hiaR_version = tryCatch(as.character(utils::packageVersion("hiaR", lib.loc = lib)),
                               error = function(e) NA))
}
