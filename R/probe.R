#' Run one diagnostic on the executor
#'
#' @param action `"info"` (environment), `"echo"` (size of the input received),
#'   `"sleep"` (wait `seconds`), `"inla"` (install and run INLA in a temporary
#'   library) or `"jags"` (look for the JAGS library).
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
