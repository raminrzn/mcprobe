# ---------------------------------------------------------------------------
# ModelsCloud / PexaCloud server gateway.
#
# Drop this file into your package's R/ directory unchanged — nothing in it is
# package-specific — then run roxygen2::roxygenise() so `gateway` is exported.
# Add `jsonlite` to Imports in DESCRIPTION.
#
# The platform never calls model_run() directly. Every request is dispatched to
# <package>::gateway(), with the request's funcInput keys spread as named
# arguments. A package that doesn't export `gateway` returns HTTP 400 on every
# call, before any of its own code runs.
# ---------------------------------------------------------------------------

#' ModelsCloud gateway (server entry point)
#'
#' Generic dispatcher invoked by the ModelsCloud platform. Reads `func` from the
#' incoming request, strips the platform-only fields, and calls the named
#' package function with whatever remains — so
#' `gateway(func = "model_run", model_input = ...)` runs [model_run()]. When
#' `func` is absent it defaults to `"model_run"`, which makes the platform's
#' default entry point land on the model without the caller naming it.
#'
#' @param ... Request fields supplied by the platform. `func`, `api_key`,
#'   `session_id`, `execution_id`, and `callback_url` are treated as control
#'   fields; everything else is passed through to the selected function.
#' @return A JSON string produced by [jsonlite::toJSON()].
#' @export
gateway <- function(...) {
  arguments <- list(...)

  func <- arguments$func
  if (is.null(func)) func <- "model_run"
  callback_url <- arguments$callback_url
  execution_id <- arguments$execution_id
  async <- isTRUE(as.logical(arguments$async))

  arguments$func <- NULL
  arguments$api_key <- NULL
  arguments$session_id <- NULL
  arguments$execution_id <- NULL
  arguments$callback_url <- NULL
  arguments$async <- NULL

  out <- if (length(arguments) == 0) {
    do.call(func, list())
  } else {
    do.call(func, arguments)
  }

  json <- jsonlite::toJSON(out, dataframe = "rows", na = "null", digits = NA)
  # Asynchronous calls finish by posting the result to the callback URL.
  if (async && !is.null(callback_url)) {
    send_callback(callback_url, execution_id, json)
  }
  json
}

send_callback <- function(callback_url, execution_id, json) {
  body <- paste0('{"execution_id":', jsonlite::toJSON(execution_id, auto_unbox = TRUE),
                 ',"func_output":', json, '}')
  h <- curl::new_handle()
  curl::handle_setheaders(h, "Content-Type" = "application/json")
  curl::handle_setopt(h, post = TRUE, postfields = body)
  tryCatch(curl::curl_fetch_memory(callback_url, handle = h),
           error = function(e) message("callback failed: ", conditionMessage(e)))
  invisible(NULL)
}

#' Run the model via the PRISM-style entry point
#'
#' Thin alias for [model_run()], kept for compatibility with clients that
#' dispatch to `prism_model_run`.
#'
#' @param model_input See [model_run()].
#' @return See [model_run()].
#' @export
prism_model_run <- function(model_input = NULL) {
  model_run(model_input)
}

# Lightweight availability check some clients call through the gateway. Not
# exported: the gateway resolves it inside the package namespace via do.call().
connect_to_model <- function(api_key = "") {
  list(error_code = 0, session_id = "", version = "", description = "ModelsCloud enabled")
}
