#' End point handlers
#'
#' @param .req Required arguments
#' @param .res Result
#' @export
pool_handler <- function(.req, .res) {
  pool_json <- ""
  json_payload <- as.character(.req$parameters_query[["payload"]])
  if (length(json_payload) == 0L) {
    raise(HTTPError$bad_request())
  }
  params <- json_to_parameters(json_payload)
  if (is.null(params$data)) {
    pool_json <- "Error: no data"
  }
  if (pool_json == "") {
    print("DEBUG: Calling pool.table (requires 3.16.4)")
    pool_json <- call_pool(params$data)
  }
  .res$set_body(pool_json)
  .res$set_content_type("text/plain")
}

#' @rdname pool_handler
#' @export
fit_handler <- function(.req, .res) {
  fit_json <- ""
  json_payload <- as.character(.req$parameters_query[["payload"]])
  # if answers are copied straight from the Swagger interface,
  # there are too many backslashes
  # json_payload <- gsub('\\\\', '', input)
  if (length(json_payload) == 0L) {
    raise(HTTPError$bad_request())
  }
  params <- json_to_parameters(json_payload)
  if (is.null(params$data)) {
    fit_json <- "Error: no data"
  }
  if (is.null(params$model)) {
    fit_json <- "Error: no model"
  }
  if (is.null(params$formula)) {
    fit_json <- "Error: no formula"
  }

  if (fit_json == "") {
    print("DEBUG: Calling with (fitting function)")
    fit_json <- call_with(params$data, params$model, params$formula)
  }
  .res$set_body(fit_json)
  .res$set_content_type("text/plain")
}

#' @rdname pool_handler
#' @export
impute_longfmt_handler <- function(.req, .res) {
  json_payload <- as.character(.req$parameters_query[["payload"]])
  impute <- list()
  impute$result <- ""
  impute$error <- ""
  check <- TRUE

  if (length(json_payload) == 0L) {
    check <- FALSE
    impute$error <- "No input"
  } 
  # check if convertible to json
  params <- json_to_parameters(json_payload)
  # impute function needs data
  if (is.null(params$data)) {
      check <- FALSE
      impute$error <- "No data"
  }
  if (is.null(params$maxit)) {
      check <- FALSE
      impute$error <- "No maxit"
  }
  if (is.null(params$seed)) {
      check <- FALSE
      impute$error <- "No seed"
  }
  if (!check) {
    impute$success <- FALSE
    .res$set_body(toJSON(impute))
    .res$set_content_type("text/plain")
    return()
  }
  else {
    if (is.null(params$m)) {
      params$m <- 5
    }
    print("DEBUG: Calling mice")
    imp <- call_mice(params)
    if (is.null(imp$error)) {
      impute$success <- TRUE
      impute$result <- imp_result_long_fmt(imp)
    } else {
      impute$success <- FALSE
      impute$error <- imp$error
    }
    .res$set_body(toJSON(impute))
    .res$set_content_type("text/plain")
  }
}

#' @rdname example_data_handler
#' @export
example_data_handler <- function(.req, .res) {
  example_name <- as.character(.req$parameters_query[["name"]])
  .res$set_body(example_data_to_json(example_name))
  .res$set_content_type("text/plain")
}

#' @rdname mice_version_handler
#' @export
mice_version_handler <- function(.req, .res) {
  version <- list()
  version$success <- TRUE
  version$error <- ""

  result <- tryCatch(
    {
      version$result <- sessionInfo("mice1")$otherPkgs$mice$Version
    },
    warning = function(w) {
      res <- list()
      res$success <- FALSE
      res$result <- ""
      res$error <- w$message
      return(res)
    }
  )
  print("RESULT")
  print(result)

  if (typeof(result) == "list") {
    version <- result
  }
  .res$set_body(toJSON(version))
  .res$set_content_type("text/plain")
}
