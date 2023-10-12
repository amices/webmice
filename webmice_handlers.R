#' End point handlers
#'
#' @param .req Required arguments
#' @param .res Result
#' @export
pool_handler <- function(.req, .res) {
  pool <- list()
  pool$result <- ""
  pool$error <- ""
  check <- TRUE

  json_payload <- as.character(.req$parameters_query[["payload"]])
  if (length(json_payload) == 0L) {
    check <- FALSE
    pool$error <- "No input"
  } else {
    params <- json_to_parameters(json_payload)
    if (is.null(params$data)) {
      check <- FALSE
      pool$error <- "Error: no data"
    }
  }
  if (!check) {
    pool$success <- FALSE
    .res$set_body(toJSON(pool))
    .res$set_content_type("text/plain")
    return()
  } else {
    print("DEBUG: Calling pool.table (requires 3.16.4)")
    poolres <- call_pool(params$data)
    if (is.null(poolres$error)) {
      pool$success <- TRUE
      pool$result <- poolres
    } else {
      pool$success <- FALSE
      pool$error <- poolres$error
    }
  }
  .res$set_body(toJSON(pool, force = TRUE))
  .res$set_content_type("text/plain")
}

#' @rdname pool_handler
#' @export
fit_handler <- function(.req, .res) {
  fit <- list()
  fit$result <- ""
  fit$error <- ""
  check <- TRUE
  
  json_payload <- as.character(.req$parameters_query[["payload"]])
  # if answers are copied straight from the Swagger interface,
  # there are too many backslashes
  json_payload <- gsub('\\\\', '', json_payload)
  if (length(json_payload) == 0L) {
    check <- FALSE
    fit$error <- "No input"
  } else {
    params <- json_to_parameters(json_payload)
    if (is.null(params$data)) {
      check <- FALSE
      fit$error <- "No data"
    }
    if (is.null(params$model)) {
      check <- FALSE
      fit$error <- "No model"
    }
    if (is.null(params$formula)) {
      check <- FALSE
      fit$error <- "No formula"
    }
  }
  
  if (!check) {
    fit$success <- FALSE
    .res$set_body(toJSON(fit))
    .res$set_content_type("text/plain")
    return()
  } else {
    print("DEBUG: Calling with (fitting function)")
    fitres <- call_with(params$data, params$model, params$formula)
    if (is.null(fitres$error)) {
      fit$success <- TRUE
      fit$result <- fitres
    } else {
      fit$success <- FALSE
      fit$error <- fitres$error
    }
  }
  print(fit)
  .res$set_body(toJSON(fit, force = TRUE))
  .res$set_content_type("text/plain")
}

#' @rdname impute_longfmt_handler
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
  } else {
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
  data <- list()
  data$success <- ""
  data$result <- ""
  data$error <- ""
  example_name <- as.character(.req$parameters_query[["name"]])

  result <- tryCatch(
    {
      data$result <- get(example_name)
      data$success <- TRUE
    },
    error = function(e) {
      res <- list()
      res$success <- FALSE
      res$result <- ""
      res$error <- as.character(e)
      return(res)
    }
  )
  if (typeof(result) == "list") {
    data <- result
  }
  .res$set_body(toJSON(data))
  .res$set_content_type("text/plain") 
}

#' @rdname mice_version_handler
#' @export
mice_version_handler <- function(.req, .res) {
  version <- list()
  version$success <- ""
  version$error <- ""

  result <- tryCatch(
    {
      version$result <- sessionInfo("mice")$otherPkgs$mice$Version
      version$success <- TRUE
    },
    warning = function(w) {
      res <- list()
      res$success <- FALSE
      res$result <- ""
      res$error <- w$message
      return(res)
    }
  )

  if (typeof(result) == "list") {
    version <- result
  }
  .res$set_body(toJSON(version))
  .res$set_content_type("text/plain")
}
