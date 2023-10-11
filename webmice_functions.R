#' Reads a csv file
#'
#' @param path The file path
read_file <- function(path) {
  tryCatch(
    {
      data <- read.csv(path)
      return(data)
    },
    error = function(e) {
      return(NULL)
    }
  )
}

#' Creates a hash for a data file name
#'
#' @param string A string
#' @export
md5_string <- function(string) {
  return(digest(paste(Sys.time(), string), algo = "md5", serialize = FALSE))
}

#' Takes a json string and returns it as R list
#'
#' @param json_payload The json payload
#' @examples
#' \dontrun{
#' input <- jsonlite::toJSON(list(data = "nhanes", maxit = 2, m = 2, seed = 1))
#' json_to_parameters(input)
#' }
json_to_parameters <- function(json_payload) {
  result <- tryCatch(
    {
      params <- fromJSON(json_payload)
      return(params)
    },
    error = function(e) {
      return(NULL)
    }
  )
}

#' Takes the result of the imputation and returns the long format of the data
#'
#' @param imp Multiply imputed data set, an object of class \link[mice]{mids}
#' @seealso \link[mice]{complete}
imp_result_long_fmt <- function(imp) {
  return(complete(imp, "long", include = TRUE))
}

#' Mice functions
#' @inheritParams mice::mice
impute <- function(data, maxit, m, seed) {
  imp <- list()
  imp$error <- ""
  result <- tryCatch(
    {
      imp <- mice(data, maxit = maxit, m = m, seed = seed)
      return(imp)
    },
    error = function(e) {
      return(e)
    }
  )

  if (!is.mids(imp)) {
    imp$error <- result
    return(imp)
  }
}

#' Calls mice's imputation function with parameters provided in a list 'params'
#'
#' @param params   A list with elements: `data`: A string with the name of the
#' dataset, a hash from an uploaded file, or a csv file; `maxit`:
#' number of iterations; `m`: number of imputations, `seed`: seed.
#' @seealso \link[mice]{mice}
call_mice <- function(params) {
  imp <- list()
  result <- tryCatch(
    {
      data <- get(params$data)
    },
    error = function(e) {
      return(-1)
    }
  )

  if (typeof(result) == "list") {
    print("DEBUG: Imputation on example data set")
    imp <- impute(data, maxit = params$maxit, m = params$m, seed = params$seed)
    return(imp)
  }
  if (typeof(params$data) == "character" && endsWith(params$data, ".csv")) {
    print("DEBUG: Imputation on local csv file")
    df <- read_file(params$data)
    if(is.null(df)){
      imp$error <- "Failure: reading local csv file"
      return(imp)
    }
    imp <- impute(mice::nhanes, maxit = params$maxit, m = params$m,
                  seed = params$seed)
    return(imp)
  }

  if (typeof(params$data) == "character") {
    print("DEBUG: Imputation on uploaded file")
    df <- read_file(file.path(get_data_uploads(), params$data))
    if (is.null(df)) {
      imp$error <-
        "Failure: reading file, not an example dataset or file on server"
      return(imp)
    }
    imp <- impute(df, maxit = params$maxit, m = params$m, seed = params$seed)
    return(imp)
  }
}

call_with <- function(data, model, formula) {
  fit <- list()
  mids_data <- list()
  result <- tryCatch(
    {
      mids_data <- as.mids(data)
    },
    error = function(e) {
      return(e)
    }
  )
  if(length(mids_data) == 0) {
    fit$error <- result
    return(fit)
  }

  result <- tryCatch(
    {
      if (model == "lm") {
        fit <- with(mids_data, lm(as.formula(formula)))
        return(summary(fit))
      } else {
        if (model == "glm") {
          fit <- with(mids_data, glm(as.formula(formula)))
          return(summary(fit))
        } else {
          fit$error <- "Model not known"
          return(fit)
          }
        }
    },
    error = function(e) {
      return(e)
    }
  ) 

  if(length(fit) == 0) {
    fit$error <- result
    return(fit)
  }
}

call_pool <- function(data) {
  pool <- list()
  if (packageVersion("mice") < "3.16.4") {
    pool$error <- "ERROR pool.table: need mice version 3.16.4 or higher"
    return(pool)
  }

  result <- tryCatch(
    {
      pool <- pool.table(data)
      return(pool)
    }, error = function(e) {
      return(e)
    }
  )
  if(length(pool) == 0) {
    pool$error <- result
    return(pool)
  }
}

get_data_uploads <- function() {
  data_uploads <- Sys.getenv("MICEROCKER_DATA_UPLOADS")
  if (data_uploads == "") {
    data_uploads <- "data_uploads"
  }
  if (!file.exists(data_uploads)) {
    dir.create(data_uploads)
  }
  print(paste("DEBUG: data_uploads set to:", data_uploads))
  return(data_uploads)
}
