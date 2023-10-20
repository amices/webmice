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

#' Takes a dataframe and checks for all columns thta consist of strings that there are only 25
#' unique values.
#'
#' @param data An R dataframe
check_factors <- function(data) {
  cols <- names(data)
  for (c in cols) {
    if (unique(sapply(data[[c]], typeof)) == list('character') & length(levels(factor(data[[c]]))) > 25) {
      return(paste("Too many factors", c, length(levels(factor(data[[c]])))))
    }
  }
  return("")
}


#' Takes the content of the data parameter from a call to the impute endpoint
#' and returns a dataframe.
#'
#' @param param_data A string with the name of the dataset, a hash from an
#' uploaded file, or a csv file
sanitize_data <- function(param_data) {
  return_list <- list()
  result <- tryCatch(
    {
      data <- get(param_data)
    },
    error = function(e) {
      return(-1)
    }
  )
  
  if (typeof(result) == "list") {
    print("DEBUG: Imputation on example data set")
    check <- check_factors(data)
    if (check == "") {
      return_list$df <- data
      return(return_list)
    } else {
      return_list$error <- check
      return(return_list)
    }
  }

  if (typeof(param_data) == "character" && endsWith(param_data, ".csv")) {
    print("DEBUG: Imputation on local csv file")
    df <- read_file(param_data)
    if (is.null(df)) {
      return_list$error <- "Failure: reading local csv file"
    }
    else {
      check <- check_factors(df)
      if (check == "") {
        return_list$df <- df
      } else {
        return_list$error <- check
      }
    }
    return(return_list)
  }

  if (typeof(param_data) == "character") {
    print("DEBUG: Imputation on uploaded file")
    df <- read_file(file.path(get_data_uploads(), param_data))
    if (is.null(df)) {
      return_list$error <-
        "Failure: reading file, not an example dataset or file on server"
    }
    else {
      check <- check_factors(df)
      if (check == "") {
        return_list$df <- df
      } else {
        return_list$error <- check
      }
    }
    return(return_list)
  }

  return_list$error <- "Failure: unrecognized format for `data` parameter."
  return(return_list)
}

#' Takes the content of the predictorMatrix parameter from a call to the impute
#' endpoint and returns a matrix.
#'
#' @param param_pred A matrix of size p \* p or a vector of length p \* p (where
#' p is the number of columns in the dataset) that can be converted to a square
#' matrix (reading by row). The order of variables in the matrix is assumed to
#' correspond to the order of the columns in the dataset.
#' @param nvar Number of columns in the dataset.
sanitize_predictorMatrix <- function(param_pred, nvar) {
  return_list <- list()

  if (is.vector(param_pred)) {
    if (length(param_pred) != nvar * nvar) {
      return_list$error <-
        "Failure: predictorMatrix is not of the correct size."
      return(return_list)
    }
    param_pred <- matrix(param_pred, nrow = nvar, byrow = TRUE)
  }

  if (is.matrix(param_pred)) {
    if (nrow(param_pred) != ncol(param_pred)) {
      return_list$error <- "Failure: predictorMatrix is not square."
      return(return_list)
    }
    if (ncol(param_pred) != nvar) {
      return_list$error <-
        "Failure: predictorMatrix is not of the correct size."
      return(return_list)
    }
    if (!all(param_pred %in% c(0, 1))) {
      return_list$error <-
        "Failure: predictorMatrix contains non-binary values."
      return(return_list)
    }
    return_list$pm <- param_pred
    return(return_list)
  }

  return_list$error <- 
    "Failure: unrecognized format for `predictorMatrix` parameter."
  return(return_list)
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
impute <- function(data, maxit, m, seed, predictorMatrix) {
  imp <- list()
  imp$error <- ""
  result <- tryCatch(
    {
      imp <- mice(data, maxit = maxit, m = m, seed = seed,
                  predictorMatrix = predictorMatrix)
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
#' number of iterations; `m`: number of imputations, `seed`: seed,
#' `predictorMatrix`: a predictor matrix.
#' @seealso \link[mice]{mice}
call_mice <- function(params) {
  imp <- list()

  san_df <- sanitize_data(params$data)
  if (!is.null(san_df$error)) {
    imp$error <- san_df$error
    return(imp)
  }
  df <- san_df$df
  nobs <- nrow(df)
  nvar <- ncol(df)

  if (is.null(params$predictorMatrix)) {
    pm <- make.predictorMatrix(df)
  }
  else {
    san_pm <- sanitize_predictorMatrix(params$predictorMatrix, nvar)
    if (!is.null(san_pm$error)) {
      imp$error <- san_pm$error
      return(imp)
    }
    pm <- san_pm$pm
  }

  if (is.null(params$m)) {
    params$m <- 5 #default value
  }

  imp <- impute(df, maxit = params$maxit, m = params$m, seed = params$seed,
                predictorMatrix = pm)
  return(imp)
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
