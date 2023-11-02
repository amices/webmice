#' Takes a dataframe and checks for all columns thta consist of strings that there are only 50
#' unique values.
#'
#' @param data An R dataframe
check_factors <- function(data) {
  cols <- names(data)
  for (c in cols) {
    if (unique(sapply(data[[c]], typeof)) == list('character') & length(levels(factor(data[[c]]))) > 50) {
      return(paste("Too many factors", c, length(levels(factor(data[[c]])))))
    }
  }
  return(NULL)
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
      return(FALSE)
    }
  )

  if (typeof(result) == "list") {
    print("DEBUG: Imputation on example data set")
    check <- check_factors(data)
    if (is.null(check)) {
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

sanitize_parcel <- function(parcel_input) {
  return_list <- list()
  result <- tryCatch(
    {
      return_list$parcel <- make.parcel(parcel_input)
    },
    error = function(e) {
      return(e)
    }
  )
  if (is.null(return_list$parcel)) {
    print(error)
    return_list$error <- result
  }
  return(return_list)
}