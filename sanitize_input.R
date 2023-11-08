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

#' Takes the content of the ignore parameter from a call to the impute endpoint
#' and returns a logical vector.
#'
#' @param param_ign a vector of length n (where n is the number of rows in the
#' dataset)
#' @param nobs Number of rows in the dataset.
sanitize_ignore <- function(param_ign, nobs) {
  return_list <- list()
  
  if (is.vector(param_ign)) {
    if (length(param_ign) != nobs) {
      return_list$error <-
        "Failure: `ignore` vector is not of the correct size."
      return(return_list)
    }

    if (!is.logical(param_ign)) {
      return_list$error <-
        "Failure: `ignore` vector contains non-logical values."
      return(return_list)
    }

    return_list$ign <- param_ign
    return(return_list)
  }
  
  return_list$error <- 
    "Failure: unrecognized format for `ignore` parameter."
  return(return_list)
}

#' Takes the content of the where parameter from a call to the impute endpoint
#' and returns a matrix.
#'
#' @param param_where A matrix of size n \* p or a vector of length n \* p
#' (where n is the number of rows in the dataset and p is the number of columns)
#' that can be converted to a matrix (reading by row).
#' @param nobs Number of rows in the dataset.
#' @param nvar Number of columns in the dataset.
sanitize_where <- function(param_where, nobs, nvar) {
  return_list <- list()
  
  if (is.vector(param_where)) {
    if (length(param_where) != nobs * nvar) {
      return_list$error <-
        "Failure: `where` matrix is not of the correct size."
      return(return_list)
    }
    param_where <- matrix(param_where, nrow = nobs, byrow = TRUE)
  }
  
  if (is.matrix(param_where)) {
    if (nrow(param_where) != nobs) {
      return_list$error <- 
        "Failure: `where` matrix is not of the correct size."
      return(return_list)
    }
    if (ncol(param_where) != nvar) {
      return_list$error <-
        "Failure: `where` matrix is not of the correct size."
      return(return_list)
    }
    if (!is.logical(param_where)) {
      return_list$error <-
        "Failure: `where` matrix contains non-logical values."
      return(return_list)
    }
    return_list$whr <- param_where
    return(return_list)
  }
  
  return_list$error <- 
    "Failure: unrecognized format for `where` parameter."
  return(return_list)
}

#' Takes the content of the formula parameter and the variables/parcels from a call to the 
#' impute endpoint and checks whether all variables in the formula match the dataset variables 
#' or parcel names.
#'
#' @param param_formula A list of strings which can be converted to formulas
#' @param parcel_names Parcel names or dataset variable names
sanitize_formula <- function(param_formula, parcel_names) {
  return_list <- list()
  valid_formulas <- list()
  for (f in param_formula) {
    vars <- all.vars(as.formula(f))
    if (all(vars %in% parcel_names)) {
      append(valid_formulas, as.formula(f))
    }
    else {
      return_list$error <- c("Formula contains unknown variable", f)
      return(return_list)
    }
  }
  return_list <- valid_formulas
  return(return_list)
}

#' Takes the content of the dots parameter from a call to the impute endpoint
#' and returns a named list.
#'
#' @param param_where A named list containing at most q lists (one for each
#' parcel) of arguments to pass down to other imputation methods.
#' @param parcel_names Parcel names or dataset variable names
sanitize_dots <- function(param_dots, parcel_names) {
  return_list <- list()
  
  if (is.list(param_dots)) {
    if (length(unique(names(param_dots))) < length(names(param_dots))) {
      return_list$error <- 
        "Failure: duplicate names in `dots` argument."
      return(return_list)
    }

    if (!all(names(param_dots) %in% parcel_names)) {
      return_list$error <- 
        "Failure: unknown key(s) in `dots` argument."
      return(return_list)
    }
    
    for (dot in param_dots) {
      if (!is.list(dot)) {
        return_list$error <- 
          "Failure: unrecognized format for element in `dots` parameter."
        return(return_list)
      }
    }

    return_list$dot <- param_dots
    return(return_list)
  }

  return_list$error <- 
    "Failure: unrecognized format for `dots` parameter."
  return(return_list)
}
