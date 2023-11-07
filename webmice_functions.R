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
impute <- function(data, maxit, m, seed, 
                   blocks, parcel, predictorMatrix, ignore, where,
                   visitSequence, method) {
  imp <- list()
  imp$error <- ""
  result <- tryCatch(
    {
      imp <- mice(data, maxit = maxit, m = m, seed = seed,
                  predictorMatrix = predictorMatrix, 
                  blocks = blocks, parcel = parcel,
                  ignore = ignore, where = where,
                  visitSequence = visitSequence, method = method
      )
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
  } else {
    san_pm <- sanitize_predictorMatrix(params$predictorMatrix, nvar)
    if (!is.null(san_pm$error)) {
      imp$error <- san_pm$error
      return(imp)
    }
    pm <- san_pm$pm
  }

  if (is.null(params$blocks)) {
    params$blocks <- names(df)
  }

  if (is.null(params$parcel)) {
    parcel <- NULL
  } else {
    san_parcel <- sanitize_parcel(params$parcel)
    if (!is.null(san_parcel$error)) {
      imp$error <- san_parcel$error
      return(imp)
    }
    parcel <- san_parcel$parcel
  }

  if (is.null(params$m)) {
    params$m <- 5 #default value
  }

  if (is.null(params$ignore)) {
    ign <- NULL
  } else {
    san_ign <- sanitize_ignore(params$ignore, nobs)
    if (!is.null(san_ign$error)) {
      imp$error <- san_ign$error
      return(imp)
    }
    ign <- san_ign$ign
  }

  if (is.null(params$where)) {
    whr <- is.na(df)
  } else {
    san_whr <- sanitize_where(params$where, nobs, nvar)
    if (!is.null(san_whr$error)) {
      imp$error <- san_whr$error
      return(imp)
    }
    whr <- san_whr$whr
  }

  if (is.null(params$visitSeq)) {
    visitSeq <- NULL
  } else {
    if (typeof(params$visitSeq) == "character") {
      visitSeq <- params$visitSeq
    } else {
       imp$error <- "VisitSequence is not a vector of type character."
       return(imp)
    }
  }

  if (is.null(params$method)) {
    method <- NULL
  } else {
    if(typeof(params$method) == "character" | is.vector(params$method)) {
      method <- params$method
    } else {
      imp$error <- "Method is not a vector or character."
      return(imp)
    }
  }
  
  imp <- impute(df, maxit = params$maxit, m = params$m, 
                seed = params$seed,
                blocks = params$blocks,
                parcel = parcel,
                predictorMatrix = pm,
                ignore = ign,
                where = whr, 
                visitSequence = visitSeq,
                method = method
                )
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
