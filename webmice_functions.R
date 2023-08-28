#' Data
#' Fetches example data from mice, returns data as json
example_data_to_json = function(name) {
  result = tryCatch({
    return(toJSON(get(name)))
  }, error = function(e)  {
    err <- c()
    err$error <- "Error: data name not found"
    return(toJSON(err))
  })
  return(result)
}

#' Reads a csv file
read_file = function(path){
    tryCatch({
      data <- read.csv(path)
      return(data)
    }, error = function(e){
      return(NULL)
    })
}

#' Creates a hash for a data file name
md5_string = function(string) {
  return(digest(paste(Sys.time(), string), algo="md5", serialize=F))
}

#' Convert JSON to R
#' Takes a json string and returns it as R list 
#' Test:
#' input <- list(data="nhanes", maxit=2, m=2, seed=1)
json_to_parameters = function(json_payload){
  result = tryCatch({
    params = fromJSON(json_payload)
    return(params)
  }, error = function(e)  {
    return(NULL)
  })
}

#' Mice return functions
#' Takes the result of the imputation and returns the long format of the data
imp_result_long_fmt = function(imp){
  return(toJSON(complete(imp, "long")))
}

imp_result_pred_matrix = function(imp){
  res <- c()
  res$error <- "not implemented"
  return(toJSON(res))
}

#' Mice functions
impute = function(data, maxit, m, seed) {
  imp <- list()
  imp$error <- ""
  result = tryCatch({
    imp <- mice(data, maxit=maxit, m=m, seed=seed)
    return(imp)
  }, error = function(e) {
    return("Failure: mice")
  })

  if(result == "Failure: mice"){
    imp$error <- result
    return(imp)
  }
}

#' Calls mice's imputation function with parameters provided in a list 'params'
#' Expected: params$data, params$maxit, params$m, params$seed
#' data: example data name, a hash from an uploaded file, or a csv filee
call_mice = function(params){
  imp <- list()
  result = tryCatch({
    data <- get(params$data)
  }, error = function(e){
    return(-1)
  })
  
  if(typeof(result) == "list") {
    print("DEBUG: Imputation on example data set")
    imp <- impute(data, maxit=params$maxit, m=params$m, seed=params$seed)
    return(imp)
  }
  if(typeof(params$data) == "character" && endsWith(params$data, ".csv")){
    print("DEBUG: Imputation on local csv file")
    df <- read_file(params$data)
    if(is.null(df)){
      imp$error <- "Failure: reading local csv file"
      return(imp)
    }
    imp <- impute(nhanes, maxit=params$maxit, m=params$m, seed=params$seed)
    return(imp)
  }
  if(typeof(params$data) == "character"){
    print("DEBUG: Imputation on uploaded file")
    df <- read_file(file.path(data_uploads, params$data))
    if(is.null(df)){
      imp$error <- "Failure: reading file, not an example dataset or file on server"
      return(imp)
    }
    imp <- impute(df, maxit=params$maxit, m=params$m, seed=params$seed)
    return(imp)
  }
}

call_with = function(data, model, formula){
  fit <- c()
  if(model == "lm"){
    fit <- with(data, lm(as.formula(formula)))
  }
  if(model == "glm"){
    fit <- with(data, glm(as.formula(formula)))
  } 
  fit$error <- "Model not known"
  return(toJSON(summary(fit), force=TRUE))
}

call_pool = function(data){
  pool <- c()
  if(packageVersion("mice") < "3.16.4" ){
    pool$error <- "ERROR pool.table: need mice version 3.16.4 or higher"
    return(toJSON(pool, force=TRUE))
  }

  if(typeof(data) == "list"){
    pool <- pool.table(data)
  } else {
    pool$error <- "Input data not of correct type (summary(fit))"
  }
  return(toJSON(pool, force=TRUE))
}
