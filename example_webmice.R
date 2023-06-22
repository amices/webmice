library(mice, warn.conflicts = FALSE)
library(RestRserve)
library(rjson)
library(readr)
webmice = Application$new()
base_folder = file.path("path", "to", "repository", "micetestrestapi")
webmice_folder = file.path(base_folder, "testdata", "upload")

# Fetches example data from mice, returns data as json
example_data_to_json = function(name) {
  if(name == "nhanes") return(to_json(nhanes))
  if(name == "nhanes2") return(to_json(nhanes2))
}

# Takes a json string and returns it as R list 
# Test:
# input <- list(data = "nhanes", maxit = 2, m = 2, seed = 1)
# result <- json_to_input(to_json(input))
json_to_input = function(json_payload){
  result = tryCatch({
    params = fromJSON(json_payload)
    return(params)
  }, error = function(e)  {
    return(NULL)
  })
}

list_to_micedf = function(data){
  #TODO: take a look at conversion from json to dataframe
  #df <- data.frame(age=unlist(params$data$age), bmi=unlist(params$data$bmi), 
  #                 hyp=unlist(params$data$hyp), chl=unlist(params$data$chl))
  #df[l == "NA"] = NA
  # > imp <- mice(df, maxit = params$maxit, m = params$m, seed = params$seed)
  #Error in edit.setup(data, setup, ...) : 
  #  `mice` detected constant and/or collinear variables. No predictors were left after their removal.
  print("DEBUG: not implemented")
  return(NULL)
}

# Turn imp object to a json
# imp <- mice(nhanes, maxit = 2, m = 2, seed = 1)
# impjson <- imp_to_json(imp)
imp_to_json = function(imp){
  #to list
  jsonlist <- NULL
  #6 is a call not a data structure; toJSON(toString(imp[6]))
  for (i in 1:length(imp)){
    result = tryCatch({
      jsonlist[i] = toJSON(imp[i])
    }, error = function(e){ return(FALSE) })

    if( result == FALSE){
      tmp <- c(toString(imp[i]))
      names(tmp) <- names(imp[i])
      jsonlist[i] = toJSON(tmp)
    }
  }
  return(toJSON(jsonlist))
}

# Turn an impjson to imp again
json_to_imp = function(impjson){
  jsonlist <- fromJSON(impjson)
  data_list <- NULL
  names <- c()
  for (i in 1:length(jsonlist)){
    elem <- fromJSON(jsonlist[i])
    names[i] <- names(elem)[1]
    data_list[i] <- elem
  }
  names(data_list) <- names
  
  #TODO: convert data to data frame, inspect other properties for formatting etc
  return(data_list)
}

read_file = function(path){
  if(endswith(path, ".csv")){
    tryCatch({
      data <- read.csv(path)
      return(data)
    }, error = function(e){
      return(NULL)
    })
  }
}

# Calls mice with parameters provided in a list 'params'
# Expected: params$data, params$maxit, params$m, params$seed
# data: "nhanes", "nhanes2", or a list that can be turned into a data.frame
call_mice = function(params){
  imp <- list()
  imp$error <- ""
  if(all(params$data == "nhanes")) {
    print("DEBUG: Calculating on nhanes")
    result = tryCatch({
      imp <- mice(nhanes, maxit = params$maxit, m = params$m, 
                  seed = params$seed)
      return(imp)
      }, error = function(e)  {
        return("Failure: mice-nhanes")
      })
    if(result == "Failure: mice-nhanes"){
      imp$error <- result
      return(imp)
    }
  } 

  if(all(params$data == "nhanes2")){
    print("DEBUG: Calculating on nhanes2")
    result = tryCatch({
      imp <- mice(nhanes2, maxit = params$maxit, m = params$m, 
                  seed = params$seed)
      return(imp)
    }, error = function(e)  {
      return("Failure: mice-nhanes2")
    })
    if(result == "Failure: mice-nhanes2"){
      imp$error <- result
      return(imp)
    }
  }

  # Full local path of a csv file, needs to be present on the server
  if(typeof(params$data) == "character" && endsWith(params$data, ".csv")){
    print("DEBUG: Read csv")
    result = tryCatch({
      df = read_file(params$data)
    }, error = function(e)  {
       print("Error")
       return("Failure: reading csv file, path not known")
    })
    if(result == "Failure: reading csv file, path not known"){
      imp$error <- result
      return(imp)
    }
    if(is.null(df)) {
      imp$error <- "Failure: reading csv file"
      return(imp)
    }
    result = tryCatch({
      imp <- mice(df, maxit = params$maxit, m = params$m,
                  seed = params$seed)
      return(imp)
    }, error = function(e)  {
      return("Failure: mice-csvfile")
    })
    if(result == "Failure: mice-csvfile"){
      imp$error <- "Failure: mice-csvfile"
      return(imp)
    }
  }
  # Data encoded as json.
  if(typeof(params$data) == "list"){
    print("DEBUG: Convert json to df")
    result = tryCatch({
      #TODO: data is not correctly converted from json to df
      df = list_to_micedf(params$data)
      if(is.null(df)){
        imp$error <- "Failure: reading json data"
        return(imp)
      }
      imp <- mice(df, maxit = params$maxit, m = params$m, 
                  seed = params$seed)
      return(imp)
      }, error = function(e)  {
        return("Failure: mice-jsondata")
    })
    if(result == "Failure: mice-jsondata"){
      imp$error <- "Failure: mice-jsondata"
      return(imp)
    }
  }
  imp$error <- "Dataset not known"
  return(imp)
}

impute_handler = function(.req, .res) {
  json_payload = as.character(.req$parameters_query[["payload"]])
  
  if (length(json_payload) == 0L) {raise(HTTPError$bad_request())}
  #check if convertible to json
  params = json_to_input(json_payload)

  # impute function needs data, maxit, m, seed
  if(is.null(params$data)) {raise(HTTPError$not_acceptable())}
  if(is.null(params$maxit)) {raise(HTTPError$not_acceptable())}
  if(is.null(params$m)) {raise(HTTPError$not_acceptable())}
  if(is.null(params$seed)) {raise(HTTPError$not_acceptable())}
  
  print("DEBUG: Calling mice")
  imp <- call_mice(params = params)
  
  .res$set_body(imp_to_json(imp = imp))
  .res$set_content_type("text/plain")
}

example_data_handler = function(.req, .res) {
  example_name = as.character(.req$parameters_query[["name"]])
  test_data = c("nhanes", "nhanes2")
  if (!(example_name %in% test_data)) {
    raise(HTTPError$not_found())
  }
  .res$set_body(example_data_to_json(example_name))
  .res$set_content_type("text/plain")
}

# Uploads a file and stores it in webmice_folder
#TODO: Swagger does not work with this function
#library(httr)
#tmp = "testdata/nhanes.csv"
#rs <- POST(url = "http:/127.0.0.1:8080/data", body = list(csvfile = upload_file(tmp)), encode = "multipart")
webmice$add_post(
  path = "/data",
  FUN = function(request, response) {
  # for debug
  print(request$parameters_body)
  #str(request$files)
  # extract multipart body field
  cnt <- request$get_file("csvfile") # 'csv' from the upload form field
  # parse CSV
  dt <- read_csv(cnt)
  tmp = file.path(webmice_folder, request$parameters_body$csvfile)
  print(tmp)
  write_csv(dt, tmp)

  # set output body
  response$set_body("Data received") # or simply response$set_body(format_csv(dt))
  response$set_content_type("text/plain")
  }
)
webmice$add_get(path = "/exampledata", FUN = example_data_handler)
webmice$add_get(path = "/imputation", FUN = impute_handler)
yaml_file = file.path(base_folder, "openapi.yaml")
webmice$add_openapi(path = "/openapi.yaml", file_path = yaml_file)
webmice$add_swagger_ui(path = "/doc", path_openapi = "/openapi.yaml", use_cdn = TRUE)

backend = BackendRserve$new()
backend$start(webmice, http_port = 8080)
