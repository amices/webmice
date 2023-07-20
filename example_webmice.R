library(mice, warn.conflicts = FALSE)
library(RestRserve)
library(rjson)
library(readr)
library(digest)

webmice = Application$new()
base_folder = file.path("", "home", "webmice") #used for data uploads
print(base_folder)
webmice_folder = file.path(base_folder, "testdata", "upload") #used for data uploads

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

list_to_df = function(data){
  df <- data.frame(age=unlist(data_list$data$age), bmi=as.numeric(unlist(data_list$data$bmi)), 
                 hyp=as.numeric(unlist(data_list$data$hyp)), chl=as.numeric(unlist(data_list$data$chl)))
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
    tryCatch({
      data <- read.csv(path)
      return(data)
    }, error = function(e){
      return(NULL)
    })
}

impute = function(data, maxit, m, seed) {
  imp <- list()
  imp$error <- ""
  result = tryCatch({
    imp <- mice(data, maxit = maxit, m = m, seed = seed)
    return(imp)
  }, error = function(e) {
    return("Failure: mice")
  })

  if(result == "Failure: mice"){
    imp$error <- result
    return(imp)
  }
}

# Calls mice with parameters provided in a list 'params'
# Expected: params$data, params$maxit, params$m, params$seed
# data: "nhanes", "nhanes2", or a list that can be turned into a data.frame
call_mice = function(params){
  imp <- list()
  result = tryCatch({
    data <- get(params$data)
  }, error = function(e){
    return(-1)
  })

  if(typeof(result) == "list") {
    print("DEBUG: Calculating on example data set")
    imp <- impute(data, maxit = params$maxit, m = params$m,
                  seed = params$seed)
    return(imp)
  }
  # Full local path of a csv file, needs to be present on the server
  if(typeof(params$data) == "character" && endsWith(params$data, ".csv")){
    print("DEBUG: Read csv")
    df = read_file(params$data)
    if(is.null(df)){
      imp$error <- "Failure: reading local csv file"
      return(imp)
    }
    imp <- impute(nhanes, maxit = params$maxit, m = params$m,
                  seed = params$seed)
    return(imp)
  }
  # Retrieve uploaded csv file by hash
  if(typeof(params$data) == "character"){
    print("DEBUG: Retrieve uploaded file")
    df = read_file(file.path(webmice_folder, params$data))
    if(is.null(df)){
      imp$error <- "Failure: reading file on server"
      return(imp)
    }
    imp <- impute(nhanes, maxit = params$maxit, m = params$m,
                  seed = params$seed)
    return(imp)
  }
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

md5_string = function(string) {
  return(digest(paste(Sys.time(), string), algo="md5", serialize=F))
}

# Uploads a file and stores it in webmice_folder
#TODO: Swagger does not work with this function
#library(httr)
#tmp = "testdata/nhanes.csv"
#rs <- POST(url = "http:/127.0.0.1:8080/data", body = list(csvfile = upload_file(tmp)), encode = "multipart")
# Returns a data_token
# token <- rs$headers$data_token
webmice$add_post(
  path = "/data",
  FUN = function(request, response) {
    print(str(request$body))
    print(str(request$files))
    print(request$parameters_body$csvfile)
    cnt <- request$get_file("csvfile") # 'csv' from the upload form field
    # parse CSV
    print(cnt)
    dt <- read_csv(cnt)
    print(dt)
    hash <- md5_string(request$parameters_body$csvfile)
    tmp = file.path(webmice_folder, hash)
    print(tmp)
    write_csv(dt, tmp)
    
    # set output body
    response$append_header("data_token", hash)
  }
)


webmice$add_get(path = "/exampledata", FUN = example_data_handler)
webmice$add_get(path = "/imputation", FUN = impute_handler)
yaml_file = file.path(base_folder, "openapi.yaml")
webmice$add_openapi(path = "/openapi.yaml", file_path = yaml_file)
webmice$add_swagger_ui(path = "/doc", path_openapi = "/openapi.yaml", use_cdn = TRUE)

backend = BackendRserve$new()
backend$start(webmice, http_port = 8080)
