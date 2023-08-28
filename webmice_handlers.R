pool_handler = function(.req, .res) {
  poolJson <- ''
  json_payload <- as.character(.req$parameters_query[["payload"]])
  if (length(json_payload) == 0L) {raise(HTTPError$bad_request())}
  params <- json_to_parameters(json_payload)
  if(is.null(params$data)) {poolJson <- "Error: no data"}
  if(poolJson == ''){
    print("DEBUG: Calling pool.table (requires 3.16.4)")
    poolJson <- call_pool(params$data)
  }
  .res$set_body(poolJson)
  .res$set_content_type("text/plain")
}

fit_handler = function(.req, .res) {
  fitJson <- ''
  json_payload <- as.character(.req$parameters_query[["payload"]])
  # if answers are copied straight from the Swagger interface, there are too many backslashes
  # json_payload <- gsub('\\\\', '', input)
  if (length(json_payload) == 0L) {raise(HTTPError$bad_request())}
  params <- json_to_parameters(json_payload)
  if(is.null(params$data)) {fitJson <- "Error: no data"}
  if(is.null(params$model)) {fitJson <- "Error: no model"}
  if(is.null(params$formula)) {fitJson <- "Error: no formula"}

  if(fitJson == ''){
    print("DEBUG: Calling with (fitting function)")
    fitJson <- call_with(params$data, params$model, params$formula)
  }
  .res$set_body(fitJson)
  .res$set_content_type("text/plain")
}

impute_longfmt_handler = function(.req, .res) {
  json_payload <- as.character(.req$parameters_query[["payload"]])
  
  if (length(json_payload) == 0L) {raise(HTTPError$bad_request())}
  # check if convertible to json
  params <- json_to_parameters(json_payload)

  # impute function needs data, maxit, m, seed
  if(is.null(params$data)) {raise(HTTPError$not_acceptable())}
  if(is.null(params$maxit)) {raise(HTTPError$not_acceptable())}
  if(is.null(params$m)) {raise(HTTPError$not_acceptable())}
  if(is.null(params$seed)) {raise(HTTPError$not_acceptable())}
  
  print("DEBUG: Calling mice")
  imp <- call_mice(params)

  if(is.null(imp$error)){
    .res$set_body(imp_result_long_fmt(imp))
  } else{
    .res$set_body(toJSON(imp))
  }
  .res$set_content_type("text/plain")
}

example_data_handler = function(.req, .res) {
  example_name <- as.character(.req$parameters_query[["name"]])
  .res$set_body(example_data_to_json(example_name))
  .res$set_content_type("text/plain")
}

mice_version_handler = function(.req, .res) {
  version <- list()
  version$mice <- sessionInfo("mice")$otherPkgs$mice$Version
  .res$set_body(toJSON(version))
  .res$set_content_type("text/plain")
}
