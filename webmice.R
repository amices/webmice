library(RestRserve)
library(mice, warn.conflicts = FALSE)
library(jsonlite)
library(readr)
library(digest)

#' Parameters
#' Code location in Docker, set through bash variable
base_folder <- Sys.getenv("WEBMICE_LOC")
if(base_folder == "") {
  base_folder <- getwd()
  print('No base folder for webmice set (export WEBMICE_LOC="directory").')
  print(paste("Set to: ", base_folder))
}

#' Imports
source(file.path(base_folder, "webmice_handlers.R"))
source(file.path(base_folder, "webmice_functions.R"))

#' Data upload location
data_uploads = file.path(base_folder, "data_uploads")
if(!file.exists(data_uploads)){
  dir.create(data_uploads)
}

#' Application
webmice = Application$new()

#' Endpoints
webmice$add_post(
  path = "/data",
  FUN = function(request, response) {
    cnt <- request$get_file("csvfile") # 'csv' from the upload form field
    # parse CSV
    dt <- read_csv(cnt)
    hash <- md5_string(request$parameters_body$csvfile)
    tmp <- file.path(data_uploads, hash)
    write_csv(dt, tmp)

    # set output body
    response$append_header("data_token", hash)
  }
)

webmice$add_get(path = "/version", FUN = mice_version_handler)
webmice$add_get(path = "/exampledata", FUN = example_data_handler)
webmice$add_get(path = "/long", FUN = impute_longfmt_handler)
webmice$add_get(path = "/fit", FUN = fit_handler)
webmice$add_get(path = "/pool", FUN = pool_handler)

#' Swagger 
yaml_file = file.path(base_folder, "openapi.yaml")
webmice$add_openapi(path = "/openapi.yaml", file_path = yaml_file)
webmice$add_swagger_ui(path = "/doc", path_openapi = "/openapi.yaml", use_cdn = TRUE)

backend = BackendRserve$new()
backend$start(webmice, http_port = 8080)
