library('httr')

source('conversion_utils.R')

# This URL is exactly the provided example of the imputation endpoint,
# requested from local host.
response <- GET("http://127.0.0.1:8080/imputation?payload=%7B%22data%22%3A%22nhanes%22%2C%22maxit%22%3A2%2C%22m%22%3A2%2C%22seed%22%3A1%7D")
json <- content(response, as="parsed", type="application/json")

mids_obj <- json_to_mids(json)
