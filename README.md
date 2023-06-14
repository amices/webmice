# *webmice*: Experimental REST API

## Summary

We provide a. very easy example of a REST API for the R package *mice*. The HTT server is run locally on http://localhost:8080/ and also runs a Swagger API endpoint on http://localhost:8080/doc.

## Endpoints
- */exampledata*: retrieves the tywo example datasets *nhanes* and *nhanes2*
- */imputation*: calls mice and returns the imputed data. Currently only runs on the two example datasets.

## Start the REST API
```sh
Rscript example_webmice.R
```
Navigate to your browser http://localhost:8080/doc.