# *webmice*: Experimental REST API

## Summary

We provide a very easy example of a REST API for the R package *mice*. The HTT server is run locally on http://localhost:8080/ and also runs a Swagger API endpoint on http://localhost:8080/doc.

## Endpoints
- */exampledata*: retrieves the tywo example datasets *nhanes* and *nhanes2*
- */imputation*: calls mice and returns the imputed data. Currently only runs on the two example datasets.

## Start the REST API
Adjust line 6 in `example_webmice.R` to the folder where the code lies:

```
base_folder = file.path("", "home", "webmice")
```

Start the app:

```sh
Rscript example_webmice.R
```
Navigate to your browser http://localhost:8080/doc

## Docker

```
docker build -t webmice -f docker/Dockerfile .
docker run -it -p 8080:8080 -v </path/tp/your local/datafolder>:/home/webmice/testdata webmice
# get a shell inside of the docker image for debugging
docker exec -it webmice bash
```
Navigate to your browser http://localhost:8080/doc
