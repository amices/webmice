# *webmice*: Experimental REST API

## Summary

We provide a very easy example of a REST API for the R package *mice*. The HTTP server is run locally on http://localhost:8080/ and also runs a Swagger API endpoint on http://localhost:8080/doc.

## Start the REST API
### Docker
```
docker build -t webmice -f docker/Dockerfile .
docker run -it -p 8080:8080 -v </path/to/local/datafolder>:/home/webmice/testdata webmice
# get a shell inside of the docker image for debugging
docker exec -it webmice bash
```
You only need the `-v </path/to/local/datafolder>:/home/webmice/testdata` flag if you want to make some of your local data known to the docker image to use them as input data for the HTTP API.

Navigate to your browser http://localhost:8080/doc

### Local deploy
If you run the API locally please export the variable `WEBMICE_LOC`

```
export WEBMICE_LOC="<path to the webmice folder>"
```

Start the app:

```sh
Rscript webmice.R
```
Navigate to your browser http://localhost:8080/doc



