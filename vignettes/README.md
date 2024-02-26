# `webmice`: Experimental REST API for Multivariate Imputation by Chained Equations

<a title="Five by Five, CC0, via Wikimedia Commons" href="https://commons.wikimedia.org/wiki/File:API_-_The_Noun_Project.svg"><img width="110" alt="API - The Noun Project" src="https://upload.wikimedia.org/wikipedia/commons/thumb/f/f6/API_-_The_Noun_Project.svg/512px-API_-_The_Noun_Project.svg.png"></a> 
<a href="https://amices.org/mice/"><img src="https://github.com/amices/mice/blob/master/man/figures/logo.png?raw=true" width="100" /></a> 

## Overview

WEBMICE is a web service for imputing and analysing incomplete datasets. WEBMICE provides a language-independent API to the functionality of the MICE R package. 
It can 

1. read data from local system of the client;
2. create multiple complete versions from incomplete data;
3. analyse each of the imputed datasets;
4. pool the analyses results into one result.

WEBMICE is a RESTful API that runs on a (remote) host. In principle, any `HTTP` client will work with WEBMICE. The following sections illustrate how a client can make requests to WEBMICE using various client languages. 

### Primary WEBMICE end points

  | Verb  | API Endpoint          | Description                                | Maps to `mice` function |
  |:------|:-------------------------- |:------------------------------------------ |:-------------------------|
  | GET  | `/version`       | Get the `mice`'s version                          | `utils::packageVersion("mice")`          |
  | GET  | `/exampledata`       | Get example of an incomplete data (`nhanes` and `nhanes2`)            | `mice::nhanes`  or `mice::nhanes2`   |
  | POST  | `/data`       | Upload an incomplete data in CSV format                          |          |
  | GET  | `/long`       | Impute missing data            | `mice::mice()`    |
  | GET  | `/fit`       | Get some predefined fit value                          | `mice::getfit(with())`        |
  | GET  | `/pool`       | Pool results of a fit given its summary table            | `mice::pool()`    
  |
The table lists the defined API end points and the internal mapping between the API end point and the corresponding R function. 

### Objective

This document provides a quick introduction into the main WEBMICE features, and how these can be assessed from `R` and from the bash command line.

## Installation

See the [installation guide](./vignettes/installation.Rmd).


## Summary

We provide a very easy example of a REST API for the R package `mice`. The HTTP server is run locally on http://localhost:8080/ and also runs a Swagger API endpoint on http://localhost:8080/doc.

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

## Acknowledgements

This project was supported by Utrecht University's [Open Science Fund](https://www.uu.nl/en/research/open-science/about-us/open-science-fund).

<a href="https://www.uu.nl/en"><img src="https://www.uu.nl/themes/custom/corp/src/images/uu-logo-en.svg" width="100" /></a> 
