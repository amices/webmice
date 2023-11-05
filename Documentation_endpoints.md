# Webmice REST API
## Endpoints
- version: get the current mioce version
- data/exampledata: get example data by its name
- data: upload a csv to the HTTP server, retrieve a hash `data_token` to reuse the data
- long: long format of the imputed data
- fit
- pool 

## The impute endpoint
The function `mice` takes a lot of different arguments.
Required arguments:

- data: full path to a csv file, name of an example dataset in the mice package or a hash from the data endpoint
- maxit: as defined in mice, integer number
- m: as defined in mice, integer number
- seed: as defined in mice, integer number

Optional arguments:

- *predictorMatrix*: a list of numbers of length columns x columns; a matrix of size columns x columns; 
 
 Example (3 variables/columns):  
>  "predictorMatrix": [[0,1,1,1],[1,0,1,1],[1,1,0,1],[1,1,1,0]] 

- *blocks*: a json dictionary (in R a named list) mapping variables to blocks of variables;

 Example:
> "blocks": {"b1": ["chl", "hyp"], "b2": ["bmi"]}

- *parcel*: see blocks
- *ignore*: a list of length number of rows with the values true or false
 
 Example:
> "ignore":[true,true,true,true,true,true,true,true,true,true,true,true,true,true, true,false,false,false,false,false,false,false,false,false,false]

- *where*: a list of length rows x columns, or a matrix of the same size as the data, of avlues true and false

 Example:
> "where":[[false,true,true,true],[false,false,false,false],[false,true,false,false], [false,true,true,true],[false,false,false,false],[false,true,true,false], [false,false,false,false],[false,false,false,false],[false,false,false,false], [false,true,true,true],[false,true,true,true],[false,true,true,true], [false,false,false,false],[false,false,false,false],[false,false,false,true], [false,true,true,true],[false,false,false,false],[false,false,false,false], [false,false,false,false],[false,false,false,true],[false,true,true,true], [false,false,false,false],[false,false,false,false],[false,false,false,true], [false,false,false,false]]