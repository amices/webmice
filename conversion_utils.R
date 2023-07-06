library('rjson')

#' Convert a (well-formed) (list of) json strings into a MIDS object.
#'
#' @param json List of string representations of the JSON.
#'
#' @return MIDS object.
#'
#' @examples
#' TODO
json_to_mids <- function(json) {
  for (row in json) {
    # Specify parsing method for each expected key
    switch(names(fromJSON(row)),
           # data={data <- parseData(row)},
           # imp={imp <- parseImp(row)},
           m={m <- as.integer(fromJSON(row))},
           # where={where <- parseWhere(row)},
           # blocks = blocks,
           # call = call,
           # nmis = nmis,
           # method = method,
           # predictorMatrix = predictorMatrix,
           # visitSequence = visitSequence,
           # formulas = formulas,
           # post = post,
           # blots = blots,
           # ignore = ignore,
           seed={seed <- as.integer(fromJSON(row))},
           iteration={iteration <- as.integer(fromJSON(row))},
           # lastSeedValue = get(".Random.seed",
           #                     envir = globalenv(), mode = "integer",
           #                     inherits = FALSE
           # ),
           # chainMean = q$chainMean,
           # chainVar = q$chainVar,
           # loggedEvents = loggedEvents,
           # version = packageVersion("mice"),
           # date = Sys.Date()
    )
  }

  # Test if all members have been instantiated
  attr_list = c("data", "imp", "m", "where", "blocks", "call", "nmis", "method",
                "predictorMatrix", "visitSequence", "formulas", "post", "blots",
                "ignore", "seed", "iteration", "lastSeedValue", "chainMean",
                "chainVar", "loggedEvents", "version", "date")
  attr_exist = sapply(attr_list, function(x) {exists(x)})
  # if (!all(attr_exist)) {
  #   stop(paste("The following members of the mids class were missing: ",
  #              paste(attr_list[!attr_exist], collapse = ', ')))
  # }

  # Reconstruct data
  midsobj <- list(
    # TODO:
    # data = data,
    # imp = imp,
    m = m,
    # where = where,
    # blocks = blocks,
    # call = call,
    # nmis = nmis,
    # method = method,
    # predictorMatrix = predictorMatrix,
    # visitSequence = visitSequence,
    # formulas = formulas,
    # post = post,
    # blots = blots,
    # ignore = ignore,
    seed = seed,
    iteration = iteration
    # lastSeedValue = lastSeedValue,
    # chainMean = chainMean,
    # chainVar = chainVar,
    # loggedEvents = loggedEvents,
    # version = packageVersion("mice"), # Do we want the version of the original mids, or of the current instance?
    # date = Sys.Date() # Same for the date
  )
  oldClass(midsobj) <- "mids"

  midsobj
}

parseData <- function(row) {
  "test"
}

parseImp <- function(row) {
  "test"
}

parseWhere <- function(row) {
  "test"
}