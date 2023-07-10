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
           m = {m <- as.integer(fromJSON(row))},
           where = {where <- as.logical(fromJSON(row)$where)},
           blocks = {blocks <- lapply(fromJSON(row)$blocks, function(x) {as.character(x)})},
           # call = call,
           nmis = {nmis <- lapply(fromJSON(row)$nmis, function(x) {as.integer(x)})},
           # method = method,
           predictorMatrix = {predictorMatrix <- as.numeric(fromJSON(row)$predictorMatrix)},
           visitSequence = {visitSequence <- as.character(fromJSON(row)$visitSequence)},
           # formulas = formulas,
           post = {post <- lapply(fromJSON(row)$post, function(x) {as.character(x)})},
           # blots = blots,
           ignore = {ignore <- as.logical(fromJSON(row)$ignore)},
           seed = {seed <- as.integer(fromJSON(row))},
           iteration = {iteration <- as.integer(fromJSON(row))},
           lastSeedValue = {lastSeedValue <- as.integer(fromJSON(row)$lastSeedValue)},
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

  # Optionally do some additional sanity checks here, like dimensions, etc.

  # Reconstruct data
  midsobj <- list(
    # TODO:
    # data = data,
    # imp = imp,
    m = m,
    where = where, # should this be a proper matrix?
    blocks = blocks,
    # call = call,
    nmis = nmis,
    # method = method,
    predictorMatrix = predictorMatrix, # should this be a proper matrix? 
    visitSequence = visitSequence,
    # formulas = formulas,
    post = post,
    # blots = blots,
    ignore = ignore,
    seed = seed,
    iteration = iteration,
    lastSeedValue = lastSeedValue
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
