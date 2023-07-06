#' Convert a (well-formed) (list of) json strings into a MIDS object.
#'
#' @param json List of string representations of the JSON.
#'
#' @return MIDS object.
#'
#' @examples
#' TODO
json_to_mids <- function(json) {
  midsobj <- list(
    # TODO:
    # data = data,
    # imp = q$imp,
    # m = m,
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
    # seed = seed,
    # iteration = q$iteration,
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
  oldClass(midsobj) <- "mids"

  midsobj
}