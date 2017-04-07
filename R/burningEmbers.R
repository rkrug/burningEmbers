#' create \code{burningEmbers} object
#'
#'Create a \code{burningEmbers} object from the given parameter.
#'
#' @param bot bottom of the colour cascade, i.e. lowest value
#' @param top top of the colour cascade, i.e. highest value
#' @param colNames names of the different columns in the graph
#'
#' @return object of class \code{burningEmbers} which can be plotted.
#' @importFrom grDevices heat.colors
#' @importFrom graphics abline axis image rect
#' @export
#'
#' @examples
#' x <- burningEmbers()
#' plot(x)
#'
#'x <- burningEmbers(
#'                    bot = c(1, -2, 3, -3),
#'                    top = c(2,  5, 4, -2)
#'                    )
#' plot(x)
#'
burningEmbers <- function(bot,
                          top,
                          colNames = NULL) {
  x <- list()

  if ((missing(top) &
       !(missing(bot)))
      | (missing(top) & !(missing(top)))) {
    stop(
      "top and bot have to be specified to be used! If none are specified, a sample object is created."
    )
  } else if ((missing(bot) & missing(top))) {
    x$colRange = data.frame(bot = sin(1:5),
                            top = (cos(1:5) + 1.1) * 4)
    row.names(x$colRange) <-
      c("RFC1", "RFC2", "RFC3", "RFC4", "RFC5")
  } else {
    if (length(bot) != length(top)) {
      stop("bot and top have to have the same length!")
    }
      if (any(top < bot)) {
        stop("All colRange$top have to be larger than colRange$bot!")
      }

    x$colRange <- data.frame(bot = bot,
                             top = top)
    colnames(x$colRange) <- colNames
  }


  attr(x, "class") <- "burningEmbers"
  return(x)
}
