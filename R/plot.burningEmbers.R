##' The \code{plot} method for the \code{burningEmbers} objects
##'
##' Plot a Burning Amber Diagram
##'
##' @title plot.burningEmbers
##'
##' @usage \method{plot}{burningEmbers}(
##'      x,
##'      ylim,
##'      labels,
##'      ny = 1000,
##'      relSpace = 7,
##'      add0line = TRUE,
##'      col = rev(heat.colors(1000)),
##'      borderB = "black",
##'      lwdB = 3,
##'      xlab = "",
##'      ylab = "",
##'      ...)
##'
##' @param x \code{burningEmbers} object to be plotted
##' @param ylim see \link{plot}
##' @param labels labels for the bars. If not specified, will be taken from the row names of the \code{x$colRange} parameter
##' @param ny default 1000; number of y blocks - 1000 should be sufficient
##' @param relSpace relative width of the gap between bars compared to width to bars
##' @param add0line default \code{TRUE}; if a line at \code{y = 0} should be drawn
##' @param col default \code{rev(heat.colors(1000))}; the colour to be used for the graph. The more values, the more continuous the gradient. 1000 should be fine.
##' @param borderB default "black"; the colour of the borders arounf=d the bars
##' @param lwdB default 3; the width of the borders arount=fd =he bars
##' @param xlab x label
##' @param ylab y label
##' @param ... additional parameter for the \code{image()} function. Examples are \code{main}, ...
##'
##' @return the data structur which was plotted
##' @author Rainer M. Krug
##' @export
##'
##' @examples
##' x <- burningEmbers()
##' plot(x)
##'
##' x <- burningEmbers(
##'                    bot = c(1, -2, 3, -3),
##'                    top = c(2,  5, 4, -2)
##'                    )
##' plot(x, main = "This is a header")
##' plot(x, main = "This is a header", ylim = c(-1, 3))
##'
plot.burningEmbers <- function(x,
                               ylim,
                               labels,
                               ny = 1000,
                               relSpace = 7,
                               add0line = TRUE,
                               col = rev(heat.colors(1000)),
                               borderB = "black",
                               lwdB = 3,
                               xlab = "",
                               ylab = "",
                               ...) {
  # Prepare arguments for plotting ------------------------------------------
  if (missing(labels)) {
    labels <- rownames(x$colRange)
  }

  if (missing(ylim)) {
    ylim <- range(x$colRange)
  }

  # Prepare data for plotting -----------------------------------------------


  # Helper Function converting value into color, taking into consideration  --------
  val2col <- function(val, colRange, noCols) {
    m <-  (noCols - 1) / (colRange[[2]] - colRange[[1]])
    b <- 1 - m * colRange[[1]]
    col <- m * val + b
    # Setting values below colRange to NA, above to noCols
    col[val < colRange[[1]]] <- NA
    col[val > colRange[[2]]] <- noCols
    #
    return(col)
  }

  # Cretae matrix for plotting ----------------------------------------------
  val <- seq(from = ylim[1],
             to = ylim[2],
             length.out = ny)
  xmat <- matrix(data = val,
                 ncol = nrow(x$colRange),
                 nrow = ny)
  for (i in 1:ncol(xmat)) {
    xmat[, i] <- val2col(
      val = xmat[, i],
      colRange = x$colRange[i, ],
      noCols = length(col)
    )
  }


  # repeat each column relSpace times and set the last one to NA. This creates a
  # space in the image to separate the columns
  xmat <- matrix(rep(t(xmat), each = relSpace),
                 ncol = relSpace * nrow(x$colRange),
                 byrow = TRUE)
  xmat[, relSpace * (1:nrow(x$colRange))] <- NA

  # Plot the graph ----------------------------------------------------------
  # plot matrix xmat
  image(
    x = 1:ncol(xmat) * (1 / relSpace) + 0.5,
    y = (seq(
      from = 1,
      to = 2 * nrow(xmat),
      by = 2
    ) / (2 * nrow(xmat))) * (ylim[2] - ylim[1]) + ylim[1],
    z = t(xmat),
    col = col,
    axes = FALSE,
    xlab = xlab,
    ylab = ylab,
    ...
  )

  # plot boxes around bars
  rect(
    xleft = (1:nrow(x$colRange)) - (0.5 - 1 / relSpace / 2) ,
    xright = (1:nrow(x$colRange)) + (0.5 - 1 / relSpace / 2),
    ybottom = ylim[1],
    ytop = ylim[2],
    border = borderB,
    lty = "solid",
    lwd = lwdB
  )

  # Add x labels
  axis(
    side = 1,
    at = 1:nrow(x$colRange),
    labels = labels,
    tick = FALSE
  )

  # Add y axis
  axis(side = 2)

  # Add zero line
  if (add0line) {
    abline(h = 0)
  }
  # Finalize everything -----------------------------------------------------
  invisible(xmat)

}
