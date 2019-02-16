#' Create a vertical line under the curve
#'
#' @param x x-value
#' @param mean mean
#' @param sd standard deviation
#'
#' @importFrom stats dnorm
#' @importFrom graphics segments
#' @export
mn.vline.at <- function(x, mean, sd) {
  segments(x, 0, x, dnorm(x, mean, sd), lty = 2)
}

#' Draw shaded area under curve
#'
#' @param mean mean
#' @param sd standard deviation
#' @param x1 left x-value for shaded region (upper tail if x2 not specified)
#' @param x2 right x-value for shaded region (lower tail if x1 not specified)
#'
#' @importFrom graphics polygon
#' @export
mn.shade <- function(mean, sd, x1, x2) {
  mn.vline.at(x1, mean, sd)
  mn.vline.at(x2, mean, sd)
  x.seq <- seq(x1, x2)
  x <- c(x1, x.seq, x2)
  y <- c(0, dnorm(x.seq, mean, sd), 0)
  polygon(x, y, density = 30, angle = 45, col = "grey", border = NA)
}

#' Quick Normal Distribution plot
#'
#' A Normal Distribution plot, nice lables, optional highlighting of a specified region.
#' @param mean mean
#' @param sd standard deviation
#' @param x1 left x-value for shaded region (upper tail if x2 not specified)
#' @param x2 right x-value for shaded region (lower tail if x1 not specified)
#' @param label label for your shaded region
#' @param label.x.axis if TRUE, show x-axis labels
#' @param label.y.axis if TRUE, show y-axis labels
#' @param max.sd.from.mean how many standard deviations from the mean to include for x-range
#'
#' @keywords normal dnorm pnorm qnorm normal-distribution normal-model
#' @importFrom graphics abline axis curve text
#' @importFrom stats dnorm
#' @export
#' @examples
#' my.normal(mean = 72, sd = 16, x1 = 80)
my.normal <- function(
  mean, sd,
  x1, x2,
  label, label.x.axis = TRUE, label.y.axis = TRUE,
  max.sd.from.mean = 4
) {
  # We'll only support at least 2 SD from mean; simplifies labelling
  num.sd <- max(2, max.sd.from.mean)

  x.val <- function(z) { mean + z * sd }
  x.values <- Map(x.val, seq.int(-num.sd, num.sd))
  y.max <- dnorm(mean,mean,sd)
  y.values <- Map(function(x){ y.max * x }, seq(0, 1, by = 0.25))

  # the curve
  my.dnorm <- function(x) { dnorm(x, mean, sd) }
  x.first <- x.val(-num.sd)
  x.last <- x.val(num.sd)
  curve(my.dnorm, x.first, x.last, lwd=1, axes = F, xlab = "", ylab = "")

  # x-axis
  abline(h = 0, lwd = 1)

  # Label axes
  if (label.x.axis) { axis(1, at = x.values,labels = x.values) }
  if (label.y.axis) { axis(2, at = y.values,labels = Map(function(x) { round(x, 3) }, y.values)) }

  # vertical line at mean
  #abline(v=mean, lty = 2)

  # shaded area
  if (!(missing(x1) && missing(x2))) {
    mn.shade(mean, sd, if (missing(x1)) x.first else x1, if (missing(x2)) x.last else x2)
  }

  # label for shaded area
  if (!missing(label)) {
    x1 <- if (missing(x1)) mean - 2 * sd else x1
    x2 <- if (missing(x2)) mean + 2 * sd else x2
    x.mid = (x1 + x2) / 2
    text(x = x.mid, y = dnorm(x.mid, mean, sd) / 2, labels = c(label), cex = 0.8)
  }
}
