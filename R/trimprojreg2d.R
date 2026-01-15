#' @name trimProjReg2d
#' @title trimProjReg2d
#' @export
#'
#' @description Computes projection trimmed regression in 2 dimensions.
#'
#' @param x Independent variable
#' @param y Dependent variable
#' @param alpha Percentage of trimmed observations
#'
#' @author Zygmunt Zawadzki from Cracow University of Economics.
#'
#' @examples
#'
#' # EXAMPLE 1
#' data("pension", package = "robustbase")
#' plot(pension)
#' abline(lm(Reserves ~ Income, data = pension), lty = 3, lwd = 2) # lm
#' abline(DepthProc::trimProjReg2d(pension[, 1], pension[, 2]), lwd = 2) # trimprojreg2d
#' legend("bottomright", c("OLS", "TrimLS"), lty = 1:2)
#'
#' # EXAMPLE 2
#' data("under5.mort", package = "DepthProc")
#' data("inf.mort", package = "DepthProc")
#' data("maesles.imm", package = "DepthProc")
#'
#' data2011 <- na.omit(cbind(under5.mort[, 22], inf.mort[, 22],
#'                           maesles.imm[, 22]))
#' x <- data2011[, 3]
#' y <- data2011[, 2]
#' plot(x, y, cex = 1.2, ylab = "infant mortality rate per 1000 live birth",
#'      xlab = "against masles immunized percentage",
#'      main = "Projection Depth Trimmed vs. LS regressions")
#' abline(lm(x ~ y), lwd = 2, col = "black") # lm
#' abline(DepthProc::trimProjReg2d(x, y), lwd = 2, col = "red") # trimmed reg
#' legend("bottomleft", c("LS", "TrimReg"), fill = c("black", "red"), cex = 1.4,
#'        bty = "n")
#'
#' ##### Comparsion of a few regression methods #####
#'
#' data("france", package = "DepthProc")
#' plot(UR ~ MW, pch = 19, data = france)
#'
#' # linear regression
#' lm.fit <- lm(UR ~ MW, data = france)
#' abline(lm.fit, lwd=2, cex=3, col='red')
#'
#' # M-estimator
#' rlm.fit <- MASS::rlm(UR ~ MW, data = france)
#' abline(rlm.fit, lwd = 2,col = "blue")
#'
#' # LMS
#' lqs.lms <- MASS::lqs(UR ~ MW, method = "lms", data = france) #least median of squares#
#' lqs.lts <- MASS::lqs(UR ~ MW, method = "lts", data = france) #least trimmed squares#
#' abline(lqs.lms, lwd = 2, col="green")
#' abline(lqs.lts, lwd = 2, col="pink")
#'
#' # Lowess
#' lines(stats::lowess(france$MW, france$UR, f = 0.5, iter = 0), lwd = 2) # loess
#'
#' # Depth trimmed regression
#' trim.reg <- DepthProc::trimProjReg2d(france$MW, france$UR) #trimprojreg2d
#' abline(trim.reg, lwd = 4, col = 'orange')
#'
trimProjReg2d <- function(x, y, alpha = 0.1) {
  yX <- cbind(y, x)
  depth <- depth(yX, yX, method = "Projection")
  cut <- quantile(depth, alpha)

  ycut <- y[depth > cut]
  xcut <- x[depth > cut]

  data <- data.frame(ycut, xcut)

  fitcut <- lm(ycut ~ xcut, data = data)$coeff
  methods::new("TrimReg2d", coef = fitcut)
}
