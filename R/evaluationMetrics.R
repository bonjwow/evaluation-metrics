### Format a number
formatNumber <- function(numb, decimal) {
  return(format(round(as.numeric(numb), decimal), nsmall=decimal, big.mark=","))
}

#' Calculate R-squared score
#'
#' Calculate R-squared score by parsing the fitted linear model object
#' @param model The fitted linear model object
#' @return R-squared score in a numeric value with optional decimal places
#' @examples
#' RSQ(fit.lm, decimal=3)

### Calculate R-squared
RSQ <- function(model, decimal=3) {
  result <- summary(model)$r.squared
  return(formatNumber(result, decimal))
}

#' Calculate Mean Squared Error (MSE) score
#'
#' Calculate MSE score by parsing the fitted linear model object
#' @param model The fitted linear model object
#' @return MSE score in a numeric value with optional decimal places
#' @examples
#' MSE(fit.lm, decimal=3)

### Calculate MSE
MSE <- function(model, decimal=3) {
  result <- mean(summary(model)$residuals^2)
  return(formatNumber(result, decimal))
}

#' Calculate Root Mean Squared Error (RMSE) score
#'
#' Calculate RMSE score by parsing the fitted linear model object
#' @param model The fitted linear model object
#' @return RMSE score in a numeric value with optional decimal places
#' @examples
#' RMSE(fit.lm, decimal=3)

### Calculate RMSE
RMSE <- function(model, decimal=3) {
  result <- sqrt(mean(summary(model)$residuals^2))
  return(formatNumber(result, decimal))
}
