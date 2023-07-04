#' @title Quantify metabolite
#' @description Quantify metabolite.
#' @param mod linear regression model.
#' @param Intensity user entered intensities used to predict concentration.
#' @return a data frame with Intensity and Concentration being the two columns.
#' @noRd
#' @examples
#' names(cars) <- c("Intensity", "Concentration")
#' result <- plotCalCurve(cars)
#' Int <- c(10, 15, 20)
#' prediction <- getQuan(mod = result$fit, Intensity = Int)

getQuan <- function(mod, Intensity){
  Intensity <- data.frame(Intensity)
  Concentration <- predict.lm(mod, Intensity)
  Result <- cbind.data.frame(Intensity = Intensity, Concentration = Concentration)
  return(Result)
}

#' @title Update calibration curve with predicted data
#' @description Update calibration curve with predicted data. If the intensity fall within the range of the calibration data, the predicted data color is green, else is red.
#' @param plot The calibration plot.
#' @param subDF The data frame used to generate the calibration plot.
#' @param prediction The predicted data frame.
#' @return Updated calibration curve with predicted data points.
#' @noRd
#' @examples
#' names(cars) <- c("Intensity", "Concentration")
#' result <- plotCalCurve(cars)
#' Int <- c(10, 15, 20)
#' prediction <- getQuan(mod = result$fit, Intensity = Int)
#' updateCalPlot(plot = result$plot, subDF = cars, prediction = prediction)

updateCalPlot <- function(plot, subDF, prediction){
  prediction$color <- ifelse(prediction$Intensity >= min(subDF$Intensity) & prediction$Intensity <= max(subDF$Intensity), "#9ed9b4", "#fc0317")
  plot %>%
    plotly::add_markers(data = prediction,
                        x = ~ Intensity,
                        y = ~ Concentration,
                        marker = list(color = prediction$color, symbol = 'x', size = 15, line = list(color = "#999797", width = 2))
                        )
}

