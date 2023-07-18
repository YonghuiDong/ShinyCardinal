#' @title Plot calibration curve
#' @description Plot calibration curve.
#' @param df a data frame, which contains a Concentration and a Intensity column.
#' @return a plotly object.
#' @noRd
#' @examples
#' names(cars) <- c("Intensity", "Concentration")
#' result <- plotCalCurve(cars)

plotCalCurve <- function(df){
  fit <- lm(Concentration ~ Intensity, data = df)
  title <- paste0("\n", "y = ", round(coef(fit)[1], 2), " + ", round(coef(fit)[2], 2), "x; ", "R² = ", round(summary(fit)$r.squared, 3))
  plot <- df %>%
    plotly::plot_ly(x = ~Intensity) %>%
    plotly::add_markers(y = ~Concentration, marker = list(color = "#ffe9ec", size = 10, line = list(color = "#cf8a8a", width = 2))) %>%
    plotly::add_lines(x = ~Intensity, y = fitted(fit), line = list(color = "#6c87b8", dash = 'dot', width = 4)) %>%
    plotly::layout(showlegend = FALSE, title = title) %>%
    plotly::config(toImageButtonOptions = list(format = "svg", filename = "calibrationCurve"))

  result <- list(fit = fit, plot = plot)
  return(result)
}


