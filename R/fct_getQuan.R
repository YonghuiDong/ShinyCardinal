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
  #(1) Predict concentration ---------------------------------------------------
  Concentration <- predict.lm(mod, data.frame(Intensity))
  #(2) Format result -----------------------------------------------------------
  if(any(Intensity >= 1)){
    Intensity <- round(Intensity, 2)
  } else{
    Intensity <- round(Intensity, 4)
  }
  if(any(Concentration >= 1)){
    Concentration <- round(Concentration, 2)
  } else{
    Concentration <- round(Concentration, 4)
  }
  Result <- cbind.data.frame(Intensity = Intensity, Concentration = Concentration)
  return(Result)
}

