#' @title Convert text input 2 numeric input
#' @description Convert text input into numeric input in ShinyApp.
#' @param text a text input.
#' @return The return value, if any, from executing the function.
#'
#' @noRd

text2Num <- function(text){
  gsub(" ", "", text) |>
    strsplit(x = _, ",", fixed = FALSE) |>
    unlist(x = _) |>
    as.numeric(x = _)
}
