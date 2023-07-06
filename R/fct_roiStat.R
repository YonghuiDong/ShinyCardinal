#' @title Perform statistics on different ROIs.
#' @description Perform statistics on different ROIs.
#' @param roiMSIData Combined ROI MSI data. The output from combine2 function.
#' @return A data frame, including mean ion intensities of the ROIs and/or
#' @noRd
#' @examples
#' library(Cardinal)
#' #(1) from roiList
#' set.seed(1)
#' x <- simulateImage(preset=1, nruns=2, npeaks=10, dim=c(2,2))
#' a <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
#' b <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE, TRUE)
#' c <- c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
#' l <- list(a, b, c)
#' names(l) <- c("a_X", "b_Y", "c_Y")
#' Result1 <- combine2(msiData = x, roiList = l)
#' roiStat(roiMSIData = Result1)
#' #(2) with statistics
#' set.seed(2)
#' x <- simulateImage(preset=4, nruns=3, npeaks=10, dim=c(10,10))
#' roiStat(roiMSIData = x)

roiStat <- function(roiMSIData){
  #(1) Get mean intensity for each ROI MSI data --------------------------------
  roiMean <- Cardinal::aggregate(x = roiMSIData, FUN= c('mean'), groups = Cardinal::run(roiMSIData), as = 'DataFrame') |>
    as.data.frame(x = _) |>
    transform(mz = round(mz, 4))
  roiMean <- round(roiMean, 4)

  #(2) Remove rows with all means == 0 -----------------------------------------
  roiMean <- roiMean[rowSums(roiMean[, -which(names(roiMean) == "mz")], na.rm = TRUE) > 0, ]

  #(3) Calculate fold change ---------------------------------------------------
  dfNames <- colnames(roiMean)
  columns <- grepl("mean", dfNames)
  dfSub <- roiMean[, columns]
  string <- colnames(dfSub)
  Group <- sub(".*_(.*?)\\..*", "\\1", string)
  FC <- getFC(t(dfSub), Group)

  #(4) Means test --------------------------------------------------------------
  if(any(table(Group) > 1)){
    fit <- Cardinal::meansTest(x = roiMSIData, ~ condition, groups = Cardinal::run(roiMSIData))
    roiStat <- Cardinal::summary(fit) |>
      as.data.frame(x = _) |>
      subset(x = _, select = -Feature)
  } else{
    roiStat <- NULL
  }

  #(5) Show result -------------------------------------------------------------
  if(!is.null(roiStat)){
    cbind(roiMean, FC, roiStat)
  } else{
    cbind(roiMean, FC)
  }
}



#' @title Calculate fold change
#' @description Calculate fold change among different samples.
#' @param x sample ion intensity matrix, row are samples, columns are features.
#' @param Group sample group information
#' @return a dataframe with mean values and fold changes
#' @export
#' @noRd
#' @examples
#' dat <- matrix(runif(2*300), ncol = 2, nrow = 300)
#' Group <- rep_len(LETTERS[1:2], 300)
#' ret <- getFC(dat, Group = Group)

getFC <- function(x, Group = NULL){
  cat ("\n- Calculating Fold Changes...\n")
  #(1) Check input -------------------------------------------------------------
  Group <- as.factor(Group)
  if(is.null(Group)){stop("Please include group information")}
  if(length(levels(Group)) <= 1){stop("At least two sample groups should be included")}
  if(length(Group) != nrow(x)){stop("Missing group informaiton detected")}

  #(2) Calculate FC ------------------------------------------------------------
  df <- data.frame(x, Group = Group)
  mean_int <- stats::aggregate(. ~ Group, data = df, FUN = mean)
  row_name <- mean_int$Group
  mean_int <- as.matrix(subset(x = mean_int, select = -Group))
  rownames(mean_int) <- row_name
  j <- utils::combn(levels(Group), 2)
  f_change1 <- mean_int[j[1,],] / mean_int[j[2,],]
  f_change2 <- mean_int[j[2,],] / mean_int[j[1,],]

  #(3) remove NaN in f_change Matrix -------------------------------------------
  f_change <- rbind(f_change1, f_change2)
  f_change[is.nan(f_change)] <- 0
  rownames(f_change) <- c(paste0("FoldChange_", j[1,], "_vs_", j[2,]), paste0("FoldChange_", j[2,], "_vs_", j[1,]))
  ret <- as.data.frame(t(f_change))
  return(round(ret, 2))
}
