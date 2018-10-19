#' @import xlsx
#' @import data.table
#' @importFrom methods is setClass setGeneric setMethod setRefClass slot new validObject slot<-
#' @importFrom stats as.formula
#' @importFrom grDevices colors col2rgb
#' @importFrom utils getFromNamespace
NULL

globalVariables(c(".", "X", "VALUEIDS_"))

#' Import the non-exported FILL_STYLES_ from the xlsx package namespace 
FILL_STYLES_ <- getFromNamespace("FILL_STYLES_", "xlsx")
