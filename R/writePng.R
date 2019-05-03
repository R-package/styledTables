#' Write a styledTable object to png
#' 
#' Use \code{tools::texi2dvi} and \code{ghostconvert} to compile the output from \code{createLatexTable}
#' to a pdf and then into a png. This function works similat to \code{write.csv} in the sense
#' that the first argument is the table to write and the second
#' argument is a local or global path.
#' 
#' @param x a styledTable object to save.
#' @param file the path to the output file.
#' @param resizePng Should the Png be resized to the dimensions of
#'                   the table? Default is `TRUE`.
#' 
#' @importFrom tools texi2pdf
#' 
#' @export
writePng <- function(x, file = "table.png", resizePng = TRUE) {
  dir.create(tmp <- tempfile())
  oldWd <- setwd(tmp)
  on.exit({
    setwd(oldWd)
    unlink(tmp, recursive = TRUE)
  })
  writeLines(wrapStPreamble(x, resizePng), "table.tex") 
  ## compile LaTeX file
  texi2pdf("table.tex")
  ghostconvert(x = "table.pdf", y = "table.png")
  
  ## change back wd so relative paths for file are handled properly
  ## when copying
  setwd(oldWd)
  file.copy(file.path(tmp, "table.png"), file, overwrite = TRUE)
}
