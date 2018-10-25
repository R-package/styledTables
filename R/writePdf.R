wrapStPreamble <- function(sTbl, resizePdf) {
  paste(
    ifelse(
      resizePdf,
      "\\documentclass[tightpage]{standalone}",
      "\\documentclass[a4paper]{article}"
    ),
    "\\usepackage{ragged2e}",
    "\\usepackage{multirow}",
    "\\usepackage{pbox}",
    "\\usepackage{hhline}",
    "\\usepackage[table]{xcolor}",
    "\\begin{document}",
    createLatexTable(sTbl),
    "\\end{document}",
    sep = "\n"
  )
}

#' Write a styledTable object to pdf
#' 
#' Use [tools::texi2pdf] to compile the output from [createLatexTable]
#' to a pdf. This function works similat ro [write.csv] in the sense
#' that the first argument is the table to write and the second
#' argument is a local or global path.
#' 
#' @param x a styledTable object to save.
#' @param file the path to the output file.
#' @param resizePdf Should the pdf be resized to the dimensions of
#'                   the table? Default is `TRUE`.
#' 
#' @importFrom tools texi2pdf
#' 
#' @export
writePdf <- function(x, file = "table.pdf", resizePdf = TRUE) {
  dir.create(tmp <- tempfile())
  oldWd <- setwd(tmp)
  on.exit({
    setwd(oldWd)
    unlink(tmp, recursive = TRUE)
  })
  writeLines(wrapStPreamble(x, resizePdf), "table.tex") 
  ## compile LaTeX file
  texi2pdf("table.tex")
  ## change back wd so relative paths for file are handled properly
  ## when copying
  setwd(oldWd)
  file.copy(file.path(tmp, "table.pdf"), file, overwrite = TRUE)
}
