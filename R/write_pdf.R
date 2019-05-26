wrap_latex_preamble <- function(st, resize) {
    resize <- FALSE
    paste(
        ifelse(
            resize,
            "\\documentclass{standalone}",
            "\\documentclass[a4paper]{article}"
        ),
        "\\usepackage{ragged2e}",
        "\\usepackage{multirow}",
        "\\usepackage{pbox}",
        "\\usepackage{hhline}",
        "\\usepackage[table]{xcolor}",
        "\\usepackage[utf8]{inputenc}",
        "\\usepackage[scaled]{helvet}",
        "\\renewcommand\\familydefault{\\sfdefault}",
        "\\usepackage[T1]{fontenc}",
        "\\begin{document}",
        create_latex_table(st),
        "\\end{document}",
        sep = "\n"
    )
}

#' Write a styledTable object to pdf
#'
#' Use [tools::texi2pdf()] to compile the output from
#' [create_latex_table()] to a pdf and save the file.
#'
#' @param st The styledTable object to save.
#' @param file The path where the output file should be saved.
#' @param resize Should the pdf be resized to the dimensions of
#'                   the table? Default is `TRUE`.
#' @importFrom tools texi2pdf
#' @examples
#' library(dplyr)
#' mtcars[1:5, 1:2] %>%
#'   styled_table %>%
#'   write_pdf("table_image.pdf")
#' @export
#' @seealso [write_png()], [write_excel()], [create_latex_table()], [append_latex_table()], [create_latex_table_body()]
write_pdf <- function(st, file = "table.pdf", resize = TRUE) {
    dir.create(tmp <- tempfile())
    oldWd <- setwd(tmp)
    on.exit({
        setwd(oldWd)
        unlink(tmp, recursive = TRUE)
    })
    writeLines(wrap_latex_preamble(st, resize), "table.tex")
    ## compile LaTeX file
    tools::texi2pdf("table.tex")
    ## change back wd so relative paths for file are handled properly
    ## when copying
    setwd(oldWd)
    invisible(file.copy(file.path(tmp, "table.pdf"), file, overwrite = TRUE))
}
