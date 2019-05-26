#' Write a styledTable object to png
#'
#' Use [tools::texi2pdf()] to compile the output from
#' [create_latex_table()] to a png and save the file.
#'
#' @param st a styledTable object to save.
#' @param file the path to the output file.
#' @param resize Should the Png be resized to the dimensions of
#'                   the table? Default is `TRUE`.
#'
#' @importFrom tools texi2pdf
#' @examples
#' library(dplyr)
#' mtcars[1:5, 1:2] %>%
#'   styled_table %>%
#'   write_png("table_image.png")
#' @export
#' @seealso [write_pdf()], [write_excel()], [create_latex_table()], [append_latex_table()], [create_latex_table_body()]
write_png <- function(st, file = "table.png", resize = TRUE) {
    dir.create(tmp <- tempfile())
    oldWd <- setwd(tmp)
    on.exit({
        setwd(oldWd)
        unlink(tmp, recursive = TRUE)
    })
    writeLines(wrap_latex_preamble(st, resize), "table.tex")
    ## compile LaTeX file
    tinytex::pdflatex("table.tex")
    magick::image_read_pdf("table.pdf") %>%
        magick::image_write("table.png")

    ## change back wd so relative paths for file are handled properly
    ## when copying
    setwd(oldWd)
    invisible(file.copy(file.path(tmp, "table.png"), file, overwrite = TRUE))
}
