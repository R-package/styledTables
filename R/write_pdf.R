wrap_latex_preamble <- function(st, resize) {
    paste(
        ifelse(
            resize,
            "\\documentclass[tightpage]{standalone}",
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

compile_tex <- function(tex_string, tex_file = "table.tex") {
    writeLines(tex_string, tex_file)
    res <- suppressWarnings(system(
        paste("pdflatex -interaction=nonstopmode", tex_file),
        intern = TRUE
    ))
    if (!is.null(attr(res, "status"))) {
        message(paste(res, collapse = "\n"))
        stop("could not compile ", tex_file)
    }
    return(paste0(tools::file_path_sans_ext(tex_file), ".pdf"))
}

# see withr::with_dir() for reference
with_tempdir <- function(code) {
    new <- tempfile()
    dir.create(new)
    old <- setwd(dir = new)
    on.exit(setwd(old))
    force(code)
    new
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
    render_dir <- with_tempdir({
        generated_pdf <- st %>%
            wrap_latex_preamble(resize) %>%
            compile_tex() %>%
            normalizePath()
    })
    on.exit(unlink(render_dir))
    ## change back wd so relative paths for file are handled properly
    invisible(file.copy(generated_pdf, file, overwrite = TRUE))
}
