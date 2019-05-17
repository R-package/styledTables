#' @importFrom knitr knit_print
#' @export
knit_print.StyledTable <- function(x, ...) {
  fig_path <- knitr::opts_chunk$get("fig.path")
  fig_name <- paste0(
    fig_path, 
    "StyledTable-",
    substr(digest::digest(x), 1, 10), 
    ".png"
  )
  dir_name <- dirname(fig_name)
  if (!dir.exists(dir_name))
    dir.create(dir_name, recursive = TRUE)
  write_png(x, fig_name)
  knitr::include_graphics(fig_name)
}

inject_preamble <- function() {
  knitr::set_header(
    styledTables = paste(
      "\\usepackage{ragged2e}",
      "\\usepackage{multirow}",
      "\\usepackage{pbox}",
      "\\usepackage{hhline}",
      "\\usepackage[table]{xcolor}",
      sep = "\n"
    )
  )
}

#' Embed a styledTable object in a LaTeX (Rnw, Rmd) document
#' 
#' Generate latex code and ebed it into a LaTex document. This function applies the following steps
#' * make sure all LaTeX packages needed by styledTables are added to the preamble
#' * create LaTeX code for the object and tell knit to print it "asis" 
#' 
#' `embed_latex_rmd` is a variation of `embed_latex` for rmd files. TODO: automatically detect
#' render engine.
#' 
#' @param obj A styledTable object
#' @export
embed_latex <- function(obj) {
  stopifnot(inherits(obj, "StyledTable"))
  inject_preamble()
  knitr::asis_output(create_latex_table(obj))
}

#' @rdname embed_latex
#' @export
embed_latex_rmd <- function(obj) {
  inject_preamble()
  knitr::raw_block(type = "latex", create_latex_table(obj), meta = list(
    rmarkdown::latex_dependency("ragged2e"),
    rmarkdown::latex_dependency("multirow"),
    rmarkdown::latex_dependency("pbox"),
    rmarkdown::latex_dependency("hhline"),
    rmarkdown::latex_dependency("xcolor", options = "table"),
    rmarkdown::latex_dependency("geometry")
  ))
}

