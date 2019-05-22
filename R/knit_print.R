knit_print_rmd_html <- function(x, ...) {
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

knit_print_rnw <- function(obj) {
  stopifnot(inherits(obj, "StyledTable"))
  inject_preamble()
  knitr::asis_output(create_latex_table(obj))
}

knit_print_rmd_latex <- function(obj) {
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

pandoc_to <- getFromNamespace("pandoc_to", "knitr")

#' @importFrom knitr knit_print
#' @export
knit_print.StyledTable <- function(x, ...) {
  pt <- pandoc_to()
  if (is.null(pt))
    return(knit_print_rnw(x))
  switch(
    pt,
    latex = knit_print_rmd_latex(x),
    html = knit_print_rmd_html(x)
  )
}
