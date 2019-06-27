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

inject_preamble <- function(pkgs) {
  user_pkgs <- knitr::opts_knit$get("header")["styledTables_userpkgs"]
  if (is.na(user_pkgs))
    user_pkgs <- NULL
  knitr::set_header(
    styledTables = st_preamble(),
    styledTables_userpkgs = paste(
      user_pkgs,
      "\n",
      rmarkdown:::latex_dependencies_as_string(pkgs)
    )
  )
}

knit_print_rnw <- function(obj) {
  stopifnot(inherits(obj, "StyledTable"))
  inject_preamble(attr(obj, "packages"))
  knitr::asis_output(create_latex_table(obj))
}

knit_print_rmd_latex <- function(obj) {
  knitr::raw_block(
    type = "latex",
    create_latex_table(obj),
    meta = c(
      list(
        rmarkdown::latex_dependency("ragged2e"),
        rmarkdown::latex_dependency("multirow"),
        rmarkdown::latex_dependency("pbox"),
        rmarkdown::latex_dependency("hhline"),
        rmarkdown::latex_dependency("xcolor", options = "table"),
        rmarkdown::latex_dependency("geometry")
      ),
      attr(obj, "packages")
    )
  )
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
    html = knit_print_rmd_html(x),
    knit_print_rmd_html(x)
  )
}

#' Get the preamble code necessary to compile a stiled table
#'
#' Generate a string that can be included in the preamble of a latex document
#' to compile a styledtable object.
#'
#' @export
st_preamble <- function() {
  paste(
    "\\usepackage{ragged2e}",
    "\\usepackage{multirow}",
    "\\usepackage{pbox}",
    "\\usepackage{hhline}",
    "\\usepackage[table]{xcolor}",
    sep = "\n"
  )
}

inject_package <- function(obj, package, options = NULL, extra_lines = NULL) {
  new_dependency <- rmarkdown::latex_dependency(package, options, extra_lines)
  current_dependencies <- attr(obj, "packages")
  attr(obj, "packages") <- c(current_dependencies, list(new_dependency))
  obj
}
