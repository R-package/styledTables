knit_print_rmd_html <- function(x, ...) {
  if (!isFALSE(knitr::opts_current$get("tab.print"))) {
    caption <- knitr::opts_current$get("tab.cap")
    label <- knitr::opts_current$get("label")
    x %>%
      create_html(
        caption = if (!is.null(caption))
          paste(
            c(
              if (!is.null(label))
                "(\\#tab:", label, ")",
              caption
            ),
            collapse = ""
          ),
        footer = knitr::opts_current$get("tab.footer"),
        use_scope = !isFALSE(knitr::opts_current$get("tab.use_scope"))
      ) %>%
      {
        if (!is.null(caption) && !is.null(label) && isTRUE(knitr::opts_current$get("tab.add_dummy_table"))) {
          paste(
            c(
              "::: lua-remove",
              "",
              knitr::kable(
                "Please add `styledTables::lua_remove_filter()` after `loft.lua` in `pandoc_args`.",
                caption = caption, 
                format = 'pipe',
                col.names = c(' ')
              ),
              "",
              ":::",
              "",
              "```{=html}",
              .,
              "```"
            ),
            collapse = "\n"
          )
        } else {
          .
        }
      } %>%
      (knitr::asis_output)(meta = x@html_dependencies)  
  } else {
    invisible(NULL)
  }
}

rmarkdown_latex_dependencies_as_string <- utils::getFromNamespace("latex_dependencies_as_string", "rmarkdown")

inject_preamble <- function(pkgs) {
  user_pkgs <- knitr::opts_knit$get("header")["styledTables_userpkgs"]
  if (is.na(user_pkgs))
    user_pkgs <- NULL
  knitr::set_header(
    styledTables = st_preamble(),
    styledTables_userpkgs = paste(
      user_pkgs,
      "\n",
      rmarkdown_latex_dependencies_as_string(pkgs)
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

#' Knit-Print StyledTable
#' 
#' Add a knit_print method to the [styledTable][styledTables::styled_table()]
#' class.
#' In case the knitting destination is `latex`, then a LaTeX text block is printed,
#' which holds the LaTeX code of the table.
#' In case the knitting destination is `html`, then an HTML text block is printed,
#' holding the HTML code of the table.
#' In both cases the dependencies (LaTex and HTML dependencies) will be
#' automatically added to the [knitr metadata][knitr::knit_meta()].
#' In the `html` case, the the following chunk options are used for printing:
#' - `tab.cap`: An optional string holding the table caption. If no `tab.cap` is
#'   set, then no table caption is created and the table is not listed in the
#'   __list of tables__.
#' - `label`: If `tab.cap` is set, then the chunk label is used as table id in
#'   order to reference to it (using `\@ref(tab:LABEL)`). If no chunk label
#'   is defined, then the table will not be listed in the __list of tables__.
#' - `tab.footer`: A character vector holding the strings for the table footer entries.
#'   Each string is placed inside of a `<td>` table element in a separate table row
#'   in the `<tfoot>` part of the table.
#' - `tab.print`: An optional logical value. If set to `FALSE`, then the table
#'   is not printed.
#' - `tab.add_dummy_table`: An optional logical value. If set to `TRUE`, then
#'   an empty [kable][knitr::kable()] table is printed before the given table.
#'   This __dummy table__ is contained inside of a `<div class="lua-remove">` html-element.
#'   This is a quick fix for the open feature request in
#'   [https://github.com/rstudio/pagedown/issues/191](https://github.com/rstudio/pagedown/issues/191),
#'   since only markdown tables are currently supported by the `loft.lua` filter
#'   of `pagedown`.
#'   As a workaround for this missing feature, we add an empty dummy table before
#'   the desired table and remove it with the **lua-filter**
#'   `system.file("lua-filters", "lua_remove.lua", package = "styledTables")`.
#' @param x A [StyledTable][styled_table()] class object to be printed
#' @param ... Additional arguments (ignored)
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

#' Pandoc argument for adding the **lua filter** `lua_remove.lua`
#' 
#' Knitting an **RMD** file containing `styledTables` to **HTML**, then 
#' the chunk option `tab.add_dummy_table` must be set to `TRUE`, since 
#' the default **lua filter** `loft.lua` (see [pagedown](https://github.com/rstudio/pagedown/blob/main/inst/resources/lua/loft.lua)) for adding the **list of tables** at the report
#' beginning does not recognize `<table>` tags. Therefore, a dummy table
#' is inserted before the current table. The **lua filter** `lua_remove.lua`
#' should be applied after `loft.lua`. This functions returns character vector
#' defining the required pandoc argument in order to add `lua_remove.lua` to
#' the list of lua filters of pandoc.
#' @export
lua_remove_filter <- function() {
  rbind("--lua-filter", system.file("lua-filters", "lua_remove.lua", package = "styledTables"))
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
