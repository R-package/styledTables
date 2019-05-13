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
  writePng(x, fig_name)
  knitr::include_graphics(fig_name)
}
