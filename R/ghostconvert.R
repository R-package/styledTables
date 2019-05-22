#' Call Ghostscript.
#'
#' THIS CODE WAS TAKEN FROM \pkg{latexpdf} \cr
#' CRAN URL: \url{https://CRAN.R-project.org/package=latexpdf} \cr
#' File: \code{R/png.R} \cr
#' Version:  0.1.6 \cr
#' Published:  2018-10-26 \cr
#' Author:   Tim Bergsma \cr
#' Maintainer:   Tim Bergsma <bergsmat at gmail.com> \cr
#' License:  GPL-3 \cr
#'
#' Call Ghostscript, converting by default from PDF to PNG.
#' @importFrom tools find_gs_cmd
#' @param x path for file to be converted
#' @param y path for output file
#' @param gdir directory for png output
#' @param out filename for output file
#' @param gs_cmd passed to \code{\link[tools]{find_gs_cmd}}; perhaps 'gs' or 'gswin32c' or 'mgs' (from Miktex)
#' @param device output device type
#' @param multipage whether to convert multiple pages
#' @param multifix a filename suffix when converting multiple pages
#' @param suffix file extension for output
#' @param antialias font antialiasing
#' @param resolution raster image resolution
#' @param replace whether to delete \code{x} if successful
#' @param other other arguments to ghostscript
#' @param ... ignored
#' @return the name of the file created
#' @examples
#' \dontrun{
#' pdf <- as.pdf(head(Theoph),dir = tempdir())
#' png <- ghostconvert(pdf, gs_cmd = 'mgs')
#' browseURL(png)
#' }
ghostconvert <- function(
  x,
  y = file.path(gdir, out),
  gdir = dirname(x),
  out = sub("\\.[^.]+$", paste0(if (multipage) multifix else NULL, ".", suffix), basename(x)),
  gs_cmd = "",
  device = "pngalpha",
  multipage = FALSE,
  multifix = "-%03d",
  suffix = "png",
  antialias = 4,
  resolution = 300,
  replace = TRUE,
  other = "",
  ...
){
  stopifnot(length(x) == 1, length(y) == 1)
  exec <- find_gs_cmd(gs_cmd)
  if (exec == "") stop("gs_cmd not found")
  dev <- paste0("-sDEVICE=", device)
  file <- paste("-o", y)
  alias <- paste0("-dTextAlphaBits=", antialias)
  res <- paste0("-r", resolution)
  command <- paste(exec, dev, file, alias, res, other, x)
  result <- tryCatch(error = function(e) e, system(command))
  bad <- inherits(result, "try-error") || !file.exists(y)
  if (bad) stop("could not make ", y)
  if (replace) unlink(x)
  invisible(y)
}
