#' Bug fix in xlsx package (indent != 0 is not supported)
#'
#' @param horizontal Character string for horizontal alignement
#' @param vertical Character string for vertical alignement
#' @param wrapText Logical should cell content be wrapped
#' @param rotation Numerical value for cell content rotation
#' @param indent Number of white spaces that should be used for indentation
Alignment <- function(horizontal = NULL, vertical = NULL, wrapText = FALSE,
                        rotation = 0, indent = 0)
{
      if (!is.null(horizontal) && !(horizontal %in% names(HALIGN_STYLES_)))
              stop("Not a valid horizontal value.  See help page.")

  if (!is.null(vertical) && !(vertical %in% names(VALIGN_STYLES_)))
          stop("Not a valid vertical value.  See help page.")

    structure(list(horizontal = horizontal, vertical = vertical,
                       wrapText = wrapText, rotation = rotation, indent = indent),
                  class = "Alignment")
}
