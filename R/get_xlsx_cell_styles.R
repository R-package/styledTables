#' Method get_xlsx_alignment
#'
#' @name get_xlsx_alignment
#' @rdname get_xlsx_alignment-methods
#' @exportMethod get_xlsx_alignment
setGeneric(
    "get_xlsx_alignment",
    function(sc) standardGeneric("get_xlsx_alignment")
)

#' @rdname get_xlsx_alignment-methods
#' @aliases get_xlsx_alignment,StyledCell-method
#' @param sc A StyledCell object
setMethod("get_xlsx_alignment", signature(sc = "StyledCell"),
    function(sc) {
        do_call_without_missing(Alignment, list(
            horizontal = sc@horizontal,
            vertical = sc@vertical,
            wrapText = sc@excel_wrapped,
            rotation = sc@rotation,
            indent = sc@indent
        ))
    }
)

#' Method get_xlsx_border
#'
#' @name get_xlsx_border
#' @rdname get_xlsx_border-methods
#' @exportMethod get_xlsx_border
setGeneric(
    "get_xlsx_border",
    function(sc) standardGeneric("get_xlsx_border")
)

#' @rdname get_xlsx_border-methods
#' @aliases get_xlsx_border,StyledCell-method
#' @param sc A StyledCell object
setMethod("get_xlsx_border", signature(sc = "StyledCell"),
    function(sc) {
        do_call_without_missing(Border, list(
            position = sc@border_position,
            color = sc@border_color,
            pen = sc@excel_border_pen
        ))
    }
)

#' Method get_xlsx_cell_protection
#'
#' @name get_xlsx_cell_protection
#' @rdname get_xlsx_cell_protection-methods
#' @exportMethod get_xlsx_cell_protection
setGeneric(
    "get_xlsx_cell_protection", 
    function(sc) standardGeneric("get_xlsx_cell_protection")
)

#' @rdname get_xlsx_cell_protection-methods
#' @aliases get_xlsx_cell_protection,StyledCell-method
#' @param sc A StyledCell object
setMethod("get_xlsx_cell_protection", signature(sc = "StyledCell"),
    function(sc) {
        do_call_without_missing(CellProtection, list(
            locked = sc@excel_locked,
            hidden = sc@excel_hidden
        ))
    }
)

#' Method get_xlsx_fill
#'
#' @name get_xlsx_fill
#' @rdname get_xlsx_fill-methods
#' @exportMethod get_xlsx_fill
setGeneric(
    "get_xlsx_fill", 
    function(sc) standardGeneric("get_xlsx_fill")
)

#' @rdname get_xlsx_fill-methods
#' @aliases get_xlsx_fill,StyledCell-method
#' @param sc A StyledCell object
setMethod("get_xlsx_fill", signature(sc = "StyledCell"),
    function(sc) {
        do_call_without_missing(Fill, list(
            foregroundColor = sc@fill_color,
            backgroundColor = sc@excel_background_color,
            pattern = sc@excel_fill_pattern
        ))
    }
)

#' Method get_xlsx_data_format
#'
#' @name get_xlsx_data_format
#' @rdname get_xlsx_data_format-methods
#' @exportMethod get_xlsx_data_format
setGeneric(
    "get_xlsx_data_format", 
    function(sc) standardGeneric("get_xlsx_data_format")
)

#' @rdname get_xlsx_data_format-methods
#' @aliases get_xlsx_data_format,StyledCell-method
#' @param sc A StyledCell object
setMethod("get_xlsx_data_format", signature(sc = "StyledCell"),
    function(sc) {
        do_call_without_missing(DataFormat, list(
            x = sc@excel_data_format
        ))
    }
)

#' Method get_xlsx_cell_style
#'
#' @name get_xlsx_cell_style
#' @rdname get_xlsx_cell_style-methods
#' @exportMethod get_xlsx_cell_style
setGeneric("get_xlsx_cell_style", function(wb, sc) standardGeneric("get_xlsx_cell_style"))

#' @rdname get_xlsx_cell_style-methods
#' @aliases get_xlsx_cell_style,ANY,StyledCell-method
#' @param wb An xlsx workbook object
#' @param sc A StyledCell object
setMethod("get_xlsx_cell_style", signature(wb = "ANY", sc = "StyledCell"),
    function(wb, sc) {
        do_call_without_missing(CellStyle, list(
            wb = wb, 
            font = getXlsxFontCellStyle(wb, sc),
            alignment = get_xlsx_alignment(sc),
            border = get_xlsx_border(sc),
            fill = get_xlsx_fill(sc),
            cellProtection = get_xlsx_cell_protection(sc),
            dataFormat = get_xlsx_data_format(sc)
        ))
    }
)

