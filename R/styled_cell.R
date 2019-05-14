#' Check Validity of StyledCell class sc and return an error description, if there is an error
#'
#' @param sc StyledCell object
#' @param style_name A character holding the name of the style that should be validated
check_validity <- function(sc, style_name) {
    check_fn <- switch(
        style_name,
        font_color = function(sc) asser_color_value(sc@font_color),
        horizontal = function(sc) assert_value(sc@horizontal, names(xlsx::HALIGN_STYLES_)),
        vertical = function(sc) assert_value(sc@vertical, names(xlsx::VALIGN_STYLES_)),
        border_color = function(sc) c(
                asser_color_value(sc@border_color),
                {
                    if (length(sc@border_color) > 0 && length(sc@border_position) == 0)
                        paste0("When argument 'border_color' is",
                                " given, the argument 'border_position' is ", 
                                "mandatory.")
                }
            ),
        excel_border_pen = function(sc) c(
                assert_value(sc@excel_border_pen, names(xlsx::BORDER_STYLES_)),
                {
                    if (length(sc@border_color) > 0 && length(sc@border_position) == 0)
                        paste0("When argument 'excel_border_pen' is",
                                " given, the argument 'border_position' is ", 
                                "mandatory.")
                }
            ),
        border_position = function(sc) assert_value(sc@border_position, c("BOTTOM", "LEFT", "TOP", "RIGHT")),
        excel_fill_pattern = function(sc) assert_value(sc@excel_fill_pattern, FILL_STYLES_),
        fill_color = function(sc) asser_color_value(sc@fill_color),
        excel_background_color = function(sc) asser_color_value(sc@excel_background_color),
        latex_font_size = function(sc) assert_value(
                    sc@latex_font_size,
                    c(
                        "\\miniscule",
                        "\\tiny",
                        "\\scriptsize",
                        "\\footnotesize",
                        "\\small",
                        "\\normalsize",
                        "\\large",
                        "\\Large",
                        "\\LARGE",
                        "\\huge",
                        "\\Huge",
                        "\\HUGE"
                    )
                )
    )
    if (is.null(check_fn)) {
        return(NULL)
    }
    check_fn(sc)
}

#' @title S4 helper class that summarizes NULL and missing values
#' 
#' @description This class is used as signature class for missing or NULL values
#' @exportClass MissingOrNull
setClassUnion(name = "MissingOrNull", members = c("missing", "NULL"))

#' Style class for single cells in [StyledTable] objects
#' 
#' This class holds all style settings for a single excel cell. The values are 
#' filled in by the user.
#' @name StyledCell-class
#' @rdname StyledCell-class
#' @exportClass StyledCell
setClass(
    "StyledCell",
    representation(
        excel_font_name = "character",
        latex_font_name = "character",
        excel_font_size = "numeric",
        font_color = "character",
        bold = "logical",
        italic = "logical",
        strikeout = "logical",
        underline = "numeric",
        excel_boldweight = "numeric",
        excel_wrapped = "logical",
        horizontal = "character",
        vertical = "character",
        rotation = "numeric",
        indent = "numeric",
        border_position = "character",
        border_color = "character",
        excel_border_pen = "character",
        fill_color = "character",
        excel_background_color = "character",
        excel_fill_pattern = "character",
        excel_data_format = "character",
        excel_locked = "logical",
        excel_hidden = "logical",
        latex_vertical_move = "character",
        excel_pre_process = "function",
        latex_pre_process = "function",
        latex_font_size = "character"
    ),
    validity = function(object) {
        # Check if the values of the arguments are correct
        return("blabla")
        error <- c(
            asser_color_value(object@font_color),
            assert_value(object@horizontal, names(xlsx::HALIGN_STYLES_)),
            assert_value(object@vertical, names(xlsx::VALIGN_STYLES_)),
            asser_color_value(object@border_color),
            assert_value(object@excel_border_pen, names(xlsx::BORDER_STYLES_)),
            assert_value(object@border_position, c("BOTTOM", "LEFT", "TOP", "RIGHT")),
            assert_value(object@excel_fill_pattern, xlsx:::FILL_STYLES_),
            asser_color_value(object@fill_color),
            asser_color_value(object@excel_background_color),
            assert_value(
                object@latex_font_size,
                c(
                    "\\miniscule",
                    "\\tiny",
                    "\\scriptsize",
                    "\\footnotesize",
                    "\\small",
                    "\\normalsize",
                    "\\large",
                    "\\Large",
                    "\\LARGE",
                    "\\huge",
                    "\\Huge",
                    "\\HUGE"
                )
            ),
            {
                # Check if border values are given together with border_position
                if ((length(object@excel_border_pen) > 0 || length(object@border_color) > 0) && length(object@border_position) == 0)
                    paste0("When arguments 'excel_border_pen' or 'border_color' are",
                            " given, the argument 'border_position' is ", 
                            "mandatory.")
            }
        )
        if (!is.null(error))
            return(error)
        TRUE
    }
)

#' Constructor method of StyledCell Class.
#'
#' @name StyledCell 
#' @rdname StyledCell-class
#' @aliases initialize,StyledCell-method
#' @param .Object A StyledCell object
#' @param excel_font_name The name of the font that should be used for the Excel table generation
#' @param latex_font_name The name of the font that should be used for the LaTeX table generation
#' @param excel_font_size The font height
#' @param font_color The font color
#' @param bold Is bold? TRUE/FALSE
#' @param italic Is italic? TRUE/FALSE
#' @param strikeout Is striked out? TRUE/FALSE
#' @param underline Numeric weight of the underline
#' @param excel_boldweight Numeric font weight
#' @param excel_wrapped Is wrappend? TRUE/FALSE
#' @param horizontal String with horizontal alignment
#' @param vertical String with vertical alignment
#' @param rotation Numeric holding the rotation of the text
#' @param indent Numeric holding the indnentation
#' @param border_position Character vector with possible border positions
#' @param border_color Color string (hex or color name)
#' @param excel_border_pen String with border pen value
#' @param fill_color Color string for foreground color
#' @param excel_background_color Color string for background color
#' @param excel_fill_pattern String with fill pattern
#' @param excel_data_format String for Excel-Cell-Data-Format
#' @param excel_locked Is Locked? TRUE/FALSE
#' @param excel_hidden Is Hidden? TRUE/FALSE
#' @param latex_vertical_move A latex string that gives the distance for vertically moving the cell content (e.g.: in order to to vertically center it).
#' @param excel_pre_process A function that can be used to pre process cell values for the Excel table generation
#' @param latex_pre_process A function that can be used to pre process cell values for the LaTeX table generation
#' @param latex_font_size The LaTeX command to set the font size
setMethod("initialize", signature(.Object = "StyledCell"), 
    function(
        .Object,
        excel_font_name,
        latex_font_name,
        excel_font_size,
        font_color,
        bold,
        italic,
        strikeout,
        underline,
        excel_boldweight,
        excel_wrapped,
        horizontal,
        vertical,
        rotation,
        indent,
        border_position,
        border_color,
        excel_border_pen,
        fill_color,
        excel_background_color,
        excel_fill_pattern,
        excel_data_format,
        excel_locked,
        excel_hidden,
        latex_vertical_move,
        excel_pre_process,
        latex_pre_process,
        latex_font_size
    ) {
        # check function for the slots
        check_slot <- function(style_name) {
            description <- check_validity(.Object, style_name)
            if (!is.null(description))
                stop(paste0(
                    "Error in the initialization of 'StyledCell' ",
                    "the passed in value for slot '", style_name, ",' does not ",
                    "does not meet the class requirements: ",
                    description
                ), call. = FALSE)
        }
        if (!missing(excel_font_name)) {
            .Object@excel_font_name = excel_font_name
            check_slot("excel_font_name")
        }
        if (!missing(latex_font_name)) {
            .Object@latex_font_name = latex_font_name
            check_slot("latex_font_name")
        }
        if (!missing(excel_font_size)) {
            .Object@excel_font_size = excel_font_size
            check_slot("excel_font_size")
        }
        if (!missing(font_color)) {
            font_color <- transform_char(font_color, toUpper = TRUE)
            .Object@font_color = font_color
            check_slot("font_color")
        }
        if (!missing(bold)) {
            .Object@bold = bold
        }
        if (!missing(italic)) {
            .Object@italic = italic
        }
        if (!missing(strikeout)) {
            .Object@strikeout = strikeout
        }
        if (!missing(underline)) {
            .Object@underline = underline
        }
        if (!missing(excel_boldweight)) {
            .Object@excel_boldweight = excel_boldweight
        }
        if (!missing(excel_wrapped)) {
            .Object@excel_wrapped = excel_wrapped
        }
        if (!missing(horizontal)) {
            horizontal <- transform_char(horizontal, pre = "ALIGN_", toUpper = TRUE)
            .Object@horizontal = horizontal
            check_slot("horizontal")
        }
        if (!missing(vertical)) {
            vertical <- transform_char(vertical, pre = "VERTICAL_", toUpper = TRUE)
            .Object@vertical = vertical
            check_slot("vertical")
        }
        if (!missing(rotation)) {
            .Object@rotation = rotation
        }
        if (!missing(indent)) {
            .Object@indent = indent
        }
        if (!missing(border_position)) {
            border_position <- transform_char(border_position, toUpper = TRUE)
            .Object@border_position = border_position
            check_slot("border_position")
        }
        if (!missing(border_color)) {
            border_color <- transform_char(border_color, toUpper = TRUE)
            .Object@border_color = border_color
            check_slot("border_color")
        }
        if (!missing(excel_border_pen)) {
            excel_border_pen <- transform_char(excel_border_pen, pre = "BORDER_", toUpper = TRUE)
            .Object@excel_border_pen = excel_border_pen
            check_slot("excel_border_pen")
        }
        if (!missing(fill_color)) {
            fill_color <- transform_char(fill_color, toUpper = TRUE)
            .Object@fill_color = fill_color
            check_slot("fill_color")
        }
        if (!missing(excel_background_color)) {
            excel_background_color <- transform_char(excel_background_color, toUpper = TRUE)
            .Object@excel_background_color = excel_background_color
            check_slot("excel_background_color")
        }
        if (!missing(excel_fill_pattern)) {
            excel_fill_pattern <- transform_char(excel_fill_pattern, toUpper = TRUE)
            .Object@excel_fill_pattern = excel_fill_pattern
            check_slot("excel_fill_pattern")
        }
        if (!missing(excel_data_format)) {
            .Object@excel_data_format = excel_data_format
        }
        if (!missing(excel_locked)) {
            .Object@excel_locked = excel_locked
        }
        if (!missing(excel_hidden)) {
            .Object@excel_hidden = excel_hidden
        }
        if (!missing(latex_vertical_move)) {
            .Object@latex_vertical_move = latex_vertical_move
        }
        if (!missing(excel_pre_process)) {
            .Object@excel_pre_process = excel_pre_process
        } else {
            .Object@excel_pre_process = function(x) x
        }
        if (!missing(latex_pre_process)) {
            .Object@latex_pre_process = latex_pre_process
        } else {
            .Object@latex_pre_process = sanitize
        }
        if (!missing(latex_font_size)) {
            .Object@latex_font_size = latex_font_size
            check_slot("latex_font_size")
        }
        .Object
})

#' Set a single attribute in a style class
#'
#' @name setStyledCell
#' @rdname setStyledCell-methods
#' @exportMethod setStyledCell
setGeneric(
    "setStyledCell",
    function(sc, value, style_name) standardGeneric("setStyledCell")
)

#' @rdname setStyledCell-methods
#' @aliases setStyledCell,StyledCell-method
#' @param sc A StyledCell object
#' @param value The value that should be set
#' @param style_name The name of the style setting that should be set
setMethod("setStyledCell", signature(sc = "StyledCell", value = "ANY", style_name = "character"),
    function(sc, value, style_name) {
        # Transform input values
        if (style_name %in% c(
            "font_color", 
            "border_color", 
            "fill_color", 
            "excel_background_color", 
            "excel_fill_pattern", 
            "border_position")
        )
            value <- transform_char(value, toUpper = TRUE)
        if (style_name == "horizontal")
            value <- transform_char(value, pre = "ALIGN_", toUpper = TRUE)
        if (style_name == "vertical")
            value <- transform_char(value, pre = "VERTICAL_", toUpper = TRUE)
        if (style_name == "excel_border_pen")
            value <- transform_char(value, pre = "BORDER_", toUpper = TRUE)

        slot(sc, style_name) <- value
        description <- check_validity(sc, style_name)
        if (!is.null(description))
            stop(paste0(
                "Error in 'setStyledCell' ",
                "the passed in value for style '", style_name, ",' does not ",
                "does not meet the class requirements: ",
                description
            ), call. = FALSE)
        sc
    }
)

#' @rdname setStyledCell-methods
#' @aliases setStyledCell,StyledCell-method
setMethod("setStyledCell", signature(sc = "MissingOrNull", value = "ANY", style_name = "character"),
    function(sc, value, style_name) {
        setStyledCell(new("StyledCell"), value, style_name)
    }
)


#' Method getStyledCell
#'
#' @name getStyledCell
#' @rdname getStyledCell-methods
#' @exportMethod getStyledCell
#' @param ... Various arguments
setGeneric("getStyledCell", function(sc, ...) standardGeneric("getStyledCell"))

#' @rdname getStyledCell-methods
#' @aliases getStyledCell,StyledCell-method
#' @param sc A StyledCell object
#' @param style_name The name of the style setting that should be retrieved
setMethod("getStyledCell", signature(sc = "StyledCell"),
    function(sc, style_name) {
        slot(sc, style_name)
    }
)

#' @rdname getStyledCell-methods
#' @aliases getStyledCell,MissingOrNull-method
setMethod("getStyledCell", signature(sc = "MissingOrNull"),
    function(sc, style_name) {
        if (style_name %in% c("excel_pre_process", "latex_pre_process"))
            return(function(x) x)
        NULL
    }
)

#' Method getXlsxFontCellStyle
#'
#' @name getXlsxFontCellStyle
#' @rdname getXlsxFontCellStyle-methods
#' @exportMethod getXlsxFontCellStyle
setGeneric(
    "getXlsxFontCellStyle",
    function(wb, sc) standardGeneric("getXlsxFontCellStyle")
)

#' @rdname getXlsxFontCellStyle-methods
#' @aliases getXlsxFontCellStyle,StyledCell-method
#' @param wb An xlsx workbook object
#' @param sc A StyledCell object
setMethod("getXlsxFontCellStyle", signature(wb = "ANY", sc = "StyledCell"),
    function(wb, sc) {
        font <- remove_missing(list(
            name = sc@excel_font_name,
            height = sc@excel_font_size,
            color = sc@font_color,
            bold = sc@bold,
            italic = sc@italic,
            strikeout = sc@strikeout,
            boldweight = sc@excel_boldweight,
            underline = sc@underline
        )) 
        if (length(font) > 0) {
            argList <- font[names(font)[unlist(lapply(font, function(x) !is.null(x)))]]
            argList$wb <- wb
            do.call(
                Font,
                argList
            )
        } else {
            NULL
        }
    }
)

