#' Check Validity of STCellStyle class object and return an error description, if there is an error
#'
#' @param object STCellStyle object
#' @param styleName A character holding the name of the style that should be validated
checkValidity <- function(object, styleName) {
    checkFn <- switch(
        styleName,
        fontColor = function(object) AssertColorValue(object@fontColor),
        horizontal = function(object) AssertValue(object@horizontal, names(xlsx::HALIGN_STYLES_)),
        vertical = function(object) AssertValue(object@vertical, names(xlsx::VALIGN_STYLES_)),
        borderColor = function(object) c(
                AssertColorValue(object@borderColor),
                {
                    if (length(object@borderColor) > 0 && length(object@borderPosition) == 0)
                        paste0("When argument 'borderColor' is",
                                " given, the argument 'borderPosition' is ", 
                                "mandatory.")
                }
            ),
        borderPen = function(object) c(
                AssertValue(object@borderPen, names(xlsx::BORDER_STYLES_)),
                {
                    if (length(object@borderColor) > 0 && length(object@borderPosition) == 0)
                        paste0("When argument 'borderPen' is",
                                " given, the argument 'borderPosition' is ", 
                                "mandatory.")
                }
            ),
        borderPosition = function(object) AssertValue(object@borderPosition, c("BOTTOM", "LEFT", "TOP", "RIGHT")),
        fillPattern = function(object) AssertValue(object@fillPattern, FILL_STYLES_),
        foregroundColor = function(object) AssertColorValue(object@foregroundColor),
        backgroundColor = function(object) AssertColorValue(object@backgroundColor),
        latexFontSize = function(object) AssertValue(
                    object@latexFontSize,
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
    if (is.null(checkFn)) {
        return(NULL)
    }
    checkFn(object)
}

#' @title S4 helper class that summarizes NULL and missing values
#' 
#' @description This class is used as signature class for missing or NULL values
#' @exportClass MissingOrNull
setClassUnion(name = "MissingOrNull", members = c("missing", "NULL"))

#' Style class for single cells in styled tables
#' 
#' This class holds all style settings for a single excel cell. The values are 
#' filled in by the user.
#' @name STCellStyle-class
#' @rdname STCellStyle-class
#' @exportClass STCellStyle
setClass(
    "STCellStyle",
    representation(
        fontName = "character",
        fontHeight = "numeric",
        fontColor = "character",
        isBold = "logical",
        isItalic = "logical",
        isStrikeout = "logical",
        underline = "numeric",
        boldweight = "numeric",
        isWrapped = "logical",
        horizontal = "character",
        vertical = "character",
        rotation = "numeric",
        indent = "numeric",
        borderPosition = "character",
        borderColor = "character",
        borderPen = "character",
        foregroundColor = "character",
        backgroundColor = "character",
        fillPattern = "character",
        dataFormat = "character",
        isLocked = "logical",
        isHidden = "logical",
        latexVerticalMove = "character",
        excelPreProcess = "function",
        latexPreProcess = "function",
        latexFontSize = "character"
    ),
    validity = function(object) {
        # Check if the values of the arguments are correct
        return("blabla")
        error <- c(
            AssertColorValue(object@fontColor),
            AssertValue(object@horizontal, names(xlsx::HALIGN_STYLES_)),
            AssertValue(object@vertical, names(xlsx::VALIGN_STYLES_)),
            AssertColorValue(object@borderColor),
            AssertValue(object@borderPen, names(xlsx::BORDER_STYLES_)),
            AssertValue(object@borderPosition, c("BOTTOM", "LEFT", "TOP", "RIGHT")),
            AssertValue(object@fillPattern, xlsx:::FILL_STYLES_),
            AssertColorValue(object@foregroundColor),
            AssertColorValue(object@backgroundColor),
            AssertValue(
                object@latexFontSize,
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
                # Check if border values are given together with borderPosition
                if ((length(object@borderPen) > 0 || length(object@borderColor) > 0) && length(object@borderPosition) == 0)
                    paste0("When arguments 'borderPen' or 'borderColor' are",
                            " given, the argument 'borderPosition' is ", 
                            "mandatory.")
            }
        )
        if (!is.null(error))
            return(error)
        TRUE
    }
)

#' Constructor method of STCellStyle Class.
#'
#' @name STCellStyle 
#' @rdname STCellStyle-class
#' @aliases initialize,STCellStyle-method
#' @param .Object A STCellStyle object
#' @param fontName The name of the font that should be used
#' @param fontHeight The font height
#' @param fontColor The font color
#' @param isBold Is bold? TRUE/FALSE
#' @param isItalic Is italic? TRUE/FALSE
#' @param isStrikeout Is striked out? TRUE/FALSE
#' @param underline Numeric weight of the underline
#' @param boldweight Numeric font weight
#' @param isWrapped Is wrappend? TRUE/FALSE
#' @param horizontal String with horizontal alignment
#' @param vertical String with vertical alignment
#' @param rotation Numeric holding the rotation of the text
#' @param indent Numeric holding the indnentation
#' @param borderPosition Character vector with possible border positions
#' @param borderColor Color string (hex or color name)
#' @param borderPen String with border pen value
#' @param foregroundColor Color string for foreground color
#' @param backgroundColor Color string for background color
#' @param fillPattern String with fill pattern
#' @param dataFormat String for Excel-Cell-Data-Format
#' @param isLocked Is Locked? TRUE/FALSE
#' @param isHidden Is Hidden? TRUE/FALSE
#' @param latexVerticalMove A latex string that gives the distance for vertically moving the cell content (e.g.: in order to to vertically center it).
#' @param excelPreProcess A function that can be used to pre process cell values for the Excel table generation
#' @param latexPreProcess A function that can be used to pre process cell values for the LaTeX table generation
#' @param latexFontSize The LaTeX command to set the font size
setMethod("initialize", signature(.Object = "STCellStyle"), 
    function(
        .Object,
        fontName,
        fontHeight,
        fontColor,
        isBold,
        isItalic,
        isStrikeout,
        underline,
        boldweight,
        isWrapped,
        horizontal,
        vertical,
        rotation,
        indent,
        borderPosition,
        borderColor,
        borderPen,
        foregroundColor,
        backgroundColor,
        fillPattern,
        dataFormat,
        isLocked,
        isHidden,
        latexVerticalMove,
        excelPreProcess,
        latexPreProcess,
        latexFontSize
    ) {
        # check function for the slots
        checkSlot <- function(styleName) {
            description <- checkValidity(.Object, styleName)
            if (!is.null(description))
                stop(paste0(
                    "Error in the initialization of 'STCellStyle' ",
                    "the passed in value for slot '", styleName, ",' does not ",
                    "does not meet the class requirements: ",
                    description
                ), call. = FALSE)
        }
        if (!missing(fontName)) {
            .Object@fontName = fontName
            checkSlot("fontName")
        }
        if (!missing(fontHeight)) {
            .Object@fontHeight = fontHeight
            checkSlot("fontHeight")
        }
        if (!missing(fontColor)) {
            fontColor <- transformChar(fontColor, toUpper = TRUE)
            .Object@fontColor = fontColor
            checkSlot("fontColor")
        }
        if (!missing(isBold)) {
            .Object@isBold = isBold
        }
        if (!missing(isItalic)) {
            .Object@isItalic = isItalic
        }
        if (!missing(isStrikeout)) {
            .Object@isStrikeout = isStrikeout
        }
        if (!missing(underline)) {
            .Object@underline = underline
        }
        if (!missing(boldweight)) {
            .Object@boldweight = boldweight
        }
        if (!missing(isWrapped)) {
            .Object@isWrapped = isWrapped
        }
        if (!missing(horizontal)) {
            horizontal <- transformChar(horizontal, pre = "ALIGN_", toUpper = TRUE)
            .Object@horizontal = horizontal
            checkSlot("horizontal")
        }
        if (!missing(vertical)) {
            vertical <- transformChar(vertical, pre = "VERTICAL_", toUpper = TRUE)
            .Object@vertical = vertical
            checkSlot("vertical")
        }
        if (!missing(rotation)) {
            .Object@rotation = rotation
        }
        if (!missing(indent)) {
            .Object@indent = indent
        }
        if (!missing(borderPosition)) {
            borderPosition <- transformChar(borderPosition, toUpper = TRUE)
            .Object@borderPosition = borderPosition
            checkSlot("borderPosition")
        }
        if (!missing(borderColor)) {
            borderColor <- transformChar(borderColor, toUpper = TRUE)
            .Object@borderColor = borderColor
            checkSlot("borderColor")
        }
        if (!missing(borderPen)) {
            borderPen <- transformChar(borderPen, pre = "BORDER_", toUpper = TRUE)
            .Object@borderPen = borderPen
            checkSlot("borderPen")
        }
        if (!missing(foregroundColor)) {
            foregroundColor <- transformChar(foregroundColor, toUpper = TRUE)
            .Object@foregroundColor = foregroundColor
            checkSlot("foregroundColor")
        }
        if (!missing(backgroundColor)) {
            backgroundColor <- transformChar(backgroundColor, toUpper = TRUE)
            .Object@backgroundColor = backgroundColor
            checkSlot("backgroundColor")
        }
        if (!missing(fillPattern)) {
            fillPattern <- transformChar(fillPattern, toUpper = TRUE)
            .Object@fillPattern = fillPattern
            checkSlot("fillPattern")
        }
        if (!missing(dataFormat)) {
            .Object@dataFormat = dataFormat
        }
        if (!missing(isLocked)) {
            .Object@isLocked = isLocked
        }
        if (!missing(isHidden)) {
            .Object@isHidden = isHidden
        }
        if (!missing(latexVerticalMove)) {
            .Object@latexVerticalMove = latexVerticalMove
        }
        if (!missing(excelPreProcess)) {
            .Object@excelPreProcess = excelPreProcess
        } else {
            .Object@excelPreProcess = function(x) x
        }
        if (!missing(latexPreProcess)) {
            .Object@latexPreProcess = latexPreProcess
        } else {
            .Object@latexPreProcess = sanitize
        }
        if (!missing(latexFontSize)) {
            .Object@latexFontSize = latexFontSize
            checkSlot("latexFontSize")
        }
        .Object
})

#' Set a single attribute in a style class
#'
#' @name setSTCellStyle
#' @rdname setSTCellStyle-methods
#' @exportMethod setSTCellStyle
setGeneric(
    "setSTCellStyle",
    function(object, value, styleName) standardGeneric("setSTCellStyle")
)

#' @rdname setSTCellStyle-methods
#' @aliases setSTCellStyle,STCellStyle-method
#' @param object A STCellStyle object
#' @param value The value that should be set
#' @param styleName The name of the style setting that should be set
setMethod("setSTCellStyle", signature(object = "STCellStyle", value = "ANY", styleName = "character"),
    function(object, value, styleName) {
        # Transform input values
        if (styleName %in% c(
            "fontColor", 
            "borderColor", 
            "foregroundColor", 
            "backgroundColor", 
            "fillPattern", 
            "borderPosition")
        )
            value <- transformChar(value, toUpper = TRUE)
        if (styleName == "horizontal")
            value <- transformChar(value, pre = "ALIGN_", toUpper = TRUE)
        if (styleName == "vertical")
            value <- transformChar(value, pre = "VERTICAL_", toUpper = TRUE)
        if (styleName == "borderPen")
            value <- transformChar(value, pre = "BORDER_", toUpper = TRUE)

        slot(object, styleName) <- value
        description <- checkValidity(object, styleName)
        if (!is.null(description))
            stop(paste0(
                "Error in 'setSTCellStyle' ",
                "the passed in value for style '", styleName, ",' does not ",
                "does not meet the class requirements: ",
                description
            ), call. = FALSE)
        object
    }
)

#' @rdname setSTCellStyle-methods
#' @aliases setSTCellStyle,STCellStyle-method
setMethod("setSTCellStyle", signature(object = "MissingOrNull", value = "ANY", styleName = "character"),
    function(object, value, styleName) {
        setSTCellStyle(new("STCellStyle"), value, styleName)
    }
)


#' Method getSTCellStyle
#'
#' @name getSTCellStyle
#' @rdname getSTCellStyle-methods
#' @exportMethod getSTCellStyle
#' @param ... Various arguments
setGeneric("getSTCellStyle", function(object, ...) standardGeneric("getSTCellStyle"))

#' @rdname getSTCellStyle-methods
#' @aliases getSTCellStyle,STCellStyle-method
#' @param object A STCellStyle object
#' @param styleName The name of the style setting that should be retrieved
setMethod("getSTCellStyle", signature(object = "STCellStyle"),
    function(object, styleName) {
        slot(object, styleName)
    }
)

#' @rdname getSTCellStyle-methods
#' @aliases getSTCellStyle,MissingOrNull-method
setMethod("getSTCellStyle", signature(object = "MissingOrNull"),
    function(object, styleName) {
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
    function(wb, object) standardGeneric("getXlsxFontCellStyle")
)

#' @rdname getXlsxFontCellStyle-methods
#' @aliases getXlsxFontCellStyle,STCellStyle-method
#' @param wb An xlsx workbook object
#' @param object A Style object
setMethod("getXlsxFontCellStyle", signature(wb = "ANY", object = "STCellStyle"),
    function(wb, object) {
        font <- removeMissing(list(
            name = object@fontName,
            height = object@fontHeight,
            color = object@fontColor,
            isBold = object@isBold,
            isItalic = object@isItalic,
            isStrikeout = object@isStrikeout,
            boldweight = object@boldweight,
            underline = object@underline
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

#' Method getXlsxAlignmentCellStyle
#'
#' @name getXlsxAlignmentCellStyle
#' @rdname getXlsxAlignmentCellStyle-methods
#' @exportMethod getXlsxAlignmentCellStyle
setGeneric(
    "getXlsxAlignmentCellStyle",
    function(object) standardGeneric("getXlsxAlignmentCellStyle")
)

#' @rdname getXlsxAlignmentCellStyle-methods
#' @aliases getXlsxAlignmentCellStyle,STCellStyle-method
#' @param object A Style object
setMethod("getXlsxAlignmentCellStyle", signature(object = "STCellStyle"),
    function(object) {
        doCallWithoutMissing(Alignment, list(
            horizontal = object@horizontal,
            vertical = object@vertical,
            wrapText = object@isWrapped,
            rotation = object@rotation,
            indent = object@indent
        ))
    }
)

#' Method getXlsxBorderCellStyle
#'
#' @name getXlsxBorderCellStyle
#' @rdname getXlsxBorderCellStyle-methods
#' @exportMethod getXlsxBorderCellStyle
setGeneric(
    "getXlsxBorderCellStyle",
    function(object) standardGeneric("getXlsxBorderCellStyle")
)

#' @rdname getXlsxBorderCellStyle-methods
#' @aliases getXlsxBorderCellStyle,STCellStyle-method
#' @param object A Style object
setMethod("getXlsxBorderCellStyle", signature(object = "STCellStyle"),
    function(object) {
        doCallWithoutMissing(Border, list(
            position = object@borderPosition,
            color = object@borderColor,
            pen = object@borderPen
        ))
    }
)

#' Method getCellProtectionCellStyle
#'
#' @name getCellProtectionCellStyle
#' @rdname getCellProtectionCellStyle-methods
#' @exportMethod getCellProtectionCellStyle
setGeneric(
    "getCellProtectionCellStyle", 
    function(object) standardGeneric("getCellProtectionCellStyle")
)

#' @rdname getCellProtectionCellStyle-methods
#' @aliases getCellProtectionCellStyle,STCellStyle-method
#' @param object A Style object
setMethod("getCellProtectionCellStyle", signature(object = "STCellStyle"),
    function(object) {
        doCallWithoutMissing(CellProtection, list(
            locked = object@isLocked,
            hidden = object@isHidden
        ))
    }
)

#' Method getXlsxFillCellStyle
#'
#' @name getXlsxFillCellStyle
#' @rdname getXlsxFillCellStyle-methods
#' @exportMethod getXlsxFillCellStyle
setGeneric(
    "getXlsxFillCellStyle", 
    function(object) standardGeneric("getXlsxFillCellStyle")
)

#' @rdname getXlsxFillCellStyle-methods
#' @aliases getXlsxFillCellStyle,STCellStyle-method
#' @param object A Style object
setMethod("getXlsxFillCellStyle", signature(object = "STCellStyle"),
    function(object) {
        doCallWithoutMissing(Fill, list(
            foregroundColor = object@foregroundColor,
            backgroundColor = object@backgroundColor,
            pattern = object@fillPattern
        ))
    }
)

#' Method getXlsxDataFormatCellStyle
#'
#' @name getXlsxDataFormatCellStyle
#' @rdname getXlsxDataFormatCellStyle-methods
#' @exportMethod getXlsxDataFormatCellStyle
setGeneric(
    "getXlsxDataFormatCellStyle", 
    function(object) standardGeneric("getXlsxDataFormatCellStyle")
)

#' @rdname getXlsxDataFormatCellStyle-methods
#' @aliases getXlsxDataFormatCellStyle,STCellStyle-method
#' @param object A Style object
setMethod("getXlsxDataFormatCellStyle", signature(object = "STCellStyle"),
    function(object) {
        doCallWithoutMissing(DataFormat, list(
            x = object@dataFormat
        ))
    }
)

#' Method getXlsxCellStyle
#'
#' @name getXlsxCellStyle
#' @rdname getXlsxCellStyle-methods
#' @exportMethod getXlsxCellStyle
setGeneric("getXlsxCellStyle", function(wb, object) standardGeneric("getXlsxCellStyle"))

#' @rdname getXlsxCellStyle-methods
#' @aliases getXlsxCellStyle,ANY,STCellStyle-method
#' @param wb An xlsx workbook object
#' @param object A Style object
setMethod("getXlsxCellStyle", signature(wb = "ANY", object = "STCellStyle"),
    function(wb, object) {
        doCallWithoutMissing(CellStyle, list(
            wb = wb, 
            font = getXlsxFontCellStyle(wb, object),
            alignment = getXlsxAlignmentCellStyle(object),
            border = getXlsxBorderCellStyle(object),
            fill = getXlsxFillCellStyle(object),
            cellProtection = getCellProtectionCellStyle(object),
            dataFormat = getXlsxDataFormatCellStyle(object)
        ))
    }
)


# wb <- createWorkbook()
# sheet <- createSheet(wb, "test")
# rows <- createRow(sheet, rowIndex = 1)
# cell <- createCell(rows, colIndex = 1)[[1 ,1]]
# cs <- .jcall(wb, "Lorg/apache/poi/ss/usermodel/CellStyle;", "createCellStyle")
# .jcall(cs, "V", "setFillForegroundColor", .jshort(20))
# #.jcall(cs, "V", "setFillBackgroundColor", xlsx:::.xssfcolor("#FF0000"))
# .jcall(cs, "V", "setFillPattern", .jshort(1))
# .jcall(cell, "V", "setCellStyle", cs)
# saveWorkbook(wb, "test.xlsx")
# 
