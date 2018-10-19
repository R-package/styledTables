#' Set styles of multiple cells
#' @name setSpecificStyle
#' @rdname setSpecificStyle-methods
#' @exportMethod setSpecificStyle
#' @param ... Various arguments
#' @include StyledTable.R
setGeneric(
    "setSpecificStyle", 
    function(object, value, styleName, styleType, ...) 
        standardGeneric("setSpecificStyle")
)

#' @rdname setSpecificStyle-methods
#' @aliases setSpecificStyle,StyledTable,character,ANY-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param styleName The name of the style slot that should be set
#' @param styleType The type which the value has to have (used for type checking)
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param appendMode A character string that defines if the styling value of the cell should be replaced (appendMode = "replace"), if the new styling should be appended at the end of the current styling value of the cell (appendMode = "appendBehind"), if the new styling should be appended in front of the current styling value of the cell (appendMode = "appendBefore")
setMethod(
    "setSpecificStyle", 
    signature(
        object = "StyledTable", 
        value = "ANY", 
        styleName = "character", 
        styleType = "character"
    ),
    function(
        object, 
        value, 
        styleName, 
        styleType, 
        rows = NULL, 
        cols = NULL, 
        condition = NULL, 
        conditionText = NULL,
        appendMode = "replace"
    ) {
        errHandler <- function(description) {
            stop(paste0(
                "Error while setting the style '",
                styleName,
                "' on the 'StyledTable' class: ",
                description
            ), call. = FALSE)
        }
        # Check of the passed value has the correct type
        if (!typeof(value) %in% styleType)
            errHandler(paste0(
                    "The passed 'value' has to be of type '",
                    paste0(styleType, collapse = ", "),
                    "'."
            ))
                
        # Substitute N in the rows selector
        if (is.null(rows)) {
            rows <- 1:nRows(object)
        } else {
            if (!is.numeric(rows))
                errHandler("The argument 'rows' must be numeric (Number of rows may be replaced by 'N').")
            if (any(!rows %in% 1:nRows(object)))
                errHandler(paste0(
                        "The argument 'rows' must be a subinterval of 1:",
                        nRows(object),
                        "."
                    ))
        }
        # Substitute N in the cols selector
        if (is.null(cols)) {
            cols <- 1:nCols(object)
        } else {
            if (!is.numeric(cols))
                errHandler("The argument 'cols' must be numeric (Number of rows may be replaced by 'N').")
            if (any(!cols %in% 1:nCols(object)))
                errHandler(paste0(
                        "The argument 'cols' must be a subinterval of 1:",
                        nCols(object),
                        "."
                    ))
        }
        # Evaluate the condition on the 'data' and calculate the remaining set of rows
        # In order to evaluate the condition the chosen rows in 'data' are transformed into a data.frame
        if (!is.null(condition) | !is.null(conditionText)) {
            if (!is.null(condition) & !is.null(conditionText))
                errHandler("The arguments 'condition' and 'conditionText' cannot be supplied at the same time.")
            if (!is.null(conditionText)) {
                if (!is.character(conditionText) || length(conditionText) != 1)
                    errHandler("The argument 'conditionText' must be a character vector of length one.")
                condition <- parse(text = conditionText)
            }
            errHandlerCondition <- function(x)
                errHandler(paste0(
                        "Error while evaluating the condition '",
                        deparse(condition),
                        "'. ",
                        x
                    ))
            tryCatch({
                    df <- rbindlist(lapply(object@data[rows], data.frame))
                    setnames(df, paste0("X", 1:nCols(object)))
                },
                warning = function(w)
                    errHandlerCondition("Pick a set of rows where each row has the same type (character/numeric). "),
                error = function(e)
                    errHandlerCondition("Pick a set of rows where each row has the same type (character/numeric). ")
            )
            tryCatch(
                {
                    for (j in cols) {
                        df[, X := df[, paste0("X", j)]]
                        curr_rows <- rows[eval(condition, df)]
                        object <- setStyles(object, value, styleName, curr_rows, j, appendMode)
                    }
                },
                warning = function(w)
                    errHandlerCondition("Pick a set of columns where the condition can be evaluated."),
                error = function(e)
                    errHandlerCondition("Pick a set of columns where the condition can be evaluated.")
            )
        } else {
            object <- setStyles(object, value, styleName, rows, cols, appendMode)
        }
        # Assign the value on the style
        object
    }
)

#' Set font name for LaTeX tables
#' @name setLatexFontName
#' @rdname setLatexFontName-methods
#' @exportMethod setLatexFontName
#' @param ... Various arguments
setGeneric("setLatexFontName", function(object, value, ...) standardGeneric("setLatexFontName"))

#' @rdname  setLatexFontName-methods
#' @aliases setLatexFontName,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
setMethod(
    "setLatexFontName", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL) {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "fontName", "character", rows, cols, condition, conditionText)
    }
)

#' Set font name for the Excel tables
#' @name setExcelFontName
#' @rdname setExcelFontName-methods
#' @exportMethod setExcelFontName
#' @param ... Various arguments
setGeneric("setExcelFontName", function(object, value, ...) standardGeneric("setExcelFontName"))

#' @rdname  setExcelFontName-methods
#' @aliases setExcelFontName,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
setMethod(
    "setExcelFontName", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL) {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "fontName", "character", rows, cols, condition, conditionText)
    }
)

#' Set font size for the Excel tables
#' @name setExcelFontSize
#' @rdname setExcelFontSize-methods
#' @exportMethod setExcelFontSize
#' @param ... Various arguments
setGeneric("setExcelFontSize", function(object, value, ...) standardGeneric("setExcelFontSize"))

#' @rdname  setExcelFontSize-methods
#' @aliases setExcelFontSize,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
setMethod(
    "setExcelFontSize", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL) {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "fontHeight", "double", rows, cols, condition, conditionText)
    }
)

#' Set font color 
#' @name setFontColor
#' @rdname setFontColor-methods
#' @exportMethod setFontColor
#' @param ... Various arguments
setGeneric("setFontColor", function(object, value, ...) standardGeneric("setFontColor"))

#' @rdname  setFontColor-methods
#' @aliases setFontColor,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
setMethod(
    "setFontColor", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL) {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "fontColor", "character", rows, cols, condition, conditionText)
    }
)

#' Set bold font 
#' @name setBold
#' @rdname setBold-methods
#' @exportMethod setBold
#' @param ... Various arguments
setGeneric("setBold", function(object, value, ...) standardGeneric("setBold"))

#' @rdname  setBold-methods
#' @aliases setBold,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
setMethod(
    "setBold", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL) {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "isBold", "logical", rows, cols, condition, conditionText)
    }
)

#' Set italic font 
#' @name setItalic
#' @rdname setItalic-methods
#' @exportMethod setItalic
#' @param ... Various arguments
setGeneric("setItalic", function(object, value, ...) standardGeneric("setItalic"))

#' @rdname  setItalic-methods
#' @aliases setItalic,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
setMethod(
    "setItalic", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL) {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "isItalic", "logical", rows, cols, condition, conditionText)
    }
)

#' Set strikout font 
#' @name setStrikeout
#' @rdname setStrikeout-methods
#' @exportMethod setStrikeout
#' @param ... Various arguments
setGeneric("setStrikeout", function(object, value, ...) standardGeneric("setStrikeout"))

#' @rdname  setStrikeout-methods
#' @aliases setStrikeout,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
setMethod(
    "setStrikeout", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL) {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "isStrikeout", "logical", rows, cols, condition, conditionText)
    }
)

#' Set underlined font 
#' @name setUnderline
#' @rdname setUnderline-methods
#' @exportMethod setUnderline
#' @param ... Various arguments
setGeneric("setUnderline", function(object, value, ...) standardGeneric("setUnderline"))

#' @rdname  setUnderline-methods
#' @aliases setUnderline,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
setMethod(
    "setUnderline", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL) {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "underline", "double", rows, cols, condition, conditionText)
    }
)

#' Set boldweight font 
#' @name setExcelBoldweight
#' @rdname setExcelBoldweight-methods
#' @exportMethod setExcelBoldweight
#' @param ... Various arguments
setGeneric("setExcelBoldweight", function(object, value, ...) standardGeneric("setExcelBoldweight"))

#' @rdname  setExcelBoldweight-methods
#' @aliases setExcelBoldweight,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
setMethod(
    "setExcelBoldweight", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL) {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "boldweight", "double", rows, cols, condition, conditionText)
    }
)

#' Set wrapped cell 
#' @name setWrapped
#' @rdname setWrapped-methods
#' @exportMethod setWrapped
#' @param ... Various arguments
setGeneric("setWrapped", function(object, value, ...) standardGeneric("setWrapped"))

#' @rdname  setWrapped-methods
#' @aliases setWrapped,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
setMethod(
    "setWrapped", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL) {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "isWrapped", "logical", rows, cols, condition, conditionText)
    }
)

#' Set horizontal alignment of cells 
#' @name setHorizontal
#' @rdname setHorizontal-methods
#' @exportMethod setHorizontal
#' @param ... Various arguments
setGeneric("setHorizontal", function(object, value, ...) standardGeneric("setHorizontal"))

#' @rdname  setHorizontal-methods
#' @aliases setHorizontal,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
setMethod(
    "setHorizontal", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL) {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "horizontal", "character", rows, cols, condition, conditionText)
    }
)

#' Set vertical alignment of cells 
#' @name setExcelVertical
#' @rdname setExcelVertical-methods
#' @exportMethod setExcelVertical
#' @param ... Various arguments
setGeneric("setExcelVertical", function(object, value, ...) standardGeneric("setExcelVertical"))

#' @rdname  setExcelVertical-methods
#' @aliases setExcelVertical,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
setMethod(
    "setExcelVertical", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL) {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "vertical", "character", rows, cols, condition, conditionText)
    }
)

#' Set rotation of cell content 
#' @name setRotation
#' @rdname setRotation-methods
#' @exportMethod setRotation
#' @param ... Various arguments
setGeneric("setRotation", function(object, value, ...) standardGeneric("setRotation"))

#' @rdname  setRotation-methods
#' @aliases setRotation,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
setMethod(
    "setRotation", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL) {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "rotation", "double", rows, cols, condition, conditionText)
    }
)

#' Set indentation of cell content 
#' @name setIndent
#' @rdname setIndent-methods
#' @exportMethod setIndent
#' @param ... Various arguments
setGeneric("setIndent", function(object, value, ...) standardGeneric("setIndent"))

#' @rdname  setIndent-methods
#' @aliases setIndent,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
setMethod(
    "setIndent", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL) {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "indent", "double", rows, cols, condition, conditionText)
    }
)

#' Set border position of cells 
#' @name setBorderPosition
#' @rdname setBorderPosition-methods
#' @exportMethod setBorderPosition
#' @param ... Various arguments
setGeneric("setBorderPosition", function(object, value, ...) standardGeneric("setBorderPosition"))

#' @rdname  setBorderPosition-methods
#' @aliases setBorderPosition,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param appendMode A character string that defines if the styling value of the cell should be replaced (appendMode = "replace"), if the new styling should be appended at the end of the current styling value of the cell (appendMode = "appendBehind"), if the new styling should be appended in front of the current styling value of the cell (appendMode = "appendBefore")
setMethod(
    "setBorderPosition", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL, appendMode = "appendBehind") {
        condition <- substitute(condition)
        if (is.character(value) && all(value == "all"))
            value = c("left", "right", "top", "bottom")
        setSpecificStyle(object, value, "borderPosition", "character", rows, cols, condition, conditionText, appendMode)
    }
)

#' Set border color of cells 
#' @name setBorderColor
#' @rdname setBorderColor-methods
#' @exportMethod setBorderColor
#' @param ... Various arguments
setGeneric("setBorderColor", function(object, value, ...) standardGeneric("setBorderColor"))

#' @rdname  setBorderColor-methods
#' @aliases setBorderColor,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
setMethod(
    "setBorderColor", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL) {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "borderColor", "character", rows, cols, condition, conditionText)
    }
)

#' Set border pen of cells 
#' @name setBorderPen
#' @rdname setBorderPen-methods
#' @exportMethod setBorderPen
#' @param ... Various arguments
setGeneric("setBorderPen", function(object, value, ...) standardGeneric("setBorderPen"))

#' @rdname  setBorderPen-methods
#' @aliases setBorderPen,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
setMethod(
    "setBorderPen", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL) {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "borderPen", "character", rows, cols, condition, conditionText)
    }
)

#' Set fill color of cells 
#' @name setFillColor
#' @rdname setFillColor-methods
#' @exportMethod setFillColor
#' @param ... Various arguments
setGeneric("setFillColor", function(object, value, ...) standardGeneric("setFillColor"))

#' @rdname  setFillColor-methods
#' @aliases setFillColor,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
setMethod(
    "setFillColor", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL) {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "foregroundColor", "character", rows, cols, condition, conditionText)
    }
)

#' Set data format of cells 
#' @name setExcelDataFormat
#' @rdname setExcelDataFormat-methods
#' @exportMethod setExcelDataFormat
#' @param ... Various arguments
setGeneric("setExcelDataFormat", function(object, value, ...) standardGeneric("setExcelDataFormat"))

#' @rdname  setExcelDataFormat-methods
#' @aliases setExcelDataFormat,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param appendMode A character string that defines if the styling value of the cell should be replaced (appendMode = "replace"), if the new styling should be appended at the end of the current styling value of the cell (appendMode = "appendBehind"), if the new styling should be appended in front of the current styling value of the cell (appendMode = "appendBefore")
setMethod(
    "setExcelDataFormat", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL, appendMode = "replace") {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "dataFormat", "character", rows, cols, condition, conditionText, appendMode)
    }
)

#' Set locked cell 
#' @name setExcelLocked
#' @rdname setExcelLocked-methods
#' @exportMethod setExcelLocked
#' @param ... Various arguments
setGeneric("setExcelLocked", function(object, value, ...) standardGeneric("setExcelLocked"))

#' @rdname  setExcelLocked-methods
#' @aliases setExcelLocked,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
setMethod(
    "setExcelLocked", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL) {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "isLocked", "logical", rows, cols, condition, conditionText)
    }
)

#' Set hidden cell 
#' @name setExcelHidden
#' @rdname setExcelHidden-methods
#' @exportMethod setExcelHidden
#' @param ... Various arguments
setGeneric("setExcelHidden", function(object, value, ...) standardGeneric("setExcelHidden"))

#' @rdname  setExcelHidden-methods
#' @aliases setExcelHidden,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
setMethod(
    "setExcelHidden", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL) {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "isHidden", "logical", rows, cols, condition, conditionText)
    }
)

#' Set vertical movement of cell content for LaTeX table generation
#' @name setLatexVerticalMove
#' @rdname setLatexVerticalMove-methods
#' @exportMethod setLatexVerticalMove
#' @param ... Various arguments
setGeneric("setLatexVerticalMove", function(object, value, ...) standardGeneric("setLatexVerticalMove"))

#' @rdname  setLatexVerticalMove-methods
#' @aliases setLatexVerticalMove,StyledTable,ANY-method
#' @param object A StyledTable
#' @param value The function that should be used for pre processing
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
setMethod(
    "setLatexVerticalMove", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL) {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "latexVerticalMove", "character", rows, cols, condition, conditionText)
    }
)

#' Set pre processing of cell content for LaTeX table generation
#' @name setLatexPreProcess
#' @rdname setLatexPreProcess-methods
#' @exportMethod setLatexPreProcess
#' @param ... Various arguments
setGeneric("setLatexPreProcess", function(object, value, ...) standardGeneric("setLatexPreProcess"))

#' @rdname  setLatexPreProcess-methods
#' @aliases setLatexPreProcess,StyledTable,ANY-method
#' @param object A StyledTable
#' @param value The function that should be used for pre processing
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param appendMode A character string that defines if the styling value of the cell should be replaced (appendMode = "replace"), if the new styling should be appended at the end of the current styling value of the cell (appendMode = "appendBehind"), if the new styling should be appended in front of the current styling value of the cell (appendMode = "appendBefore")
setMethod(
    "setLatexPreProcess", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL, appendMode = "replace") {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "latexPreProcess", "closure", rows, cols, condition, conditionText, appendMode)
    }
)

#' Set pre processing of cell content for excel table generation
#' @name setExcelPreProcess
#' @rdname setExcelPreProcess-methods
#' @exportMethod setExcelPreProcess
#' @param ... Various arguments
setGeneric("setExcelPreProcess", function(object, value, ...) standardGeneric("setExcelPreProcess"))

#' @rdname  setExcelPreProcess-methods
#' @aliases setExcelPreProcess,StyledTable,ANY-method
#' @param object A StyledTable
#' @param value The function that should be used for pre processing
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param appendMode A character string that defines if the styling value of the cell should be replaced (appendMode = "replace"), if the new styling should be appended at the end of the current styling value of the cell (appendMode = "appendBehind"), if the new styling should be appended in front of the current styling value of the cell (appendMode = "appendBefore")
setMethod(
    "setExcelPreProcess", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL, appendMode = "replace") {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "excelPreProcess", "closure", rows, cols, condition, conditionText, appendMode)
    }
)

#' Set LaTeX font size 
#' @name setLatexFontSize
#' @rdname setLatexFontSize-methods
#' @exportMethod setLatexFontSize
#' @param ... Various arguments
setGeneric("setLatexFontSize", function(object, value, ...) standardGeneric("setLatexFontSize"))

#' @rdname  setLatexFontSize-methods
#' @aliases setLatexFontSize,StyledTable,character-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param rows A vector of row numbers to which the change should be applied to (\code{N} is substituted with the total number of table rows)
#' @param cols A vector of col numbers to which the change should be applied to (\code{N} is substituted with the total number of table cols)
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
#' @param conditionText An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the styled table should be used. Be aware that the styled table columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{cols} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{conditionText} and \code{condition} cannot be passed at the same time.
setMethod(
    "setLatexFontSize", 
    signature(object = "StyledTable", value = "ANY"), 
    function(object, value, rows = NULL, cols = NULL, condition = NULL, conditionText = NULL) {
        condition <- substitute(condition)
        setSpecificStyle(object, value, "latexFontSize", "character", rows, cols, condition, conditionText)
    }
)

