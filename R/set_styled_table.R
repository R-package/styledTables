#' Set styles of multiple cells
#'
#' This function can only be used inside another styling function, since
#' the \code{row_id} and \code{col_id} calculation happens in the level-2 parent.frame.
#' @name set_specific_style
#' @rdname set_specific_style-methods
#' @exportMethod set_specific_style
#' @param ... Various arguments
#' @include styled_table.R
setGeneric(
    "set_specific_style",
    function(st, value, style_name, style_type, ...)
        standardGeneric("set_specific_style")
)

#' @rdname set_specific_style-methods
#' @aliases set_specific_style,StyledTable,character,ANY-method
#' @param st A [StyledTable] object
#' @param value The value that should be set
#' @param style_name The name of the style slot that should be set
#' @param style_type The type which the value has to have (used for type checking)
#' @param row_id A vector of row ids to which the change should be applied to. The variable \code{n_row} can be used in the expression to name the total number of columns (will be replaced). If the argument \code{row_id} is omitted, then the change will be applied to all rows
#' @param col_id A vector of column ids to which the change should be applied to. The variable \code{n_col} can be used in the expression to name the total number of columns (will be replaced). If the argument \code{col_id} is omitted, then the change will be applied to all columns
#' @param condition An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the [StyledTable] object should be used. Be aware that the [StyledTable] object columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{col_id} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{condition_text} and \code{condition} cannot be passed at the same time.
#' @param condition_text An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the [StyledTable] object should be used. Be aware that the [StyledTable] object columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{col_id} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{condition_text} and \code{condition} cannot be passed at the same time.
#' @param append_mode A character string that defines if the styling value of the cell should be replaced (append_mode = "replace"), if the new styling should be appended at the end of the current styling value of the cell (append_mode = "appendBehind"), if the new styling should be appended in front of the current styling value of the cell (append_mode = "appendBefore")
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_specific_style",
    signature(
        st = "StyledTable",
        value = "ANY",
        style_name = "character",
        style_type = "character"
    ),
    function(
        st,
        value,
        style_name,
        style_type,
        row_id = NULL,
        col_id = NULL,
        condition = NULL,
        condition_text = NULL,
        append_mode = "replace"
    ) {
        errHandler <- function(description) {
            stop(paste0(
                "Error while setting the style '",
                style_name,
                "' on the 'StyledTable' class: ",
                description
            ), call. = FALSE)
        }
        # Check of the passed value has the correct type
        if (!typeof(value) %in% style_type)
            errHandler(paste0(
                    "The passed 'value' has to be of type '",
                    paste0(style_type, collapse = ", "),
                    "'."
            ))
        # Substitute n_col and n_row in the rows and columns selector
        row_id <- substitute_row_id(st, row_id, stack_level = 5, errHandler)
        col_id <- substitute_col_id(st, col_id, stack_level = 5, errHandler)
        if (is.null(row_id))
            row_id <- 1:count_rows(st)
        if (is.null(col_id))
            col_id <- 1:count_cols(st)
        # Evaluate the condition on the 'data' and calculate the remaining set of row_id
        # In order to evaluate the condition the chosen rows in 'data' are transformed into a data.frame
        if (!is.null(condition) | !is.null(condition_text)) {
            if (!is.null(condition) & !is.null(condition_text))
                errHandler("The arguments 'condition' and 'condition_text' cannot be supplied at the same time.")
            if (!is.null(condition_text)) {
                if (!is.character(condition_text) || length(condition_text) != 1)
                    errHandler("The argument 'condition_text' must be a character vector of length one.")
                condition <- parse(text = condition_text)
            }
            errHandlerCondition <- function(x)
                errHandler(paste0(
                        "Error while evaluating the condition '",
                        deparse(condition),
                        "'. ",
                        x
                    ))
            tryCatch({
                    df <- rbindlist(lapply(st@data[row_id], data.frame), use.names = FALSE)
                    setnames(df, paste0("X", 1:count_cols(st)))
                },
                warning = function(w)
                    errHandlerCondition("Pick a set of rows where each row has the same type (character/numeric)."),
                error = function(e)
                    errHandlerCondition("Pick a set of rows where each row has the same type (character/numeric).")
            )
            tryCatch(
                {
                    for (j in col_id) {
                        df[, X := df[[paste0("X", j)]]]
                        ind <- eval(condition, df)
                        if (!is.logical(ind))
                            stop("The condition does not evaluate to a logical result.")
                        ind[is.na(ind)] <- FALSE
                        curr_row_id <- row_id[ind]
                        st <- setStyles(st, value, style_name, curr_row_id, j, append_mode)
                    }
                },
                warning = function(w)
                    errHandlerCondition("Pick a set of columns where the condition can be evaluated."),
                error = function(e)
                    errHandlerCondition("Pick a set of columns where the condition can be evaluated.")
            )
        } else {
            st <- setStyles(st, value, style_name, row_id, col_id, append_mode)
        }
        # Assign the value on the style
        st
    }
)

#' Set font name for LaTeX tables
#' @name set_latex_font_name
#' @rdname set_latex_font_name-methods
#' @exportMethod set_latex_font_name
#' @param ... Various arguments
setGeneric("set_latex_font_name", function(st, value, ...) standardGeneric("set_latex_font_name"))

#' @rdname  set_latex_font_name-methods
#' @aliases set_latex_font_name,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_latex_font_name",
    signature(st = "StyledTable", value = "ANY"),
    function(st, value, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL) {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "latex_font_name", "character", row_id, col_id, condition, condition_text)
    }
)

#' Set font name for the Excel tables
#' @name set_excel_font_name
#' @rdname set_excel_font_name-methods
#' @exportMethod set_excel_font_name
#' @param ... Various arguments
setGeneric("set_excel_font_name", function(st, value, ...) standardGeneric("set_excel_font_name"))

#' @rdname  set_excel_font_name-methods
#' @aliases set_excel_font_name,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_excel_font_name",
    signature(st = "StyledTable", value = "ANY"),
    function(st, value, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL) {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "excel_font_name", "character", row_id, col_id, condition, condition_text)
    }
)

#' Set LaTeX font size
#' @name set_latex_font_size
#' @rdname set_latex_font_size-methods
#' @exportMethod set_latex_font_size
#' @param ... Various arguments
setGeneric("set_latex_font_size", function(st, value, ...) standardGeneric("set_latex_font_size"))

#' @rdname  set_latex_font_size-methods
#' @aliases set_latex_font_size,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_latex_font_size",
    signature(st = "StyledTable", value = "ANY"),
    function(st, value, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL) {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "latex_font_size", "character", row_id, col_id, condition, condition_text)
    }
)

#' Set font size for the Excel tables
#' @name set_excel_font_size
#' @rdname set_excel_font_size-methods
#' @exportMethod set_excel_font_size
#' @param ... Various arguments
setGeneric("set_excel_font_size", function(st, value, ...) standardGeneric("set_excel_font_size"))

#' @rdname  set_excel_font_size-methods
#' @aliases set_excel_font_size,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_excel_font_size",
    signature(st = "StyledTable", value = "ANY"),
    function(st, value, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL) {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "excel_font_size", "double", row_id, col_id, condition, condition_text)
    }
)

#' Set font color
#' @name set_font_color
#' @rdname set_font_color-methods
#' @exportMethod set_font_color
#' @param ... Various arguments
setGeneric("set_font_color", function(st, value, ...) standardGeneric("set_font_color"))

#' @rdname  set_font_color-methods
#' @aliases set_font_color,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_font_color",
    signature(st = "StyledTable", value = "ANY"),
    function(st, value, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL) {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "font_color", "character", row_id, col_id, condition, condition_text)
    }
)

#' Set bold font
#' @name set_bold
#' @rdname set_bold-methods
#' @exportMethod set_bold
#' @param ... Various arguments
setGeneric("set_bold", function(st, ...) standardGeneric("set_bold"))

#' @rdname  set_bold-methods
#' @aliases set_bold,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_bold",
    signature(st = "StyledTable"),
    function(st, value = TRUE, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL) {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "bold", "logical", row_id, col_id, condition, condition_text)
    }
)

#' Set italic font
#' @name set_italic
#' @rdname set_italic-methods
#' @exportMethod set_italic
#' @param ... Various arguments
setGeneric("set_italic", function(st, ...) standardGeneric("set_italic"))

#' @rdname  set_italic-methods
#' @aliases set_italic,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_italic",
    signature(st = "StyledTable"),
    function(st, value = TRUE, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL) {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "italic", "logical", row_id, col_id, condition, condition_text)
    }
)

#' Set strikout font
#' @name set_strikeout
#' @rdname set_strikeout-methods
#' @exportMethod set_strikeout
#' @param ... Various arguments
setGeneric("set_strikeout", function(st, ...) standardGeneric("set_strikeout"))

#' @rdname  set_strikeout-methods
#' @aliases set_strikeout,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_strikeout",
    signature(st = "StyledTable"),
    function(st, value = TRUE, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL) {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "strikeout", "logical", row_id, col_id, condition, condition_text)
    }
)

#' Set underlined font
#' @name set_underline
#' @rdname set_underline-methods
#' @exportMethod set_underline
#' @param ... Various arguments
setGeneric("set_underline", function(st, ...) standardGeneric("set_underline"))

#' @rdname  set_underline-methods
#' @aliases set_underline,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_underline",
    signature(st = "StyledTable"),
    function(st, value = TRUE, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL) {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "underline", "double", row_id, col_id, condition, condition_text)
    }
)

#' Set excel_boldweight font
#' @name set_excel_boldweight
#' @rdname set_excel_boldweight-methods
#' @exportMethod set_excel_boldweight
#' @param ... Various arguments
setGeneric("set_excel_boldweight", function(st, value, ...) standardGeneric("set_excel_boldweight"))

#' @rdname  set_excel_boldweight-methods
#' @aliases set_excel_boldweight,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_excel_boldweight",
    signature(st = "StyledTable", value = "ANY"),
    function(st, value, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL) {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "excel_boldweight", "double", row_id, col_id, condition, condition_text)
    }
)

#' Set wrapped cell
#' @name set_excel_wrapped
#' @rdname set_excel_wrapped-methods
#' @exportMethod set_excel_wrapped
#' @param ... Various arguments
setGeneric("set_excel_wrapped", function(st, ...) standardGeneric("set_excel_wrapped"))

#' @rdname  set_excel_wrapped-methods
#' @aliases set_excel_wrapped,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_excel_wrapped",
    signature(st = "StyledTable"),
    function(st, value = TRUE, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL) {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "excel_wrapped", "logical", row_id, col_id, condition, condition_text)
    }
)

#' Set horizontal alignment of cells
#' @name set_horizontal
#' @rdname set_horizontal-methods
#' @exportMethod set_horizontal
#' @param ... Various arguments
setGeneric("set_horizontal", function(st, value, ...) standardGeneric("set_horizontal"))

#' @rdname  set_horizontal-methods
#' @aliases set_horizontal,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_horizontal",
    signature(st = "StyledTable", value = "ANY"),
    function(st, value, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL) {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "horizontal", "character", row_id, col_id, condition, condition_text)
    }
)

#' Set vertical alignment of cells
#' @name set_excel_vertical
#' @rdname set_excel_vertical-methods
#' @exportMethod set_excel_vertical
#' @param ... Various arguments
setGeneric("set_excel_vertical", function(st, value, ...) standardGeneric("set_excel_vertical"))

#' @rdname  set_excel_vertical-methods
#' @aliases set_excel_vertical,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_excel_vertical",
    signature(st = "StyledTable", value = "ANY"),
    function(st, value, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL) {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "vertical", "character", row_id, col_id, condition, condition_text)
    }
)

#' Set rotation of cell content
#' @name set_rotation
#' @rdname set_rotation-methods
#' @exportMethod set_rotation
#' @param ... Various arguments
setGeneric("set_rotation", function(st, value, ...) standardGeneric("set_rotation"))

#' @rdname  set_rotation-methods
#' @aliases set_rotation,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_rotation",
    signature(st = "StyledTable", value = "ANY"),
    function(st, value, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL) {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "rotation", "double", row_id, col_id, condition, condition_text)
    }
)

#' Set indentation of cell content
#' @name set_indent
#' @rdname set_indent-methods
#' @exportMethod set_indent
#' @param ... Various arguments
setGeneric("set_indent", function(st, value, ...) standardGeneric("set_indent"))

#' @rdname  set_indent-methods
#' @aliases set_indent,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_indent",
    signature(st = "StyledTable", value = "ANY"),
    function(st, value, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL) {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "indent", "double", row_id, col_id, condition, condition_text)
    }
)

#' Set border position of cells
#' @name set_border_position
#' @rdname set_border_position-methods
#' @exportMethod set_border_position
#' @param ... Various arguments
setGeneric("set_border_position", function(st, value, ...) standardGeneric("set_border_position"))

#' @rdname  set_border_position-methods
#' @aliases set_border_position,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_border_position",
    signature(st = "StyledTable", value = "ANY"),
    function(st, value, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL, append_mode = "appendBehind") {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        if (is.character(value) && all(value == "all"))
            value <- c("left", "right", "top", "bottom")
        set_specific_style(st, value, "border_position", "character", row_id, col_id, condition, condition_text, append_mode)
    }
)

#' Set border color of cells
#' @name set_border_color
#' @rdname set_border_color-methods
#' @exportMethod set_border_color
#' @param ... Various arguments
setGeneric("set_border_color", function(st, value, ...) standardGeneric("set_border_color"))

#' @rdname  set_border_color-methods
#' @aliases set_border_color,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_border_color",
    signature(st = "StyledTable", value = "ANY"),
    function(st, value, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL) {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "border_color", "character", row_id, col_id, condition, condition_text)
    }
)

#' Set border pen of cells
#' @name set_excel_border_pen
#' @rdname set_excel_border_pen-methods
#' @exportMethod set_excel_border_pen
#' @param ... Various arguments
setGeneric("set_excel_border_pen", function(st, value, ...) standardGeneric("set_excel_border_pen"))

#' @rdname  set_excel_border_pen-methods
#' @aliases set_excel_border_pen,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_excel_border_pen",
    signature(st = "StyledTable", value = "ANY"),
    function(st, value, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL) {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "excel_border_pen", "character", row_id, col_id, condition, condition_text)
    }
)

#' Set fill color of cells
#' @name set_fill_color
#' @rdname set_fill_color-methods
#' @exportMethod set_fill_color
#' @param ... Various arguments
setGeneric("set_fill_color", function(st, value, ...) standardGeneric("set_fill_color"))

#' @rdname  set_fill_color-methods
#' @aliases set_fill_color,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_fill_color",
    signature(st = "StyledTable", value = "ANY"),
    function(st, value, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL) {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "fill_color", "character", row_id, col_id, condition, condition_text)
    }
)




#' Set data format of cells
#' @name set_excel_data_format
#' @rdname set_excel_data_format-methods
#' @exportMethod set_excel_data_format
#' @param ... Various arguments
setGeneric("set_excel_data_format", function(st, value, ...) standardGeneric("set_excel_data_format"))

#' @rdname  set_excel_data_format-methods
#' @aliases set_excel_data_format,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_excel_data_format",
    signature(st = "StyledTable", value = "ANY"),
    function(st, value, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL, append_mode = "replace") {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "excel_data_format", "character", row_id, col_id, condition, condition_text, append_mode)
    }
)

#' Set locked cell
#' @name set_excel_locked
#' @rdname set_excel_locked-methods
#' @exportMethod set_excel_locked
#' @param ... Various arguments
setGeneric("set_excel_locked", function(st, ...) standardGeneric("set_excel_locked"))

#' @rdname  set_excel_locked-methods
#' @aliases set_excel_locked,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_excel_locked",
    signature(st = "StyledTable"),
    function(st, value = TRUE, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL) {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "excel_locked", "logical", row_id, col_id, condition, condition_text)
    }
)

#' Set hidden cell
#' @name set_excel_hidden
#' @rdname set_excel_hidden-methods
#' @exportMethod set_excel_hidden
#' @param ... Various arguments
setGeneric("set_excel_hidden", function(st, ...) standardGeneric("set_excel_hidden"))

#' @rdname  set_excel_hidden-methods
#' @aliases set_excel_hidden,StyledTable,character-method
#' @inheritParams set_specific_style
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_excel_hidden",
    signature(st = "StyledTable"),
    function(st, value = TRUE, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL) {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "excel_hidden", "logical", row_id, col_id, condition, condition_text)
    }
)

#' Set vertical movement of cell content for LaTeX table generation
#' @name set_latex_vertical_move
#' @rdname set_latex_vertical_move-methods
#' @exportMethod set_latex_vertical_move
#' @param ... Various arguments
setGeneric("set_latex_vertical_move", function(st, value, ...) standardGeneric("set_latex_vertical_move"))

#' @rdname  set_latex_vertical_move-methods
#' @aliases set_latex_vertical_move,StyledTable,ANY-method
#' @inheritParams set_specific_style
#' @param value The function that should be used for pre processing
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_latex_vertical_move",
    signature(st = "StyledTable", value = "ANY"),
    function(st, value, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL) {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "latex_vertical_move", "character", row_id, col_id, condition, condition_text)
    }
)

#' Set pre processing of cell content for LaTeX table generation
#' @name set_latex_pre_process
#' @rdname set_latex_pre_process-methods
#' @exportMethod set_latex_pre_process
#' @param ... Various arguments
setGeneric("set_latex_pre_process", function(st, value, ...) standardGeneric("set_latex_pre_process"))

#' @rdname  set_latex_pre_process-methods
#' @aliases set_latex_pre_process,StyledTable,ANY-method
#' @inheritParams set_specific_style
#' @param value The function that should be used for pre processing
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_latex_pre_process",
    signature(st = "StyledTable", value = "ANY"),
    function(st, value, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL, append_mode = "replace") {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "latex_pre_process", "closure", row_id, col_id, condition, condition_text, append_mode)
    }
)

#' Set pre processing of cell content for excel table generation
#' @name set_excel_pre_process
#' @rdname set_excel_pre_process-methods
#' @exportMethod set_excel_pre_process
#' @param ... Various arguments
setGeneric("set_excel_pre_process", function(st, value, ...) standardGeneric("set_excel_pre_process"))

#' @rdname  set_excel_pre_process-methods
#' @aliases set_excel_pre_process,StyledTable,ANY-method
#' @inheritParams set_specific_style
#' @param value The function that should be used for pre processing
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
    "set_excel_pre_process",
    signature(st = "StyledTable", value = "ANY"),
    function(st, value, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL, append_mode = "replace") {
        condition <- substitute(condition)
        row_id <- substitute(row_id)
        col_id <- substitute(col_id)
        set_specific_style(st, value, "excel_pre_process", "closure", row_id, col_id, condition, condition_text, append_mode)
    }
)

#' Set pre processing of cell content for html table generation
#' @name set_html_pre_process
#' @rdname set_html_pre_process-methods
#' @exportMethod set_html_pre_process
#' @param ... Various arguments
setGeneric("set_html_pre_process", function(st, value, ...) standardGeneric("set_html_pre_process"))

#' @rdname  set_html_pre_process-methods
#' @aliases set_html_pre_process,StyledTable,ANY-method
#' @inheritParams set_specific_style
#' @param value The function that should be used for pre processing
#' @return The modified [StyledTable] object
#' @family styledtable setters
setMethod(
  "set_html_pre_process",
  signature(st = "StyledTable", value = "ANY"),
  function(st, value, row_id = NULL, col_id = NULL, condition = NULL, condition_text = NULL, append_mode = "replace") {
    condition <- substitute(condition)
    row_id <- substitute(row_id)
    col_id <- substitute(col_id)
    set_specific_style(st, value, "html_pre_process", "closure", row_id, col_id, condition, condition_text, append_mode)
  }
)
