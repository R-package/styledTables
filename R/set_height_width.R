#' Function to set the excel_row_height/latex_row_height/excel_col_width/latex_col_width
#'
#' @param st The [StyledTable] object whoose heights/widths should be changed
#' @param slot_name The name of the S4-class slot that should be changed (\code{excel_row_height}, \code{latex_row_height}, \code{excel_col_width}, \code{latex_col_width})
#' @param id The row/column ids that should be changed (subset of 1:N where N is the number of rows/cols of the [StyledTable] object).
#' @param value A vector holding the row height/col width that should be applied
set_height_width <- function(st, slot_name, id, value) {
    # Switch between row heights/col widths
    switch(slot_name,
        excel_row_height = {
            typeInd <- "row_id"
            typeVal <- "height"
            maxInd <- count_rows(st)
            fnName <- "set_excel_row_height"
        },
        latex_row_height = {
            typeInd <- "row_id"
            typeVal <- "height"
            maxInd <- count_rows(st)
            fnName <- "set_latex_row_height"
        },
        excel_col_width = {
            typeInd <- "col_id"
            typeVal <- "width"
            maxInd <- count_cols(st)
            fnName <- "set_excel_col_width"
        },
        latex_col_width = {
            typeInd <- "col_id"
            typeVal <- "width"
            maxInd <- count_cols(st)
            fnName <- "set_latex_col_width"
        })
    # Error handling
    errHandler <- function(description) {
        stop(paste0(
            "Error in '",
            fnName,
            "': ",
            description
        ), call. = FALSE)
    }
    # ---- Check the arguments ---
    # Check if the rows/cols selector is numeric and part of 1:N
    if (is.null(id)) {
        id <- 1:maxInd
    } else {
        if (!is.numeric(id) || length(id) == 0 || any(is.na(id)))
            errHandler(paste0(
                "The argument '",
                typeInd,
                "' must be an unbroken numeric vector. "
            ))
        if (any(!id %in% 1:maxInd))
            errHandler(paste0(
                    "The argument '",
                    typeInd,
                    "' must be a subinterval of 1:",
                    maxInd,
                    "."
                ))
    }
    # Check if values has the right type and length
    if (
            !is.numeric(value) ||
            !length(value) %in% c(1, length(id)) ||
            any(is.na(value))
        )
        errHandler(paste0(
                "The argument 'value' has to be a numeric vector of length 1 ",
                "or the same number of ",
                typeInd,
                "s as the styled table."
            ))
    # Check if values are all > 0
    if (any(value <= 0))
        errHandler("The argument 'value' must be a positive vector.")
    # If the value argument has length 1 then it should be applied to every indces
    if (length(value) == 1)
        value <- rep(value, length(id))
    # --- Assign width/height information ----
    # rows/cols that are alreade defined will be overwritten
    # and add new row/cols definitions will be appended
    idRemove <- slot(st, slot_name)[[typeInd]] %in% id
    slot(st, slot_name)[[typeInd]] <- c(slot(st, slot_name)[[typeInd]][!idRemove], id)
    slot(st, slot_name)[[typeVal]] <- c(slot(st, slot_name)[[typeVal]][!idRemove], value)
    st
}

#' Method set_excel_row_height.
#'
#' @name set_excel_row_height
#' @rdname set_excel_row_height-methods
#' @exportMethod set_excel_row_height
#' @param ... Various arguments
#' @include styled_table.R
setGeneric("set_excel_row_height", function(st, value, ...) standardGeneric("set_excel_row_height"))

#' @rdname set_excel_row_height-methods
#' @aliases setRowHeiths,StyledTable,numeric-method
#' @param st A [StyledTable] object
#' @param value A numeric vector (length 1 or same lengt as \code{row_id}) holding the row heights
#' @param row_id A vector of row numbers (N is substituted as \code{count_rows(st)})
#' @return The modified [StyledTable] object
setMethod(
    "set_excel_row_height",
    signature(
        st = "StyledTable",
        value = "numeric"
    ),
    function(st, value, row_id = NULL) {
        set_height_width(st, "excel_row_height", row_id, value)
    }
)

#' Method set_excel_col_width.
#'
#' @name set_excel_col_width
#' @rdname set_excel_col_width-methods
#' @exportMethod set_excel_col_width
#' @param ... Various arguments
setGeneric("set_excel_col_width", function(st, value, ...) standardGeneric("set_excel_col_width"))

#' @rdname set_excel_col_width-methods
#' @aliases set_excel_col_width,StyledTable,numeric-method
#' @param st A [StyledTable] object
#' @param value A numeric vector (length 1 or same lengt as \code{col_id}) holding the column widths
#' @param col_id A vector of column numbers (N is substituted as \code{count_cols(st)})
#' @return The modified [StyledTable] object
setMethod(
    "set_excel_col_width",
    signature(
        st = "StyledTable",
        value = "numeric"
    ),
    function(st, value, col_id = NULL) {
        set_height_width(st, "excel_col_width", col_id, value)
    }
)

#' Method set_latex_row_height.
#'
#' @name set_latex_row_height
#' @rdname set_latex_row_height-methods
#' @exportMethod set_latex_row_height
#' @param ... Various arguments
#' @include styled_table.R
setGeneric("set_latex_row_height", function(st, value, ...) standardGeneric("set_latex_row_height"))

#' @rdname set_latex_row_height-methods
#' @aliases setLatexRowHeiths,StyledTable,numeric-method
#' @param st A [StyledTable] object
#' @param value A numeric vector (length 1 or same lengt as \code{row_id}) holding the row heights
#' @param row_id A vector of row numbers (N is substituted as \code{count_rows(st)})
#' @return The modified [StyledTable] object
setMethod(
    "set_latex_row_height",
    signature(
        st = "StyledTable",
        value = "numeric"
    ),
    function(st, value, row_id = NULL) {
        set_height_width(st, "latex_row_height", row_id, value)
    }
)

#' Method set_latex_col_width.
#'
#' @name set_latex_col_width
#' @rdname set_latex_col_width-methods
#' @exportMethod set_latex_col_width
#' @param ... Various arguments
setGeneric("set_latex_col_width", function(st, value, ...) standardGeneric("set_latex_col_width"))

#' @rdname set_latex_col_width-methods
#' @aliases set_latex_col_width,StyledTable,numeric-method
#' @param st A [StyledTable] object
#' @param value A numeric vector (length 1 or same lengt as \code{col_id}) holding the column widths
#' @param col_id A vector of column numbers (N is substituted as \code{count_cols(st)})
#' @return The modified [StyledTable] object
setMethod(
    "set_latex_col_width",
    signature(
        st = "StyledTable",
        value = "numeric"
    ),
    function(st, value, col_id = NULL) {
        set_height_width(st, "latex_col_width", col_id, value)
    }
)
