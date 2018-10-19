#' Function to set the rowHeights/latexRowHeights/colWidths/latexColWidths
#'
#' @param object The styled table whoose heights/widths should be changed
#' @param slotName The name of the object slot that should be changed ("rowHeights", "latexRowHeights", "colWidths", "latexColWidths")
#' @param ind The row/col numbers that should be changed (subset of 1:N where N is the number of rows/cols of the styled table).
#' @param value A vector holding the row height/col width that should be applied
setHeightsWidths <- function(object, slotName, ind, value) {
    # Switch between row heights/col widths
    switch(slotName,
        rowHeights = {
            typeInd <- "rows"
            typeVal <- "heights"
            maxInd <- nRows(object)
            fnName <- "setExcelRowHeights"
        },
        latexRowHeights = {
            typeInd <- "rows"
            typeVal <- "heights"
            maxInd <- nRows(object)
            fnName <- "setLatexRowHeights"
        },
        colWidths = {
            typeInd <- "cols"
            typeVal <- "widths"
            maxInd <- nCols(object)
            fnName <- "setExcelColWidths"
        },
        latexColWidths = {
            typeInd <- "cols"
            typeVal <- "widths"
            maxInd <- nCols(object)
            fnName <- "setLatexColWidths"
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
    if (is.null(ind)) {
        ind <- 1:maxInd
    } else {
        if (!is.numeric(ind) || length(ind) == 0 || any(is.na(ind)))
            errHandler(paste0(
                "The argument '",
                typeInd,
                "' must be an unbroken numeric vector. "
            ))
        if (any(!ind %in% 1:maxInd))
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
            !length(value) %in% c(1, length(ind)) ||
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
    # If the value argument has length 1 then it should be applied to every indices
    if (length(value) == 1)
        value <- rep(value, length(ind))
    # --- Assign width/height information ----
    # rows/cols that are alreade defined will be overwritten
    # and add new row/cols definitions will be appended
    indRemove <- slot(object, slotName)[[typeInd]] %in% ind
    slot(object, slotName)[[typeInd]] <- c(slot(object, slotName)[[typeInd]][!indRemove], ind)
    slot(object, slotName)[[typeVal]] <- c(slot(object, slotName)[[typeVal]][!indRemove], value)
    object
}

#' Method setExcelRowHeights.
#'
#' @name setExcelRowHeights
#' @rdname setExcelRowHeights-methods
#' @exportMethod setExcelRowHeights
#' @param ... Various arguments
#' @include StyledTable.R
setGeneric("setExcelRowHeights", function(object, value, ...) standardGeneric("setExcelRowHeights"))

#' @rdname setExcelRowHeights-methods
#' @aliases setRowHeiths,StyledTable,numeric-method
#' @param object A StyledTable
#' @param value A numeric vector (length 1 or same lengt as \code{rows}) holding the row heights
#' @param rows A vector of row numbers (N is substituted as \code{nRows(object)})
setMethod(
    "setExcelRowHeights", 
    signature(
        object = "StyledTable",
        value = "numeric"
    ),
    function(object, value, rows = NULL) {
        setHeightsWidths(object, "rowHeights", rows, value)
    }
)

#' Method setExcelColWidths.
#'
#' @name setExcelColWidths
#' @rdname setExcelColWidths-methods
#' @exportMethod setExcelColWidths
#' @param ... Various arguments
setGeneric("setExcelColWidths", function(object, value, ...) standardGeneric("setExcelColWidths"))

#' @rdname setExcelColWidths-methods
#' @aliases setExcelColWidths,StyledTable,numeric-method
#' @param object A StyledTable
#' @param value A numeric vector (length 1 or same lengt as \code{cols}) holding the column widths
#' @param cols A vector of column numbers (N is substituted as \code{nCols(object)})
setMethod(
    "setExcelColWidths", 
    signature(
        object = "StyledTable",
        value = "numeric"
    ),
    function(object, value, cols = NULL) {
        setHeightsWidths(object, "colWidths", cols, value)
    }
)

#' Method setLatexRowHeights.
#'
#' @name setLatexRowHeights
#' @rdname setLatexRowHeights-methods
#' @exportMethod setLatexRowHeights
#' @param ... Various arguments
#' @include StyledTable.R
setGeneric("setLatexRowHeights", function(object, value, ...) standardGeneric("setLatexRowHeights"))

#' @rdname setLatexRowHeights-methods
#' @aliases setLatexRowHeiths,StyledTable,numeric-method
#' @param object A StyledTable
#' @param value A numeric vector (length 1 or same lengt as \code{rows}) holding the row heights
#' @param rows A vector of row numbers (N is substituted as \code{nRows(object)})
setMethod(
    "setLatexRowHeights", 
    signature(
        object = "StyledTable",
        value = "numeric"
    ),
    function(object, value, rows = NULL) {
        setHeightsWidths(object, "latexRowHeights", rows, value)
    }
)

#' Method setLatexColWidths.
#'
#' @name setLatexColWidths
#' @rdname setLatexColWidths-methods
#' @exportMethod setLatexColWidths
#' @param ... Various arguments
setGeneric("setLatexColWidths", function(object, value, ...) standardGeneric("setLatexColWidths"))

#' @rdname setLatexColWidths-methods
#' @aliases setLatexColWidths,StyledTable,numeric-method
#' @param object A StyledTable
#' @param value A numeric vector (length 1 or same lengt as \code{cols}) holding the column widths
#' @param cols A vector of column numbers (N is substituted as \code{nCols(object)})
setMethod(
    "setLatexColWidths", 
    signature(
        object = "StyledTable",
        value = "numeric"
    ),
    function(object, value, cols = NULL) {
        setHeightsWidths(object, "latexColWidths", cols, value)
    }
)
