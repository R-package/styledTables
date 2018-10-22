#' Class StyledTable
#'
#' @name StyledTable-class
#' @rdname StyledTable-class
#' @exportClass StyledTable
#' @include Style.R
setClass(
    "StyledTable",
    representation(
        data = "list",
        merges = "list",
        styles = "list",
        rowHeights = "list",
        colWidths = "list",
        latexRowHeights = "list",
        latexColWidths = "list"
    )
)

#' Constructor method of StyledTable Class.
#'
#' @name StyledTable
#' @aliases initialize,StyledTable-method
#' @rdname StyledTable-class
#' @param .Object A StyledTable
#' @param ... All slots that should be set
setMethod("initialize", signature(.Object = "StyledTable"), function(.Object, ...) {
    # check function for the slots
    errHandler <- function(description)
        stop(paste0(
            "Error in the initialization of class 'StyledTable' ",
            "the passed in values ",
            "do not meet the class requirements: ",
            description
        ), call. = FALSE)
    # passed in arguments
    args <- list(...)
    ######### read data ###########
    if (!"data" %in% names(args))
        errHandler("Argument 'data' must not be empty.")
    if (!is.data.frame(args$data) && !is.matrix(args$data))
        errHandler("The argument 'data' has to be a non empty matrix or data.frame.")
    if (is.matrix(args$data))
        args$data <- data.frame(args$data)
    # number of data rows
    nRow <- nrow(args$data)
    # number of data cols
    nCol <- ncol(args$data)
    if (nRow == 0 || nCol == 0) 
        errHandler("The argument 'data' has to be a non empty  matrix or data.frame.")
    .Object@data <- lapply(1:nRow, function(i) unname(as.list(args$data[i,])))
    ######### read merges ###########
    if ("merges" %in% names(args)) {
        # Check if merges is a list
        if (!is.list(args$merges))
            errHandler("The argument 'merges' must be a list.")
        # Check if merges is list containing lists(rows =..., cols = ...)
        for (m in args$merges) {
            if (
                    !is.list(m) ||
                    !is.numeric(m$rows) || 
                    !is.numeric(m$cols) || 
                    any(is.na(m$rows)) ||
                    any(is.na(m$cols)) ||
                    length(m$rows) != 2 || 
                    length(m$cols) != 2 ||
                    any(!m$rows %in% 1:nRow) ||
                    any(!m$cols %in% 1:nCol) ||
                    diff(m$rows) < 0 ||
                    diff(m$cols) < 0
                )
                errHandler(paste0(
                        "The each element in the argument 'merges' ",
                        "must be a list containing a ",
                        "'rows' and a 'cols' variable ",
                        "of length two."
                    ))
        }
        # Check if merged regions are not overlapping 
        for (i in seq_len(length(args$merges))) {
            m1 <- args$merges[[i]]
            for (j in seq.save(i + 1, length(args$merges))) {
                m2 <- args$merges[[j]]
                if (
                        any(betweenVec(m1$rows, m2$rows)) && 
                        any(betweenVec(m1$cols, m2$cols))
                    )
                    errHandler("The 'merges' argument contains an overlapping cell.")
            }
        }
        # assign merges
        .Object@merges <- args$merges 
    } else {
        .Object@merges <- list()
    }
    ######### read styles ###########
    if ("styles" %in% names(args)) {
        # Check if styles is a list
        if (!is.list(args$styles) || length(args$styles) != nRow)
            errHandler("The argument 'styles' must be a list, whose length is the same as the number of rows of the argument 'data'.")
        # Check if each element of styles is a list of length nCol and each element is NULL or a Style
        for (l in args$styles) {
            if (!is.list(l) || length(l) != nCol)
                errHandler(paste0(
                        "The argument 'styles'",
                        "must be list of length 'nrow(data)' of lists of",
                        "length 'ncol(data)' and each inner element must be",
                        "NULL or an object of class 'Style'."
                    ))
            for (style in l)
                if (!is.null(style) && !"STCellStyle" %in% class(style))
                    errHandler(paste0(
                            "The argument 'styles'",
                            "must be list of length 'nrow(data)' of lists of",
                            "length 'ncol(data)' and each inner element must be",
                            "NULL or an object of class 'Style'."
                        ))
        }
        # assign styles
        .Object@styles <- args$styles 
    } else {
        .Object@styles <- lapply(seq_len(nRow), function(i) vector("list", nCol))
    }
    ######### read rowHeights ###########
    if ("rowHeights" %in% names(args)) {
        # Check if rowHeights$rows and $heights have correct types and same length
        if (
                !is.list(args$rowHeights) ||
                !is.numeric(args$rowHeights$rows) || 
                !is.numeric(args$rowHeights$heights) || 
                length(args$rowHeights$rows) != length(args$rowHeights$heights)
            )
            errHandler(paste0(
                    "The slot 'rowHeights' must contain a list ", 
                    "which holds numeric vectors 'rows' and ", 
                    "'heights' of the same length."
                )) 
        # Check if rowHeights$rows is a subset of 1:N
        if (any(!args$rowHeights$rows %in% 1:nRow))
            errHandler(paste0(
                    "The list element 'rows' in the 'rowHeights' slot ",
                    "must be a subset of possible row indices of the ",
                    "data list in 'data'."
                )) 
        # Check if rowHeights$heights >0
        if (any(!args$rowHeights$heights <= 0))
            errHandler(paste0(
                    "The list element 'heights' in the 'rowHeights' slot ",
                    "must be a vector of positive numbers."
                )) 
        # assign rowHeights
        .Object@rowHeights <- args$rowHeights
    } else {
        .Object@rowHeights <- list(rows = numeric(0), heights = numeric(0))
    }
    ######### read colWidths ###########
    if ("colWidths" %in% names(args)) {
        # Check if colWidths$cols and $widths have correct types and same length
        if (
                !is.list(args$colWidths) ||
                !is.numeric(args$colWidths$cols) || 
                !is.numeric(args$colWidths$widths) || 
                length(args$colWidths$cols) != length(args$colWidths$widths)
            )
            errHandler(paste0(
                    "The slot 'colWidths' must contain a list ", 
                    "which holds numeric vectors 'cols' and ", 
                    "'widths' of the same length."
                )) 
        # Check if colWidths$cols is a subset of 1:N
        if (any(!args$colWidths$cols %in% 1:nCol))
            errHandler(paste0(
                    "The list element 'cols' in the 'colWidths' slot ",
                    "must be a subset of possible col indices of the ",
                    "data list in 'data'."
                )) 
        # Check if colWidths$widths >0
        if (any(!args$colWidths$widths <= 0))
            errHandler(paste0(
                    "The list element 'widths' in the 'colWidths' slot ",
                    "must be a vector of positive numbers."
                )) 
        # assign colWidths
        .Object@colWidths <- args$colWidths
    } else {
        .Object@colWidths <- list(cols = numeric(0), widths = numeric(0))
    }
    ######### read latexRowHeights ###########
    if ("latexRowHeights" %in% names(args)) {
        # Check if latexRowHeights$rows and $heights have correct types and same length
        if (
                !is.list(args$latexRowHeights) ||
                !is.numeric(args$latexRowHeights$rows) || 
                !is.numeric(args$latexRowHeights$heights) || 
                length(args$latexRowHeights$rows) != length(args$latexRowHeights$heights)
            )
            errHandler(paste0(
                    "The slot 'latexRowHeights' must contain a list ", 
                    "which holds numeric vectors 'rows' and ", 
                    "'heights' of the same length."
                )) 
        # Check if latexRowHeights$rows is a subset of 1:N
        if (any(!args$latexRowHeights$rows %in% 1:nRow))
            errHandler(paste0(
                    "The list element 'rows' in the 'latexRowHeights' slot ",
                    "must be a subset of possible row indices of the ",
                    "data list in 'data'."
                )) 
        # Check if latexRowHeights$heights >0
        if (any(!args$latexRowHeights$heights <= 0))
            errHandler(paste0(
                    "The list element 'heights' in the 'latexRowHeights' slot ",
                    "must be a vector of positive numbers."
                )) 
        # assign latexRowHeights
        .Object@latexRowHeights <- args$latexRowHeights
    } else {
        .Object@latexRowHeights <- list(rows = numeric(0), heights = numeric(0))
    }
    ######### read latexColWidths ###########
    if ("latexColWidths" %in% names(args)) {
        # Check if latexColWidths$cols and $widths have correct types and same length
        if (
                !is.list(args$latexColWidths) ||
                !is.numeric(args$latexColWidths$cols) || 
                !is.numeric(args$latexColWidths$widths) || 
                length(args$latexColWidths$cols) != length(args$latexColWidths$widths)
            )
            errHandler(paste0(
                    "The slot 'latexColWidths' must contain a list ", 
                    "which holds numeric vectors 'cols' and ", 
                    "'widths' of the same length."
                )) 
        # Check if latexColWidths$cols is a subset of 1:N
        if (any(!args$latexColWidths$cols %in% 1:nCol))
            errHandler(paste0(
                    "The list element 'cols' in the 'latexColWidths' slot ",
                    "must be a subset of possible col indices of the ",
                    "data list in 'data'."
                )) 
        # Check if latexColWidths$widths >0
        if (any(!args$latexColWidths$widths <= 0))
            errHandler(paste0(
                    "The list element 'widths' in the 'latexColWidths' slot ",
                    "must be a vector of positive numbers."
                )) 
        # assign latexColWidths
        .Object@latexColWidths <- args$latexColWidths
    } else {
        .Object@latexColWidths <- list(cols = numeric(0), widths = numeric(0))
    }
    .Object
})

#' Number of rows in a StyledTable
#'
#' @name nRows
#' @rdname StyledTable-nRows-method
#' @exportMethod nRows
setGeneric("nRows", function(object) standardGeneric("nRows"))

#' @rdname StyledTable-nRows-method
#' @aliases nRows,StyledTable-method
#' @param object A styled table
setMethod(
    "nRows",
    signature(
        object = "StyledTable"
    ),
    function(object) {
        length(object@data)
    }
)

#' Number of cols in a StyledTable
#'
#' @name nCols
#' @rdname StyledTable-nCols-method
#' @exportMethod nCols
setGeneric("nCols", function(object) standardGeneric("nCols"))

#' @rdname StyledTable-nCols-method
#' @aliases nCols,StyledTable-method
#' @param object A styled table
setMethod(
    "nCols",
    signature(
        object = "StyledTable"
    ),
    function(object) {
        max(unlist(lapply(object@data, length)))
    }
)

#' Create a styled table
#'
#' This function creates a styled table from a single data.frame or multiple data.frames and other styled tables. If multiple styled tables and data.frames are supplied, then they are concatenated vertically. Therefore, all supplied data.frames and styled tables must have the same number of columns.
#' @name styledTable
#' @rdname StyledTable-styledTable-method
#' @exportMethod styledTable
setGeneric("styledTable", function(...) standardGeneric("styledTable"))

#' @rdname StyledTable-styledTable-method
#' @aliases styledTable-method
#' @param ... Multiple matrices, data.frames or styled tables that should be concatenated
#' @return A styled table object
setMethod(
    "styledTable",
    signature(),
    function(...) {
        object <- NULL
        lapply(
            list(...),
            function(x) {
                errHandler <- function(description)
                    stop(paste0(
                        "Error in 'styledTable'-method: ",
                        description
                    ), call. = FALSE)
                if (!is.null(x)) {
                    flagIsStyledTable <- is(x, "StyledTable")
                    if (!is.matrix(x) && !is.data.frame(x) && !flagIsStyledTable)
                        errHandler("All objects have to be of the class 'matrix', 'data.frame' or 'StyledTable'.")
                    if (!flagIsStyledTable)
                        x <- new("StyledTable", data = x)
                    if (is.null(object)) {
                        object <<- x
                    } else {
                        if (nCols(object) != nCols(x))
                            errHandler("All objects must have the same number of columns.")
                        nRow <- nRows(object)
                        # Concat data
                        object@data <<- c(object@data, x@data)
                        # Concat styles
                        object@styles <<- c(object@styles, x@styles)
                        # Concat row heights
                        object@rowHeights$rows <<- c(object@rowHeights$rows, x@rowHeights$rows + nRow)
                        object@rowHeights$heights <<- c(object@rowHeights$heights, x@rowHeights$heights)
                        # Concat col widths
                        colIds <- object@colWidths$cols %in% x@colWidths$cols
                        object@colWidths$cols <<- c(object@colWidths$cols[!colIds], x@colWidths$cols)
                        object@colWidths$widths <<- c(object@colWidths$widths[!colIds], x@colWidths$widths)
                        # Concat merged cells
                        x@merges <- lapply(x@merges, function(y) list(rows = nRow + y$rows, cols = y$cols))
                        object@merges <<- c(object@merges, x@merges)
                    }
                }
            }
        )
        object
    }
)

#' Set styles of StyledTable cells
#'
#' @name setStyles
#' @rdname StyledTable-setStyles-method
#' @exportMethod setStyles
#' @param ... Various Arguments
setGeneric("setStyles", function(object, ...) standardGeneric("setStyles"))

#' @rdname StyledTable-setStyles-method
#' @aliases setStyles,StyledTable-method
#' @param object A StyledTable
#' @param value The value that should be set
#' @param styleName The name of the style slot that should be set
#' @param rows A vector of row numbers to which the change should be applied to
#' @param cols A vector of col numbers to which the change should be applied to
#' @param appendMode A character string that defines if the styling value of the cell should be replaced (appendMode = "replace"), if the new styling should be appended at the end of the current styling value of the cell (appendMode = "appendBehind"), if the new styling should be appended in front of the current styling value of the cell (appendMode = "appendBefore")
setMethod(
    "setStyles",
    signature(
        object = "StyledTable"
    ),
    function(object, value, styleName = "", rows = NULL, cols = NULL, appendMode = "replace") {
        for (i in rows) {
            for (j in cols) {
                cellStyle <- object@styles[[i]][[j]]
                if (appendMode == "replace") {
                    currValue <- value
                } else {
                    v1 <- getSTCellStyle(cellStyle, styleName)
                    if (appendMode == "appendBehind") {
                        v2 <- value
                    } else {
                        v2 <- v1
                        v1 <- value
                    }
                    if (styleName %in% c("latexPreProcess", "excelPreProcess")) {
                        currValue <- concatFunctions(v1, v2)
                    } else if (styleName == "dataFormat") {
                        currValue <- paste0(v1, v2)
                    } else {
                        currValue <- c(v1, v2)
                    }
                }
                object@styles[[i]][[j]] <- setSTCellStyle(cellStyle, currValue, styleName)
            }
        }
        object
    }
)
