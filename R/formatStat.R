#' Method formatStatHeader.
#'
#' @name formatStatHeader
#' @rdname formatStatHeader-methods
#' @exportMethod formatStatHeader
#' @param ... Various arguments
#' @include StyledTable.R
#' @include Style.R
#' @include setCellStyles.R
#' @include addMerge.R
#' @include setHeightsWidths.R
setGeneric("formatStatHeader", function(object, ...) standardGeneric("formatStatHeader"))

#' @rdname formatStatHeader-methods
#' @aliases formatStatHeader,StyledTable-method
#' @param object A StyledTable
#' @param rows A vector of row numbers (N is substituted as \code{nRows(object)})
setMethod(
    "formatStatHeader",
    signature(
        object = "StyledTable"
    ),
    function(object, rows = NULL) {
        errHandler <- function(description)
            stop(paste0(
                "Error in 'formatStatHeader'-method: ",
                description
            ), call. = FALSE)
        # Substitute N in the rows selector
        if (missing(rows)) {
            rows <- 1:nRows(object)
        } else {
            if (!is.numeric(rows) || length(rows) == 0 || any(is.na(rows)))
                errHandler(paste0(
                    "The argument 'rows' must be an unbroken numeric vector. ",
                    "(Number of rows may be replaced by 'N')."
                ))
                
            if (any(!rows %in% 1:nRows(object)))
                errHandler(paste0(
                        "The argument 'rows' must be a subinterval of 1:",
                        nRows(object),
                        "."
                    )) 
        }
        object <- setExcelFontSize(object, 7, rows = rows)
        object <- setExcelFontName(object, "Arial", rows = rows)
        object <- setExcelVertical(object, "center", rows = rows)
        object <- setHorizontal(object, "center", rows = rows)
        object <- setWrapped(object, TRUE, rows = rows)
        object <- setBorderPosition(object, c("top", "right", "bottom", "left"), rows = rows)
        setExcelRowHeights(object, 12.75, rows = rows)
    }
)

#' Method formatStatBody.
#'
#' @name formatStatBody
#' @rdname formatStatBody-methods
#' @param ... Various arguments
#' @exportMethod formatStatBody
setGeneric("formatStatBody", function(object, ...) standardGeneric("formatStatBody"))

#' @rdname formatStatBody-methods
#' @aliases formatStatBody,StyledTable-method
#' @param object A StyledTable
#' @param rows A vector of row numbers (N is substituted as \code{nRows(object)})
setMethod(
    "formatStatBody",
    signature(
        object = "StyledTable"
    ),
    function(object, rows = NULL) {
        errHandler <- function(description)
            stop(paste0(
                "Error in 'formatStatBody'-method: ",
                description
            ), call. = FALSE)
        # Substitute N in the rows selector
        if (missing(rows)) {
            rows <- 1:nRows(object)
        } else {
            if (!is.numeric(rows) || length(rows) == 0 || any(is.na(rows)))
                errHandler(paste0(
                    "The argument 'rows' must be an unbroken numeric vector. ",
                    "(Number of rows may be replaced by 'N')."
                ))
            if (any(!rows %in% 1:nRows(object)))
                errHandler(paste0(
                        "The argument 'rows' must be a subinterval of 1:",
                        nRows(object),
                        "."
                    )) 
        }
        cols = 1:nCols(object)
        if (length(rows) > 1)
            object <- setExcelRowHeights(object, 12.25, rows = rows[2:length(rows)])
        if (length(cols) > 1)
            object <- setHorizontal(object, "right", rows = rows, cols = cols[2: length(cols)])
        object <- setExcelFontSize(object, 7, rows = rows)
        object <- setExcelFontName(object, "Arial", rows = rows)
        object <- setExcelRowHeights(object, 12.75, rows = rows[1])
        object <- setExcelRowHeights(object, 9.75, rows = rows[rows != rows[1]])
        object <- setExcelVertical(object, "bottom", rows = rows)
        object <- setHorizontal(object, "left", rows = rows, cols = cols[1])
        setHorizontal(object, "right", rows = rows, cols = cols[cols != cols[1]])
    }
)

#' Method formatStatSubHeading
#'
#' @name formatStatSubHeading
#' @rdname formatStatSubHeading-methods
#' @param ... Various arguments
#' @exportMethod formatStatSubHeading
setGeneric("formatStatSubHeading", function(object, ...) standardGeneric("formatStatSubHeading"))

#' @rdname formatStatSubHeading-methods
#' @aliases formatStatSubHeading,StyledTable-method
#' @param object A StyledTable
#' @param rows A vector of row numbers
setMethod(
    "formatStatSubHeading",
    signature(
        object = "StyledTable"
    ),
    function(object, rows = NULL) {
        errHandler <- function(description)
            stop(paste0(
                "Error in 'formatStatSubHeading'-method: ",
                description
            ), call. = FALSE)
        # Substitute N in the rows selector
        if (missing(rows)) {
            rows <- 1:nRows(object)
        } else {
            if (!is.numeric(rows) || length(rows) == 0 || any(is.na(rows)))
                errHandler(paste0(
                    "The argument 'rows' must be an unbroken numeric vector. ",
                    "(Number of rows may be replaced by 'N')."
                ))
                
            if (any(!rows %in% 1:nRows(object)))
                errHandler(paste0(
                        "The argument 'rows' must be a subinterval of 1:",
                        nRows(object),
                        "."
                    )) 
        }
        ncols <- nCols(object)
        object <- addMerge(object, rows = rows, cols = 1:ncols)
        object <- setExcelFontSize(object, 7, rows = rows)
        object <- setExcelFontName(object, "Arial", rows = rows)
        object <- setExcelRowHeights(object, 15, rows = rows)
        object <- setExcelVertical(object, "bottom", rows = rows)
        object <- setHorizontal(object, "center", rows = rows)
        setBold(object, TRUE, rows = rows)
    }
)

#' Method formatStatAbsolute
#'
#' @name formatStatAbsolute
#' @rdname formatStatAbsolute-methods
#' @param ... Various arguments
#' @exportMethod formatStatAbsolute
setGeneric("formatStatAbsolute", function(object, ...) standardGeneric("formatStatAbsolute"))

#' @rdname formatStatAbsolute-methods
#' @aliases formatStatAbsolute,StyledTable-method
#' @param object A StyledTable
#' @param rows A vector of row numbers
#' @param cols A vector of col numbers
setMethod(
    "formatStatAbsolute",
    signature(
        object = "StyledTable"
    ),
    function(object, rows = NULL, cols = NULL) {
        errHandler <- function(description)
            stop(paste0(
                "Error in 'formatStatAbsolute'-method: ",
                description
            ), call. = FALSE)
        # Substitute N in the rows selector
        if (missing(rows)) {
            rows <- 1:nRows(object)
        } else {
            if (!is.numeric(rows) || length(rows) == 0 || any(is.na(rows)))
                errHandler(paste0(
                    "The argument 'rows' must be an unbroken numeric vector. ",
                    "(Number of rows may be replaced by 'N')."
                ))
                
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
        object <- setExcelDataFormat(object, "#,##0", rows = rows, cols = cols)
        setLatexPreProcess(
            object,
            function(x) {
                if (!is.na(x) && x != 0) {
                    format(round(x, 0), nsmall = 0, big.mark = ".", decimal.mark = ",")
                } else {
                    "-"
                }
            }, 
            rows = rows, 
            cols = cols)
    }
)

#' Method formatStatRelative
#'
#' @name formatStatRelative
#' @rdname formatStatRelative-methods
#' @param ... Various arguments
#' @exportMethod formatStatRelative
setGeneric("formatStatRelative", function(object, ...) standardGeneric("formatStatRelative"))

#' @rdname formatStatRelative-methods
#' @aliases formatStatRelative,StyledTable-method
#' @param object A StyledTable
#' @param rows A vector of row numbers
#' @param cols A vector of col numbers
setMethod(
    "formatStatRelative",
    signature(
        object = "StyledTable"
    ),
    function(object, rows = NULL, cols = NULL) {
        errHandler <- function(description)
            stop(paste0(
                "Error in 'formatStatRelative'-method: ",
                description
            ), call. = FALSE)
        # Substitute N in the rows selector
        if (missing(rows)) {
            rows <- 1:nRows(object)
        } else {
            if (!is.numeric(rows) || length(rows) == 0 || any(is.na(rows)))
                errHandler(paste0(
                    "The argument 'rows' must be an unbroken numeric vector. ",
                    "(Number of rows may be replaced by 'N')."
                ))
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
        object <- setExcelDataFormat(object, "#,##0.0", rows = rows, cols = cols)
        setLatexPreProcess(
            object,
            function(x) {
                if (!is.na(x) && x != 0) {
                    format(round(x, 1), nsmall = 1, decimal.mark = ",") 
                } else {
                    "-"
                }
            },
            rows = rows, 
            cols = cols)
    }
)

