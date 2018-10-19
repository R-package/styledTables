#' Remove columns from a styledTable
#'
#' @name removeColumns
#' @rdname removeColumns-methods
#' @exportMethod removeColumns
setGeneric("removeColumns", function(object, cols) standardGeneric("removeColumns"))

#' @rdname removeColumns-methods
#' @aliases removeColuqns,StyledTable,ANY-method
#' @param object A StyledTable
#' @param cols A vector of col numbers which should be removed from the styled table.
setMethod(
    "removeColumns", 
    signature(
        object = "StyledTable", 
        cols = "ANY"
    ),
    function(
        object, 
        cols
    ) {
        errHandler <- function(description) {
            stop(paste(
                "Error while removing columns from a 'StyledTable':",
                description
            ), call. = FALSE)
        }
        # Check if cols is correct
        if (!is.numeric(cols) | length(cols) == 0 | any(!cols %in% 1:nCols(object)))
            errHandler(paste(
                "The argument 'rows' must be a non empty numeric vector",
            ))
        # remove columns from data slot
        remainingCols = setdiff(1:nCols(object), cols)
        object@data <- lapply(object@data, function(row) row[remainingCols])
        # remove columns from styles slot
        object@styles <- lapply(object@styles, function(row) row[remainingCols])
        # remove columns from colWidths slot
        colIndex <- which(!object@colWidths$cols %in% cols)
        object@colWidths$widths <- object@colWidths$widths[colIndex] 
        object@colWidths$cols <- sapply(
                object@colWidths$cols[colIndex], 
                function(x) x - sum(cols < x)
            )
        # remove columns from latexColWidths slot
        colIndex <- which(!object@latexColWidths$cols %in% cols)
        object@latexColWidths$widths <- object@latexColWidths$widths[colIndex]
        object@latexColWidths$cols <- sapply(
                object@latexColWidths$cols[colIndex], 
                function(x) x - sum(cols < x)
            )
        # remove columns from merges slot
        object@merges <- lapply(
                object@merges,
                function(m) {
                    c1 <- m$cols[1]
                    c2 <- m$cols[2]
                    m$cols <- c(
                            c1 - sum(cols < c1),
                            c2 - sum(cols < c2) - sum(c2 %in% cols)
                        )
                    m
                }
            )
        object@merges <- object@merges[unlist(sapply(
                object@merges, 
                function(m) {
                    if (m$rows[1] < m$rows[2])
                        return(m$cols[1] <= m$cols[2])
                    m$cols[1] < m$cols[2]
                }
            ))]
        object
    }
)

#' Remove rows from a styledTable
#'
#' @name removeRows
#' @rdname removeRows-methods
#' @exportMethod removeRows
setGeneric("removeRows", function(object, rows) standardGeneric("removeRows"))

#' @rdname removeRows-methods
#' @aliases removeRows,StyledTable,ANY-method
#' @param object A StyledTable
#' @param rows A vector of row numbers which should be removed from the styled table.
setMethod(
    "removeRows", 
    signature(
        object = "StyledTable", 
        rows = "ANY"
    ),
    function(
        object, 
        rows
    ) {
        errHandler <- function(description) {
            stop(paste(
                "Error while removing rows from a 'StyledTable':",
                description
            ), call. = FALSE)
        }
        # Check if rows is correct
        if (!is.numeric(rows) | length(rows) == 0 | any(!rows %in% 1:nRows(object)))
            errHandler(paste(
                "The argument 'rows' must be a non empty numeric vector",
                "holding the numbers of the rows that should be deleted."
            ))
        # remove rows from data slot
        remainingRows = setdiff(1:nRows(object), rows)
        object@data <- object@data[remainingRows]
        # remove rows from styles slot
        object@styles <- object@styles[remainingRows]
        # remove rows from rowHeights slot
        rowIndex <- which(!object@rowHeights$rows %in% rows)
        object@rowHeights$widths <- object@rowHeights$widths[rowIndex] 
        object@rowHeights$rows <- sapply(
                object@rowHeights$rows[rowIndex], 
                function(x) x - sum(rows < x)
            )
        # remove rows from latexRowHeights slot
        rowIndex <- which(!object@latexRowHeights$rows %in% rows)
        object@latexRowHeights$widths <- object@latexRowHeights$widths[rowIndex]
        object@latexRowHeights$rows <- sapply(
                object@latexRowHeights$rows[rowIndex], 
                function(x) x - sum(rows < x)
            )
        # remove rows from merges slot
        object@merges <- lapply(
                object@merges,
                function(m) {
                    r1 <- m$rows[1]
                    r2 <- m$rows[2]
                    m$rows <- c(
                            r1 - sum(rows < r1),
                            r2 - sum(rows < r2) - sum(r2 %in% rows)
                        )
                    m
                }
            )
        object@merges <- object@merges[unlist(sapply(
                object@merges, 
                function(m) {
                    if (m$cols[1] < m$cols[2])
                        return(m$rows[1] <= m$rows[2])
                    m$rows[1] < m$rows[2]
                }
            ))]
        object
    }
)
