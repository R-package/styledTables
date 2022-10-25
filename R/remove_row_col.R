#' Remove columns from a [StyledTable] object
#'
#' @name remove_col
#' @rdname remove_col-methods
#' @exportMethod remove_col
setGeneric("remove_col", function(st, col_id) standardGeneric("remove_col"))

#' @rdname remove_col-methods
#' @aliases removeColuqns,StyledTable,ANY-method
#' @param st A [StyledTable] object
#' @param col_id A vector of col numbers which should be removed from the [StyledTable] object.
#' @return The modified [StyledTable] object
setMethod(
    "remove_col",
    signature(
        st = "StyledTable",
        col_id = "ANY"
    ),
    function(
        st,
        col_id
    ) {
        errHandler <- function(description) {
            stop(paste(
                "Error while removing columns from a 'StyledTable':",
                description
            ), call. = FALSE)
        }
        # If no column should be deleted, exit
        if (length(col_id) == 0)
            return(st)
        # Check if col_id is correct
        if (!is.numeric(col_id))
            errHandler(paste(
                "The argument 'col_id' must be a numeric vector",
            ))
        if (any(!col_id %in% 1:count_cols(st)))
            errHandler(paste(
                "The argument 'col_id' must be a sunon empty numeric vector",
            ))
        # remove columns from data slot
        remainingCols <- setdiff(1:count_cols(st), col_id)
        st@data <- lapply(st@data, function(col) col[remainingCols])
        # remove columns from styles slot
        st@styles <- lapply(st@styles, function(col) col[remainingCols])
        # remove columns from excel_col_width slot
        colIndex <- which(!st@excel_col_width$col_id %in% col_id)
        st@excel_col_width$width <- st@excel_col_width$width[colIndex]
        st@excel_col_width$col_id <- sapply(
                st@excel_col_width$col_id[colIndex],
                function(x) x - sum(col_id < x)
            )
        # remove columns from latex_col_width slot
        colIndex <- which(!st@latex_col_width$col_id %in% col_id)
        st@latex_col_width$width <- st@latex_col_width$width[colIndex]
        st@latex_col_width$col_id <- sapply(
                st@latex_col_width$col_id[colIndex],
                function(x) x - sum(col_id < x)
            )
        # remove columns from the html slot
        colIds <- which(!st@html$rowheader_col_id %in% col_id)
        st@html$rowheader_col_id <<- sapply(
          st@html$rowheader_col_id[!colIds],
          function(x) x-sum(col_id < x)
        )
        
        # remove columns from merges slot
        st@merges <- lapply(
                st@merges,
                function(m) {
                    c1 <- m$col_id[1]
                    c2 <- m$col_id[2]
                    m$col_id <- c(
                            c1 - sum(col_id < c1),
                            c2 - sum(col_id < c2) - sum(c2 %in% col_id)
                        )
                    m
                }
            )
        st@merges <- st@merges[unlist(sapply(
                st@merges,
                function(m) {
                    if (m$row_id[1] < m$row_id[2])
                        return(m$col_id[1] <= m$col_id[2])
                    m$col_id[1] < m$col_id[2]
                }
            ))]
        st
    }
)

#' Remove rows from a [StyledTable] object
#'
#' @name remove_row
#' @rdname remove_row-methods
#' @exportMethod remove_row
setGeneric("remove_row", function(st, row_id) standardGeneric("remove_row"))

#' @rdname remove_row-methods
#' @aliases remove_row,StyledTable,ANY-method
#' @param st A [StyledTable] object
#' @param row_id A vector of row numbers which should be removed from the [StyledTable] object.
#' @return The modified [StyledTable] object
setMethod(
    "remove_row",
    signature(
        st = "StyledTable",
        row_id = "ANY"
    ),
    function(
        st,
        row_id
    ) {
        errHandler <- function(description) {
            stop(paste(
                "Error while removing rows from a 'StyledTable':",
                description
            ), call. = FALSE)
        }
        # Check if row_id is correct
        if (!is.numeric(row_id) | length(row_id) == 0 | any(!row_id %in% 1:count_rows(st)))
            errHandler(paste(
                "The argument 'row_id' must be a non empty numeric vector",
                "holding the numbers of the rows that should be deleted."
            ))
        # remove rows from data slot
        remainingRows <- setdiff(1:count_rows(st), row_id)
        st@data <- st@data[remainingRows]
        # remove row_id from styles slot
        st@styles <- st@styles[remainingRows]
        # remove rows from excel_row_height slot
        rowIndex <- which(!st@excel_row_height$row_id %in% row_id)
        st@excel_row_height$height <- st@excel_row_height$height[rowIndex]
        st@excel_row_height$row_id <- sapply(
                st@excel_row_height$row_id[rowIndex],
                function(x) x - sum(row_id < x)
            )
        # remove rows from latex_padding_top slot
        rowIndex <- which(!st@latex_padding_top$row_id %in% row_id)
        st@latex_padding_top$height <- st@latex_padding_top$height[rowIndex]
        st@latex_padding_top$row_id <- sapply(
                st@latex_padding_top$row_id[rowIndex],
                function(x) x - sum(row_id < x)
            )
        # remove rows from latex_padding_bottom slot
        rowIndex <- which(!st@latex_padding_bottom$row_id %in% row_id)
        st@latex_padding_bottom$height <- st@latex_padding_bottom$height[rowIndex]
        st@latex_padding_bottom$row_id <- sapply(
            st@latex_padding_bottom$row_id[rowIndex],
            function(x) x - sum(row_id < x)
        )
        # remove rows from html slot
        st@html$colheader_row_id <- sapply(
          setdiff(st@html$colheader_row_id, row_id),
          function(x) x - sum(row_id < x)
        )
        st@html$st@html$subheading_row_id <- sapply(
          setdiff(st@html$st@html$subheading_row_id, row_id),
          function(x) x - sum(row_id < x)
        )
        st@html$tr_class$row_class <- st@html$tr_class$row_class[
          !st@html$tr_class$row_id %in% row_id
        ]
        st@html$tr_class$row_id <- sapply(
          setdiff(st@html$tr_class$row_id, row_id),
          function(x) x - sum(row_id < x)
        )
        # remove row_id from merges slot
        st@merges <- lapply(
                st@merges,
                function(m) {
                    r1 <- m$row_id[1]
                    r2 <- m$row_id[2]
                    m$row_id <- c(
                            r1 - sum(row_id < r1),
                            r2 - sum(row_id < r2) - sum(r2 %in% row_id)
                        )
                    m
                }
            )
        st@merges <- st@merges[unlist(sapply(
                st@merges,
                function(m) {
                    if (m$col_id[1] < m$col_id[2])
                        return(m$row_id[1] <= m$row_id[2])
                    m$row_id[1] < m$row_id[2]
                }
            ))]
        st
    }
)
