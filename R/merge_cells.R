#' Merge multiple cells in a [StyledTable] object
#' @name merge_cells
#' @rdname merge_cells-methods
#' @exportMethod merge_cells
#' @param ... Various arguments
#' @include styled_table.R
setGeneric("merge_cells", function(st, ...) standardGeneric("merge_cells"))

#' @rdname  merge_cells-methods
#' @aliases merge_cells,StyledTable,character-method
#' @param st A [StyledTable] object
#' @param row_id A vector holding two numbers, the start row and the end row of the merged region
#' @param col_id A vector holding two numbers, the start col and the end col of the merged region
#' @return The modified [StyledTable] object
#' @seealso [merge_equal_cells()]
#' @seealso [remove_col()], [remove_row()]
setMethod(
    "merge_cells",
    signature(st = "StyledTable"),
    function(st, row_id = NULL, col_id = NULL) {
        errHandler <- function(description)
            stop(paste0(
                "Error in 'merge_cells'-method: ",
                description
            ), call. = FALSE)
        # Substitute n_col and n_row in the rows and columns selector
        row_id <- substitute_row_id(st, substitute(row_id), stack_level = 1, errHandler)
        col_id <- substitute_col_id(st, substitute(col_id), stack_level = 1, errHandler)
        if (is.null(row_id))
            row_id <- 1:count_rows(st)
        if (!all(diff(row_id) == 1))
            errHandler(paste0(
                "The argument 'row_id' must be an unbroken numeric vector. ",
                "(Number of rows may be replaced by 'n_row')."
            ))
        if (is.null(col_id))
            col_id <- 1:count_cols(st)
        if (!all(diff(col_id) == 1))
            errHandler(paste0(
                "The argument 'col_id' must be an unbroken numeric vector. ",
                "(Number of cols may be replaced by 'n_col')."
            ))
        # First check which merges are overlapping and
        # filter out the assimilated merged areas (entirely contained in new merge)
        st@merges <- st@merges[unlist(sapply(st@merges, function(m) {
            # Check if any existing merge area is overlapping
            if (
                (any(row_id %in% m$row_id) || any(m$row_id %in% row_id)) &&
                    (any(col_id %in% m$col_id) || any(m$col_id %in% col_id))
            ) {
                # If the conflicting merged area is not entirely contained in the new merge
                # throw an error
                if (!all(m$row_id %in% row_id) || !all(m$col_id %in% col_id))
                    errHandler("The given region for the merged cell is already part of a merged cell.")
                return(FALSE)
            }
            TRUE
        }))]
        # Add merge to StyledTable
        st@merges[[length(st@merges) + 1]] <- list(row_id = c(min(row_id), max(row_id)), col_id = c(min(col_id), max(col_id)))
        st
    }
)

#' Merge all cells of [StyledTable] object whose cell values are equal
#'
#' Merges all cells in a [StyledTable] object that hold the same value.
#' This function can produce multiple merged regions at once.
#' @name merge_equal_cells
#' @rdname merge_equal_cells-methods
#' @exportMethod merge_equal_cells
#' @param ... Various arguments
#' @include styled_table.R
setGeneric("merge_equal_cells", function(st, ...) standardGeneric("merge_equal_cells"))

#' @rdname  merge_equal_cells-methods
#' @aliases merge_equal_cells,StyledTable,character-method
#' @param st A [StyledTable] object
#' @param row_id A vector holding the row numbers the start row and the end row of the merge region
#' @param col_id A vector holding two numbers, the start col and the end col of the merge region
#' @return The modified [StyledTable] object
#' @seealso [merge_cells()]
#' @seealso [remove_col()], [remove_row()]
setMethod(
    "merge_equal_cells",
    signature(st = "StyledTable"),
    function(st, row_id = NULL, col_id = NULL) {
        errHandler <- function(description)
            stop(paste0(
                "Error in 'merge_equal_cells'-method: ",
                description
            ), call. = FALSE)
        # Substitute n_col and n_row in the rows and columns selector
        row_id <- substitute_row_id(st, substitute(row_id), stack_level = 1, errHandler)
        col_id <- substitute_col_id(st, substitute(col_id), stack_level = 1, errHandler)
        if (is.null(row_id))
            row_id <- 1:count_rows(st)
        if (!all(diff(row_id) == 1))
            errHandler(paste0(
                "The argument 'row_id' must be an unbroken numeric vector. ",
                "(Number of rows may be replaced by 'n_row')."
            ))
        if (is.null(col_id))
            col_id <- 1:count_cols(st)
        if (!all(diff(col_id) == 1))
            errHandler(paste0(
                "The argument 'col_id' must be an unbroken numeric vector. ",
                "(Number of cols may be replaced by 'n_col')."
            ))
        # First check which merges are overlapping and
        # filter out the assimilated merged areas (entirely contained in new merge)
        cData <- st@data
        for (m in st@merges) {
            for (i in m$row_id[1]:m$row_id[2]) {
                cData[[i]][m$col_id[1]:m$col_id[2]] <- "%USEDINMERGE%"
            }
        }
        for (i in row_id) {
            for (j in col_id) {
                val <- cData[[i]][[j]]
                if (!identical(val, "%USEDINMERGE%")) {
                    imax <- i
                    jmax <- j
                    flag <- TRUE
                    while (flag) {
                        flag <- FALSE
                        if ((imax + 1) %in% row_id && identical(cData[[imax + 1]][[jmax]], val)) {
                            flag <- all(sapply(cData[[imax + 1]][j:jmax], function(x) identical(x, val)))
                            if (flag)
                                imax <- imax + 1
                        }
                        if ((jmax + 1) %in% col_id && identical(cData[[imax]][[jmax + 1]], val)) {
                            if (all(sapply(cData[i:imax], function(x) identical(x[[jmax + 1]], val)))) {
                                jmax <- jmax + 1
                                flag <- TRUE
                            }
                        }
                    }
                    for (i1 in i:imax) {
                        cData[[i1]][j:jmax] <- "%USEDINMERGE%"
                    }
                    st <- merge_cells(st, row_id = i:imax, col_id = j:jmax)
                }
            }
        }
        st
    }
)
