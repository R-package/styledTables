#' Add merged region
#' @name addMerge
#' @rdname addMerge-methods
#' @exportMethod addMerge
#' @param ... Various arguments
#' @include StyledTable.R
setGeneric("addMerge", function(object, ...) standardGeneric("addMerge"))

#' @rdname  addMerge-methods
#' @aliases addMerge,StyledTable,character-method
#' @param object A StyledTable
#' @param rows A vector holding two numbers, the start row and the end row of the merge region
#' @param cols A vector holding two numbers, the start col and the end col of the merge region
setMethod(
    "addMerge", 
    signature(object = "StyledTable"),
    function(object, rows = NULL, cols = NULL) {
        errHandler <- function(description)
            stop(paste0(
                "Error in 'addMerge'-method: ",
                description
            ), call. = FALSE)
        # Substitute N in the rows selector
        nRow <- nRows(object)
        nCol <- nCols(object)
        if (missing(rows)) {
            rows <- c(1, nRow)
        } else {
            if (!is.numeric(rows) || length(rows) == 0 || !all(diff(rows) == 1))
                errHandler(paste0(
                    "The argument 'rows' must be an unbroken numeric vector. ",
                    "(Number of rows may be replaced by 'N')."
                ))
                
            if (any(!rows %in% 1:nRow))
                errHandler(paste0(
                        "The argument 'rows' must be a subinterval of 1:",
                        nRow,
                        "."
                    ))
            rows <- c(min(rows), max(rows))
        }
        # Substitute N in the cols selector
        if (missing(cols)) {
            cols <- c(1, nCol)
        } else {
            if (!is.numeric(cols) || length(cols) == 0 || !all(diff(cols) == 1))
                errHandler(paste0(
                    "The argument 'cols' must be an unbroken numeric vector. ",
                    "(Number of cols may be replaced by 'N')."
                ))
            if (any(!cols %in% 1:nCol))
                errHandler(paste0(
                        "The argument 'cols' must be a subinterval of 1:",
                        nCol,
                        "."
                    ))
            cols <- c(min(cols), max(cols))
        }
        # First check which merges are overlapping and 
        # filter out the assimilated merged areas (entirely contained in new merge)
        object@merges <- object@merges[unlist(sapply(object@merges, function(m) {
            # Check if any existing merge area is overlapping
            if (
                (any(betweenVec(rows,  m$rows)) || any(betweenVec(m$rows, rows))) &&
                    (any(betweenVec(cols, m$cols)) || any(betweenVec(m$cols, cols)))
            ) {
                # If the conflicting merged area is not entirely contained in the new merge
                # throw an error
                if (!all(betweenVec(m$rows, rows)) || !all(betweenVec(m$cols, cols)))
                    errHandler("The given region for the merged cell is already part of a merged cell.")
                return(FALSE)
            }
            TRUE
        }))]
        # Add merge to StyledTable
        object@merges[[length(object@merges) + 1]] <- list(rows = rows, cols = cols)
        object
    }
)

#' Merge all cells whose cell values are equal
#'
#' Merges all styled table cells that have the same value. Can generate several
#' merged regions at once.
#' @name mergeEqualCells
#' @rdname mergeEqualCells-methods
#' @exportMethod mergeEqualCells
#' @param ... Various arguments
#' @include StyledTable.R
setGeneric("mergeEqualCells", function(object, ...) standardGeneric("mergeEqualCells"))

#' @rdname  mergeEqualCells-methods
#' @aliases mergeEqualCells,StyledTable,character-method
#' @param object A StyledTable
#' @param rows A vector holding the row numbers the start row and the end row of the merge region
#' @param cols A vector holding two numbers, the start col and the end col of the merge region
setMethod(
    "mergeEqualCells", 
    signature(object = "StyledTable"),
    function(object, rows = NULL, cols = NULL) {
        errHandler <- function(description)
            stop(paste0(
                "Error in 'mergeEqualCells'-method: ",
                description
            ), call. = FALSE)
        # Substitute N in the rows selector
        nRow <- nRows(object)
        nCol <- nCols(object)
        if (missing(rows)) {
            rows <- 1:nRow
        } else {
            if (!is.numeric(rows) || length(rows) == 0)
                errHandler(paste0(
                    "The argument 'rows' must be a numeric vector."
                ))
            if (any(!rows %in% 1:nRow))
                errHandler(paste0(
                        "The argument 'rows' must be a subinterval of 1:",
                        nRow,
                        "."
                    ))
        }
        # Substitute N in the cols selector
        if (missing(cols)) {
            cols <- 1:nCol
        } else {
            if (!is.numeric(cols) || length(cols) == 0)
                errHandler(paste0(
                    "The argument 'cols' must be a numeric vector."
                ))
            if (any(!cols %in% 1:nCol))
                errHandler(paste0(
                        "The argument 'cols' must be a subinterval of 1:",
                        nCol,
                        "."
                    ))
        }
        # First check which merges are overlapping and 
        # filter out the assimilated merged areas (entirely contained in new merge)
        cData <- object@data
        for (m in object@merges) {
            for (i in m$rows[1]:m$rows[2]) {
                cData[[i]][m$cols[1]:m$cols[2]] <- "%USEDINMERGE%"
            }
        }
        for (i in rows) {
            for (j in cols) {
                val <- cData[[i]][[j]]
                if (!identical(val, "%USEDINMERGE%")) {
                    imax <- i
                    jmax <- j
                    flag <- TRUE
                    while (flag) {
                        flag <- FALSE
                        if ((imax + 1) %in% rows && identical(cData[[imax + 1]][[jmax]], val)) {
                            flag <- all(sapply(cData[[imax + 1]][j:jmax], function(x) identical(x, val)))
                            if (flag)
                                imax <- imax + 1
                        }
                        if ((jmax + 1) %in% cols && identical(cData[[imax]][[jmax + 1]], val)) {
                            if (all(sapply(cData[i:imax], function(x) identical(x[[jmax + 1]], val)))) {
                                jmax <- jmax + 1
                                flag <- TRUE
                            }
                        }
                    }
                    for (i1 in i:imax) {
                        cData[[i1]][j:jmax] <- "%USEDINMERGE%"
                    }
                    object <- addMerge(object, rows = i:imax, cols = j:jmax)
                }
            }
        }
        object
    }
)
