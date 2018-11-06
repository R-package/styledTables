#' Write a StyledTable to an Excel sheet
#'
#' @name writeToExcelSheet
#' @rdname writeToExcelSheet-methods
#' @exportMethod writeToExcelSheet
#' @param ... Various arguments
#' @include StyledTable.R
setGeneric("writeToExcelSheet", function(sheet, object, ...) standardGeneric("writeToExcelSheet"))

#' @rdname writeToExcelSheet-methods
#' @aliases writeToExcelSheet,ANY,StyledTable-method
#' @param sheet An xlsx sheet object
#' @param object A StyledTable
#' @param firstRow The number of the starting row of the table in the sheet
#' @param firstCol The number of the starting col of the table in the sheet
setMethod(
    "writeToExcelSheet",
    signature(
        sheet = "ANY",
        object = "StyledTable"
    ),
    function(sheet, object, firstRow = NULL, firstCol = NULL) {
        # Error handler
        errHandler <- function(description) {
            stop(paste0(
                "Error in 'writeToExcelSheet'-method: ",
                description
            ), call. = FALSE)
        }
        # get firstRow, firstCol
        if (is.null(firstRow)) {
            if (!is.null(attr(sheet, "lastRow"))) {
                firstRow <- attr(sheet, "lastRow") + 1L
            } else {
                firstRow <- 1L
            }
        }
        if (is.null(firstCol)) {
            if (!is.null(attr(sheet, "firstCol"))) {
                firstCol <- attr(sheet, "firstCol")
            } else {
                firstCol <- 1L
            }
        }
        # Workbook
        wb <- sheet$getWorkbook()
        # init cellStyle list for this workbook
        createdCellStyles <- list()
        createCS <- function(styleVal) {
            # Generate a cellStyle element for the styling given in "styleVal"
            # if a similar styling was passed in earlier then do not generate a
            # new cellStyle element, but return the earlier created one
            candidates <- createdCellStyles[unlist(lapply(
                createdCellStyles, 
                function(x) compareStyles(x$style, styleVal)
            ))]
            if (length(candidates) == 0) {
                cs <- getXlsxCellStyle(wb, styleVal)
                createdCellStyles[[length(createdCellStyles) + 1L]] <<- list(cs = cs, style = styleVal)
            } else
                cs <- candidates[[1]]$cs
            cs
        }
        # Dimensions of table
        nCol = nCols(object)
        nRow = nRows(object)
        rowsIndex <- firstRow:(nRow + firstRow - 1L)
        colsIndex <- firstCol:(nCol + firstCol - 1L)
        # Delete existing rows that will be overwritten
        rowsIndex.existing <- intersect(
            rowsIndex, 
            as.numeric(names(getRows(sheet)))
        )
        if (length(rowsIndex.existing) > 0L) {
                errHandler(paste0(
                        "The rows were the table would be written too (", 
                        paste0(rowsIndex.existing, collapse = ", "),
                        ") are not empty."
                    ))
            removeRow(sheet, getRows(sheet, rowsIndex.existing))
        }
        # Generate new rows
        rows   <- createRow(sheet, rowsIndex)
        cells  <- createCell(rows, colIndex = firstCol:(nCol + firstCol - 1L))
        # Set height of each row
        for (i in seq_len(length(object@rowHeights$rows))) {
            xlsx::setRowHeight(
                rows = rows[as.character(firstRow + object@rowHeights$rows[i] - 1L)], 
                object@rowHeights$heights[i]
            )
        }
        # Set width of each column    
        for (i in seq_len(length(object@colWidths$cols))) {
            xlsx::setColumnWidth(
                sheet,
                firstCol + object@colWidths$cols[i] - 1L, 
                object@colWidths$widths[i]
            )
        }
        # Write values and styles to cells
        for (i in seq_len(nRow)) {
            for (j in seq_len(nCol)) {
                merges <- object@merges[
                        unlist(lapply(
                            object@merges, 
                            function(m) betweenVec(i, m$rows) && betweenVec(j, m$cols)
                        ))
                    ]
                value <- NULL
                if (length(merges) == 1) {
                    m <- merges[[1]]
                    if (m$rows[1] == i && m$cols[1] == j)
                        value <- object@data[[i]][[j]]
                } else {
                    value <- object@data[[i]][[j]]
                }
                # Preprocess the value
                preProcess <- getSTCellStyle(object@styles[[i]][[j]], "excelPreProcess")
                tryCatch({
                        value <- preProcess(value)
                    },
                    error = function(description) {
                        stop(paste0("Error in 'writeToExcelSheet' ",
                                "while evaluating the function given ",
                                "in 'setExcelPreProcess' on cell value (", 
                                "row: ", i,
                                "col: ", j,
                                "value:", as.character(value),
                                "). Check the function definition. ",
                                "Details: ", as.character(description)
                            ), call. = FALSE)
                            
                    })
                # write value to cell
                if (!is.null(value))
                    setCellValue(cells[[i, j]], value)
                # set cell style
                style <- object@styles[[i]][[j]]
                if (!is.null(style))
                    setCellStyle(
                        cells[[i, j]],
                        createCS(style)
                    )
            }
        }
        # Merge cells
        for (m in object@merges) {
            addMergedRegion(
                sheet, 
                firstRow - 1L + m$rows[1],
                firstRow - 1L + m$rows[2],
                firstCol - 1L + m$cols[1],
                firstCol - 1L + m$cols[2]
            )
        
        }
        # write last row to sheet
        attr(sheet, "lastRow") <- firstRow - 1L + nRow
        attr(sheet, "firstCol") <- firstCol
        invisible(sheet)
    }
)
