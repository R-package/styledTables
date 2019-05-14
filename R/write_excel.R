#' Write a StyledTable to an Excel sheet
#'
#' @name write_excel
#' @rdname write_excel-methods
#' @exportMethod write_excel
#' @param ... Various arguments
#' @include styled_table.R
setGeneric("write_excel", function(sheet, st, ...) standardGeneric("write_excel"))

#' @rdname write_excel-methods
#' @aliases write_excel,ANY,StyledTable-method
#' @param sheet An xlsx sheet object
#' @param st A [StyledTable] object object
#' @param first_row The number of the starting row of the table in the sheet
#' @param first_col The number of the starting col of the table in the sheet
#' @seealso [write_pdf()], [write_png()], [create_latex_table()], [append_latex_table()], [create_latex_table_body()]
setMethod(
    "write_excel",
    signature(
        sheet = "ANY",
        st = "StyledTable"
    ),
    function(sheet, st, first_row = NULL, first_col = NULL) {
        # Error handler
        errHandler <- function(description) {
            stop(paste0(
                "Error in 'write_excel'-method: ",
                description
            ), call. = FALSE)
        }
        # get first_row, first_col
        if (is.null(first_row)) {
            if (!is.null(attr(sheet, "lastRow"))) {
                first_row <- attr(sheet, "lastRow") + 1L
            } else {
                first_row <- 1L
            }
        }
        if (is.null(first_col)) {
            if (!is.null(attr(sheet, "first_col"))) {
                first_col <- attr(sheet, "first_col")
            } else {
                first_col <- 1L
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
                function(x) compare_styles(x$style, styleVal)
            ))]
            if (length(candidates) == 0) {
                cs <- get_xlsx_cell_style(wb, styleVal)
                createdCellStyles[[length(createdCellStyles) + 1L]] <<- list(cs = cs, style = styleVal)
            } else
                cs <- candidates[[1]]$cs
            cs
        }
        # Dimensions of table
        nCol = count_cols(st)
        nRow = count_rows(st)
        row_id <- first_row:(nRow + first_row - 1L)
        # Delete existing rows that will be overwritten
        row_id_existing <- intersect(
            row_id, 
            as.numeric(names(getRows(sheet)))
        )
        if (length(row_id_existing) > 0L) {
                errHandler(paste0(
                        "The rows were the table would be written too (", 
                        paste0(row_id_existing, collapse = ", "),
                        ") are not empty."
                    ))
            removeRow(sheet, getRows(sheet, row_id_existing))
        }
        # Generate new rows
        rows   <- createRow(sheet, row_id)
        cells  <- createCell(rows, colIndex = first_col:(nCol + first_col - 1L))
        # Set height of each row
        for (i in seq_len(length(st@excel_row_height$row_id))) {
            xlsx::setRowHeight(
                rows = rows[as.character(first_row + st@excel_row_height$row_id[i] - 1L)], 
                st@excel_row_height$heights[i]
            )
        }
        # Set width of each column    
        for (i in seq_len(length(st@excel_col_width$col_id))) {
            xlsx::setColumnWidth(
                sheet,
                first_col + st@excel_col_width$col_id[i] - 1L, 
                st@excel_col_width$widths[i]
            )
        }
        # Write values and styles to cells
        for (i in seq_len(nRow)) {
            for (j in seq_len(nCol)) {
                merges <- st@merges[
                        unlist(lapply(
                            st@merges, 
                            function(m) between_vec(i, m$row_id) && between_vec(j, m$col_id)
                        ))
                    ]
                value <- NULL
                if (length(merges) == 1) {
                    m <- merges[[1]]
                    if (m$row_id[1] == i && m$col_id[1] == j)
                        value <- st@data[[i]][[j]]
                } else {
                    value <- st@data[[i]][[j]]
                }
                # Preprocess the value
                preProcess <- getStyledCell(st@styles[[i]][[j]], "excel_pre_process")
                tryCatch({
                        value <- preProcess(value)
                    },
                    error = function(description) {
                        stop(paste0("Error in 'write_excel' ",
                                "while evaluating the function given ",
                                "in 'set_excel_pre_process' on cell value (", 
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
                style <- st@styles[[i]][[j]]
                if (!is.null(style))
                    setCellStyle(
                        cells[[i, j]],
                        createCS(style)
                    )
            }
        }
        # Merge cells
        for (m in st@merges) {
            addMergedRegion(
                sheet, 
                first_row - 1L + m$row_id[1],
                first_row - 1L + m$row_id[2],
                first_col - 1L + m$col_id[1],
                first_col - 1L + m$col_id[2]
            )
        
        }
        # write last row to sheet
        attr(sheet, "lastRow") <- first_row - 1L + nRow
        attr(sheet, "first_col") <- first_col
        invisible(sheet)
    }
)
