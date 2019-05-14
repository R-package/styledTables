#' Class StyledTable
#'
#' An \code{S4 class} which is used to export your data as a beautifully styled table.
#' Convert your \code{data.frame} or \code{matrix} into a \code{StyledTable} object by using the [styled_table()] function.
#' @name StyledTable-class
#' @rdname StyledTable-class
#' @exportClass StyledTable
#' @include styled_cell.R
#' @seealso [styled_table()]
#' @seealso [write_pdf()], [write_png()], [write_excel()], [create_latex_table()], [append_latex_table()], [create_latex_table_body()]
#' @seealso [set_excel_font_name()], [set_latex_font_name()], [set_excel_font_size()], [set_latex_font_size()], [set_font_color()], [set_bold()], [set_excel_boldweight()], [set_italic()], [set_underline()], [set_strikeout()], [set_rotation()], [set_indent()], [set_fill_color()], [set_horizontal()], [set_excel_vertical()], [set_latex_vertical_move()], [set_latex_vertical_move()], [set_excel_row_height()], [set_latex_row_height()], [set_excel_col_width()], [set_latex_col_width()], [set_border_position()], [set_border_color()], [set_excel_border_pen()], [set_excel_data_format()], [set_excel_pre_process()], [set_latex_pre_process()], [set_excel_wrapped()], [set_excel_locked()], [set_excel_hidden()]
#' @seealso [remove_col()], [remove_row()]
#' @seealso [merge_cells()], [merge_equal_cells()]
#' @seealso [format_stat_header()], [format_stat_body()], [format_stat_sub_heading()], [format_stat_absolute()], [format_stat_relative()]
#' @seealso [create_cross_table_body()], [create_cross_table_header()]
#' @seealso [count_rows()], [count_cols()]
setClass(
    "StyledTable",
    representation(
        data = "list",
        merges = "list",
        styles = "list",
        excel_row_height = "list",
        excel_col_width = "list",
        latex_row_height = "list",
        latex_col_width = "list"
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
        # Check if merges is list containing lists(row_id =..., col_id = ...)
        for (m in args$merges) {
            if (
                    !is.list(m) ||
                    !is.numeric(m$row_id) || 
                    !is.numeric(m$col_id) || 
                    any(is.na(m$row_id)) ||
                    any(is.na(m$col_id)) ||
                    length(m$row_id) != 2 || 
                    length(m$col_id) != 2 ||
                    any(!m$row_id %in% 1:nRow) ||
                    any(!m$col_id %in% 1:nCol) ||
                    diff(m$row_id) < 0 ||
                    diff(m$col_id) < 0
                )
                errHandler(paste0(
                        "The each element in the argument 'merges' ",
                        "must be a list containing a ",
                        "'row_id' and a 'col_id' variable ",
                        "of length two."
                    ))
        }
        # Check if merged regions are not overlapping 
        for (i in seq_len(length(args$merges))) {
            m1 <- args$merges[[i]]
            for (j in seq_save(i + 1, length(args$merges))) {
                m2 <- args$merges[[j]]
                if (
                        any(between_vec(m1$row_id, m2$row_id)) && 
                        any(between_vec(m1$col_id, m2$col_id))
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
            errHandler("The argument 'styles' must be a list, whose length is the same as the number of row_id of the argument 'data'.")
        # Check if each element of styles is a list of length nCol and each element is NULL or a Style
        for (l in args$styles) {
            if (!is.list(l) || length(l) != nCol)
                errHandler(paste0(
                        "The argument 'styles'",
                        "must be list of length 'nrow(data)' of lists of",
                        "length 'ncol(data)' and each inner element must be",
                        "NULL or an st of class 'Style'."
                    ))
            for (style in l)
                if (!is.null(style) && !"StyledCell" %in% class(style))
                    errHandler(paste0(
                            "The argument 'styles'",
                            "must be list of length 'nrow(data)' of lists of",
                            "length 'ncol(data)' and each inner element must be",
                            "NULL or an st of class 'Style'."
                        ))
        }
        # assign styles
        .Object@styles <- args$styles 
    } else {
        .Object@styles <- lapply(seq_len(nRow), function(i) vector("list", nCol))
    }
    ######### read excel_row_height ###########
    if ("excel_row_height" %in% names(args)) {
        # Check if excel_row_height$row_id and $height have correct types and same length
        if (
                !is.list(args$excel_row_height) ||
                !is.numeric(args$excel_row_height$row_id) || 
                !is.numeric(args$excel_row_height$height) || 
                length(args$excel_row_height$row_id) != length(args$excel_row_height$height)
            )
            errHandler(paste0(
                    "The slot 'excel_row_height' must contain a list ", 
                    "which holds numeric vectors 'row_id' and ", 
                    "'height' of the same length."
                )) 
        # Check if excel_row_height$row_id is a subset of 1:N
        if (any(!args$excel_row_height$row_id %in% 1:nRow))
            errHandler(paste0(
                    "The list element 'row_id' in the 'excel_row_height' slot ",
                    "must be a subset of possible row indices of the ",
                    "data list in 'data'."
                )) 
        # Check if excel_row_height$height >0
        if (any(!args$excel_row_height$height <= 0))
            errHandler(paste0(
                    "The list element 'height' in the 'excel_row_height' slot ",
                    "must be a vector of positive numbers."
                )) 
        # assign excel_row_height
        .Object@excel_row_height <- args$excel_row_height
    } else {
        .Object@excel_row_height <- list(row_id = numeric(0), height = numeric(0))
    }
    ######### read excel_col_width ###########
    if ("excel_col_width" %in% names(args)) {
        # Check if excel_col_width$col_id and $width have correct types and same length
        if (
                !is.list(args$excel_col_width) ||
                !is.numeric(args$excel_col_width$col_id) || 
                !is.numeric(args$excel_col_width$width) || 
                length(args$excel_col_width$col_id) != length(args$excel_col_width$width)
            )
            errHandler(paste0(
                    "The slot 'excel_col_width' must contain a list ", 
                    "which holds numeric vectors 'col_id' and ", 
                    "'width' of the same length."
                )) 
        # Check if excel_col_width$col_id is a subset of 1:N
        if (any(!args$excel_col_width$col_id %in% 1:nCol))
            errHandler(paste0(
                    "The list element 'col_id' in the 'excel_col_width' slot ",
                    "must be a subset of possible col indices of the ",
                    "data list in 'data'."
                )) 
        # Check if excel_col_width$width >0
        if (any(!args$excel_col_width$width <= 0))
            errHandler(paste0(
                    "The list element 'width' in the 'excel_col_width' slot ",
                    "must be a vector of positive numbers."
                )) 
        # assign excel_col_width
        .Object@excel_col_width <- args$excel_col_width
    } else {
        .Object@excel_col_width <- list(col_id = numeric(0), width = numeric(0))
    }
    ######### read latex_row_height ###########
    if ("latex_row_height" %in% names(args)) {
        # Check if latex_row_height$row_id and $height have correct types and same length
        if (
                !is.list(args$latex_row_height) ||
                !is.numeric(args$latex_row_height$row_id) || 
                !is.numeric(args$latex_row_height$height) || 
                length(args$latex_row_height$row_id) != length(args$latex_row_height$height)
            )
            errHandler(paste0(
                    "The slot 'latex_row_height' must contain a list ", 
                    "which holds numeric vectors 'row_id' and ", 
                    "'height' of the same length."
                )) 
        # Check if latex_row_height$row_id is a subset of 1:N
        if (any(!args$latex_row_height$row_id %in% 1:nRow))
            errHandler(paste0(
                    "The list element 'row_id' in the 'latex_row_height' slot ",
                    "must be a subset of possible row indices of the ",
                    "data list in 'data'."
                )) 
        # Check if latex_row_height$height >0
        if (any(!args$latex_row_height$height <= 0))
            errHandler(paste0(
                    "The list element 'height' in the 'latex_row_height' slot ",
                    "must be a vector of positive numbers."
                )) 
        # assign latex_row_height
        .Object@latex_row_height <- args$latex_row_height
    } else {
        .Object@latex_row_height <- list(row_id = numeric(0), height = numeric(0))
    }
    ######### read latex_col_width ###########
    if ("latex_col_width" %in% names(args)) {
        # Check if latex_col_width$col_id and $width have correct types and same length
        if (
                !is.list(args$latex_col_width) ||
                !is.numeric(args$latex_col_width$col_id) || 
                !is.numeric(args$latex_col_width$width) || 
                length(args$latex_col_width$col_id) != length(args$latex_col_width$width)
            )
            errHandler(paste0(
                    "The slot 'latex_col_width' must contain a list ", 
                    "which holds numeric vectors 'col_id' and ", 
                    "'width' of the same length."
                )) 
        # Check if latex_col_width$col_id is a subset of 1:N
        if (any(!args$latex_col_width$col_id %in% 1:nCol))
            errHandler(paste0(
                    "The list element 'col_id' in the 'latex_col_width' slot ",
                    "must be a subset of possible col indices of the ",
                    "data list in 'data'."
                )) 
        # Check if latex_col_width$width >0
        if (any(!args$latex_col_width$width <= 0))
            errHandler(paste0(
                    "The list element 'width' in the 'latex_col_width' slot ",
                    "must be a vector of positive numbers."
                )) 
        # assign latex_col_width
        .Object@latex_col_width <- args$latex_col_width
    } else {
        .Object@latex_col_width <- list(col_id = numeric(0), width = numeric(0))
    }
    .Object
})

#' Count number of rows in a [StyledTable]
#'
#' @name count_rows
#' @rdname StyledTable-count_rows-method
#' @exportMethod count_rows
setGeneric("count_rows", function(st) standardGeneric("count_rows"))

#' @rdname StyledTable-count_rows-method
#' @aliases count_rows,StyledTable-method
#' @param st A [StyledTable] object
setMethod(
    "count_rows",
    signature(
        st = "StyledTable"
    ),
    function(st) {
        length(st@data)
    }
)

#' Count number of columns in a [StyledTable]
#'
#' @name count_cols
#' @rdname StyledTable-count_cols-method
#' @exportMethod count_cols
setGeneric("count_cols", function(st) standardGeneric("count_cols"))

#' @rdname StyledTable-count_cols-method
#' @aliases count_cols,StyledTable-method
#' @param st A [StyledTable] object
setMethod(
    "count_cols",
    signature(
        st = "StyledTable"
    ),
    function(st) {
        max(unlist(lapply(st@data, length)))
    }
)

#' Create a [StyledTable] object
#'
#' This function creates a [StyledTable] object from a single data.frame or multiple data.frames or other [StyledTable] objects. If multiple [StyledTable] objects or data.frames are supplied, then they are concatenated vertically. Therefore, all supplied data.frames and [StyledTable] objects must have the same number of columns.
#' @name styled_table
#' @rdname StyledTable-styled_table-method
#' @exportMethod styled_table
setGeneric("styled_table", function(..., keep_header = FALSE) standardGeneric("styled_table"))

#' @rdname StyledTable-styled_table-method
#' @aliases styled_table-method
#' @param ... Multiple matrices, data.frames or [StyledTable] objects that should be concatenated
#' @param keep_header (optional) A logical flag, if the column names of passed
#'    \code{data.frame} should be written in the first line of the resulting
#'    [StyledTable] object object.
#' @return A [StyledTable] object object
setMethod(
    "styled_table",
    signature(),
    function(..., keep_header = FALSE) {
        st <- NULL
        tbl_list <- list(...)
        if (keep_header && length(tbl_list) > 0 && is.data.frame(tbl_list[[1]]))
            tbl_list <- c(
                list(matrix(names(tbl_list[[1]]), nrow = 1)),
                tbl_list
            )
        lapply(
            tbl_list,
            function(x) {
                errHandler <- function(description)
                    stop(paste0(
                        "Error in 'styled_table'-method: ",
                        description
                    ), call. = FALSE)
                if (!is.null(x)) {
                    flagIsStyledTable <- is(x, "StyledTable")
                    if (!is.matrix(x) && !is.data.frame(x) && !flagIsStyledTable)
                        errHandler("All sts have to be of the class 'matrix', 'data.frame' or 'StyledTable'.")
                    if (!flagIsStyledTable)
                        x <- new("StyledTable", data = x)
                    if (is.null(st)) {
                        st <<- x
                    } else {
                        if (count_cols(st) != count_cols(x))
                            errHandler("All sts must have the same number of columns.")
                        nRow <- count_rows(st)
                        # Concat data
                        st@data <<- c(st@data, x@data)
                        # Concat styles
                        st@styles <<- c(st@styles, x@styles)
                        # Concat row height
                        st@excel_row_height$row_id <<- c(st@excel_row_height$row_id, x@excel_row_height$row_id + nRow)
                        st@excel_row_height$height <<- c(st@excel_row_height$height, x@excel_row_height$height)
                        # Concat col width
                        colIds <- st@excel_col_width$col_id %in% x@excel_col_width$col_id
                        st@excel_col_width$col_id <<- c(st@excel_col_width$col_id[!colIds], x@excel_col_width$col_id)
                        st@excel_col_width$width <<- c(st@excel_col_width$width[!colIds], x@excel_col_width$width)
                        # Concat merged cells
                        x@merges <- lapply(x@merges, function(y) list(row_id = nRow + y$row_id, col_id = y$col_id))
                        st@merges <<- c(st@merges, x@merges)
                    }
                }
            }
        )
        st
    }
)

#' Set styles of StyledTable cells
#'
#' @name setStyles
#' @rdname StyledTable-setStyles-method
#' @exportMethod setStyles
#' @param ... Various Arguments
setGeneric("setStyles", function(st, ...) standardGeneric("setStyles"))

#' @rdname StyledTable-setStyles-method
#' @aliases setStyles,StyledTable-method
#' @param st A [StyledTable] object
#' @param value The value that should be set
#' @param style_name The name of the style slot that should be set
#' @param row_id A vector of row numbers to which the change should be applied to
#' @param col_id A vector of col numbers to which the change should be applied to
#' @param append_mode A character string that defines if the styling value of the cell should be replaced (append_mode = "replace"), if the new styling should be appended at the end of the current styling value of the cell (append_mode = "appendBehind"), if the new styling should be appended in front of the current styling value of the cell (append_mode = "appendBefore")
setMethod(
    "setStyles",
    signature(
        st = "StyledTable"
    ),
    function(st, value, style_name = "", row_id = NULL, col_id = NULL, append_mode = "replace") {
        for (i in row_id) {
            for (j in col_id) {
                cellStyle <- st@styles[[i]][[j]]
                if (append_mode == "replace") {
                    currValue <- value
                } else {
                    v1 <- getStyledCell(cellStyle, style_name)
                    if (append_mode == "appendBehind") {
                        v2 <- value
                    } else {
                        v2 <- v1
                        v1 <- value
                    }
                    if (style_name %in% c("latex_pre_process", "excel_pre_process")) {
                        currValue <- concat_functions(v1, v2)
                    } else if (style_name == "excel_data_format") {
                        currValue <- paste0(v1, v2)
                    } else {
                        currValue <- c(v1, v2)
                    }
                }
                st@styles[[i]][[j]] <- setStyledCell(cellStyle, currValue, style_name)
            }
        }
        st
    }
)
