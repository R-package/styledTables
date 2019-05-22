#' Method format_stat_header.
#'
#' @name format_stat_header
#' @rdname format_stat_header-methods
#' @exportMethod format_stat_header
#' @param ... Various arguments
#' @include styled_table.R
#' @include styled_cell.R
#' @include set_styled_table.R
#' @include merge_cells.R
#' @include set_height_width.R
setGeneric("format_stat_header", function(st, ...) standardGeneric("format_stat_header"))

#' @rdname format_stat_header-methods
#' @aliases format_stat_header,StyledTable-method
#' @param st A [StyledTable] object
#' @param row_id A vector of row numbers. If omitted, the formatting will be applied to all rows
#' @return The modified [StyledTable] object
setMethod(
    "format_stat_header",
    signature(
        st = "StyledTable"
    ),
    function(st, row_id = NULL) {
        errHandler <- function(description)
            stop(paste0(
                "Error in 'format_stat_header'-method: ",
                description
            ), call. = FALSE)
        # Substitute N in the row_id selector
        if (missing(row_id)) {
            row_id <- 1:count_rows(st)
        } else {
            if (!is.numeric(row_id) || length(row_id) == 0 || any(is.na(row_id)))
                errHandler(paste0(
                    "The argument 'row_id' must be an unbroken numeric vector."
                ))

            if (any(!row_id %in% 1:count_rows(st)))
                errHandler(paste0(
                        "The argument 'row_id' must be a subinterval of 1:",
                        count_rows(st),
                        "."
                    ))
        }
        st <- set_excel_font_size(st, 7, row_id = row_id)
        st <- set_excel_font_name(st, "Arial", row_id = row_id)
        st <- set_excel_vertical(st, "center", row_id = row_id)
        st <- set_horizontal(st, "center", row_id = row_id)
        st <- set_excel_wrapped(st, TRUE, row_id = row_id)
        st <- set_border_position(st, c("top", "right", "bottom", "left"), row_id = row_id)
        set_excel_row_height(st, 12.75, row_id = row_id)
    }
)

#' Method format_stat_body.
#'
#' @name format_stat_body
#' @rdname format_stat_body-methods
#' @param ... Various arguments
#' @exportMethod format_stat_body
setGeneric("format_stat_body", function(st, ...) standardGeneric("format_stat_body"))

#' @rdname format_stat_body-methods
#' @aliases format_stat_body,StyledTable-method
#' @param st A [StyledTable] object
#' @param row_id A vector of row numbers. If omitted, the formatting will be applied to all rows
#' @return The modified [StyledTable] object
setMethod(
    "format_stat_body",
    signature(
        st = "StyledTable"
    ),
    function(st, row_id = NULL) {
        errHandler <- function(description)
            stop(paste0(
                "Error in 'format_stat_body'-method: ",
                description
            ), call. = FALSE)
        # Substitute N in the row_id selector
        if (missing(row_id)) {
            row_id <- 1:count_rows(st)
        } else {
            if (!is.numeric(row_id) || length(row_id) == 0 || any(is.na(row_id)))
                errHandler(paste0(
                    "The argument 'row_id' must be an unbroken numeric vector."
                ))
            if (any(!row_id %in% 1:count_rows(st)))
                errHandler(paste0(
                        "The argument 'row_id' must be a subinterval of 1:",
                        count_rows(st),
                        "."
                    ))
        }
        col_id = 1:count_cols(st)
        if (length(row_id) > 1)
            st <- set_excel_row_height(st, 12.25, row_id = row_id[2:length(row_id)])
        if (length(col_id) > 1)
            st <- set_horizontal(st, "right", row_id = row_id, col_id = col_id[2: length(col_id)])
        st <- set_excel_font_size(st, 7, row_id = row_id)
        st <- set_excel_font_name(st, "Arial", row_id = row_id)
        st <- set_excel_row_height(st, 12.75, row_id = row_id[1])
        if (length(row_id) > 1)
          st <- set_excel_row_height(st, 9.75, row_id = row_id[row_id != row_id[1]])
        st <- set_excel_vertical(st, "bottom", row_id = row_id)
        st <- set_horizontal(st, "left", row_id = row_id, col_id = col_id[1])
        if (length(col_id) > 1)
          set_horizontal(st, "right", row_id = row_id, col_id = col_id[col_id != col_id[1]])
        st
    }
)

#' Method format_stat_sub_heading
#'
#' @name format_stat_sub_heading
#' @rdname format_stat_sub_heading-methods
#' @param ... Various arguments
#' @exportMethod format_stat_sub_heading
setGeneric("format_stat_sub_heading", function(st, ...) standardGeneric("format_stat_sub_heading"))

#' @rdname format_stat_sub_heading-methods
#' @aliases format_stat_sub_heading,StyledTable-method
#' @param st A [StyledTable] object
#' @param row_id A vector of row numbers. If omitted, the formatting will be applied to all rows
#' @return The modified [StyledTable] object
setMethod(
    "format_stat_sub_heading",
    signature(
        st = "StyledTable"
    ),
    function(st, row_id = NULL) {
        errHandler <- function(description)
            stop(paste0(
                "Error in 'format_stat_sub_heading'-method: ",
                description
            ), call. = FALSE)
        # Substitute N in the row_id selector
        if (missing(row_id)) {
            row_id <- 1:count_rows(st)
        } else {
            if (!is.numeric(row_id) || length(row_id) == 0 || any(is.na(row_id)))
                errHandler(paste0(
                    "The argument 'row_id' must be an unbroken numeric vector."
                ))

            if (any(!row_id %in% 1:count_rows(st)))
                errHandler(paste0(
                        "The argument 'row_id' must be a subinterval of 1:",
                        count_rows(st),
                        "."
                    ))
        }
        ncols <- count_cols(st)
        st <- merge_cells(st, row_id = row_id, col_id = 1:ncols)
        st <- set_excel_font_size(st, 7, row_id = row_id)
        st <- set_excel_font_name(st, "Arial", row_id = row_id)
        st <- set_excel_row_height(st, 15, row_id = row_id)
        st <- set_excel_vertical(st, "bottom", row_id = row_id)
        st <- set_horizontal(st, "center", row_id = row_id)
        set_bold(st, TRUE, row_id = row_id)
    }
)

#' Method format_stat_absolute
#'
#' @name format_stat_absolute
#' @rdname format_stat_absolute-methods
#' @param ... Various arguments
#' @exportMethod format_stat_absolute
setGeneric("format_stat_absolute", function(st, ...) standardGeneric("format_stat_absolute"))

#' @rdname format_stat_absolute-methods
#' @aliases format_stat_absolute,StyledTable-method
#' @param st A [StyledTable] object
#' @param row_id A vector of row numbers. If omitted, the formatting will be applied to all rows
#' @param col_id A vector of column numbers. If omitted, the formatting will be applied to all columns
#' @return The modified [StyledTable] object
setMethod(
    "format_stat_absolute",
    signature(
        st = "StyledTable"
    ),
    function(st, row_id = NULL, col_id = NULL) {
        errHandler <- function(description)
            stop(paste0(
                "Error in 'format_stat_absolute'-method: ",
                description
            ), call. = FALSE)
        # Substitute N in the row_id selector
        if (missing(row_id)) {
            row_id <- 1:count_rows(st)
        } else {
            if (!is.numeric(row_id) || length(row_id) == 0 || any(is.na(row_id)))
                errHandler(paste0(
                    "The argument 'row_id' must be an unbroken numeric vector."
                ))

            if (any(!row_id %in% 1:count_rows(st)))
                errHandler(paste0(
                        "The argument 'row_id' must be a subinterval of 1:",
                        count_rows(st),
                        "."
                    ))
        }
        # Substitute N in the col_id selector
        if (is.null(col_id)) {
            col_id <- 1:count_cols(st)
        } else {
            if (!is.numeric(col_id))
                errHandler("The argument 'col_id' must be numeric vector.")
            if (any(!col_id %in% 1:count_cols(st)))
                errHandler(paste0(
                        "The argument 'col_id' must be a subinterval of 1:",
                        count_cols(st),
                        "."
                    ))
        }
        st <- set_excel_data_format(st, "#,##0", row_id = row_id, col_id = col_id)
        set_latex_pre_process(
            st,
            function(x) {
                if (!is.na(x) && x != 0) {
                    format(round(x, 0), nsmall = 0, big.mark = ".", decimal.mark = ",")
                } else if (!is.na(x) && x == 0) {
                    "-"
                } else {
                    "$\\cdot$"
                }
            },
            row_id = row_id,
            col_id = col_id)
    }
)

#' Method format_stat_relative
#'
#' @name format_stat_relative
#' @rdname format_stat_relative-methods
#' @param ... Various arguments
#' @exportMethod format_stat_relative
setGeneric("format_stat_relative", function(st, ...) standardGeneric("format_stat_relative"))

#' @rdname format_stat_relative-methods
#' @aliases format_stat_relative,StyledTable-method
#' @param st A [StyledTable] object
#' @param row_id A vector of row numbers. If omitted, the formatting will be applied to all rows
#' @param col_id A vector of column numbers. If omitted, the formatting will be applied to all columns
#' @return The modified [StyledTable] object
setMethod(
    "format_stat_relative",
    signature(
        st = "StyledTable"
    ),
    function(st, row_id = NULL, col_id = NULL) {
        errHandler <- function(description)
            stop(paste0(
                "Error in 'format_stat_relative'-method: ",
                description
            ), call. = FALSE)
        # Substitute N in the row_id selector
        if (missing(row_id)) {
            row_id <- 1:count_rows(st)
        } else {
            if (!is.numeric(row_id) || length(row_id) == 0 || any(is.na(row_id)))
                errHandler(paste0(
                    "The argument 'row_id' must be an unbroken numeric vector."
                ))
            if (any(!row_id %in% 1:count_rows(st)))
                errHandler(paste0(
                        "The argument 'row_id' must be a subinterval of 1:",
                        count_rows(st),
                        "."
                    ))
        }
        # Substitute N in the col_id selector
        if (is.null(col_id)) {
            col_id <- 1:count_cols(st)
        } else {
            if (!is.numeric(col_id))
                errHandler("The argument 'col_id' must be numeric vector.")
            if (any(!col_id %in% 1:count_cols(st)))
                errHandler(paste0(
                        "The argument 'col_id' must be a subinterval of 1:",
                        count_cols(st),
                        "."
                    ))
        }
        st <- set_excel_data_format(st, "#,##0.0", row_id = row_id, col_id = col_id)
        set_latex_pre_process(
            st,
            function(x) {
                if (!is.na(x) && x != 0) {
                    format(round(x, 1), nsmall = 1, decimal.mark = ",")
                } else if (!is.na(x) && x == 0) {
                    "-"
                } else {
                    "$\\cdot$"
                }
            },
            row_id = row_id,
            col_id = col_id)
    }
)

