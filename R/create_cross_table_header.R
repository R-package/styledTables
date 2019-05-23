#' Method create_cross_table_header
#'
#' @name create_cross_table_header
#' @rdname create_cross_table_header-methods
#' @exportMethod create_cross_table_header
setGeneric(
    "create_cross_table_header", 
    function(
        y_col_headings, 
        ...
    ) standardGeneric("create_cross_table_header")
)

#' Create a styled header for a cross table
#'
#' This function creates a styled header for a cross table. It is best used
#' together with [create_cross_table_body()] and [styled_table()]. 
#' It assumes that the cross table consists of several Y columns that come first
#' and has nested cross table part. That means that the cross table X columns
#' can have more than one header level. 
#' For example: Male > Positive | Male > Negative | Female > Positive | Female > Negative
#' In this case the 'Male' and 'Female' headings will be in merged cells above the 'Positive' and 'Negative' cells.
#' @rdname create_cross_table_header-methods
#' @aliases create_cross_table_header,character,character,character,character,character,character-method
#' @param y_col_headings A character vector that holds the header texts for the Y columns of the cross table. If no other arguments are given the resulting is not a cross table, but a normal table.
#' @param cross_table_heading (optional) A single character string that is printed in a merged cell above the Y cross table cols.
#' @param ... (optional) One or more vectors of character strings that hold the levels of the first/second/third/... cross table X column headings. These levels are used as header texts in the first/second/third/... row of the generated cross table columns. If no character vector is supplied, then the resulting table is not a cross table, but a normal table.
#' @return The generated [StyledTable] object holding the styled table header rows
#' @examples 
#' library(dplyr)
#' # prepare data set for cross table
#' students_data <- data.frame(
#'     country = rep(c("Germany", "Austria"), each = 16),
#'     year = c(rep("2010", 8), rep("2011", 8)),
#'     subject = rep(c(rep("Mathematics", 4), rep("Statistics", 4)), 2),
#'     gender = rep(c(rep("Male", 2), rep("Female", 2)), 4),
#'     result = rep(c("positive", "negative"), 16),
#'     N = sample(1:1000, 32)
#'   ) %>%
#'   group_by(country, year, subject, gender) %>%
#'   mutate(rel = 100 * N / sum(N)) %>%
#'   ungroup
#' # setup styled header
#' s_header <- create_cross_table_header(
#'     y_col_headings = c("Year", "Subject"),
#'     cross_table_heading = "Comparison of test results",
#'     c("Male", "Female"),
#'     c("Positive", "Negative"),
#'     c("count", "in %")
#'   ) %>%
#'   format_stat_header
#' # setup styled cross table body
#' s_body <- create_cross_table_body(
#'     data = students_data,
#'     sub_table_cols = "country",
#'     sub_heading_styling = function(st) set_horizontal(st, "center"),
#'     body_styling = function(st) set_horizontal(st, "center"),
#'     y_cols_left = c("year", "subject"),
#'     x_cols = c("gender", "result"),
#'     value_cols = c("N", "rel"),
#'     value_col_stylings = list(
#'       format_stat_absolute,
#'       format_stat_relative
#'     ),
#'     fill_values = list(0L, 0)
#'   )
#' # Concat styled header and styled body into a single styled table
#' s_tbl <- styled_table(
#'     s_header,
#'     s_body
#'   )
#' s_tbl %>%
#'   write_png
setMethod(
    "create_cross_table_header",
    signature(
        y_col_headings = "character"
    ),
    function(
        y_col_headings,
        cross_table_heading = NULL,
        ...
    ) {
        # All headings of the columns that are used for the cross table columns
        xColHeadings <- list(...)
        # Filter out all NULL entries
        if (length(xColHeadings) > 0)
            xColHeadings <- xColHeadings[which(sapply(xColHeadings, function(x) !is.null(x)))]
        
        ### Check consistency of supplied arguments
        errHandler <- function(description) {
            stop(paste0(
                "Error in '",
                "create_cross_table_header",
                "': ",
                description
            ), call. = FALSE)
        }
        # Check y_cols_left_headings
        if (!is.null(y_col_headings) & !is.character(y_col_headings))
            errHandler(paste0(
                "Argument 'y_col_headings' must be a vector of",
                " character strings."
            ))
        # Check x_col_headings (...) and cross_table_heading
        if (length(xColHeadings) > 0) {
            if (
                !is.null(cross_table_heading) && 
                    (!is.character(cross_table_heading) || length(cross_table_heading) != 1)
            )
                errHandler(paste0("Argument 'cross_table_heading' must be NULL or a character string."))
            for (colHeadings in xColHeadings) {
                if (!is.character(colHeadings) || length(colHeadings) == 0)
                    errHandler(paste("The argument '...' must be omitted or given one ore more non empty character vectors."))
            }
        } else {
            if (!is.null(cross_table_heading))
                errHandler(paste(
                    "The Argument 'cross_table_heading' can only be set, if",
                    "any X cross column headings are given",
                    "(at least one character must be passed into the argument '...')."
                ))
        }
        ### Create header data
        # append the main cross table heading to the cross column headings
        # (actually it is a special case of the cross col heading system: a heading without column)
        if (!is.null(cross_table_heading)) 
            xColHeadings <- c(cross_table_heading, xColHeadings)
        # Calculate number of header rows
        if (length(xColHeadings) > 0) {
            nrows <- length(xColHeadings)
        } else {
            nrows <- 1L
        }
        # Generate the header for all Y cols (no cross table cols)
        ny_cols_left <- length(y_col_headings)
        headerData <- data.table(matrix(rep(y_col_headings, each = nrows), ncol = ny_cols_left))
        # List of merged cells (will be filled up later on)
        merges <- lapply(seq_save(1L, ny_cols_left), function(i) list(col_id = c(i, i), row_id = c(1L, nrows)))
        # Generate cross table header cols and put them to the right of the y cols
        if (length(xColHeadings) > 0) {
            sHeader <- styled_table(cbind(
                    headerData,
                    rbindlist(lapply(1:nrows, function(i) {
                        if (i < nrows) {
                            nLevelsBelow <- prod(sapply(
                                        xColHeadings[seq_save(i + 1L, nrows)], 
                                        length
                                    ))
                        } else {
                            nLevelsBelow <- 1L
                        }
                        if (i > 1) {
                            nLevelsAbove <- prod(
                                sapply(xColHeadings[seq_save(1, i - 1L)], length))
                        } else {
                            nLevelsAbove <- 1L
                        }
                        data.table(matrix(
                            rep(
                                rep(xColHeadings[[i]], each = nLevelsBelow), 
                                nLevelsAbove
                            ), nrow = 1))
                    }), use.names = FALSE)
                ))
            # Add the merges for the cross table header levels
            merges <- c(
                    merges,
                    unlist(lapply(seq_save(1L,(nrows - 1L)), function(i) {
                        nLevelsBelow <- prod(sapply(
                                    xColHeadings[seq_save(i + 1L, nrows)], 
                                    length
                                ))
                        if (i > 1L) {
                            nLevelsAbove <- prod(
                                sapply(xColHeadings[seq_save(1L, i - 1L)], length))
                        } else {
                            nLevelsAbove <- 1L
                        }
                        if (nLevelsBelow == 1L)
                            return(NULL)
                        unlist(lapply(
                            0L:(length(xColHeadings[[i]]) - 1L), 
                            function(j) lapply(
                                0L:(nLevelsAbove - 1L), 
                                function(k) list(
                                    row_id = c(i, i), 
                                    col_id = ny_cols_left + 
                                        (j + k * nLevelsAbove) * nLevelsBelow + 
                                        c(1L, nLevelsBelow))
                            )), recursive = FALSE)
                    }), recursive = FALSE)
                )
        } else {
            sHeader <- styled_table(headerData)
        }
        # write the merges list directly into the styledTable (faster)
        sHeader@merges <- merges
        sHeader
    }
)
