#' Method create_cross_table_header
#'
#' @name create_cross_table_header
#' @rdname create_cross_table_header-methods
#' @exportMethod create_cross_table_header
setGeneric(
  "create_cross_table_header",
  function(
    y_headings_left,
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
#' @param y_headings_left A character vector that holds the header texts for the Y columns at the left side of the cross table. If no other arguments are given the resulting is not a cross table, but a normal table.
#' @param y_headings_right A character vector that holds the header texts for the Y columns at the right side of the cross table. 
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
#'     y_headings_left = c("Year", "Subject"),
#'     "Comparison of test results",
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
#' st <- styled_table(
#'     s_header,
#'     s_body
#'   )
#' st %>%
#'   write_png
setMethod(
  "create_cross_table_header",
  signature(
    y_headings_left = "character"
  ),
  function(
    y_headings_left,
    ...,
    y_headings_right = NULL
  ) {
    # All headings of the columns that are used for the cross table columns
    x_headings <- list(...)
    # Filter out all NULL entries
    if (length(x_headings) > 0)
      x_headings <- x_headings[which(sapply(x_headings, function(x) !is.null(x)))]
    ### Check consistency of supplied arguments
    err_handler <- function(description) {
        stop(paste0(
            "Error in '",
            "create_cross_table_header",
            "': ",
            description
        ), call. = FALSE)
    }
    # Check y_cols_left_headings
    if (!is.null(y_headings_left) & !is.character(y_headings_left))
      err_handler(paste0(
        "Argument 'y_headings_left' must be a vector of",
        " character strings."
      ))
    # Check y_cols_right_headings
    if (!is.null(y_headings_right) & !is.character(y_headings_right))
      err_handler(paste0(
        "Argument 'y_headings_right' must be a vector of",
        " character strings."
      ))
    # Check x_col_headings (...)
    if (length(x_headings) > 0) {
      for (curr_headings in x_headings) {
        if (!is.character(curr_headings) || length(curr_headings) == 0)
          err_handler(paste("The argument '...' must be omitted or given one ore more non empty character vectors."))
      }
    } else {
      if (!is.null(y_headings_right))
        err_handler(paste0(
          "Using argument 'y_headings_right' is only allowed when cross table",
          " columns are specified as well (argument '...')."
        ))
    }
    ### Create header data
    # Calculate number of header rows
    if (length(x_headings) > 0) {
      nrows <- length(x_headings)
    } else {
      nrows <- 1L
    }
    # Generate the header for all Y cols (no cross table cols)
    ny_cols_left <- length(y_headings_left)
    ny_cols_right <- length(y_headings_right)
    header_data <- data.table(matrix(rep(y_headings_left, each = nrows), ncol = ny_cols_left))
    # List of merged cells (will be filled up later on)
    merges <- lapply(seq_save(1L, ny_cols_left), function(i) list(col_id = c(i, i), row_id = c(1L, nrows)))
    # Generate cross table header cols and put them to the right of the y cols
    if (length(x_headings) > 0) {
      header_data <- cbind(
        header_data,
        rbindlist(lapply(1:nrows, function(i) {
          if (i < nrows) {
            nLevelsBelow <- prod(sapply(
              x_headings[seq_save(i + 1L, nrows)],
              length
            ))
          } else {
            nLevelsBelow <- 1L
          }
          if (i > 1) {
            nLevelsAbove <- prod(
              sapply(x_headings[seq_save(1, i - 1L)], length))
          } else {
            nLevelsAbove <- 1L
          }
          data.table(matrix(
            rep(
              rep(x_headings[[i]], each = nLevelsBelow),
              nLevelsAbove
            ), nrow = 1))
        }), use.names = FALSE)
      )
      if (ny_cols_right > 0)
        header_data <- cbind(
          header_data,
          data.table(matrix(rep(y_headings_right, each = nrows), ncol = ny_cols_right))
        )
      st <- styled_table(header_data) 
      # Add the merges for the cross table header levels
      merges <- c(
        merges,
        unlist(lapply(seq_save(1L, (nrows - 1L)), function(i) {
          nLevelsBelow <- prod(sapply(
              x_headings[seq_save(i + 1L, nrows)],
              length
          ))
          if (i > 1L) {
            nLevelsAbove <- prod(
              sapply(x_headings[seq_save(1L, i - 1L)], length))
          } else {
            nLevelsAbove <- 1L
          }
          if (nLevelsBelow == 1L)
            return(NULL)
          unlist(lapply(
            0L:(length(x_headings[[i]]) - 1L),
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
      if (ny_cols_right > 0)
        merges <- c(
          merges, 
          lapply(
            ny_cols_left + prod(sapply(x_headings, length)) + seq_save(1, ny_cols_right), 
            function(i) list(col_id = c(i, i), row_id = c(1L, nrows))
          )
        )
    } else {
      st <- styled_table(header_data)
    }
    # write the merges list directly into the styledTable (faster)
    st@merges <- merges
    st
  }
)
