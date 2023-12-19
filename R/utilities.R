#' Check if type is as expected (without using non standard evaluation)
#'
#' @param var Variable whose type should be checked
#' @param types (vector of strings) A vector of allowed type values
#' @param var_name (string) Name of the variable passed in in \code{var}. This
#' name is used for the error message if it has the wrong type.
#' @param allow_na (logical) Is a missing value allowed?
assert_type_ <- function(var, types, var_name, allow_na = TRUE) {
    if ((!allow_na && is.na(var)) || (!is.na(var) && !typeof(var) %in% types))
        sprintf(
            "Argument '%s' must be of type '%s'.",
            var_name,
            paste(paste0("'", types, "'"), collapse = " oder ")
        )
}

#' Check if a variable is a character, if not throw an error (using non standard evaluation)
#'
#' @param var The value that should be checked
assert_char <- function(var) {
    if (!is.null(var)) {
        var_name <- deparse(substitute(var))
        assert_type_(var, "character", var_name)
    }
}

#' Check if a variable is a number, if not throw an error (using non standard evaluation)
#'
#' @param var The value that should be checked
assert_number <- function(var) {
    if (!is.null(var)) {
        var_name <- deparse(substitute(var))
        assert_type_(var, c("integer", "double"), var_name)
    }
}

#' Check if a variable is a logical, if not throw an error (using non standard evaluation)
#'
#' @param var The value that should be checked
assert_logical <- function(var) {
    if (!is.null(var)) {
        var_name <- deparse(substitute(var))
        assert_type_(var, "logical", var_name)
    }
}

#' Check if a variable holds an allowed value (using non standard evaluation)
#'
#' @param var Variable whose value should be checked
#' @param values (vector) A vector of allowed values
assert_value <- function(var, values) {
    # Check if variable "var" has an allowed value (value list given by "values")
    # if not generate an error message and return it
    if (length(var) > 0) {
        var_name <- deparse(substitute(var))
        assert_value_(var, var_name, values)
    }
}

#' Check if a variable holds an allowed value (without non standard evaluation)
#'
#' @param var Variable whose value should be checked
#' @param var_name name of the variable whose value should be checked
#' @param values (vector) A vector of allowed values
assert_value_ <- function(var, var_name, values) {
    # Check if variable "var" has an allowed value (value list given by "values")
    # if not generate an error message and return it
    if (!all(var %in% values)) {
        if (is.character(values))
            strValues <- paste0("'", values, "'")
        else
            strValues <- values
        strValues <- paste0(strValues, collapse = ", ")
        sprintf("Argument '%s' must be in c(%s).", var_name, strValues)
    }
}

#' Check if a variable holds a color value (using non standard evaluation)
#'
#' @param var Variable whose value should be checked
asser_color_value <- function(var) {
    # Check if variable "var" has a color value
    # if not, throw an error
    if (length(var) > 0) {
        var_name <- deparse(substitute(var))
        if (!all(grepl("^#[0-9abcdefABCDEF]{6}$", var)))
            assert_value_(var, var_name, transform_char(grDevices::colors(), toUpper = TRUE))
    }
}

#' Transform text
#'
#' Transform a text to upper case and maybe append some text at front of the
#' beginning or at the end
#' @param var (string) Text that should be transformed
#' @param pre (string) Text that should be appended in front
#' @param post (string) Text that should be appended at the end
#' @param toUpper (logical) Should the text be transformed to upper case?
transform_char <- function(var = NULL, pre = "", post = "", toUpper = TRUE) {
    if (!is.null(var) && is.character(var)) {
        if (toUpper)
            var <- toupper(var)
        paste0(pre, var, post)
    } else
        var
}

#' Remove empty elements from a list
#'
#' Remove all elements who are totally nested NULLs or elements of length == 0) in the first level of lists
#' @param li A list
remove_missing <- function(li) {
    Filter(function(x) length(unlist(x)) > 0, li)
}

#' Call function with arguments and remove all empty arguments (NULL or length == 0)
#'
#' Invokes a function with a list of arguments, but removes all \code{NULL} arguments first
#' @param fn (function) Function that should be invoked
#' @param args (named list) List of arguments (\code{NULL} arguments will be removed)
#' @param skipIfAllMissing (logical) If \code{skipIfAllMissing = TRUE} and all arguments in \code{args} are \code{NULL}, then the function \code{fn} is not invoked and \code{NULL} is returned instead.
do_call_without_missing <- function(fn, args, skipIfAllMissing = TRUE) {
    rArgs <- remove_missing(args)
    if (!skipIfAllMissing || length(rArgs) > 0)
        do.call(fn, rArgs)
}

#' Improved \code{seq} command that returns an empty vector if lower > upper boundary
#'
#' Enhance the \code{seq} such that it can handle the situation if \code{from > to}
#' @param from First element in the sequence
#' @param to Last element in the sequence
#' @return The sequence
seq_save <- function(
    from,
    to
) {
    if (to < from)
        return(integer(0))
    seq(from, to)
}

#' Improved \code{seq} command which allows an two element vector holding the sequence boundaries
#'
#' The boundaries for the sequence command are given as two element vector and passed to the \code{seq_save} function.
#' @param bLU A two element vector. The first element is the lower boundary the second element the upper boundarie. If the lower boundary is greater than the upper boundary, then an empty vector is returned.
#' @return The sequence
seq_vec <- function(
    bLU
) {
    if (length(bLU) != 2)
        return(integer(0))
    seq_save(bLU[1], bLU[2])
}

#' Calculate the hex color value from a color name
#'
#' This little function takes a color name (e.g.: "red") and calculates the corresponding hex color value.
#' @param col A string holding a color name (e.g.: "red") or a color value (e.g.: "#F00000")
#' @return The hex color value as string without hashtag (e.g.: "F00000")
calc_hex_color <- function(col) {
    if (!all(grepl("^#[0-9abcdefABCDEF]{6}$", col))) {
        colVec <- grDevices::col2rgb(col)
        col <- toupper(sprintf("#%02X%02X%02X", colVec[1], colVec[2], colVec[3]))
    }
    gsub("#", "", col)
}

#' Vectorized version of 'between'
#'
#' @param x A numeric value that should be between two other values
#' @param vec A vector holding the upper and lower bound values
between_vec <- function(x, vec) {
    vec[1] <= x & x <= vec[2]
}

#' Escape latex commands
#'
#' @param val A character string that should be sanitized
sanitize <- function(val) {
    val <- gsub("\\", "\\textbackslash{}", val, fixed = TRUE)
    val <- gsub("%", "\\%", val, fixed = TRUE)
    val <- gsub("\\{", "\\{", val, fixed = TRUE)
    val <- gsub("\\}", "\\}", val, fixed = TRUE)
    val <- gsub("&", "\\&", val, fixed = TRUE)
    val <- gsub("\\$", "\\$", val, fixed = TRUE)
    val <- gsub("#", "\\#", val, fixed = TRUE)
    val <- gsub("_", "\\textunderscore{}", val, fixed = TRUE)
    val <- gsub("\\~", "\\textasciitilde{}", val, fixed = TRUE)
    gsub("\\^", "\\textasciicircum{}", val, fixed = TRUE)
}

#' Concatenate functions
#'
#' @param ... Several functions that should be concatenated
concat_functions <- function(...) {
    fnList <- list(...)
    function(x) {
        for (fn in fnList) x <- fn(x)
        x
    }
}

#' Compare Styles
#'
#' @param s1 First style st
#' @param s2 First style st
#' @export
compare_styles <- function(s1, s2) {
    all(sapply(c(
            "excel_font_name",
            "excel_font_size",
            "font_color",
            "bold",
            "italic",
            "strikeout",
            "underline",
            "excel_boldweight",
            "excel_wrapped",
            "horizontal",
            "vertical",
            "rotation",
            "indent",
            "border_position",
            "border_color",
            "excel_border_pen",
            "fill_color",
            "excel_background_color",
            "excel_fill_pattern",
            "excel_data_format",
            "excel_locked",
            "excel_hidden",
            "latex_vertical_move"
        ), function(slot_name)
            identical(slot(s1, slot_name), slot(s2, slot_name))
    ))
}

#' Substitute \code{col_id} and \code{row_id}
#'
#' This function is used to substitute \code{n_col} and \code{n_row} inside
#' of the expressions passed into \code{col_id} and \code{row_id}
#' @param st The [StyledTable] object
#' @param col_id The passed expression that should be evaluated to the column ids
#' @param stack_level The level number of the environment stack (1: parent.frame, 2: parent.parent.frame))
#' @param error_handler Error handling function. Takes a message string
#' @return The evaluated column ids (unbroken numeric vector)
substitute_col_id <- function(st, col_id, stack_level = 1, error_handler) {
    # Substitute n_col in the columns selector
    env <- new.env()
    env$n_col <- count_cols(st)
    error_col <- function(obj)
        error_handler(paste0(
                "The argument 'col_id' must be a numeric vector and subset of 1:",
                count_cols(st),
                ". The total number of columns can be substituted by 'n_col'",
                "The passed in ",
                "value could not be parsed: 'col_id = ",
                deparse(col_id), "'. Details: ",
                obj$message
            ))
    tryCatch({
            col_id <- eval(col_id, list(n_row = count_rows(st)), parent.frame(n = stack_level))
        },
        warning = error_col,
        error = error_col
    )
    if (!is.null(col_id)) {
        if (!is.numeric(col_id))
            error_handler("The argument 'col_id' must be numeric.")
        if (any(!col_id %in% 1:count_cols(st)))
            error_handler(paste0(
                    "The argument 'col_id' must be a subset of 1:",
                    count_cols(st),
                    ". The total number of columns can be substituted by 'n_col'."
                ))
    }
    col_id
}

#' Substitute \code{row_id}
#'
#' This function is used to substitute \code{n_row} inside
#' of the expression passed into \code{row_id}
#' @param st The [StyledTable] object
#' @param row_id The passed expression that should be evaluated to the row ids
#' @param stack_level The level number of the environment stack (1: parent.frame, 2: parent.parent.frame))
#' @param error_handler Error handling function. Takes a message string
#' @return The evaluated row ids (unbroken numeric vector)
substitute_row_id <- function(st, row_id, stack_level = 1, error_handler) {
    # Substitute n_row in the rows selector
    env <- new.env()
    env$n_row <- count_rows(st)
    error_row <- function(obj)
        error_handler(paste0(
                "The argument 'row_id' must be a subinterval of 1:",
                count_rows(st),
                ". The total number of rows can be substituted by 'n_row'",
                "The passed in ",
                "value could not be parsed: 'row_id = ",
                deparse(row_id), "'. Details: ",
                obj$message
            ))
    tryCatch({
            row_id <- eval(row_id, list(n_row = count_rows(st)), parent.frame(n = stack_level))
        },
        warning = error_row,
        error = error_row
    )
    if (!is.null(row_id)) {
        if (!is.numeric(row_id))
            error_handler("The argument 'row_id' must be numeric.")
        if (any(!row_id %in% 1:count_rows(st)))
            error_handler(paste0(
                    "The argument 'row_id' must be a subset of 1:",
                    count_rows(st),
                    ". The total number of rows can be substituted by 'n_row'."
                ))
    }
    row_id
}

get_merged_cell <- function(st, row, col, fake_single_cell = FALSE) {
  m_curr <- NULL
  for (m in st@merges) {
    if (between_vec(row, m$row_id) && between_vec(col, m$col_id)) m_curr <- m
  }
  if (isTRUE(fake_single_cell) && is.null(m_curr))
    m_curr <- list(row_id = c(row, row), col_id = c(col, col))
  m_curr
}

#' Stringify vector
#'
#' Turn a vector (e.g. `c(1, 2, 3)`) into a string (e.g. `"'1', '2', '3'"`).
#' This function is particularly useful when creating error messages,
#' describing problematic values.
#' @param x An object, which should be turned into a string. Usually an atomic
#'   object (a vector).
#' @param quote A string, holding  the symbol which should be used for quoting
#'   every entry of `x`. Default is `'` which gives the result `'entry1'`.
#' @param collapse A string, used for separating each vector entry.
#' @param before A string placed before each vector entry.
#' @param after A string placed after each vector entry.
#' @param n_max A positive integer or `Inf`, defining the maximum number of
#'   displayed vector elements. All further elements are displayed with the
#'   text defined in `further`.
#' @param further A string indicating further (after `n_max` displayed elements)
#'   vector elements.
#' @return A string showing the entries of the vector.
stringify <- function(
  x,
  before = NULL,
  after = NULL,
  collapse = ", ",
  quote = "'",
  n_max = 10,
  further = "..."
) {
  err_h <- function(msg)
    stop(paste("Error while calling `stringify()`:", msg), call. = FALSE)
  if (!is.null(collapse) && (
    !is.character(collapse) || length(collapse) != 1 || is.na(collapse)
  ))
    err_h("Argument `collapse` must be a non-missing string value or `NULL`.")
  if (!is.null(quote) && (
    !is.character(quote) || length(quote) != 1 || is.na(quote)
  ))
    err_h("Argument `quote` must be a non-missing string value or `NULL`.")
  if (!is.null(before) && (
    !is.character(before) || length(before) != 1 || is.na(before)
  ))
    err_h("Argument `before` must be a non-missing string value or `NULL`.")
  if (!is.null(after) && (
    !is.character(after) || length(after) != 1 || is.na(after)
  ))
    err_h("Argument `after` must be a non-missing string value or `NULL`.")
  if (!is.numeric(n_max) || length(n_max) != 1 || is.na(n_max) || n_max < 1 ||
      (is.finite(n_max) && as.integer(n_max) != n_max)
  )
    err_h("Argument `n_max` be a positive integer value or `Inf`.")
  if (!is.character(further) || length(further) != 1 || is.na(further))
    err_h("Argument `further` must be a non-missing string value.")
  # stringify
  x <- tryCatch(
    as.character(x),
    error = function(e) err_h(paste0(
      "Argument `x` could not be converted into a character vector:\n", e))
  )
  add_dots <- length(x) > n_max
  if (length(x) > n_max)
    x <- x[1:n_max]
  if (!is.null(quote))
    x <- c(
      paste0(quote, x, quote),
      if (add_dots) further else NULL
    )
  paste(paste0(before, x, after), collapse = collapse)
}
