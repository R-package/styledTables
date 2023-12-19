#' @include funky.R
NULL

#' Init a `text_builder` object, used for incrementally adding text parts
#' 
#' This function is very useful, when you want to incrementally add text
#' parts to a string.
#' The `text_builder` mechanism uses two separate text stacks in order to
#' store added text parts:
#' - A `downward stack`: For this stack all new text parts will be added below the
#'   previously added text parts. With `add_text()` new text parts can be added
#'   to this text stack.
#' - An `upward stack`: This stack uses the inverse direction, therefore
#'   all new text parts will be added above the previously added text parts.
#'   With `add_text_end()` new text parts can be added to the `upward stack`.
#'   When calling `create_text()` the `downward stack` is concatenated above
#'   the `upward stack`.
#'   
#' The function `init_text_builder()` returns a list object holding the following functions
#' - `add_text(text, indent_level = 0)`: This function adds a text part
#'   to the text stack, holding all text parts you have added so far.
#'   It takes two arguments:
#'   - `text`: A character vector holding the text parts you want to add.
#'   - `indent_level`: A non-negative integer, which defines how many times
#'     you want to add the indentation string (defined in `indent`) in front
#'     of the added text part.
#' - `add_text_end(text, indent_level = 0)`: This function is similar to
#'   `add_text()`, but adds the text part to the `upward stack`.
#'   It takes two arguments:
#'   - `text`: A character vector holding the text parts you want to add.
#'     Be careful, although the `upward stack` has reversed direction, the
#'     string entries in the character vector `text` are not reversed.
#'     This means that for `add_text_end(c("bla", "blub"))` the string
#'     `"bla"` comes also in the finally builded text before `"blub"`.
#'   - `indent_level`: A non-negative integer, which defines how many times
#'     you want to add the indentation string (defined in `indent`) in front
#'     of the added text part.
#' - `create_text()`: This function returns the constructed text.
#' @param sep A string, which will automatically be inserted between
#'   two added text parts If you want to use no separator symbol at all, then
#'   just set `sep = ""`.
#' @param indent A string, used as indentation symbol. This symbol will
#'   automatically be inserted zero, one or multiple times in front of an
#'   added text part, depending on the used `indent_level` (can be `NULL, 0, 1, 2, ...`)
#'   when calling `add_line("SOME TEXT", indent_level = X)`.
#'   If you do not want to use any indentation symbols, then just set
#'   `indent = NULL`.
#' @param before A string, which will automatically be added right in front
#'   of each text part.
#' @param after A string, which will automatically be added right after
#'   each text part.
#' @return A list object holding the function `add_text()` and `get_text()`
#' @export
init_text_builder <- function(
    sep = "\n",
    indent = "\t",
    before = NULL,
    after = NULL
) {
  err_h <- function(msg)
    stop(paste("Error while calling `init_text_builder()`:", msg), call. = FALSE)
  if (!is.character(sep) || length(sep) != 1 || is.na(sep))
    err_h("The argument `sep` is not a string.")
  if (!is.null(indent) && (!is.character(indent) || length(indent) != 1 || is.na(indent)))
    err_h("The argument `indent` is not a string.")
  if (!is.null(before) && (!is.character(before) || length(before) != 1 || is.na(before)))
    err_h("The argument `before` is not a string.")
  if (!is.null(after) && (!is.character(after) || length(after) != 1 || is.na(after)))
    err_h("The argument `after` is not a string.")
  eval_closure(
    {
      text_stack <- c()
      text_stack_end <- c()
      list(
        add_text = function(text, indent_level = 0L) {
          err_h <- function(msg)
            stop(paste("Error while calling `add_text()`:", msg), call. = FALSE)
          if (!is.null(text) && !is.character(text))
            err_h("Argument `text` is not a character vector.")
          if (is.null(indent_level))
            indent_level <- 0L
          if (!is.numeric(indent_level) || length(indent_level) != 1 ||
              !is.finite(indent_level) || indent_level < 0 ||
              indent_level != as.integer(indent_level)
          )
            err_h("Argument `indent_level` is not a positive integer.")
          text_stack <<- c(
            text_stack,
            paste0(
              paste(rep(indent, indent_level), collapse = ""),
              before,
              text,
              after
            )
          )
        },
        add_text_end = function(text, indent_level = 0L) {
          err_h <- function(msg)
            stop(paste("Error while calling `add_text_end()`:", msg), call. = FALSE)
          if (!is.null(text) && !is.character(text))
            err_h("Argument `text` is not a character object.")
          if (is.null(indent_level))
            indent_level <- 0L
          if (!is.numeric(indent_level) || length(indent_level) != 1 ||
              !is.finite(indent_level) || indent_level < 0 ||
              indent_level != as.integer(indent_level)
          )
            err_h("Argument `indent_level` is not a positive integer.")
          text_stack_end <<- c(
            paste0(
              paste(rep(indent, indent_level), collapse = ""),
              before,
              text,
              after
            ),
            text_stack_end
          )
        },
        build_text = function() {
          paste(c(text_stack, text_stack_end), collapse = sep)
        }
      )
    },
    vars = c("sep", "indent", "before", "after"),
    parent_env = "funky"
  )
}