#' Define the HTML specific properties of a [StyledTable][styled_table()] class object
#' 
#' The following functions allow to configure the **html** output behaviour of
#' a [StyledTable][styled_table()] class object:
#' - `set_html_table_id()` or `remove_html_table_id()`: Assign or remove an
#'   html `id` to the surrounding `<table>` tag
#' - `set_html_table_class()` or `remove_html_table_class()`: Append or remove an
#'   html `class` name to the surrounding `<table>` tag
#' - `set_html_tr_class()` or `remove_html_tr_class()`: Append or remove an
#'   html `class` name to the `<tr>` tag of one ore more table rows
#' - `set_html_td_class()` or `remove_html_td_class()`: Append or remove an
#'   html `class` name to one or more table cells (`<td>` or `<th>` tags)
#' - `set_html_td_id()`: Assign an html `id` to one ore more table cells (`<td>` or `<th>` tags)
#' - `set_html_colheader()`: Define one or more table rows as header rows, holding
#'   the headings of the columns
#' - `set_html_rowheader()`: Define one or more table columns as header columns, holding
#'   additional headings for the rows
#' - `set_html_subheading()`: Define one or more table rows as subheading rows, grouping
#'   the html table into smaller tables
#' - `set_html_pre_process()`: Add an html specific pre-processing function to
#'   one or more table cells. This functions will modify the cell values,
#'   when [create_html()] is called.
#' - `apply_html_theme1()`: Assign the html class `styledtable-theme1` to the `<table>` tag.
#'   This will ensure that the styling defined in
#'   `styledTables/inst/stylesheets/sass_files/styledtable_theme1.sass` is used.
#'   If you want to create your own html stylings, then you should have a look
#'   at this sass file and create your own stylesheets (**sass** or **css**).
#'   Note that you can either include your own stylesheets in the
#'   `css` field in the output-format section of the **yaml header** of your
#'   **rmd** file (**sass** and **css** files are both possible) or
#'   you can add your stylesheet as **html dependency** by calling 
#'   [add_html_stylesheet()]
#' - `add_html_dependency()`: Adds an **html dependency** (stylesheet, javascript etc.)
#'   to your styledTable object. When the table is [printed][knitr::knit()], then these
#'   dependencies will automatically be included in the resulting **html** header.
#' - `add_html_stylesheet()`: A wrapper for `add_html_dependency()` for
#'   adding **css** (holding costom html stylings for your tables) to the
#'   dependencies list of the [StyledTable][styled_table()] class
#'   object.
#' @param st A [StyledTable][styled_table()] class object
#' @param col_id A numeric vector holding the ids of the specified table columns.
#' @rdname set_html
#' @export
set_html_rowheader <- function(st, col_id) {
  st@html$rowheader_col_id <- col_id
  st
}

#' @param row_id A numeric vector holding the ids of the specified table rows.
#' @export
#' @rdname set_html
set_html_colheader <- function(st, row_id) {
  st@html$colheader_row_id <- row_id
  st
}

#' @export
#' @rdname set_html
set_html_subheading <- function(st, row_id) {
  st@html$subheading_row_id <- row_id
  st
}

#' @param class A character vector holding the **html class names**,
#'   which should be added to the specific dom element
#' @export
#' @rdname set_html
set_html_table_class <- function(st, class, replace_mode = "append") {
  if (isTRUE(replace_mode == "append")) {
    st@html$table_class <- c(st@html$table_class, class)
  } else if (isTRUE(replace_mode == "replace")) { 
    st@html$table_class <- class
  }
  st
}

#' @export
#' @rdname set_html
remove_html_table_class <- function(st, class) {
  st@html$table_class <- setdiff(st@html$table_class, class)
  st
}

#' @param replace_mode One of the following strings:
#'   - `"append"`: Append the value to already assigned values.
#'   - `"replace"`: Replace already assigned values by the current value.
#' @param id A character vector holding one or more **html ids** which should be
#'   assigend to the specific dom element.
#' @export
#' @rdname set_html
set_html_table_id <- function(st, id, replace_mode = "append") {
  if (isTRUE(replace_mode == "append")) {
    st@html$table_id <- c(st@html$table_id, id)
  } else if (isTRUE(replace_mode == "replace")) { 
    st@html$table_id <- id
  }
  st
}

#' @export
#' @rdname set_html
remove_html_table_id <- function(st, id) {
  st@html$table_id <- setdiff(st@html$table_id, id)
  st
}

#' @export
#' @rdname set_html
set_html_tr_class <- function(st, class, row_id = NULL, replace_mode = "append") {
  if (is.null(row_id))
    row_id <- seq_len(count_rows(st))
  if (isTRUE(replace_mode == "append")) {
    st@html$tr_class$row_id <- c(st@html$tr_class$row_id, row_id)
    st@html$tr_class$row_class <- c(st@html$tr_class$row_class, rep(class, length(row_id)))
  } else if (isTRUE(replace_mode == "replace")) {
    id_keep <- which(!st@html$tr_class$row_id %in% row_id)
    st@html$tr_class$row_id <- c(st@html$tr_class$row_id[id_keep], row_id)
    st@html$tr_class$row_class <- c(st@html$tr_class$row_class[id_keep], rep(class, length(row_id)))
  }
  st
}

#' @export
#' @rdname set_html
remove_html_tr_class <- function(st, class, row_id = NULL) {
  if (is.null(row_id))
    row_id <- seq_len(count_rows(st))
  for (i in row_id){
    id <- which(st@html$tr_class$row_id == i)
    st@html$tr_class$row_class <- setdiff(st@html$tr_class$row_class[i], class)
  }
  st
}

#' @export
#' @rdname set_html
set_html_td_class <- function(st, class, row_id = NULL, col_id = NULL, replace_mode = "append") {
  if (is.null(row_id))
    row_id <- seq_len(count_rows(st))
  if (is.null(col_id))
    col_id <- seq_len(count_cols(st))
  for (i in row_id) {
    for (j in col_id) {
      if (is.null(st@styles[[i]][[j]])) {
        st@styles[[i]][[j]] <- setStyledCell(new("StyledCell"), class, "html_class")
      }
      if (isTRUE(replace_mode == "append")) {
        st@styles[[i]][[j]]@html_class <- c(st@styles[[i]][[j]]@html_class, class)
      } else if (isTRUE(replace_mode == "replace")) {
        st@styles[[i]][[j]]@html_class <- class
      }
      
    }
  }
  st
}

#' @export
#' @rdname set_html
set_html_td_id <- function(st, id, row_id = NULL, col_id = NULL, replace_mode = "append") {
  if (is.null(row_id))
    row_id <- seq_len(count_rows(st))
  if (is.null(col_id))
    col_id <- seq_len(count_cols(st))
  for (i in row_id) {
    for (j in col_id) {
      if (is.null(st@styles[[i]][[j]])) {
        st@styles[[i]][[j]] <- setStyledCell(new("StyledCell"), id, "html_id")
      }
      if (isTRUE(replace_mode == "append")) {
        st@styles[[i]][[j]]@html_id <- c(st@styles[[i]][[j]]@html_id, id)
      } else if (isTRUE(replace_mode == "replace")) {
        st@styles[[i]][[j]]@html_id <- id
      }
      
    }
  }
  st
}

#' @export
#' @rdname set_html
remove_html_td_class <- function(st, class, row_id = NULL, col_id = NULL) {
  if (is.null(row_id))
    row_id <- seq_len(count_rows(st))
  if (is.null(col_id))
    col_id <- seq_len(count_cols(st))
  for (i in row_id) {
    for (j in col_id) {
      st@styles[[i]][[j]]@html_class <- setdiff(st@styles[[i]][[j]]@html_class, class)
    }
  }
  st
}

#' @param fn A function, which takes a single argument (`function(x) {...}`).
#'   If [create_html()] is called, then this function will be applied to the
#'   value of each cell which is included in `col_id` and `row_id`.
#' @export
#' @rdname set_html
set_html_pre_process <- function(st, fn, row_id = NULL, col_id = NULL, replace_mode = "append") {
  # TODO: differentiate between append_inside and append_outside
  if (is.null(row_id))
    row_id <- seq_len(count_rows(st))
  if (is.null(col_id))
    col_id <- seq_len(count_cols(st))
  for (i in row_id) {
    for (j in col_id) {
      if (isTRUE(replace_mode == "append")) {
        fn_old <- st@styles[[i]][[j]]@html_pre_process
        st@styles[[i]][[j]]@html_pre_process <- funky::restrict_fn_env(
          fn = function(x)
            fn(fn_old(x)),
          vars = list(
            fn_old = fn_old,
            fn = fn
          ),
          parent_env = "styledTables"
        )
      } else if (isTRUE(replace_mode == "replace")) {
        st@styles[[i]][[j]]@html_pre_process <- fn
      }
    }
  }
  st
}

random_name <- function(len = 10) {
  paste(sample(c(LETTERS, letters), len, replace = TRUE), collapse = "")
}

#' @param file_path The path to the **css** file holding the table styling.
#' @param name A string holding the library name (has no effect on the output).
#' @param version A string holding the version of the stylesheet (has no effect on the output)
#' @export
#' @rdname set_html
add_html_stylesheet <- function(st, file_path, name = random_name(), version = "0.0.0") {
  err_h <- function(msg)
    stop(paste("Error while calling `add_html_stylesheet()`:", msg), call. = FALSE)
  if (grepl("\\.s(a|c)ss$", file_path))
    err_h("Only `css` files are allowed. Please transpile `sass` or `scss` files to `css` first.")
  if (!grepl("\\.css$", file_path))
    err_h("Please supply a valid `css` file.")
  add_html_dependency(
    st,
    dep = htmltools::htmlDependency(
      name = name,
      version = version,
      src = dirname(file_path),
      stylesheet = basename(file_path)
    )
  )
}

#' @param file_path The path to the **css** file holding the table styling.
#' @param dep An **html dependecy** created by [htmltools::htmlDependency()]
#' @export
#' @rdname set_html
add_html_dependency <- function(st, dep) {
  err_h <- function(msg)
    stop(paste("Error while calling `add_html_dependency()`:", msg), call. = FALSE)
  st@html_dependencies <- append(
    st@html_dependencies,
    list(dep)
  )
  st@html_dependencies <- st@html_dependencies[!duplicated(st@html_dependencies)]
  st
}

#' @export
#' @rdname set_html
apply_html_theme1 <- function(st) {
  st %>%
    add_html_stylesheet(
      file_path = system.file(
        "stylesheets",
        "styledtable_theme1.min.css",
        package = "styledTables"
      ),
      name = "styledtable_theme1",
      version = "1.0.0"
    ) %>%
    set_html_table_class("styledtable-theme1")
}
