#' @include utilities.R
NULL

#' Create a string defining a property of an html tag
#'
#' @param name Name of the property
#' @param values A character vector holding a set of values which should be assigned
#'   to the property.
#' @return A string holding the text for the html tag property.
create_html_tag_property <- function(name, values = NULL) {
  if (length(values) == 0)
    return(NULL)
  paste0(
    name,
    '="',
    paste(values, collapse = " "),
    '"'
  )
}

#' Create a string defining the style property of an html tag
#' 
#' @param styles An character vector holding the names of the style properties for an html dom element.
#' @param values A vector of same length as `styles` holding the corresponding
#'   values.
#' @return A string holding the text for the html tag styling property
create_html_tag_style <- function(styles, values = NULL) {
  create_html_tag_property(
    name = "style",
    values = sapply(
      seq_len(length(values)),
      function(i) {
        paste0(styles[i], ":", values[i], ";")
      }
    )
  )
}

#' Create the text for an html tag opening
#'
#' @param tag String holding the tag name
#' @param properties A character vector holding the tag properties. Each
#'   entry could be created by [create_html_tag_property()].
#' @param close An optional logical flag, defining if the html tag should be 
#'   immediately be closed (eg. `<tag/>`).
#' @return A string holding the text for the html opening tag.
create_html_tag <- function(tag, properties, close = FALSE) {
  paste0(
    paste(
      c(
        paste0("<", tag),
        properties
      ),
      collapse = " "
    ),
    if (isTRUE(close))
      "/",
    ">"
  )
}

#' Create the **html** code of a **styledTable**
#' 
#' @param st A [StyledTable][styled_table()] class object
#' @param caption An optional string holding the table caption
#' @param footer An optional character vector holding the table footer remarks
#' @param indent_level An optional integer defining, the indentation level of
#'   the resulting html code
#' @param use_scope An optional flag, defining if the html table should
#'   also use the `scope` property. This property is an additional method
#'   for improving the accessibility of an html table. 
#'   By default this is turned off, since the usage of `scope` does not allow
#'   **sub headings** and accessibility is already ensured by assigning
#'   unique header ids to the `id` property of the header cells and
#'   linking data cells via their `headers` property to the corresponding header
#'   cells.
#' @return A string holding the entire **html** code of the table.
#' @export
create_html <- function(st, caption = NULL, footer = NULL, indent_level = 0L, use_scope = TRUE) {
  hash <- sample(c(letters, LETTERS, 0:9), 4, replace = TRUE) %>%
    paste(collapse = "")
  tb <- init_text_builder(indent = "  ")
  tb$add_text(
    create_html_tag(
      tag = "table",
      properties = c(
        create_html_tag_property("class", c("styledtable-table", st@html$table_class)),
        create_html_tag_property("id", st@html$table_id)
      )
    ),
    indent_level = indent_level
  )
  tb$add_text_end("</table>", indent_level = indent_level)
  if (!is.null(caption))
    tb$add_text(paste0("<caption>", caption, "</caption>"), indent_level = indent_level + 1L)
  tb$add_text("<colgroup>", indent_level = indent_level + 1L)
  for (j in seq_len(count_cols(st))) {
    if (!is.null(st@html_col_width) && !is.null(st@html_col_width$col_id)) {
      width <- st@html_col_width$width[match(j, st@html_col_width$col_id)]
      if (all(is.na(width)))
        width <- NULL
    } else {
      width <- NULL
    }
    tb$add_text(
      create_html_tag(
        tag = "col",
        properties = if (!is.null(width)) create_html_tag_style("width", width)
      ), indent_level = indent_level + 2L)
  }
  tb$add_text("</colgroup>", indent_level = indent_level + 1L)
  num_thead_rows <- 0L
  while((num_thead_rows + 1) %in% st@html$colheader_row_id)
    num_thead_rows <- num_thead_rows + 1L
  if (num_thead_rows > 0)
    add_html_part(
      st,
      tb = tb,
      rows = 1:num_thead_rows,
      tag_part = "thead",
      hash = hash,
      indent_level = indent_level + 1L
    )
  if (num_thead_rows < count_rows(st))
    add_html_part(
      st,
      tb = tb,
      rows = (num_thead_rows+1):count_rows(st),
      tag_part = "tbody",
      hash = hash,
      indent_level = indent_level + 1L
    )
  if (!is.null(footer)) {
    tb$add_text("<tfoot>", indent_level = indent_level + 1L)
    for (ftext in footer) {
      tb$add_text(
        paste0('<tr><td colspan="', count_cols(st) ,'">'),
        indent_level = indent_level + 2L
      )
      tb$add_text(ftext, indent_level = indent_level + 3L)
      tb$add_text("</td></tr>", indent_level = indent_level + 2L)
    }
    tb$add_text("</tfoot>", indent_level = indent_level + 1L)
  }
  tb$build_text()
}

#' Add html text of table part
#' 
#' @inheritParams create_html
#' @param tb A text builder
#' @param rows The rows of the `st` which should be added
#' @param tag_part A string holding the html tag of the table part.
#'   Usually either `"thead"` or `"tbody"`.
#' @param hash A string holding the (almost) unique hash value for the table.
add_html_part <- function(st, tb, rows, tag_part, hash, indent_level) {
  colheader_tr_class <- "styledtable-tr-colheader"
  subheading_tr_class <- "styledtable-tr-subheading"
  body_tr_class <- "styledtable-tr-body"
  tb$add_text(paste0("<", tag_part, ">"), indent_level = indent_level)
  for (i in rows) {
    tb$add_text(
      create_html_tag(
        tag = "tr",
        properties = c(
          create_html_tag_property(
            "class",
            {
              tr_class_id <- which(st@html$tr_class$row_id == i)
              c(
                if (i %in% st@html$colheader_row_id) {
                  colheader_tr_class
                } else if (i %in% st@html$subheading_row_id) {
                  subheading_tr_class
                } else {
                  body_tr_class
                },
                if (length(tr_class_id == 1)) {
                  st@html$tr_class$row_class[tr_class_id]
                }
              )
            }
          )
        )
      ),
      indent_level = indent_level + 1L
    )
    for (j in seq_len(count_cols(st))) {
      curr_merge <- get_merged_cell(st, i, j)
      if (is.null(curr_merge) || (i == curr_merge$row_id[1] && j == curr_merge$col_id[1])) {
        class <- NULL
        flag_header <- i %in% c(st@html$colheader_row_id, st@html$subheading_row_id) ||
          j %in% st@html$rowheader_col_id
        if (flag_header) {
          tag <- "th"
        } else {
          tag <- "td"
        }
        if (j == 1 && !is.null(st@html_row_height) && !is.null(st@html_row_height$row_id)) {
          height <- st@html_row_height$height[match(i, st@html_row_height$row_id)]
          if (all(is.na(height)))
            height <- NULL
        } else {
          height <- NULL
        }
        tb$add_text(
          paste0(
            create_html_tag(
              tag = tag,
              properties = c(
                create_html_tag_property(
                  name = "rowspan",
                  values = if (!is.null(curr_merge) && diff(curr_merge$row_id) > 0)
                    diff(curr_merge$row_id) + 1L
                ),
                create_html_tag_property(
                  name = "colspan",
                  values = if (!is.null(curr_merge) && diff(curr_merge$col_id) > 0)
                    diff(curr_merge$col_id) + 1L
                ),
                create_html_tag_property(
                  name = "id",
                  values = c(
                    if (!is.null(st@styles[[i]][[j]]))
                      st@styles[[i]][[j]]@html_id,
                    if (flag_header) {
                      if (i %in% st@html$colheader_row_id) {
                        colheader_id(i = i, j = j, hash = hash)
                      } else if (i %in% st@html$subheading_row_id) {
                        subheading_id(i = i, hash = hash)
                      } else if (j %in% st@html$rowheader_col_id) {
                        rowheader_id(i = i, j = j, hash = hash)
                      }
                    }
                  )
                ),
                create_html_tag_property(
                  name = "class",
                  values = {
                    cs <- st@styles[[i]][[j]]
                    if (!is.null(cs))
                      c(
                        cs@html_class,
                        if (isTRUE(cs@bold))
                          "styledtable-td-bold",
                        if (isTRUE(cs@horizontal == "ALIGN_LEFT"))
                          "styledtable-td-textalign-left",
                        if (isTRUE(cs@horizontal == "ALIGN_CENTER"))
                          "styledtable-td-textalign-center",
                        if (isTRUE(cs@horizontal == "ALIGN_RIGHT"))
                          "styledtable-td-textalign-right",
                        if (length(cs@indent) == 1)
                          paste0("styledtable-td-indent-", gsub("\\.", "-", as.character(cs@indent)))
                      )
                  }
                ),
                if (flag_header) {
                  create_html_tag_property(
                    name = "scope",
                    values = {
                      if (i %in% c(st@html$colheader_row_id, st@html$subheading_row_id)) {
                        if (!is.null(curr_merge) && diff(curr_merge$col_id) > 0) {
                          "colgroup"
                        } else {
                          "col"
                        }
                      } else {
                        if (!is.null(curr_merge) && diff(curr_merge$row_id) > 0) {
                          "rowgroup"
                        } else {
                          "row"
                        }
                      }
                    }
                  )
                },
                if (!i %in% c(st@html$colheader_row_id, st@html$subheading_row_id) &&
                    (
                      any(c(st@html$colheader_row_id, st@html$subheading_row_id) < i) ||
                        (length(st@html$rowheader_col_id) > 0 && all(st@html$rowheader_col_id != j))
                    )
                ) {
                  create_html_tag_property(
                    name = "headers",
                    values = c(
                      sapply(
                        st@html$colheader_row_id[st@html$colheader_row_id < i],
                        function(row_id) {
                          ch_merge <- get_merged_cell(st, row_id, j, fake_single_cell = TRUE)
                          colheader_id(i = ch_merge$row_id[1], j = ch_merge$col_id[1], hash = hash)
                        }
                      ) %>% unlist,
                      {
                        row_id <- st@html$subheading_row_id[st@html$subheading_row_id < i]
                        if (length(row_id) > 0) {
                          subheading_id(i = max(row_id), hash = hash)
                        }
                      },
                      if (length(st@html$rowheader_col_id) > 0 && all(st@html$rowheader_col_id != j))
                        sapply(
                          st@html$rowheader_col_id,
                          function(col_id) {
                            rh_merge <- get_merged_cell(st, i, col_id, fake_single_cell = TRUE)
                            rowheader_id(i = rh_merge$row_id[1], j = rh_merge$col_id[1], hash = hash)
                          }
                        ) %>% unlist
                    ) %>% unique
                  )
                },
                if (!is.null(height)) create_html_tag_style("height", height)
              )
            ),
            as.character(
              if (!is.null(st@styles[[i]][[j]]) && !is.null(st@styles[[i]][[j]]@html_pre_process)) {
                st@styles[[i]][[j]]@html_pre_process(st@data[[i]][[j]])
              } else {
                st@data[[i]][[j]]
              }
            ),
            paste0("</", tag, ">")
          ),
          indent_level = indent_level + 2L
        )
      }
    }
    tb$add_text("</tr>", indent_level = indent_level + 1L)
  }
  tb$add_text(paste0("</", tag_part, ">"), indent_level = indent_level)
}

#' Define cell ids for accessibility
#' 
#' @param i Row number
#' @param j Column number
#' @inheritParams add_html_part
#' @return A string holding the cell-id
#' @rdname cellids
rowheader_id <- function(i, j, hash)
  paste(c("styledtable-rowheader-id", i, j, hash), collapse = "-")

#' @rdname cellids
subheading_id <- function(i, hash)
  paste(c("styledtable-rowheader-id", i, hash), collapse = "-")

#' @rdname cellids
colheader_id <- function(i, j, hash)
  paste(c("styledtable-colheader-id", i, j, hash), collapse = "-")