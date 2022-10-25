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
#' @param values An optional named list or named character vector holding a set
#'   of styling informations for an html dom element.
#' @return A string holding the text for the html tag styling property
create_html_tag_style <- function(values = NULL) {
  create_html_tag_property(
    name = "style",
    values = sapply(
      seq_len(length(values)),
      function(i) {
        paste0(names(values)[i], ":", values[i], ";")
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
  rowheader_id <- function(i, j)
    paste0("styledtable-rowheader-id-", i, "-", j)
  subheading_id <- function(i)
    paste0("styledtable-rowheader-id-", i)
  colheader_id <- function(i, j)
    paste0("styledtable-colheader-id-", i, "-", j)
  colheader_tr_class <- "styledtable-tr-colheader"
  subheading_tr_class <- "styledtable-tr-subheading"
  body_tr_class <- "styledtable-tr-body"
  
  tb <- funky::init_text_builder(indent = "  ")
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
  tb$add_text("<tbody>", indent_level = indent_level + 1L)
  for (i in seq_len(count_rows(st))) {
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
      indent_level = indent_level + 2L
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
        tb$add_text(
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
                      colheader_id(i, j)
                    } else if (i %in% st@html$subheading_row_id) {
                      subheading_id(i)
                    } else if (j %in% st@html$rowheader_col_id) {
                      rowheader_id(i, j)
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
                        colheader_id(ch_merge$row_id[1], ch_merge$col_id[1])
                      }
                    ) %>% unlist,
                    {
                      row_id <- st@html$subheading_row_id[st@html$subheading_row_id < i]
                      if (length(row_id) > 0) {
                        subheading_id(max(row_id))
                      }
                    },
                    if (length(st@html$rowheader_col_id) > 0 && all(st@html$rowheader_col_id != j))
                      sapply(
                        st@html$rowheader_col_id,
                        function(col_id) {
                          rh_merge <- get_merged_cell(st, i, col_id, fake_single_cell = TRUE)
                          rowheader_id(rh_merge$row_id[1], rh_merge$col_id[1])
                        }
                      ) %>% unlist
                  )
                )
              }
            )
          ),
          indent_level = indent_level + 3L
        )
        tb$add_text(
          as.character(
            if (!is.null(st@styles[[i]][[j]]) && !is.null(st@styles[[i]][[j]]@html_pre_process)) {
              st@styles[[i]][[j]]@html_pre_process(st@data[[i]][[j]])
            } else {
              st@data[[i]][[j]]
            }
          ),
          indent_level = indent_level + 4L
        )
        tb$add_text(
          paste0("</", tag, ">"),
          indent_level + 3L
        )
      }
    }
    tb$add_text("</tr>", indent_level = indent_level + 2L)
  }
  tb$add_text("</tbody>", indent_level = indent_level + 1L)
  if (!is.null(footer)) {
    tb$add_text("<tfoot>", indent_level = indent_level + 1L)
    for (ftext in footer) {
      tb$add_text('<tr><td style="padding: 0" colspan="100%">', indent_level = indent_level + 2L)
      tb$add_text(ftext, indent_level = indent_level + 3L)
      tb$add_text("</td></tr>", indent_level = indent_level + 2L)
    }
    tb$add_text("</tfoot>", indent_level = indent_level + 1L)
  }
  tb$build_text()
}