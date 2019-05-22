#' Generate the LaTeX code for a StyledTable
#'
#' @name create_latex_table
#' @rdname create_latex_table-methods
#' @exportMethod create_latex_table
#' @include styled_table.R
setGeneric("create_latex_table", function(st) standardGeneric("create_latex_table"))

#' @rdname create_latex_table-methods
#' @aliases create_latex_table,StyledTable-method
#' @param st A [StyledTable] object
#' @return A character string holding the latex code for the styled table
#' @seealso [write_pdf()], [write_png()], [write_excel()], [append_latex_table()], [create_latex_table_body()]
setMethod(
    "create_latex_table",
    signature(
        st = "StyledTable"
    ),
    function(st) {
        genTable <- create_latex_table_body(st)
        paste0(
            genTable$color_definitions,
            "\n\\begin{tabular}{",
            paste0(rep("c", count_cols(st)), collapse = ""),
            "}\n",
            genTable$textBody,
            "\n\\end{tabular}\n"
        )
    }
)

#' Append the LaTeX code for a StyledTable to an existing LaTeX table code
#'
#' @name append_latex_table
#' @rdname append_latex_table-methods
#' @exportMethod append_latex_table
#' @include styled_table.R
setGeneric(
    "append_latex_table",
    function(table_text, st) standardGeneric("append_latex_table"))

#' @rdname append_latex_table-methods
#' @aliases append_latex_table,character,StyledTable-method
#' @param table_text A string containing the LaTeX code of a table
#' @param st A [StyledTable] object
#' @return A character string holding the latex code for the extended styled table
#' @seealso [write_pdf()], [write_png()], [write_excel()], [create_latex_table()], [create_latex_table_body()]
setMethod(
    "append_latex_table",
    signature(
        table_text = "character",
        st = "StyledTable"
    ),
    function(table_text = "", st) {
        if (length(table_text) < 0 || any(table_text == "")) {
            table_text <- create_latex_table(st)
        } else {
            genTable <- create_latex_table_body(st)
            textExistingTable <- gsub("\n\\\\end\\{tabular\\}\n$", "", table_text)
            table_text <- paste(
                    genTable$color_definitions,
                    textExistingTable,
                    genTable$textBody,
                    "\\end{tabular}\n",
                    sep = "\n"
                )
        }
        table_text
    }
)

#' Generate the LaTeX code for a StyledTable without begin{tabular} and end{tabular}
#'
#' @name create_latex_table_body
#' @rdname create_latex_table_body-methods
#' @exportMethod create_latex_table_body
#' @include styled_table.R
setGeneric(
    "create_latex_table_body",
    function(st) standardGeneric("create_latex_table_body")
)

#' @rdname create_latex_table_body-methods
#' @aliases create_latex_table_body,StyledTable-method
#' @param st A [StyledTable] object
#' @return A character string holding the latex code for the styled table
#' @seealso [write_pdf()], [write_png()], [write_excel()], [create_latex_table()], [append_latex_table()]
setMethod(
    "create_latex_table_body",
    signature(
        st = "StyledTable"
    ),
    function(st) {
        # In this vector the text lines are collected
        textElements <- NULL
        addTextElement <- function(x) {
            textElements <<- c(textElements, x)
        }
        nRow <- count_rows(st)
        nCol <- count_cols(st)
        # ---- hhline color definitions ----
        # hhline colors must have names and must therefore be defined
        # before the \begin{tabular}
        # we collect all color names (random name generation (hopefully no overlapping))
        # and return a "color_definitions"
        styledTableColors <- list(html = c(), name = c())
        generateHLineColor <- function(html) {
            html <- calc_hex_color(html)
            posHtml <- which(styledTableColors$html == html)
            if (length(posHtml) > 0) {
                colorName <- styledTableColors$name[posHtml[1]]
            } else {
                styledTableColors$html <<- c(styledTableColors$html, html)
                colorName <- paste0(
                        "styledTableColor",
                        intToUtf8(sample(65:90, 10))
                    )
                styledTableColors$name <<- c(styledTableColors$name, colorName)
            }
            colorName
        }
        # ---- itarate through all lines in st@data ----
        for (i in seq_len(nRow)) {
            # vector of tabular cells
            cellElements <- NULL
            addCellElement <- function(x)
                cellElements <<- c(cellElements, as.character(x))
            # Inner text of \hhline (used for BOTTOM BORDER)
            textHLine <- ""
            # Inner text of \hhline (used for TOP BORDER, but only in the first row)
            textHLineTop <- ""
            # ---- itarate through all cells in a row ----
            for (j in seq_len(nCol)) {
                style <- st@styles[[i]][[j]]
                styleBorderPosition <- getStyledCell(style, "border_position")
                styleForegroundColor <- getStyledCell(style, "fill_color")
                styleHorizontal <- getStyledCell(style, "horizontal")
                styleFontColor <- getStyledCell(style, "font_color")
                styleLatexFontSize <- getStyledCell(style, "latex_font_size")
                styleIndent <- getStyledCell(style, "indent")
                styleLatexVerticalMove <- getStyledCell(style, "latex_vertical_move")
                # Look if current cell is part of a larger merged cell
                merge <- list(row_id = c(i, i), col_id = c(j, j))
                for (m in st@merges)
                    if (between_vec(i, m$row_id) && between_vec(j, m$col_id))
                        merge <- m
                # number of merged rows
                # if multiple rows are merged => use the negative number
                # (because of the cellcoloring always the lowest cell is used)
                count_rows <- diff(merge$row_id) + 1L
                # number of merged cols
                count_cols <- diff(merge$col_id) + 1L

                # ---- text for HHLINE TOP (only first line) and BOTTOM ----
                # The TOP and BOTTOM HHLINE is done by \hhline
                # (\hhline is better than \cline since it also adds the line
                # space to the cell height (important for coloring))
                # HHLINE LEFT: Only first column
                if (j == 1 && "LEFT" %in% styleBorderPosition) {
                    textHLine <- "|"
                    if (i == 1)
                        textHLineTop <- "|"
                }
                # HHLINE TOP: Only first row
                if (i == 1) {
                    if (
                        "TOP" %in% styleBorderPosition
                    ) {
                        textHLineTop <- paste0(textHLineTop, "-")
                    } else {
                        textHLineTop <- paste0(textHLineTop, "~")
                    }
                }
                # HHLINE BOTTOM:
                # If the cell has BOTTOM BORDER or it is not a middle cell
                # in a multirow cell then draw a black border
                if (
                    (is.null(merge) || i == merge$row_id[2]) && (
                        "BOTTOM" %in% styleBorderPosition || (
                            i < nRow &&
                            "TOP" %in% getStyledCell(st@styles[[i + 1L]][[j]], "border_position")
                ))) {
                    textHLine <- paste0(textHLine, "-")
                } else {
                    # If there is no border or the cell is a multirow cell, then
                    # there are two cases:
                    #   - The upper cell is a colored cell => Use border of the same color
                    #   - The upper cell is not colored => Use no border
                    if (length(styleForegroundColor) > 0) {
                        # Colored bottom border
                        textHLine <- paste0(
                            textHLine,
                            ">{\\arrayrulecolor{",
                            generateHLineColor(styleForegroundColor),
                            "}}-",
                            ">{\\arrayrulecolor{black}}"
                        )
                    } else {
                        # No bottom border
                        textHLine <- paste0(textHLine, "~")
                    }
                }
                # HHLINE RIGHT:
                if ((j < nCol && j == merge$col_id[2] &&
                        "LEFT" %in%
                        getStyledCell(st@styles[[i]][[j + 1L]], "border_position")) ||
                        "RIGHT" %in% styleBorderPosition) {
                    textHLine <- paste0(textHLine, "|")
                    if (i == 1)
                        textHLineTop <- paste0(textHLineTop, "|")
                }

                # ---- put together the alignment + border + cell value ----
                # only for the first cell of a multicolumn cell
                if (j == merge$col_id[1]) {
                    # cell style
                    # VERTICAL ALIGNMENT IS NOT WORKING IN LATEX
                    # USE latex_vertical_move instead
#                     # vertical alignment and linebreaks parsing
#                     textVAlignmentMultirow <- ""
#                     if (length(getStyledCell(style, "vertical")) > 0)
#                         textVAlignmentMultirow <- switch(getStyledCell(style, "vertical"),
#                                 VERTICAL_TOP = "t",
#                                 VERTICAL_CENTER = "c",
#                                 VERTICAL_BOTTOM = "b"
#                             )
                    # horizontal alignment
                    hAlignment <- "l"
                    if (length(styleHorizontal) > 0)
                        hAlignment <- switch(styleHorizontal,
                                ALIGN_LEFT = "l",
                                ALIGN_CENTER = "c",
                                ALIGN_RIGHT = "r"
                            )
                    # is the cell width of this cell fully given?
                    mergeCols <- merge$col_id[1]:merge$col_id[2]
                    fixedCellWidth <- all(mergeCols %in% st@latex_col_width$col_id)
                    # if the cell width is fixed
                    # then use "p{width}" alignment instead of "l","c" and "r"
                    if (fixedCellWidth) {
                        # calculate cell width
                        cellWidth <- as.character(sum(
                            st@latex_col_width$widths[
                                st@latex_col_width$col_id %in% mergeCols
                            ]
                        ) / 2)
                        # create cell width text
                        cellWidth <- paste0(
                            "\\dimexpr ",
                            cellWidth,
                            "em"
                        )
                        # if cell is a multicolumn cell then remove space for
                        # column seperation
                        if (diff(merge$col_id) > 0)
                            cellWidth <- paste0(
                                cellWidth,
                                as.character(- 2 * diff(merge$col_id)),
                                "\\tabcolsep"
                            )
                        # Width of the \multirow column
                        textMultirowWidth <- cellWidth
                        # For colored multirow cells \pbox is used
                        textPBoxWidth <- cellWidth
                        # Multicolumn Alignment is not important
                        textHAlignmentMulticol <- "c"
                        # Width of inner tabular (for linebreaks)
                        textHAlignmentTabular <- paste0("p{", cellWidth, "}")
                        # Inner of multirow behaves like a parbox
                        # => horizontal alignment is done with ragged2e
                        textHAlignmentMultirowInner <- switch(
                                hAlignment,
                                l = "\\raggedright ",
                                c = "\\centering ",
                                r = "\\raggedleft "
                            )
                    } else {
                        # Width of the \multirow column
                        textMultirowWidth <- "*"
                        # Width of inner tabular (for linebreaks)
                        textHAlignmentTabular <- hAlignment
                        # Alignment is done by \multicol
                        textHAlignmentMulticol <- hAlignment
                        # No inner alignment commands needed
                        textHAlignmentMultirowInner <- ""
                    }

                    # Add BORDER LEFT and RIGHT to textHAlignmentMulticol
                    # For the first cell in the row add border LEFT, if needed
                    if (j == 1 && "LEFT" %in% styleBorderPosition) {
                        textHAlignmentMulticol <- paste0("|", textHAlignmentMulticol)
                    }
                    # For all other cells only use border RIGHT, if needed
                    if ((j < nCol && j == merge$col_id[2] &&
                            "LEFT" %in%
                            getStyledCell(st@styles[[i]][[j + 1L]], "border_position")) ||
                            "RIGHT" %in% styleBorderPosition) {
                        textHAlignmentMulticol <- paste0(textHAlignmentMulticol, "|")
                    }

                    # Cell coloring
                    # If the cell content has fixed line breaks (\n) then
                    # use a tabular for the fixed line breaks
                    # But if the cell is a colored multirow cell
                    # then you have to use a \pbox (tabular is not positioned
                    # properly)
                    textCellColor <- ""
                    coloredMultirowCell <- FALSE
                    textMultirowNRows <- as.character(count_rows)
                    valRow <- merge$row_id[1]
                    if (length(styleForegroundColor) > 0) {
                        textCellColor <- paste0(
                            "\\cellcolor[HTML]{",
                            calc_hex_color(styleForegroundColor),
                            "}"
                        )
                        # If the colored cell is a multirow cell then
                        # the value is not written in the first cell of the
                        # merged cell, but in the last and the rowcount is negative
                        # (e.g \multirow{-2})
                        if (count_rows > 1) {
                            textMultirowNRows <- as.character(-count_rows)
                            coloredMultirowCell <- TRUE
                            valRow <- merge$row_id[2]
                        }
                    }

                    # if the cell is not a multirow-merged-cell or it is the
                    # first cell in a multirow-merged-cell, then the value
                    # will be inserted
                    if (i == valRow) {
                        # cell value
                        val <- st@data[[i]][[j]]
                        # pre process cell value if necessary
                        preProcess <- getStyledCell(style, "latex_pre_process")
                        tryCatch({
                                val <- preProcess(val)
                            },
                            error = function(description) {
                                stop(paste0("Error in 'create_latex_table_body' ",
                                        "while evaluating the function given ",
                                        "in 'set_latex_pre_process' on cell value (",
                                        "row: ", i,
                                        "col: ", j,
                                        "value:", as.character(val),
                                        "). Check the function definition. ",
                                        "Details: ", as.character(description)
                                    ), call. = FALSE)

                            })
                        # Split cell value into pieces if there are line breaks
                        val <- strsplit(as.character(val), "\\n")[[1]]
                        # bold
                        if (any(getStyledCell(style, "bold")))
                            val <- paste0("\\textbf{",val,"}")
                        # italic
                        if (any(getStyledCell(style, "italic")))
                            val <- paste0("\\textit{",val,"}")
                        # font color
                        if (length(styleFontColor) > 0)
                            val <- paste0(
                                    "\\textcolor[HTML]{",
                                    calc_hex_color(styleFontColor),
                                    "}{", val, "}"
                                )
                        # font size
                        if (length(styleLatexFontSize) > 0)
                            val <- paste0(
                                    "{",
                                    styleLatexFontSize,
                                    " ",
                                    val,
                                    "}"
                                )
                        # Indentation of cell contents
                        if (length(styleIndent) > 0)
                            val <- paste0(
                                    "\\hspace{",
                                    as.character(styleIndent),
                                    "em}",
                                    val
                                )

                        # If the cell content has fixed line breaks (\n) then
                        # use a tabular for the fixed line breaks
                        # But if the cell is a colored multirow cell
                        # then you have to use a \pbox (tabular is not positioned
                        # properly)
                        if (length(val) > 1) {
                            if (coloredMultirowCell && fixedCellWidth) {
                                # Colored multirow cell with linebreaks
                                val <- paste0(
                                        textHAlignmentMultirowInner,
                                        "\n\t\t\t\\pbox{",
                                        textPBoxWidth,
                                        "}{\\relax\\ifvmode",
                                        textHAlignmentMultirowInner,
                                        "\\fi\n\t\t\t\t",
                                        paste0(
                                            val,
                                            collapse = "\\\\\n\t\t\t\t"
                                        ),
                                        "\n\t\t\t}"
                                    )
                            } else {
                                # Non colored (multirow) cell with linebreaks
                                val <- paste0(
                                        "\n\t\t\t\\begin{tabular}{@{}",
                                        textHAlignmentTabular,
                                        "@{}}\n\t\t\t\t",
                                        paste0(
                                            paste(
                                                textHAlignmentMultirowInner,
                                                val
                                            ),
                                            collapse = "\\\\\n\t\t\t\t"
                                        ),
                                        "\n\t\t\t\\end{tabular}"
                                    )
                            }
                        }
                        # Add multirow command
                        textMultirowVerticalMove <- "\\dimexpr -1px"
                        if (length(styleLatexVerticalMove) > 0)
                            textMultirowVerticalMove <- paste(
                                textMultirowVerticalMove,
                                styleLatexVerticalMove
                            )
                        textMultirowVerticalMove <- paste0(
                            "[",
                            textMultirowVerticalMove,
                            "]"
                        )
                        if (min(merge$row_id) != max(merge$row_id)) {
                            val <- paste0(
                                    "\\multirow{",
                                    textMultirowNRows,
                                    "}{",
                                    textMultirowWidth,
                                    "}",
                                    textMultirowVerticalMove,
                                    "{",
                                    textCellColor,
                                    textHAlignmentMultirowInner,
                                    val,
                                    "}"
                                )
                        } else {
                            if (fixedCellWidth) {
                                val <- paste0(
                                        textHAlignmentMultirowInner,
                                        "\\parbox{",
                                        textMultirowWidth,
                                        "}{\\strut",
                                        textCellColor,
                                        textHAlignmentMultirowInner,
                                        val,
                                        "\\strut}"
                                    )
                            } else {
                                val <- paste0(
                                        textCellColor,
                                        "\\begin{tabular}{",
                                        hAlignment,
                                        "}",
                                        val,
                                        "\\end{tabular}"
                                    )
                            }
                        }
                    } else {
                        # if the current position is not the first row of a merged
                        # cell, the value is not used
                        val <- textCellColor
                    }
                    # wrap the value with the multicolumn command
                    # (if no multicolumn, then it is \multicolumn{1}{...})
                    addCellElement(paste0(
                        "\\multicolumn{",
                        count_cols,
                        "}{",
                        textHAlignmentMulticol,
                        "}{",
                        val,
                        "}"
                    ))
                }
            }
            # append TOP HHLINE commands
            if (i == 1 && textHLineTop != "")
                addTextElement(paste0("\\hhline{", textHLineTop, "}"))
            # append all cell texts and newline command
            addTextElement(paste0(
                paste(cellElements, collapse = " &\n\t\t"),  # current row
                "\\\\"  # newline command
            ))
            # append BOTTOM HHLINE commands
            if (textHLine != "")
                addTextElement(paste0("\\hhline{", textHLine, "}"))
        }

        # ---- create color definition string that should be placed before the table
        if (length(styledTableColors$name) > 0) {
            color_definitions <- paste0(
                paste0(
                    "\\definecolor{",
                    styledTableColors$name,
                    "}{HTML}{",
                    styledTableColors$html,
                    "}"
                ),
                collapse = "\n"
            )
        } else {
            color_definitions <- ""
        }

        # ---- put together all text elements ----
        list(
            textBody = paste0(
                "\t",
                paste0(
                    textElements,
                    collapse = "\n\t"
                )
            ),
            color_definitions = color_definitions
        )
    }
)
