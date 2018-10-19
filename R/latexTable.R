#' Generate the LaTeX code for a StyledTable
#'
#' @name createLatexTable
#' @rdname createLatexTable-methods
#' @exportMethod createLatexTable
#' @include StyledTable.R
setGeneric("createLatexTable", function(object) standardGeneric("createLatexTable"))

#' @rdname createLatexTable-methods
#' @aliases createLatexTable,StyledTable-method
#' @param object A StyledTable
setMethod(
    "createLatexTable",
    signature(
        object = "StyledTable"
    ),
    function(object) {
        genTable <- createLatexTableBody(object)
        paste0(
            genTable$textColorDefinitions,
            "\n\\begin{tabular}{", 
            paste0(rep("c", nCols(object)), collapse = ""), 
            "}\n",
            genTable$textBody,
            "\n\\end{tabular}\n"
        )
    }
)

#' Append the LaTeX code for a StyledTable to an existing LaTeX table code
#'
#' @name appendLatexTable
#' @rdname appendLatexTable-methods
#' @exportMethod appendLatexTable
#' @include StyledTable.R
setGeneric(
    "appendLatexTable", 
    function(tableText, object) standardGeneric("appendLatexTable"))

#' @rdname appendLatexTable-methods
#' @aliases appendLatexTable,character,StyledTable-method
#' @param tableText A string containing the LaTeX code of a table
#' @param object A StyledTable
setMethod(
    "appendLatexTable",
    signature(
        tableText = "character",
        object = "StyledTable"
    ),
    function(tableText = "", object) {
        if (length(tableText) < 0 || any(tableText == "")) {
            tableText <- createLatexTable(object) 
        } else {
            genTable <- createLatexTableBody(object)
            textExistingTable <- gsub("\n\\\\end\\{tabular\\}\n$", "", tableText)
            tableText <- paste(
                    genTable$textColorDefinitions,
                    textExistingTable,
                    genTable$textBody,
                    "\\end{tabular}\n", 
                    sep = "\n"
                )
        }
        tableText
    }
)

#' Generate the LaTeX code for a StyledTable without \\begin{tabular} and \\end{tabular}
#'
#' @name createLatexTableBody
#' @rdname createLatexTableBody-methods
#' @exportMethod createLatexTableBody
#' @include StyledTable.R
setGeneric(
    "createLatexTableBody", 
    function(object) standardGeneric("createLatexTableBody")
)

#' @rdname createLatexTableBody-methods
#' @aliases createLatexTableBody,StyledTable-method
#' @param object A StyledTable
setMethod(
    "createLatexTableBody",
    signature(
        object = "StyledTable"
    ),
    function(object) {
        # In this vector the text lines are collected
        textElements <- NULL
        addTextElement <- function(x) {
            textElements <<- c(textElements, x)
        }
        nRow <- nRows(object)
        nCol <- nCols(object)
        # ---- hhline color definitions ----
        # hhline colors must have names and must therefore be defined
        # before the \begin{tabular} 
        # we collect all color names (random name generation (hopefully no overlapping)) 
        # and return a "textColorDefinitions"
        styledTableColors <- list(html = c(), name = c())
        generateHLineColor <- function(html) {
            html <- colorToHex(html)
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
        # ---- itarate through all lines in object@data ----
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
                style <- object@styles[[i]][[j]]
                styleBorderPosition <- getStyle(style, "borderPosition")
                styleForegroundColor <- getStyle(style, "foregroundColor")
                styleHorizontal <- getStyle(style, "horizontal")
                styleFontColor <- getStyle(style, "fontColor")
                styleLatexFontSize <- getStyle(style, "latexFontSize")
                styleIndent <- getStyle(style, "indent")
                styleLatexVerticalMove <- getStyle(style, "latexVerticalMove")
                # Look if current cell is part of a larger merged cell
                merge <- list(rows = c(i, i), cols = c(j, j))
                for (m in object@merges)
                    if (betweenVec(i, m$rows) && betweenVec(j, m$cols))
                        merge <- m
                # number of merged rows
                # if multiple rows are merged => use the negative number
                # (because of the cellcoloring always the lowest cell is used)
                nRows <- diff(merge$rows) + 1L
                # number of merged cols
                nCols <- diff(merge$cols) + 1L

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
                    (is.null(merge) || i == merge$rows[2]) && (
                        "BOTTOM" %in% styleBorderPosition || (
                            i < nRow && 
                            "TOP" %in% getStyle(object@styles[[i + 1L]][[j]], "borderPosition")
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
                if ((j < nCol && j == merge$cols[2] &&
                        "LEFT" %in% 
                        getStyle(object@styles[[i]][[j + 1L]], "borderPosition")) ||
                        "RIGHT" %in% styleBorderPosition) {
                    textHLine <- paste0(textHLine, "|") 
                    if (i == 1)
                        textHLineTop <- paste0(textHLineTop, "|") 
                }

                # ---- put together the alignment + border + cell value ----
                # only for the first cell of a multicolumn cell
                if (j == merge$cols[1]) {
                    # cell style
                    # VERTICAL ALIGNMENT IS NOT WORKING IN LATEX
                    # USE latexVerticalMove instead
#                     # vertical alignment and linebreaks parsing
#                     textVAlignmentMultirow <- ""
#                     if (length(getStyle(style, "vertical")) > 0)
#                         textVAlignmentMultirow <- switch(getStyle(style, "vertical"),
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
                    mergeCols <- merge$cols[1]:merge$cols[2]
                    fixedCellWidth <- all(mergeCols %in% object@latexColWidths$cols)
                    # if the cell width is fixed 
                    # then use "p{width}" alignment instead of "l","c" and "r"
                    if (fixedCellWidth) {
                        # calculate cell width
                        cellWidth <- as.character(sum(
                            object@latexColWidths$widths[
                                object@latexColWidths$cols %in% mergeCols
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
                        if (diff(merge$cols) > 0)
                            cellWidth <- paste0(
                                cellWidth,
                                as.character(- 2 * diff(merge$cols)),
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
                    if ((j < nCol && j == merge$cols[2] &&
                            "LEFT" %in% 
                            getStyle(object@styles[[i]][[j + 1L]], "borderPosition")) ||
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
                    textMultirowNRows <- as.character(nRows)
                    valRow <- merge$rows[1]
                    if (length(styleForegroundColor) > 0) {
                        textCellColor <- paste0(
                            "\\cellcolor[HTML]{",
                            colorToHex(styleForegroundColor), 
                            "}"
                        )
                        # If the colored cell is a multirow cell then
                        # the value is not written in the first cell of the
                        # merged cell, but in the last and the rowcount is negative
                        # (e.g \multirow{-2})
                        if (nRows > 1) {
                            textMultirowNRows <- as.character(-nRows)
                            coloredMultirowCell <- TRUE
                            valRow <- merge$rows[2]
                        }
                    }

                    # if the cell is not a multirow-merged-cell or it is the 
                    # first cell in a multirow-merged-cell, then the value
                    # will be inserted
                    if (i == valRow) {
                        # cell value
                        val <- object@data[[i]][[j]]
                        # pre process cell value if necessary
                        preProcess <- getStyle(style, "latexPreProcess")
                        tryCatch({
                                val <- preProcess(val)
                            },
                            error = function(description) {
                                stop(paste0("Error in 'createLatexTableBody' ",
                                        "while evaluating the function given ",
                                        "in 'setLatexPreProcess' on cell value (", 
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
                        if (any(getStyle(style, "isBold")))
                            val <- paste0("\\textbf{",val,"}")
                        # italic
                        if (any(getStyle(style, "isItalic")))
                            val <- paste0("\\textit{",val,"}")
                        # font color
                        if (length(styleFontColor) > 0)
                            val <- paste0(
                                    "\\textcolor[HTML]{",
                                    colorToHex(styleFontColor), 
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
                        # if the current position is not the first row of a merged
                        # cell, the value is not used
                        val <- textCellColor
                    }
                    # wrap the value with the multicolumn command
                    # (if no multicolumn, then it is \multicolumn{1}{...})
                    addCellElement(paste0(
                        "\\multicolumn{", 
                        nCols, 
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
            textColorDefinitions <- paste0(
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
            textColorDefinitions <- ""
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
            textColorDefinitions = textColorDefinitions
        )
    }
)
