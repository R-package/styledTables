#' Create a styled cross table body
#'
#' @name createStyledCrossTableBody
#' @rdname createStyledCrossTableBody-methods
#' @exportMethod createStyledCrossTableBody
#' @param ... Various arguments
setGeneric("createStyledCrossTableBody", function(data, ...) standardGeneric("createStyledCrossTableBody"))

#' Create a styled cross table body
#'
#' This function creates the styled cross table body from a given data.frame.
#' This function works best in combination with the method 'createStyledCrossTableHeader'.
#' For the the creation of the styled cross table body the columns specified in 'subTableCols' are used to separate the
#' table body into several sub tables, where each sub table has the current
#' value in the column defined by 'subTableCols' as sub heading.
#' The columns specified in 'yCols', 'xCols' and 'valueCols'
#' will be used for the cross table calculation.
#' If the table should not be a cross table, but a normal table then the 
#' arguments 'xCols' and 'valueCols should be omitted.
#' @rdname createStyledCrossTableBody-methods
#' @aliases createStyledCrossTableBody,data.frame-method
#' @param data A data.frame that should be used for the creation of the cross table.
#' @param subTableCols (optional) A vector of column names that should be used in order to separate the data.frame into several sub tables, that will be concatenated vertically. Each sub table will have a sub heading row above the sub table body. The sub heading text is just the corresponding value in the column defined by 'subTableCols'. If no sub heading should be introduced at all, the argument 'subTableCols' should be omitted. If the 'subTableCols' is vector with more than one column name, then a hirachy level is introduced for the sub headings, where the first column has the highest and the column given by the last entry in 'subTableCols' has the lowest hirachy level.
#' @param subHeadingStylings (optional) A list of styling functions. A styling function is a function that takes a styled table as its only argument and returns a styled function. The first styling function is applied to the level-1 sub heading (the sub heading defined by the first entry in 'subTableCols'), the second styling function is applied to the level-2 sub heading etc. Alternatively a single styling function can be passed into 'subHeadingStylings', in this case this styling function is applied to all sub heading levels.
#' @param bodyStyling (optional) A styling function that is applied to the generated styled body of each sub table. A styling function is a function that takes a styled table as its only argument and returns a styled function. In order to format the value columns of the resulting cross table use the argument 'valueColStylings'.
#' @param yCols A vector of column names that are unchanged by the cross table compuation. If the table should not be a cross table, but a normal table this argument can be omitted.
#' @param xCols (optional) A vector of column names that are used for column name generation by the cross table compuation. If the table should not be a cross table, but a normal table this argument can be omitted.
#' @param yColsRight A vector of column names that are unchanged by the cross table compuation. This columns are of the same kind as the columns defined by the argument \code{yCols}, but aligned to the right of the generated cross table.
#' @param valueCols (optional) A vector of column names that are used as value columns by the cross table compuation. Normally this is just one column. If  it is a vector with several column names, then the column names of these columns introduce an extra level in the cross table column hirachy.
#' @param valueColStylings (optional) A single column styling function or a list of column styling functions that has the same length as 'valueCols'. Each column styling function is applied to the corresponding value column (defined to the corresponding column defined by the 'valueCols' argument) of the resulting styled cross table. If a single column styling function is passed, then the same column styling function is applied to all value columns defined in 'valueCols'. If no value column specific styling should be applied, then the 'valueColStylings' argument should be omitted. If the resulting table should not be a cross table at all, but a normal table, then  this argument should also be omitted. A column styling function is a function of the type: function(object, cols) {...}, which returns a styled table, where 'object' is a styled table and 'cols' is a numeric vector of column numbers.
#' @param dropMissingRows (optional) If set to FALSE and the 'subTableCols' or 'yCols' are sparse factors then the missing factor levels introduce new rows filled filled with NAs. Otherwise no extra rows will be generated. Default value is 'TRUE'.
#' @param dropMissingCols (optional) If set to FALSE and the 'xCols' are sparse factors then the missing factor levels introduce extra cross table columns filled with NA's. Otherwise no extra columns will be generated. Default value is 'FALSE'.
#' @param fun.aggregate (optional) Defines the function that will be used for the cross table cell aggregation. If not defined, the default aggregation function of the 'data.table::dcast' function will be used.
#' @param fillValues (optional) A list of values that is used to fill the missing values in the \code{value columns} in the calculated cross table. The first list value is used for the first \code{value column}, the second value for the second \code{value column}. Hence, the list given in \code{fillValues} should have the same length as the \code{valueCols} and the types of the \code{fill values} should be the same as the corresponding \code{value columns} in the \code{data} data.frame. If the argument is omitted, then only \code{NA} is filled in for the missing cross table values. Caution: If an Excel file is generated from this cross table, \code{NA} values will be replaced by empty strings.
#' @examples
#' library(dplyr)
#' # prepare data set for cross table
#' studentsData <- data.frame(
#'     country = rep(c("Germany", "Austria"), each = 16),
#'     year = c(rep("2010", 8), rep("2011", 8)),
#'     subject = rep(c(rep("Mathematics", 4), rep("Statistics", 4)), 2),
#'     gender = rep(c(rep("Male", 2), rep("Female", 2)), 4),
#'     result = rep(c("positive", "negative"), 16),
#'     N = sample(1:1000, 32)
#'   ) %>%
#'     group_by(country, year, subject, gender) %>%
#'     mutate(rel = 100 * N / sum(N)) %>%
#'     ungroup
#' # setup styled header
#' sHeader <- createStyledCrossTableHeader(
#'   yColHeadings = c("Year", "Subject"),
#'   crossTableHeading = "Comparison of test results",
#'   c("Male", "Female"),
#'   c("Positive", "Negative"),
#'   c("count", "in %")
#' ) %>%
#'   formatStatHeader
#' # setup styled cross table body
#' sBody <- createStyledCrossTableBody(
#'   data = studentsData,
#'   subTableCols = "country",
#'   subHeadingStyling = function(object) setHorizontal(object, "center"),
#'   bodyStyling = function(object) setHorizontal(object, "center"),
#'   yCols = c("year", "subject"),
#'   xCols = c("gender", "result"),
#'   valueCols = c("N", "rel"),
#'   valueColStylings = list(
#'     formatStatAbsolute,
#'     formatStatRelative
#'   ),
#'   fillValues = list(0L, 0)
#' )
#' # Concat styled header and styled body into a single styled table
#' sTabel <- styledTable(
#'   sHeader,
#'   sBody
#' )
#' @export
setMethod(
    "createStyledCrossTableBody",
    signature(
        data = "data.frame"
    ),
    function(
        data,
        subTableCols = NULL,
        subHeadingStylings = NULL,
        bodyStyling = NULL,
        yCols,
        yColsRight = NULL,
        xCols = NULL,
        valueCols = NULL,
        valueColStylings = NULL,
        dropMissingRows = TRUE,
        dropMissingCols = FALSE,
        fun.aggregate = NULL,
        fillValues = NULL
    ) {
        data <- as.data.table(data)
        ncols = ncol(data)
        # variable names of data.frame
        colNames <- colnames(data)
        # variable vector for error messages
        colNamesMsg <- paste0("(", paste0(colNames, collapse = ", "),  ")")
        ### Check consistency of supplied arguments
        errHandler <- function(description) {
            stop(paste0(
                "Error in '",
                "createStyledCrossTableBody",
                "': ",
                description
            ), call. = FALSE)
        }
        # a vector of used column indices that is used in order to check
        # that non of the supplied column indices overlap
        usedColumnIndices <- NULL
        ### check subTableCols
        if (!is.null(subTableCols)) {
            if (
                !is.character(subTableCols) || 
                    any(!subTableCols %in% colNames) ||
                    length(unique(subTableCols)) != length(subTableCols)
            )
                errHandler(paste("Argument 'subTableCols' must be a subset of '", colNamesMsg, "'."))
            # if the subHeadingStylings is just a single function
            # then apply this function to all subHeadings
            if (is.function(subHeadingStylings))
                subHeadingStylings <- lapply(
                        seq_len(length(subTableCols)), 
                        function(x) subHeadingStylings
                    )
            if (
                !is.null(subHeadingStylings) && (
                    !is.list(subHeadingStylings) || 
                    length(subHeadingStylings) != length(subTableCols) ||
                    any(!unlist(lapply(subHeadingStylings, is.function)))
                )
            )
                errHandler(paste(
                    "The argument 'subHeadingStylings' must either be NULL,",
                    "a single styling function or a",
                    "numbered list of the same length as 'subTableCols' that",
                    "holds styling functions that will be",
                    "applied to the different sub heading levels of the sub tables.",
                    "Each styling function must be of the type:",
                    "function(object) {...}, where 'object' is a styled",
                    "table and the function must return a styled table."
                ))
                    
            # append the subTableCols indices to the usedColumnIndices
            usedColumnIndices <- subTableCols
        } else {
            if (!is.null(subHeadingStylings))
                errHandler(paste(
                    "The argument 'subHeadingStylings' can only be set, if",
                    "the argument 'subTableCols' is given as well."
                ))
        }
        ### check bodyStyling
        if (!is.null(bodyStyling) && (
            !is.function(bodyStyling)
            )
        )
            errHandler(paste(
                "The argument 'bodyStyling' must be NULL or a styling function",
                "of the type:",
                "function(object) {...}, where 'object' is a styled",
                "table and the function must return a styled table."
            ))
        ### check yCols
        if (
            !is.character(yCols) ||
                any(!yCols %in% colNames) ||
                length(yCols) == 0
        )
            errHandler(paste(
                "Argument 'yCols' must be a non empty subset of '", 
                colNamesMsg, 
                "'."
            ))
        # check if the vector overlaps with other given column indices
        if (any(yCols %in% usedColumnIndices))
            errHandler(paste(
                "The column names given in 'yCols' should not",
                "overlap with the column names given 'subTableCols'."
            ))
        # append the subTableCols indices to the usedColumnIndices
        usedColumnIndices <- c(usedColumnIndices, yCols)
        ### check xCols
        if (!is.null(xCols)) {
            if (
                !is.character(xCols) ||
                    any(!xCols %in% xCols) ||
                    length(unique(xCols)) != length(xCols)
            )
                errHandler(paste0(
                    "The argument 'xCols' must be vectors holding",
                    " the column names that should be used for the cross",
                    " table computation. These vectors must be a non",
                    " overlapping subset of '", colNamesMsg, "'."
                ))
            # check if the vector overlaps with other given column indices
            if (any(xCols %in% usedColumnIndices))
                errHandler(paste(
                    "The argument 'xCols' holding the column names that", 
                    "should be added to the right of the cross table.", 
                    "This set must be non overlapping with all other",
                    "column names given in 'subTableCols' and 'yCols' and 'xCols'."
                ))
            # append the subTableCols indices to the usedColumnIndices
            usedColumnIndices <- c(usedColumnIndices, xCols)
        }
        ### check yColsRight
        if (!is.null(yColsRight)) {
            if (
                !is.character(yColsRight) ||
                    any(!yColsRight %in% colNames)
            )
                errHandler(paste0(
                    "The argument 'yColsRight' holding ",
                    " the column names that should be used as extra y-columns",
                    " printed to the right of the generated cross table",
                    " must be a non overlapping subset of '", colNamesMsg, "'."
                ))
            # check if the vector overlaps with other given column indices
            if (any(yColsRight %in% xCols))
                errHandler(paste(
                    "The argument 'yColsRight' holding the column names that", 
                    "should be used for the cross table computation must be non",
                    "overlapping with all other",
                    "column names given in 'subTableCols' and 'yCols'."
                ))
            # append the subTableCols indices to the usedColumnIndices
            usedColumnIndices <- c(usedColumnIndices, xCols)
        }
        ### check valueCols
        if (!is.null(valueCols)) {
            if (
                !is.character(valueCols) ||
                    any(!valueCols %in% valueCols) ||
                    length(unique(valueCols)) != length(valueCols)
            )
                errHandler(paste0(
                    "The argument 'valueCols' which holds the column ",
                    " names that should be used as values for the cross table ",
                    "computation must be a non ",
                    "overlapping subset of '", colNamesMsg, "'."
                ))
            # check if the vector overlaps with other given column indices
            if (any(valueCols %in% usedColumnIndices))
                errHandler(paste(
                    "The argument 'valueCols' which holds the column ",
                    "names that should be used as values for the cross table",
                    "computation must be non overlapping with the column names",
                    "given in 'subTableCols', 'yCols'."
                ))
            # if the subHeadingStylings is just a single function
            # then apply this function to all subHeadings
            if (is.function(valueColStylings))
                valueColStylings <- lapply(
                        seq_len(length(valueCols)), 
                        function(x) valueColStylings 
                    )
            if (
                !is.null(valueColStylings) && (
                    !is.list(valueColStylings) || 
                    length(valueCols) != length(valueColStylings) ||
                    any(!unlist(lapply(valueColStylings, is.function)))
                )
            )
                errHandler(paste(
                    "The argument 'valueColStylings' must either be NULL,",
                    "a single column styling function or a",
                    "numbered list of the same length as 'valueCols' that",
                    "holds styling functions that should be",
                    "applied to the different value columns of the cross table.",
                    "The column styling functions have to be of the type ",
                    "function(object, cols) {...}, where 'object' is a styled",
                    "table and 'cols' is a numeric column vector and",
                    "the function must return a styled table."
                ))
            # append the subTableCols indices to the usedColumnIndices
            usedColumnIndices <- c(usedColumnIndices, valueCols)
        } else {
            if (!is.null(xCols))
                errHandler(paste0(
                        "If the 'xCols' argument is non empty, then ",
                        "the 'valueCols' argument must be passed as well."
                    ))
            if (!is.null(valueColStylings))
                errHandler(paste0(
                        "If the 'valueColStylings' argument is non empty, then ",
                        "the 'valueCols' argument must be passed as well."
                    ))
        }
        ### check fillValues
        if (!is.null(fillValues)) {
            if (is.null(valueCols))
                errHandler(paste(
                        "If the 'fillValues' argument is non empty, then",
                        "the 'valueCols' argument must be passed as well."
                    ))
            if (!is.list(fillValues) || length(fillValues) != length(valueCols))
                errHandler(paste(
                    "The argument 'fillValues' must be a list of the same",
                    "length as the argument 'valueCols'."
                ))
            for (i in seq_len(length(fillValues))) {
                if (typeof(fillValues[[i]]) != typeof(data[[valueCols[i]]]))
                    errHandler(paste0(
                        "The 'fillValues[[", i, "]]' has type '", 
                        typeof(fillValues[[i]]), "' whereas the column 'data[[\"",
                        valueCols[i] , "\"]]' has type '",
                        typeof(data[[valueCols[i]]]), "'."
                    ))
                if (length(fillValues[[i]]) != 1)
                    errHandler(paste0(
                        "The entry 'fillValues[[", i, "]]' has not length == 1."
                    ))
            }
        }

        #### Calculate cross table data
        if (length(valueCols) > 0) {
            # The data.frame has one or more value columns that should be used for cross table calculation 
            # Solution: 
            # - Transform the valueCols into indices columns 
            #    (so that all valueCols are of type integer, 
            #     this solves the problem that valueCols may be of diff. types)
            # - melt all value indeces columns
            # - dcast into a cross table
            # - replace the valueCol indices by the corresponding values
            #
            # The heading levels of the crossed table are the current heading 
            # texts of the columns
            # If dropMissingRows = FALSE, then no extra rows will be introduced
            # even if some of the subTableCols or yCols are sparse factors
            # If dropMissingCols = TRUE, then extra cross table columns will be generated
            # if the xCols are sparse factors 
            KEYCOL <- "KEYCOL"
            VALUECOL <- "VALUECOL"
            dcastFormula <- as.formula(paste0(
                    paste0(c(subTableCols, yCols), collapse = " + "), 
                    " ~ ", 
                    paste0(c(xCols, KEYCOL), collapse = " + ")
                ))
            dataCross <- melt(
                    data = copy(data)[, (valueCols) := .I], # replace the value columns by the row indices of the values
                    variable.name = KEYCOL, 
                    value.name = VALUECOL,
                    measure.vars = valueCols
                )
            args = list(
                    data = dataCross,
                    formula = dcastFormula, 
                    fun.aggregate = fun.aggregate,
                    value.var = VALUECOL, 
                    fill = NA, 
                    drop = c(dropMissingRows, dropMissingCols)
                )
            args <- args[sapply(args, function(x) !is.null(x))]
            dataCross <- do.call(
                dcast,
                args = args
            )
            # replace the value indices in the generated value columns by their original values
            valueColsCross <- setdiff(colnames(dataCross), c(subTableCols, yCols))
            for (i in seq_len(length(valueColsCross))) {
                # name of the value column in the crossTable
                vColCross <- valueColsCross[i]
                # original name of the value column before the crossTable generation
                iOrig <- ((i - 1L) %% length(valueCols)) + 1L
                vCol <- valueCols[iOrig]
                # replace the value indices by their original values
                setnames(dataCross, vColCross, "VALUEIDS_")
                if (!is.null(fillValues)) {
                    valueMap <- c(data[[vCol]], fillValues[[iOrig]])
                    dataCross[is.na(VALUEIDS_), VALUEIDS_ := length(valueMap)]
                } else {
                    valueMap <- data[[vCol]]
                }
                dataCross[, (vColCross) := valueMap[VALUEIDS_]]
                dataCross[, VALUEIDS_ := NULL]
            }
        } else {
            # no X columns or value colums are given
            # only print Y columns (no cross table)
            subTableColIds <- match(subTableCols, colNames)
            yColIds <- match(yCols, colNames)
            dataCross <- data[, c(subTableColIds, yColIds), with = FALSE]
        }
        dataCross <- setorderv(dataCross, yCols)

        # define a list of styling function that implements the styling of the value columns
        if (!is.null(valueColStylings)) {
            if (is.null(bodyStyling)) 
                bodyStyling <- function(object) object
            for (i in seq_len(length(valueCols))) {
                # calculate the column ids of the value column in the resulting
                # cross table
                valueColIds <- length(yCols) + i +
                    length(valueCols) * (
                        seq_len(prod(
                            sapply(
                                xCols, 
                                function(x) 
                                    length(levels(as.factor(data[[x]])))
                            )
                        )) - 1L)
                # apply the value cell styling to the corresponding value columns
                # chain the current valueColStyling function to the bodyStyling function
                # and save it again as bodyStyling
                # be careful this is a recursive definition, to get the scopes right
                env <- new.env()
                env$bodyStyling <- bodyStyling
                env$valueColStyling <- valueColStylings[[i]]
                env$valueColIds <- valueColIds
                bodyStyling <- function(object) {
                    env <- parent.env(environment())
                    env$valueColStyling(
                        env$bodyStyling(object), 
                        cols = env$valueColIds
                    )
                }
                environment(bodyStyling) <- env
            }
        }
        ### generate styled cross table body
        createStyledSubTable(
            data = dataCross, 
            subLevel = 1, 
            subTableCols = subTableCols,
            subHeadingStylings = subHeadingStylings,
            bodyStyling = bodyStyling,
            yCols = yCols,
            xCols = xCols
        )
    }
)

#' Helper function that creates a styled sub table
#'
#' This function creates a styled cross table from a given data.frame that
#' already has cross table format. 
#' It uses the columns specified in 'subTableCols' to separate the
#' table body into several sub tables, where each sub table has the current
#' value in the column defined by 'subTableCols' as sub heading.
#' The columns specified in 'yCols', 'xCols' and 'valueCols'
#' define the cross table structure.
#' The data.frame given in 'data' has already s
#' If the table should not be a cross table, but a normal table then the 
#' arguments 'xCols' and 'valueCols should be omitted.
#' The function is a recursive function that builds that subTables from inside out.
#' @param data A data.frame that should be used for the creation of the cross table.
#' @param subLevel This is the recursive counter that gives the deepness of the recursive function execution. It points to the current sub heading level. So subLevel == 1 means that we look at the sub headings given by the different values in the data.frame column that has the name given in 'subTableCols\[1\]'.
#' @param subTableCols (optional) A vector of column names that should be used in order to separate the data.frame into several sub tables, that will be concatenated vertically. Each sub table will have a sub heading row above the sub table body. The sub heading text is just the corresponding value in the column defined by 'subTableCols'. If no sub heading should be introduced at all, the argument 'subTableCols' should be omitted. If the 'subTableCols' is vector with more than one column name, then a hirachy level is introduced for the sub headings, where the first column has the highest and the column given by the last entry in 'subTableCols' has the lowest hirachy level.
#' @param subHeadingStylings (optional) A list of styling functions. A styling function is a function that takes a styled table as its only argument and returns a styled function. The first styling function is applied to the level-1 sub heading (the sub heading defined by the first entry in 'subTableCols'), the second styling function is applied to the level-2 sub heading etc. Alternatively a single styling function can be passed into 'subHeadingStylings', in this case this styling function is applied to all sub heading levels.
#' @param bodyStyling (optional) A styling function that is applied to the generated styled body of each sub table. A styling function is a function that takes a styled table as its only argument and returns a styled function. In order to format the value columns of the resulting cross table use the argument 'valueColStylings'.
#' @param yCols A vector of column names that are unchanged by the cross table compuation. If the table should not be a cross table, but a normal table this argument can be omitted.
#' @param xCols (optional) A vector of column names that are used for column name generation by the cross table compuation. If the table should not be a cross table, but a normal table this argument can be omitted.
createStyledSubTable <- function(
    data, 
    subLevel, 
    subTableCols, 
    subHeadingStylings = NULL, 
    bodyStyling = NULL, 
    yCols, 
    xCols
) {
    # check if the sub heading level is already at the lowest level
    if (subLevel > length(subTableCols)) {
        # We reached the lowest level in the sub table computation
        # this means we have to generate the sub table body
        # the data.frame has subTableColumns which should not displayed
        #  => remove the columns
        if (length(subTableCols) > 0)
            data <- data[, 
                    setdiff(colnames(data), subTableCols), 
                    with = FALSE
                ]
        setorderv(data, yCols)
        # create styled subTable
        object <- styledTable(data)
        # apply body styling
        if (!is.null(bodyStyling))
            object <- bodyStyling(object)
    } else {
        # The sub heading level is not at the lowest level
        # => we have to print a sub heading line
        subHeadingsCol <- data[[subTableCols[subLevel]]]
        subHeadings <- levels(as.factor(subHeadingsCol))
        object <- NULL
        # step through levels of the current subheading level
        for (heading in subHeadings) {
            # filter all rows of the current sub table
            subData <- data[subHeadingsCol == heading,]
            # calculate styled sub table
            if (nrow(subData) > 0) {
                styledSubTable <- createStyledSubTable(
                        subData,
                        subLevel + 1L,
                        subTableCols,
                        subHeadingStylings,
                        bodyStyling,
                        yCols,
                        xCols
                    )
            } else {
                styledSubTable <- NULL
            }
            if (!is.null(styledSubTable)) {
                # generate sub heading
                ncols <- nCols(styledSubTable)
                styledSubHeading <- styledTable(
                            matrix(c(heading, rep("", ncols - 1)), nrow = 1)
                    )
                if (!is.null(subHeadingStylings)) {
                    styledSubHeading <- subHeadingStylings[[subLevel]](styledSubHeading)
                }
                object <- styledTable(
                    object,
                    styledSubHeading,
                    styledSubTable
                )
            }
        }
    }
    object
}
