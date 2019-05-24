#' Create a styled cross table body
#'
#' @name create_cross_table_body
#' @rdname create_cross_table_body-methods
#' @exportMethod create_cross_table_body
#' @param ... Various arguments
setGeneric("create_cross_table_body", function(data, ...) standardGeneric("create_cross_table_body"))

#' Create a styled cross table body
#'
#' This function creates the styled cross table body from a given data.frame.
#' This function works best in combination with the method [create_cross_table_header()] and [styled_table()].
#' For the the creation of the styled cross table body the columns specified in \code{sub_table_cols} are used to separate the
#' table body into several sub tables, where each sub table has the current
#' value in the column defined by \code{sub_table_cols} as sub heading.
#' The columns specified in \code{y_cols_left}, \code{x_cols} and \code{value_cols}
#' will be used for the cross table calculation.
#' If the table should not be a cross table, but a normal table then the
#' arguments \code{x_cols} and \code{value_cols} should be omitted.
#' @rdname create_cross_table_body-methods
#' @aliases create_cross_table_body,data.frame-method
#' @param data A data.frame that should be used for the creation of the cross table.
#' @param sub_table_cols (optional) A vector of column names that should be used in order to separate the data.frame into several sub tables, that will be concatenated vertically. Each sub table will have a sub heading row above the sub table body. The sub heading text is just the corresponding value in the column defined by \code{sub_table_cols}. If no sub heading should be introduced at all, the argument \code{sub_table_cols} should be omitted. If the \code{sub_table_cols} is a vector with more than one column name, then a hirachy level is introduced for the sub headings, where the first column has the highest and the column given by the last entry in \code{sub_table_cols} has the lowest hirachy level.
#' @param sub_heading_stylings (optional) A list of styling functions. A styling function is a function that takes a [StyledTable] object as its only argument and returns a styled function. The first styling function is applied to the level-1 sub heading (the sub heading defined by the first entry in \code{sub_table_cols}), the second styling function is applied to the level-2 sub heading etc. Alternatively a single styling function can be passed into \code{sub_heading_stylings}, in this case this styling function is applied to all sub heading levels.
#' @param body_styling (optional) A styling function that is applied to the generated styled body of each sub table. A styling function is a function that takes a [StyledTable] object as its only argument and returns a styled function. In order to format the value columns of the resulting cross table use the argument \code{value_col_stylings}.
#' @param y_cols_left (optional) A vector of column names that are unchanged by the cross table compuation and are printed to the left of the resulting cross table columns. This argument can be omitted, if no Y columns to the left of the cross_table are needed, but it is not allowed to omit both \code{y_cols_left} and \code{y_cols_right}.
#' @param x_cols (optional) A vector of column names that are used for column name generation by the cross table compuation. If the table should not be a cross table, but a normal table this argument can be omitted.
#' @param y_cols_right A vector of column names that are unchanged by the cross table compuation. This columns are of the same kind as the columns defined by the argument \code{y_cols_left}, but aligned to the right of the generated cross table.
#' @param y_cols_right (optional) A vector of column names that are unchanged by the cross table compuation and that are printed to the right of the cross table. This argument can be omitted, if no Y columns to the left of the cross_table are needed, but it is not allowed to omit both \code{y_cols_left} and \code{y_cols_right}.
#' @param value_cols (optional) A vector of column names that are used as value columns by the cross table compuation. Normally this is just one column. If  it is a vector with several column names, then the column names of these columns introduce an extra level in the cross table column hirachy.
#' @param value_col_stylings (optional) A single column styling function or a list of column styling functions that has the same length as \code{value_cols}. Each column styling function is applied to the corresponding value column (defined to the corresponding column defined by the \code{value_cols} argument) of the resulting styled cross table. If a single column styling function is passed, then the same column styling function is applied to all value columns defined in \code{value_cols}. If no value column specific styling should be applied, then the \code{value_col_stylings} argument should be omitted. If the resulting table should not be a cross table at all, but a normal table, then  this argument should also be omitted. A column styling function is a function of the type: function(st, cols) {...}, which returns a [StyledTable] object, where \code{st} is a [StyledTable] object and \code{col_id} is a numeric vector of column numbers.
#' @param drop_missing_rows (optional) If set to \code{FALSE} and the \code{sub_table_cols} or \code{y_cols_left} are sparse factors then the missing factor levels introduce new rows filled filled with NAs. Otherwise no extra rows will be generated. Default value is \code{TRUE}.
#' @param drop_missing_cols (optional) If set to \code{FALSE} and the \code{x_cols} are sparse factors then the missing factor levels introduce extra cross table columns filled with \code{NA}. Otherwise no extra columns will be generated. Default value is \code{FALSE}.
#' @param aggregation (optional) Defines the function that will be used for the cross table cell aggregation. If not defined, the default aggregation function of the [data.table::dcast()] function will be used.
#' @param fill_values (optional) A list of values that is used to fill the missing values in the \strong{value columns} in the calculated cross table. The first list value is used for the first \strong{value column}, the second value for the second \strong{value column}. Hence, the list given in \code{fill_values} should have the same length as the \code{value_cols} and the types of the \code{fill_values} should be the same as the corresponding \strong{value columns} in the \code{data} data.frame. If the argument is omitted, then only \code{NA} is filled in for the missing cross table values. Caution: If an Excel file is generated from this cross table, \code{NA} values will be replaced by empty strings.
#' @return The generated [StyledTable] object holding the styled table body rows
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
    "create_cross_table_body",
    signature(
        data = "data.frame"
    ),
    function(
        data,
        sub_table_cols = NULL,
        sub_heading_stylings = NULL,
        body_styling = NULL,
        y_cols_left = NULL,
        y_cols_right = NULL,
        x_cols = NULL,
        value_cols = NULL,
        value_col_stylings = NULL,
        drop_missing_rows = TRUE,
        drop_missing_cols = FALSE,
        aggregation = NULL,
        fill_values = NULL
    ) {
        data <- as.data.table(data)
        ncols <- ncol(data)
        # variable names of data.frame
        colNames <- colnames(data)
        # variable vector for error messages
        colNamesMsg <- paste0(
            "(",
            paste0(colNames, collapse = ", "),
            ")"
        )
        ### Check consistency of supplied arguments
        errHandler <- function(description) {
            stop(paste0(
                "Error in '",
                "create_cross_table_body",
                "': ",
                description
            ), call. = FALSE)
        }
        ### check sub_table_cols
        if (!is.null(sub_table_cols)) {
            if (
                !is.character(sub_table_cols) ||
                    any(!sub_table_cols %in% colNames) ||
                    length(unique(sub_table_cols)) != length(sub_table_cols)
            )
                errHandler(paste0(
                    "Argument 'sub_table_cols' must be a subset of '",
                    colNamesMsg,
                    "'."
                ))
            # if the sub_heading_stylings is just a single function
            # then apply this function to all subHeadings
            if (is.function(sub_heading_stylings))
                sub_heading_stylings <- lapply(
                        seq_len(length(sub_table_cols)),
                        function(x) sub_heading_stylings
                    )
            if (
                !is.null(sub_heading_stylings) && (
                    !is.list(sub_heading_stylings) ||
                    length(sub_heading_stylings) != length(sub_table_cols) ||
                    any(!unlist(lapply(sub_heading_stylings, is.function)))
                )
            )
                errHandler(paste0(
                    "The argument 'sub_heading_stylings' must either be NULL,",
                    " a single styling function or a",
                    " numbered list of the same length as 'sub_table_cols' that",
                    " holds styling functions that will be",
                    " applied to the different sub heading levels of the sub tables.",
                    " Each styling function must be of the type:",
                    " function(st) {...}, where 'st' is a styled",
                    " table and the function must return a styled table."
                ))
        } else {
            if (!is.null(sub_heading_stylings))
                errHandler(paste0(
                    "The argument 'sub_heading_stylings' can only be set, if",
                    " the argument 'sub_table_cols' is given as well."
                ))
        }
        ### check body_styling
        if (!is.null(body_styling) && (
            !is.function(body_styling)
            )
        )
            errHandler(paste0(
                "The argument 'body_styling' must be NULL or a styling function",
                " of the type:",
                " function(st) {...}, where 'st' is a styled",
                " table and the function must return a styled table."
            ))
        ### check y_cols_left
        if (!is.null(y_cols_left)) {
            if (
                !is.character(y_cols_left) ||
                    any(!y_cols_left %in% colNames) ||
                    length(y_cols_left) == 0
            )
                errHandler(paste0(
                    "Argument 'y_cols_left' must be a subset of '", 
                    colNamesMsg, 
                    "'."
                ))
            # check if the vector overlaps with other given column indices
            if (any(y_cols_left %in% sub_table_cols))
                errHandler(paste0(
                    "The column names given in 'y_cols_left' should not",
                    " overlap with the column names given 'sub_table_cols'."
                ))
        }
        ### check x_cols
        if (!is.null(x_cols)) {
            if (
                !is.character(x_cols) ||
                    any(!x_cols %in% x_cols) ||
                    length(unique(x_cols)) != length(x_cols)
            )
                errHandler(paste0(
                    "The argument 'x_cols' must be vectors holding",
                    " the column names that should be used for the cross",
                    " table computation. These vectors must be a non",
                    " overlapping subset of '",
                    colNamesMsg,
                    "'."
                ))
            # check if the vector overlaps with other given column indices
            if (any(x_cols %in% c(sub_table_cols, y_cols_left)))
                errHandler(paste0(
                    "The argument 'x_cols' holding the column names that",
                    " should be used for the cross table computation must be non",
                    " overlapping with all other",
                    " column names given in 'sub_table_cols' and 'y_cols_left'."
                ))
        }
        ### check y_cols_right
        if (!is.null(y_cols_right)) {
            if (
                !is.character(y_cols_right) ||
                    any(!y_cols_right %in% colNames)
            )
                errHandler(paste0(
                    "The argument 'y_cols_right' holding",
                    " the column names that should be used as extra y-columns",
                    " printed to the right of the generated cross table",
                    " must be a non overlapping subset of '",
                    colNamesMsg,
                    "'."
                ))
            # check if the vector overlaps with other given column indices
            if (any(y_cols_right %in% c(sub_table_cols, x_cols)))
                errHandler(paste0(
                    "The argument 'y_cols_right' holding the column names that",
                    " should be added to the right of the cross table.",
                    " This set must be non overlapping with all other",
                    " column names given in 'sub_table_cols' and 'x_cols'."
                ))
        }
        if (is.null(y_cols_left) && is.null(y_cols_right))
            errHandler(paste0(
                "It is not allowed that 'y_cols_left' and 'y_cols_right' are both",
                " omitted at the same time. At least one of them must be a non",
                " empty subset of '",
                colNamesMsg,
                "'."
            ))
        ### check value_cols
        if (!is.null(value_cols)) {
            if (
                !is.character(value_cols) ||
                    any(!value_cols %in% value_cols) ||
                    length(unique(value_cols)) != length(value_cols)
            )
                errHandler(paste0(
                    "The argument 'value_cols' which holds the column",
                    " names that should be used as values for the cross table",
                    " computation must be a non",
                    " overlapping subset of '",
                    colNamesMsg, "'."
                ))
            # check if the vector overlaps with other given column indices
            if (any(value_cols %in% c(sub_table_cols, y_cols_left, x_cols, y_cols_right)))
                errHandler(paste0(
                    "The argument 'value_cols' which holds the column names",
                    " that should be used as values for the cross table",
                    " computation must be non overlapping with the column names",
                    " given in 'sub_table_cols', 'y_cols_left'."
                ))
            # if the sub_heading_stylings is just a single function
            # then apply this function to all subHeadings
            if (is.function(value_col_stylings))
                value_col_stylings <- lapply(
                        seq_len(length(value_cols)),
                        function(x) value_col_stylings
                    )
            if (
                !is.null(value_col_stylings) && (
                    !is.list(value_col_stylings) ||
                    length(value_cols) != length(value_col_stylings) ||
                    any(!unlist(lapply(value_col_stylings, is.function)))
                )
            )
                errHandler(paste0(
                    "The argument 'value_col_stylings' must either be NULL,",
                    " a single column styling function or a",
                    " numbered list of the same length as 'value_cols' that",
                    " holds styling functions that should be",
                    " applied to the different value columns of the cross table.",
                    " The column styling functions have to be of the type ",
                    " function(st, col_id) {...}, where 'st' is a styled",
                    " table and 'col_id' is a numeric column vector and",
                    " the function must return a styled table."
                ))
        } else {
            if (!is.null(x_cols))
                errHandler(paste0(
                    "If the 'x_cols' argument is non empty, then",
                    " the 'value_cols' argument must be passed as well."
                ))
            if (!is.null(value_col_stylings))
                errHandler(paste0(
                    "If the 'value_col_stylings' argument is non empty, then",
                    " the 'value_cols' argument must be passed as well."
                ))
        }
        ### check fill_values
        if (!is.null(fill_values)) {
            if (is.null(value_cols))
                errHandler(paste0(
                    "If the 'fill_values' argument is non empty, then",
                    " the 'value_cols' argument must be passed as well."
                ))
            if (!is.list(fill_values) || length(fill_values) != length(value_cols))
                errHandler(paste0(
                    "The argument 'fill_values' must be a list of the same",
                    " length as the argument 'value_cols'."
                ))
            for (i in seq_len(length(fill_values))) {
                if (typeof(fill_values[[i]]) != typeof(data[[value_cols[i]]]))
                    errHandler(paste0(
                        "The 'fill_values[[",
                        i,
                        "]]' has type '",
                        typeof(fill_values[[i]]),
                        "' whereas the column 'data[[\"",
                        value_cols[i],
                        "\"]]' has type '",
                        typeof(data[[value_cols[i]]]),
                        "'."
                    ))
                if (length(fill_values[[i]]) != 1)
                    errHandler(paste0(
                        "The entry 'fill_values[[",
                        i,
                        "]]' has not length == 1."
                    ))
            }
        }

        #### Calculate cross table data
        if (length(value_cols) > 0) {
            # The data.frame has one or more value columns that should be used for cross table calculation
            # Solution:
            # - Transform the value_cols into indices columns
            #    (so that all value_cols are of type integer,
            #     this solves the problem that value_cols may be of diff. types)
            # - melt all value indeces columns
            # - dcast into a cross table
            # - replace the valueCol indices by the corresponding values
            #
            # The heading levels of the crossed table are the current heading
            # texts of the columns
            # If drop_missing_rows = FALSE, then no extra rows will be introduced
            # even if some of the sub_table_cols or y_cols_left are sparse factors
            # If drop_missing_cols = TRUE, then extra cross table columns will be generated
            # if the x_cols are sparse factors
            KEYCOL <- "KEYCOL"
            VALUECOL <- "VALUECOL"
            dcastFormula <- as.formula(paste0(
                    paste0(c(sub_table_cols, y_cols_left, y_cols_right), collapse = " + "), 
                    " ~ ", 
                    paste0(c(x_cols, KEYCOL), collapse = " + ")
                ))
            dataCross <- melt(
                    data = copy(data)[, (value_cols) := .I], # replace the value columns by the row indices of the values
                    variable.name = KEYCOL,
                    value.name = VALUECOL,
                    measure.vars = value_cols
                )
            args <- list(
                    data = dataCross,
                    formula = dcastFormula,
                    fun.aggregate = aggregation,
                    value.var = VALUECOL,
                    fill = NA,
                    drop = c(drop_missing_rows, drop_missing_cols)
                )
            args <- args[sapply(args, function(x) !is.null(x))]
            dataCross <- do.call(
                dcast,
                args = args
            )

            # replace the value indices in the generated value columns by their original values
            value_colsCross <- setdiff(colnames(dataCross), c(sub_table_cols, y_cols_left, y_cols_right))
            for (i in seq_len(length(value_colsCross))) {
                # name of the value column in the crossTable
                vColCross <- value_colsCross[i]
                # original name of the value column before the crossTable generation
                iOrig <- ((i - 1L) %% length(value_cols)) + 1L
                vCol <- value_cols[iOrig]
                # replace the value indices by their original values
                setnames(dataCross, vColCross, "VALUEIDS_")
                if (!is.null(fill_values)) {
                    valueMap <- c(data[[vCol]], fill_values[[iOrig]])
                    dataCross[is.na(VALUEIDS_), VALUEIDS_ := length(valueMap)]
                } else {
                    valueMap <- data[[vCol]]
                }
                dataCross[, (vColCross) := valueMap[VALUEIDS_]]
                dataCross[, VALUEIDS_ := NULL]
            }
            colNamesNew <- colnames(dataCross)
            # all remaining rows are Cross Table Columns
            xColIds <- match(
                setdiff(colNamesNew, c(sub_table_cols, y_cols_left, y_cols_right)), 
                colNamesNew
            )
        } else {
            # no X columns or value colums are given
            # only print Y columns (no cross table)
            dataCross <- data
            colNamesNew <- colnames(dataCross)
            # No cross table columns
            xColIds <- NULL
        }
        # Pick the columns from the generated dataCross
        # [sub_table_cols, y_cols_left, #cross_table#, y_cols_right]
        subTableColIds <- match(sub_table_cols, colNamesNew)
        yColIds <- match(y_cols_left, colNamesNew)
        yColRightIds <- match(y_cols_right, colNamesNew)
        dataCross <- dataCross[, c(subTableColIds, yColIds, xColIds, yColRightIds), with = FALSE]
        dataCross <- setorderv(dataCross, c(y_cols_left, y_cols_right))

        # define a list of styling function that implements the styling of the value columns
        if (!is.null(value_col_stylings)) {
            if (is.null(body_styling))
                body_styling <- function(st) st
            for (i in seq_len(length(value_cols))) {
                # calculate the column ids of the value column in the resulting
                # cross table
                if (!is.null(x_cols)) {
                    valueColIds <- length(y_cols_left) + i +
                        length(value_cols) * (
                            seq_len(prod(
                                sapply(
                                    x_cols,
                                    function(x)
                                        length(levels(as.factor(data[[x]])))
                                )
                            )) - 1L)
                } else {
                    valueColIds <- length(y_cols_left) + i
                }
                # apply the value cell styling to the corresponding value columns
                # chain the current valueColStyling function to the body_styling function
                # and save it again as body_styling
                # be careful this is a recursive definition, to get the scopes right
                env <- new.env()
                env$body_styling <- body_styling
                env$valueColStyling <- value_col_stylings[[i]]
                env$valueColIds <- valueColIds
                body_styling <- function(st) {
                    env <- parent.env(environment())
                    env$valueColStyling(
                        env$body_styling(st),
                        col_id = env$valueColIds
                    )
                }
                environment(body_styling) <- env
            }
        }
        if (is.null(x_cols) && !is.null(value_cols))
            y_cols_left <- c(y_cols_left, value_cols)

        ### generate styled cross table body
        create_sub_table(
            data = dataCross,
            sub_level = 1,
            sub_table_cols = sub_table_cols,
            sub_heading_stylings = sub_heading_stylings,
            body_styling = body_styling,
            y_cols_left = y_cols_left,
            y_cols_right = y_cols_right
        )
    }
)

#' Helper function that creates a styled sub table
#'
#' This function creates a styled cross table from a given data.frame that
#' already has cross table format.
#' It uses the columns specified in \code{sub_table_cols} to separate the
#' table body into several sub tables, where each sub table has the current
#' value in the column defined by \code{sub_table_cols} as sub heading.
#' The columns specified in \code{y_cols_left}, \code{x_cols} and \code{value_cols}
#' define the cross table structure.
#' The data.frame given in \code{data} has already s
#' If the table should not be a cross table, but a normal table then the
#' arguments \code{x_cols} and \code{value_cols} should be omitted.
#' The function is a recursive function that builds that subTables from inside out.
#' @param data A data.frame that should be used for the creation of the cross table.
#' @param sub_level This is the recursive counter that gives the deepness of the recursive function execution. It points to the current sub heading level. So sub_level == 1 means that we look at the sub headings given by the different values in the data.frame column that has the name given in \code{sub_table_cols\[1\]}.
#' @param sub_table_cols (optional) A vector of column names that should be used in order to separate the data.frame into several sub tables, that will be concatenated vertically. Each sub table will have a sub heading row above the sub table body. The sub heading text is just the corresponding value in the column defined by \code{sub_table_cols}. If no sub heading should be introduced at all, the argument \code{sub_table_cols} should be omitted. If the \code{sub_table_cols} is vector with more than one column name, then a hirachy level is introduced for the sub headings, where the first column has the highest and the column given by the last entry in \code{sub_table_cols} has the lowest hirachy level.
#' @param sub_heading_stylings (optional) A list of styling functions. A styling function is a function that takes a [StyledTable] object as its only argument and returns a styled function. The first styling function is applied to the level-1 sub heading (the sub heading defined by the first entry in \code{sub_table_cols}), the second styling function is applied to the level-2 sub heading etc. Alternatively a single styling function can be passed into \code{sub_heading_stylings}, in this case this styling function is applied to all sub heading levels.
#' @param body_styling (optional) A styling function that is applied to the generated styled body of each sub table. A styling function is a function that takes a [StyledTable] object as its only argument and returns a styled function. In order to format the value columns of the resulting cross table use the argument \code{value_col_stylings}.
#' @param y_cols_left A vector of column names that are unchanged by the cross table compuation. If the table should not be a cross table, but a normal table this argument can be omitted.
#' @param y_cols_right A vector of column names that are unchanged by the cross table compuation. This columns are of the same kind as the columns defined by the argument \code{y_cols_left}, but aligned to the right of the generated cross table.
create_sub_table <- function(
    data, 
    sub_level, 
    sub_table_cols, 
    sub_heading_stylings = NULL, 
    body_styling = NULL, 
    y_cols_left, 
    y_cols_right
) {
    # check if the sub heading level is already at the lowest level
    if (sub_level > length(sub_table_cols)) {
        # We reached the lowest level in the sub table computation
        # this means we have to generate the sub table body
        # the data.frame has subTableColumns which should not displayed
        #  => remove the columns
        if (length(sub_table_cols) > 0)
            data <- data[,
                    setdiff(colnames(data), sub_table_cols),
                    with = FALSE
                ]
        setorderv(data, c(y_cols_left, y_cols_right))
        # create styled subTable
        st <- styled_table(data)
        # apply body styling
        if (!is.null(body_styling))
            st <- body_styling(st)
    } else {
        # The sub heading level is not at the lowest level
        # => we have to print a sub heading line
        subHeadingsCol <- data[[sub_table_cols[sub_level]]]
        subHeadings <- levels(as.factor(subHeadingsCol))
        st <- NULL
        # step through levels of the current subheading level
        for (heading in subHeadings) {
            # filter all rows of the current sub table
            subData <- data[subHeadingsCol == heading,]
            # calculate styled sub table
            if (nrow(subData) > 0) {
                styledSubTable <- create_sub_table(
                        subData,
                        sub_level + 1L,
                        sub_table_cols,
                        sub_heading_stylings,
                        body_styling,
                        y_cols_left,
                        y_cols_right
                    )
            } else {
                styledSubTable <- NULL
            }
            if (!is.null(styledSubTable)) {
                # generate sub heading
                ncols <- count_cols(styledSubTable)
                styledSubHeading <- styled_table(
                            matrix(c(heading, rep("", ncols - 1)), nrow = 1)
                    )
                if (!is.null(sub_heading_stylings)) {
                    styledSubHeading <- sub_heading_stylings[[sub_level]](styledSubHeading)
                }
                st <- styled_table(
                    st,
                    styledSubHeading,
                    styledSubTable
                )
            }
        }
    }
    st
}
