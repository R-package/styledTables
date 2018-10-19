#' Method createStyledCrossTableHeader
#'
#' @name createStyledCrossTableHeader
#' @rdname createStyledCrossTableHeader-methods
#' @exportMethod createStyledCrossTableHeader
setGeneric(
    "createStyledCrossTableHeader", 
    function(
        yColHeadings, 
        ...
    ) standardGeneric("createStyledCrossTableHeader")
)

#' Create a styled header for a cross table
#'
#' This function creates a styled header for a cross table. It is best used
#' together with 'createStyledCrossTableBody'. 
#' It assumes that the cross table consists of several Y columns that come first
#' and has nested cross table part. That means that the cross table X columns
#' can have more than one header level. 
#' For example: Male > Positive | Male > Negative | Female > Positive | Female > Negative
#' In this case the 'Male' and 'Female' headings will be in merged cells above the 'Positive' and 'Negative' cells.
#' @rdname createStyledCrossTableHeader-methods
#' @aliases createStyledCrossTableHeader,character,character,character,character,character,character-method
#' @param yColHeadings A character vector that holds the header texts for the Y columns of the cross table. If no other arguments are given the resulting is not a cross table, but a normal table.
#' @param crossTableHeading (optional) A single character string that is printed in a merged cell above the Y cross table cols.
#' @param ... (optional) One or more vectors of character strings that hold the levels of the first/second/third/... cross table X column headings. These levels are used as header texts in the first/second/third/... row of the generated cross table columns. If no character vector is supplied, then the resulting table is not a cross table, but a normal table.
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
setMethod(
    "createStyledCrossTableHeader",
    signature(
        yColHeadings = "character"
    ),
    function(
        yColHeadings,
        crossTableHeading = NULL,
        ...
    ) {
        # All headings of the columns that are used for the cross table columns
        xColHeadings <- list(...)
        # Filter out all NULL entries
        xColHeadings <- xColHeadings[which(sapply(xColHeadings, function(x) !is.null(x)))]
        ### Check consistency of supplied arguments
        errHandler <- function(description) {
            stop(paste0(
                "Error in '",
                "createStyledCrossTableHeader",
                "': ",
                description
            ), call. = FALSE)
        }
        # Check xColsHeadings
        if (!is.null(yColHeadings) & !is.character(yColHeadings))
            errHandler(paste0(
                "Argument 'yColHeadings' must be a vector of",
                " character strings."
            ))
        # Check xColHeadings (...) and crossTableHeading
        if (length(xColHeadings) > 0) {
            if (
                !is.null(crossTableHeading) && 
                    (!is.character(crossTableHeading) || length(crossTableHeading) != 1)
            )
                errHandler(paste0("Argument 'crossTableHeading' must be NULL or a character string."))
            for (colHeadings in xColHeadings) {
                if (!is.character(colHeadings) || length(colHeadings) == 0)
                    errHandler(paste("The argument '...' must be omitted or given one ore more non empty character vectors."))
            }
        } else {
            if (!is.null(crossTableHeading))
                errHandler(paste(
                    "The Argument 'crossTableHeading' can only be set, if",
                    "any X cross column headings are given",
                    "(at least one character must be passed into the argument '...')."
                ))
        }
        ### Create header data
        # append the main cross table heading to the cross column headings
        # (actually it is a special case of the cross col heading system: a heading without column)
        if (!is.null(crossTableHeading)) 
            xColHeadings <- c(crossTableHeading, xColHeadings)
        # Calculate number of header rows
        if (length(xColHeadings) > 0) {
            nrows <- length(xColHeadings)
        } else {
            nrows <- 1L
        }
        # Generate the header for all Y cols (no cross table cols)
        nyCols <- length(yColHeadings)
        headerData <- data.table(matrix(rep(yColHeadings, each = nrows), ncol = nyCols))
        # List of merged cells (will be filled up later on)
        merges <- lapply(seq.save(1L, nyCols), function(i) list(cols = c(i, i), rows = c(1L, nrows)))
        # Generate cross table header cols and put them to the right of the y cols
        if (length(xColHeadings) > 0) {
            sHeader <- styledTable(cbind(
                    headerData,
                    rbindlist(lapply(1:nrows, function(i) {
                        if (i < nrows) {
                            nLevelsBelow <- prod(sapply(
                                        xColHeadings[seq.save(i + 1L, nrows)], 
                                        length
                                    ))
                        } else {
                            nLevelsBelow <- 1L
                        }
                        if (i > 1) {
                            nLevelsAbove <- prod(
                                sapply(xColHeadings[seq.save(1, i - 1L)], length))
                        } else {
                            nLevelsAbove <- 1L
                        }
                        data.table(matrix(
                            rep(
                                rep(xColHeadings[[i]], each = nLevelsBelow), 
                                nLevelsAbove
                            ), nrow = 1))
                    }))
                ))
            # Add the merges for the cross table header levels
            merges <- c(
                    merges,
                    unlist(lapply(seq.save(1L,(nrows - 1L)), function(i) {
                        nLevelsBelow <- prod(sapply(
                                    xColHeadings[seq.save(i + 1L, nrows)], 
                                    length
                                ))
                        if (i > 1L) {
                            nLevelsAbove <- prod(
                                sapply(xColHeadings[seq.save(1L, i - 1L)], length))
                        } else {
                            nLevelsAbove <- 1L
                        }
                        if (nLevelsBelow == 1L)
                            return(NULL)
                        unlist(lapply(
                            0L:(length(xColHeadings[[i]]) - 1L), 
                            function(j) lapply(
                                0L:(nLevelsAbove - 1L), 
                                function(k) list(
                                    rows = c(i, i), 
                                    cols = nyCols + 
                                        (j + k * nLevelsAbove) * nLevelsBelow + 
                                        c(1L, nLevelsBelow))
                            )), recursive = FALSE)
                    }), recursive = FALSE)
                )
        } else {
            sHeader <- styledTable(headerData)
        }
        # write the merges list directly into the styledTable (faster)
        sHeader@merges <- merges
        sHeader
    }
)
