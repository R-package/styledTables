#' Check if type is as expected (without using non standard evaluation)
#'
#' @param var Variable whose type should be checked
#' @param types (vector of strings) A vector of allowed type values
#' @param varName (string) Name of the variable passed in in \code{var}. This
#' name is used for the error message if it has the wrong type.
#' @param allowNA (logical) Is a missing value allowed?
assertType <- function(var, types, varName, allowNA = TRUE) {
    if ((!allowNA && is.na(var)) || (!is.na(var) && !typeof(var) %in% types))
        sprintf(
            "Argument '%s' must be of type '%s'.", 
            varName, 
            paste(paste0("'", types, "'"), collapse = " oder ")
        )
}

#' Check if a variable is a character, if not throw an error (using non standard evaluation)
#'
#' @param var The value that should be checked
AssertChar <- function(var) {
    if (!is.null(var)) {
        varName = deparse(substitute(var))
        assertType(var, "character", varName)
    }
}

#' Check if a variable is a number, if not throw an error (using non standard evaluation)
#'
#' @param var The value that should be checked
AssertNumber <- function(var) {
    if (!is.null(var)) {
        varName = deparse(substitute(var))
        assertType(var, c("integer", "double"), varName)
    }
}

#' Check if a variable is a logical, if not throw an error (using non standard evaluation)
#'
#' @param var The value that should be checked
AssertLogical <- function(var) {
    if (!is.null(var)) {
        varName = deparse(substitute(var))
        assertType(var, "logical", varName)
    }
}

#' Check if a variable holds an allowed value (using non standard evaluation)
#'
#' @param var Variable whose value should be checked
#' @param values (vector) A vector of allowed values
AssertValue <- function(var, values) {
    # Check if variable "var" has an allowed value (value list given by "values")
    # if not generate an error message and return it
    if (length(var) > 0) {
        varName <- deparse(substitute(var))
        assertValue_(var, varName, values)
    }
}

#' Check if a variable holds an allowed value (without non standard evaluation)
#'
#' @param var Variable whose value should be checked
#' @param varName name of the variable whose value should be checked
#' @param values (vector) A vector of allowed values
assertValue_ <- function(var, varName, values) {
    # Check if variable "var" has an allowed value (value list given by "values")
    # if not generate an error message and return it
    if (!all(var %in% values)) {
        if (is.character(values))
            strValues <- paste0("'", values, "'")
        else
            strValues <- values
        strValues <- paste0(strValues, collapse = ", ")
        sprintf("Argument '%s' must be in c(%s).", varName, strValues)
    }
}

#' Check if a variable holds a color value (using non standard evaluation)
#'
#' @param var Variable whose value should be checked
AssertColorValue <- function(var) {
    # Check if variable "var" has a color value
    # if not, throw an error
    if (length(var) > 0) {
        varName <- deparse(substitute(var))
        if (!all(grepl("^#[0-9abcdefABCDEF]{6}$", var)))
            assertValue_(var, varName, transformChar(grDevices::colors(), toUpper = TRUE))
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
transformChar <- function(var = NULL, pre = "", post = "", toUpper = TRUE) {
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
removeMissing <- function(li) {
    Filter(function(x) length(unlist(x)) > 0, li)
}

#' Call function with arguments and remove all empty arguments (NULL or length == 0)
#'
#' Invokes a function with a list of arguments, but removes all \code{NULL} arguments first
#' @param fn (function) Function that should be invoked
#' @param args (named list) List of arguments (\code{NULL} arguments will be removed)
#' @param skipIfAllMissing (logical) If \code{skipIfAllMissing = TRUE} and all arguments in \code{args} are \code{NULL}, then the function \code{fn} is not invoked and \code{NULL} is returned instead.
doCallWithoutMissing <- function(fn, args, skipIfAllMissing = TRUE) {
    rArgs <-removeMissing(args)
    if (!skipIfAllMissing || length(rArgs) > 0)
        do.call(fn, rArgs)
}

#' Improved \code{seq} command that returns an empty vector if lower > upper boundary 
#'
#' Enhance the \code{seq} such that it can handle the situation if \code{from > to}
#' @param from First element in the sequence
#' @param to Last element in the sequence
#' @return The sequence
seq.save <- function(
    from, 
    to 
) {
    if (to < from)
        return(integer(0))
    seq(from, to)
}

#' Improved \code{seq} command which allows an two element vector holding the sequence boundaries 
#'
#' The boundaries for the sequence command are given as two element vector and passed to the \code{seq.save} function.
#' @param bLU A two element vector. The first element is the lower boundary the second element the upper boundarie. If the lower boundary is greater than the upper boundary, then an empty vector is returned.
#' @return The sequence
seq.vec <- function(
    bLU
) {
    if (length(bLU) != 2)
        return(integer(0))
    seq.save(bLU[1], bLU[2])
}

#' Calculate the hex color value from a color name
#'
#' This little function takes a color name (e.g.: "red") and calculates the corresponding hex color value.
#' @param col A string holding a color name (e.g.: "red") or a color value (e.g.: "#F00000")
#' @return The hex color value as string without hashtag (e.g.: "F00000")
colorToHex <- function(col) {
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
betweenVec <- function(x, vec) {
    vec[1] <= x && x <= vec[2]
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
    val <- gsub("_", "\\_", val, fixed = TRUE)
    val <- gsub("\\~", "\\textasciitilde{}", val, fixed = TRUE)
    gsub("\\^", "\\textasciicircum{}", val, fixed = TRUE)
}

#' Concatenate functions
#'
#' @param ... Several functions that should be concatenated
concatFunctions <- function(...) {
    fnList <- list(...)
    function(x) {
        for (fn in fnList) x <- fn(x)
        x
    }
}

#' Compare Styles
#'
#' @param s1 First style object
#' @param s2 First style object
#' @export
compareStyles <- function(s1, s2) {
    all(sapply(c(
            "fontName",
            "fontHeight",
            "fontColor",
            "isBold",
            "isItalic",
            "isStrikeout",
            "underline",
            "boldweight",
            "isWrapped",
            "horizontal",
            "vertical",
            "rotation",
            "indent",
            "borderPosition",
            "borderColor",
            "borderPen",
            "foregroundColor",
            "backgroundColor",
            "fillPattern",
            "dataFormat",
            "isLocked",
            "isHidden",
            "latexVerticalMove"
        ), function(slotName)
            identical(slot(s1, slotName), slot(s2, slotName))
    ))
}

