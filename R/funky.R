#' @include utilities.R
NULL

#' Restrict function environment
#' 
#' Build a new function with an optimal scope. Normally the entire
#' environment tree (including the entire ancestry) is kept in memory as long
#' as a function is present. This is a common cause for memory leaks.
#' With `restrict_fn_env()` you create a function that has only
#' the needed variables (copies) in the scope and the optimal scope ancestry can
#' be defined by argument `parent_env`.
#' Be sure, to choose the right environment for `parent_env`!
#' @param fn A function whose parent scope should be restricted to the
#'   set of variables given in `vars` and the loaded packages.
#' @param vars An optional object, telling which variables should be
#'   available in `fn()`. It can either be
#'   - a `character` vector holding the names of the variables which should 
#'     looked up in the environment `lookup_env`.
#'   - a named `list`: In this case, the values are not looked up in
#'     `lookup_env`, but directly taken from the list item values and the
#'     list item names are used as variable names.
#' @param lookup_env The environment holding the variables for which the names
#'   are defined in the  character vector `vars`.
#'   If `vars` is a list or `NULL`, then `lookup_env` is not used.
#'   The default for `lookup_env` is the environment where the function `fn`
#'   was defined.
#' @param parent_env The parent environment, which should be assigned to 
#'   the restricted function. This argument is very important, since it
#'   determines which objects will be available inside of your function.
#'   Usually one of the following two possibilities is the right choice for `parent_env`:
#'   - `parent_env = .GlobalEnv` (default): The **global environment** is usually the right
#'     choice for a function, which does not use any non-exported functions
#'     or any imported functions of any R package.
#'     This is usually the case, when the function is created outside
#'     of any R package (e.g. not inside of a function that is part of some R package).
#'   - `parent_env = "MY_PKG"`: This is usually the right choice when you are
#'     developing a new R package and want to create a restricted function
#'      inside of another function of this package
#'     (in this example the package name is `MY_PKG`).
#'     This ensures that also non-exported functions and imported functions of
#'     `MY_PKG` are available inside of your
#'     restricted function as well as the **global environment**.
#' @return A new function with a small scope containing the variables given 
#'   in `vars`.
restrict_fn_env <- function(fn, vars = NULL, lookup_env = environment(fn), parent_env = .GlobalEnv) {
  err_h <- function(msg)
    stop(paste("Error while calling `restrict_fn_env()`:", msg), call. = FALSE)
  if (!is.function(fn))
    err_h("Argument `fn` must be a function.")
  if (!is.null(vars) && !is.list(vars) && !is.character(vars))
    err_h("Argument `vars` must either be a named list or a character vector or omitted.")
  if (is.list(vars)) {
    names_vars <- names(vars)
    if (is.null(names_vars) || any(is.na(names_vars)))
      err_h("Argument `vars` is a list, but not a named list.")
    id_names_invalid <- which(names_vars != make.names(names_vars))
    if (length(id_names_invalid) > 0)
      err_h(paste(
        "Argument `vars` is a named list, but the names of the following",
        "entries are not valid variable names:\n  ",
        stringify(id_names_invalid)
      ))
    if (length(names_vars) != length(unique(names_vars)))
      err_h("Argument `vars` is a named list, but some names have duplicates.")
  } else if (is.character(vars)) {
    if (any(is.na(vars)))
      err_h("Argument `vars` is character vector, but contains `NA` values.")
    id_vars_invalid <- which(vars != make.names(vars))
    if (length(id_vars_invalid) > 0)
      err_h(paste(
        "Argument `vars` is a character vector, but the following",
        "entries are not valid variable names:\n  ",
        stringify(id_vars_invalid)
      ))
    if (length(vars) != length(unique(vars)))
      err_h("Argument `vars` is a character vector, but has duplicate entries.")
    if (!is.environment(lookup_env))
      err_h(paste(
        "Since argument `vars` is a character vector, then `lookup_env`",
        "must be an environment, but it is not."
      ))
  }
  if (!is.environment(parent_env)) {
    if (!is.character(parent_env) || length(parent_env) != 1 || is.na(parent_env))
      err_h(paste(
        "Argument `parent_env` must either be an environment or a string holding",
        "the name of the installed R package, whoose namespace should be used",
        "as parent environment."
      ))
    parent_env <- tryCatch(
      rlang::ns_env(parent_env),
      error = function(e)
        err_h(paste(
          "The R package name",
          stringify(parent_env),
          "given in argument `parent_env` could not be found in your R library."
        ))
    )
  }
  # restrict
  if (is.character(vars)) {
    names(vars) <- vars
    vars <- lapply(
      vars,
      function(v) get(v, envir = lookup_env)
    )
  }
  new_env <- new.env(parent = parent_env)
  for(var in names(vars)) {
    assign(
      var,
      vars[[var]],
      envir = new_env
    )
  }
  environment(fn) <- new_env
  fn
}

#' Eval code in closure without scoping problems (memory leaks)
#' 
#' Evaluate an expression inside of closure that has an optimal scope.
#' This is very important, if your closure does return a function, since the
#' the entire environment tree (including the entire ancestry) is kept
#' in memory as long as your function is present. This is a common cause for
#' memory leaks.
#' With `eval_closure()` you can assign an environment that contains only
#' the needed variables (copies) and an optimal scope ancestry can be defined by
#' `parent_env`. Be sure, to choose the right environment for `parent_env`!
#' @param vars An optional object, telling which variables should be
#'   available inside the closure. It can either be
#'   - a `character` vector holding the names of the variables which should 
#'     looked up in the environment `lookup_env`.
#'   - a named `list`: In this case, the values are not looked up in
#'     `lookup_env`, but directly taken from the list item values and the
#'     list item names are used as variable names.
#' @param expr The expression, which should be evaluated inside of the
#'   closure.
#' @param parent_env The parent environment (either an environment or a string
#'   holding the name of an R package whoose namespace should be used as parent
#'   environment), which should be assigned to 
#'   the closure. This argument is very important, since it
#'   determines which objects will be available inside of your closure.
#'   Usually one of the following two possibilities is the right choice for `parent_env`:
#'   - `parent_env = .GlobalEnv` (default): The **global environment** is usually the right
#'     choice for an expression, which does not use any non-exported functions
#'     or any imported functions of any R package.
#'     This is usually the case, when the closure is created outside
#'     of any R package (e.g. not inside of a function that is part of an R package).
#'   - `parent_env = "MY_PKG"`: This is usually the right choice when you are
#'     developing a new R package and want to create a closure
#'     inside of another function of this package
#'     (in this example the package name is `MY_PKG`).
#'     This ensures that also non-exported functions and imported functions of
#'     `MY_PKG` are available inside of your
#'     closure as well as the **global environment**.
#' @inheritParams restrict_fn_env
eval_closure <- function(
  expr,
  vars = NULL,
  lookup_env = parent.frame(),
  parent_env = .GlobalEnv
) {
  err_h <- function(msg)
    stop(paste("Error while calling `eval_closure()`:", msg), call. = FALSE)
  if (!is.null(vars) && !is.list(vars) && !is.character(vars))
    err_h("Argument `vars` must either be a named list or a character vector or omitted.")
  if (is.list(vars)) {
    names_vars <- names(vars)
    if (is.null(names_vars) || any(is.na(names_vars)))
      err_h("Argument `vars` is a list, but not a named list.")
    id_names_invalid <- which(names_vars != make.names(names_vars))
    if (length(id_names_invalid) > 0)
      err_h(paste(
        "Argument `vars` is a named list, but the names of the following",
        "entries are not valid variable names:\n  ",
        stringify(id_names_invalid)
      ))
    if (length(names_vars) != length(unique(names_vars)))
      err_h("Argument `vars` is a named list, but some names have duplicates.")
  } else if (is.character(vars)) {
    if (any(is.na(vars)))
      err_h("Argument `vars` is character vector, but contains `NA` values.")
    id_vars_invalid <- which(vars != make.names(vars))
    if (length(id_vars_invalid) > 0)
      err_h(paste(
        "Argument `vars` is a character vector, but the following",
        "entries are not valid variable names:\n  ",
        stringify(id_vars_invalid)
      ))
    if (length(vars) != length(unique(vars)))
      err_h("Argument `vars` is a character vector, but has duplicate entries.")
    if (!is.environment(lookup_env))
      err_h(paste(
        "Since argument `vars` is a character vector, then `lookup_env`",
        "must be an environment, but it is not."
      ))
  }
  if (!is.environment(parent_env)) {
    if (!is.character(parent_env) || length(parent_env) != 1 || is.na(parent_env))
      err_h(paste(
        "Argument `parent_env` must either be an environment or a string holding",
        "the name of the installed R package, whoose namespace should be used",
        "as parent environment."
      ))
    parent_env <- tryCatch(
      rlang::ns_env(parent_env),
      error = function(e)
        err_h(paste(
          "The R package name",
          stringify(parent_env),
          "given in argument `parent_env` could not be found in your R library."
        ))
    )
  }
  # eval closure
  if (is.character(vars)) {
    names(vars) <- vars
    vars <- lapply(
      vars,
      function(v) get(v, envir = lookup_env)
    )
  }
  vars[["expr"]] <- substitute(expr)
  new_env <- new.env(parent = parent_env)
  for(var in names(vars)) {
    assign(
      var,
      vars[[var]],
      envir = new_env
    )
  }
  local(
    tryCatch(
      eval(expr),
      error = function(e) stop(
        "Error while calling `eval_closure()`: ",
        "The following expression could not be evaluated:\n  '",
        deparse(expr),
        "'\n",
        e,
        "Please pass the expression directly to `eval_closure()` and ensure ",
        "that all needed variables are specified in `vars`."
      ),
      finally = rm(expr)
    ),
    envir = new_env
  )
}


