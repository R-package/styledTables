% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funky.R
\name{restrict_fn_env}
\alias{restrict_fn_env}
\title{Restrict function environment}
\usage{
restrict_fn_env(
  fn,
  vars = NULL,
  lookup_env = environment(fn),
  parent_env = .GlobalEnv
)
}
\arguments{
\item{fn}{A function whose parent scope should be restricted to the
set of variables given in \code{vars} and the loaded packages.}

\item{vars}{An optional object, telling which variables should be
available in \code{fn()}. It can either be
\itemize{
\item a \code{character} vector holding the names of the variables which should
looked up in the environment \code{lookup_env}.
\item a named \code{list}: In this case, the values are not looked up in
\code{lookup_env}, but directly taken from the list item values and the
list item names are used as variable names.
}}

\item{lookup_env}{The environment holding the variables for which the names
are defined in the  character vector \code{vars}.
If \code{vars} is a list or \code{NULL}, then \code{lookup_env} is not used.
The default for \code{lookup_env} is the environment where the function \code{fn}
was defined.}

\item{parent_env}{The parent environment, which should be assigned to
the restricted function. This argument is very important, since it
determines which objects will be available inside of your function.
Usually one of the following two possibilities is the right choice for \code{parent_env}:
\itemize{
\item \code{parent_env = .GlobalEnv} (default): The \strong{global environment} is usually the right
choice for a function, which does not use any non-exported functions
or any imported functions of any R package.
This is usually the case, when the function is created outside
of any R package (e.g. not inside of a function that is part of some R package).
\item \code{parent_env = "MY_PKG"}: This is usually the right choice when you are
developing a new R package and want to create a restricted function
inside of another function of this package
(in this example the package name is \code{MY_PKG}).
This ensures that also non-exported functions and imported functions of
\code{MY_PKG} are available inside of your
restricted function as well as the \strong{global environment}.
}}
}
\value{
A new function with a small scope containing the variables given
in \code{vars}.
}
\description{
Build a new function with an optimal scope. Normally the entire
environment tree (including the entire ancestry) is kept in memory as long
as a function is present. This is a common cause for memory leaks.
With \code{restrict_fn_env()} you create a function that has only
the needed variables (copies) in the scope and the optimal scope ancestry can
be defined by argument \code{parent_env}.
Be sure, to choose the right environment for \code{parent_env}!
}
