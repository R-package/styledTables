% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_html.R
\name{create_html}
\alias{create_html}
\title{Create the \strong{html} code of a \strong{styledTable}}
\usage{
create_html(
  st,
  caption = NULL,
  footer = NULL,
  indent_level = 0L,
  use_scope = TRUE
)
}
\arguments{
\item{st}{A \link[=styled_table]{StyledTable} class object}

\item{caption}{An optional string holding the table caption}

\item{footer}{An optional character vector holding the table footer remarks}

\item{indent_level}{An optional integer defining, the indentation level of
the resulting html code}

\item{use_scope}{An optional flag, defining if the html table should
also use the \code{scope} property. This property is an additional method
for improving the accessibility of an html table.
By default this is turned off, since the usage of \code{scope} does not allow
\strong{sub headings} and accessibility is already ensured by assigning
unique header ids to the \code{id} property of the header cells and
linking data cells via their \code{headers} property to the corresponding header
cells.}
}
\value{
A string holding the entire \strong{html} code of the table.
}
\description{
Create the \strong{html} code of a \strong{styledTable}
}
