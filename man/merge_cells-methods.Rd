% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_cells.R
\name{merge_cells}
\alias{merge_cells}
\alias{merge_cells,StyledTable-method}
\alias{merge_cells,StyledTable,character-method}
\title{Merge multiple cells in a \link{StyledTable} object}
\usage{
merge_cells(st, ...)

\S4method{merge_cells}{StyledTable}(st, row_id = NULL, col_id = NULL)
}
\arguments{
\item{st}{A \link{StyledTable} object}

\item{...}{Various arguments}

\item{row_id}{A vector holding two numbers, the start row and the end row of the merged region}

\item{col_id}{A vector holding two numbers, the start col and the end col of the merged region}
}
\value{
The modified \link{StyledTable} object
}
\description{
Merge multiple cells in a \link{StyledTable} object
}
\seealso{
\code{\link[=merge_equal_cells]{merge_equal_cells()}}

\code{\link[=remove_col]{remove_col()}}, \code{\link[=remove_row]{remove_row()}}
}
