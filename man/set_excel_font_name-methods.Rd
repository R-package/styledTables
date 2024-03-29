% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_styled_table.R
\name{set_excel_font_name}
\alias{set_excel_font_name}
\alias{set_excel_font_name,StyledTable-method}
\alias{set_excel_font_name,StyledTable,character-method}
\title{Set font name for the Excel tables}
\usage{
set_excel_font_name(st, value, ...)

\S4method{set_excel_font_name}{StyledTable}(
  st,
  value,
  row_id = NULL,
  col_id = NULL,
  condition = NULL,
  condition_text = NULL
)
}
\arguments{
\item{st}{A \link{StyledTable} object}

\item{value}{The value that should be set}

\item{...}{Various arguments}

\item{row_id}{A vector of row ids to which the change should be applied to. The variable \code{n_row} can be used in the expression to name the total number of columns (will be replaced). If the argument \code{row_id} is omitted, then the change will be applied to all rows}

\item{col_id}{A vector of column ids to which the change should be applied to. The variable \code{n_col} can be used in the expression to name the total number of columns (will be replaced). If the argument \code{col_id} is omitted, then the change will be applied to all columns}

\item{condition}{An equation (non standard evaluation) that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the \link{StyledTable} object should be used. Be aware that the \link{StyledTable} object columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{col_id} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{condition_text} and \code{condition} cannot be passed at the same time.}

\item{condition_text}{An character string holding an equation that is evaluated on the data.frame \code{data} in order to decide on which rows or cells the change should be applied. In the equation the column names of the \link{StyledTable} object should be used. Be aware that the \link{StyledTable} object columns have the names \code{X1}, ..., \code{XN}, where \code{N} is the total number of columns. If you do not want to apply the change to all columns given in the \code{col_id} argument, but only to single cells which fullfill a special condition you can use also the variable name \code{X} which is substituted by each column name separately. The arguments \code{condition_text} and \code{condition} cannot be passed at the same time.}
}
\value{
The modified \link{StyledTable} object
}
\description{
Set font name for the Excel tables
}
\seealso{
Other styledtable setters: 
\code{\link{set_bold}()},
\code{\link{set_border_color}()},
\code{\link{set_border_position}()},
\code{\link{set_excel_boldweight}()},
\code{\link{set_excel_border_pen}()},
\code{\link{set_excel_data_format}()},
\code{\link{set_excel_font_size}()},
\code{\link{set_excel_hidden}()},
\code{\link{set_excel_locked}()},
\code{\link{set_excel_pre_process}()},
\code{\link{set_excel_vertical}()},
\code{\link{set_excel_wrapped}()},
\code{\link{set_fill_color}()},
\code{\link{set_font_color}()},
\code{\link{set_horizontal}()},
\code{\link{set_html_pre_process}()},
\code{\link{set_indent}()},
\code{\link{set_italic}()},
\code{\link{set_latex_font_name}()},
\code{\link{set_latex_font_size}()},
\code{\link{set_latex_pre_process}()},
\code{\link{set_latex_vertical_move}()},
\code{\link{set_rotation}()},
\code{\link{set_specific_style}()},
\code{\link{set_strikeout}()},
\code{\link{set_underline}()}
}
\concept{styledtable setters}
