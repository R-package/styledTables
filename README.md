
# styledTables <img src="man/figures/logo.png" align="right" alt="" width=140 height=162 />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/R-package/styledTables.svg?branch=master)](https://travis-ci.org/R-package/styledTables)
[![GitHub last
commit](https://img.shields.io/github/last-commit/R-package/styledTables.svg?logo=github)](https://github.com/R-package/styledTables/commits/master)
[![GitHub code size in
bytes](https://img.shields.io/github/languages/code-size/R-package/styledTables.svg?logo=github)](https://github.com/R-package/styledTables)

`styledTables` is an **R** package that makes it possible to export a
`data.frame` as beautifully styled **Excel** and **LaTeX** table.

> Produce tables, which you want to look at.

Have a look at the \[get started vignette\] to see `styledTables` in
action.

## Caution

The `styledTables` package is deprecated and will soon be relaunched
under the package name `styledtable`. This package will have its main
focus on HTML based reports (see <https://pagedown.rbind.io/>), which
are fully accessible to assistive reading devices and come along with
all the nice bookbinding features that LaTeX offers. Unfortunately,
there won’t be any more support for this legacy package.

If you still want to use the deprecated `styledTables` package:

``` r
# Install last stable version from GitHub
devtools::install_github('R-package/styledTables', build_opts = NULL)
```

## License

\[GPL-3\]
