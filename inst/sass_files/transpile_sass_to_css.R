library(dplyr)
devtools::load_all()

sass_files <- list.files(
  system.file("sass_files", package = "styledTables"),
  pattern = ".*\\.sass$",
  full.names = TRUE
)

css_files <- c()
for (i in seq_len(length(sass_files))) {
  css_files[i] <- sass::sass(
    sass::sass_file(sass_files[i]),
    output = file.path(
      system.file("stylesheets", package = "styledTables"),
      paste0(xfun::sans_ext(basename(sass_files[i])), ".min.css")
    ),
    options = sass::sass_options(output_style = "compressed"),
    cache = FALSE
  )
}
css_files

