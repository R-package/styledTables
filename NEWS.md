# CHANGES IN styledTalbes VERSION 2.5.3

- remove obsolete depedencies
- include needed function from `funky` package


# CHANGES IN styledTalbes VERSION 2.5.2

- fix minor bugs in documentation
- allow multiple `<tbody>` elements in the generated HTML table
- wrap the generated HTML table in a raw-html-block
- implement `set_html_pre_process()`

# CHANGES IN styledTalbes VERSION 2.5.1

- fix bug `set_html_pre_process()`
- improve `set_html_col_header()` and `set_html_row_header()`: Allow optional
  `row_id` and `col_id` arguments

# CHANGES IN styledTalbes VERSION 2.5.0

- changes in `create_html()`:
    - move col headers in `<thead>` element
    - fix bug in `<tfoot>`: wrong colspan attribute value
    - fix bug in headers-id-assignment in `<td>` and `<th>`
    - fix minor bug in `styledtable_theme1.sass`
- add `NEWS.md`

# CHANGES IN styledTalbes VERSION 2.4.0

- add `set_html_row_height()` and `set_html_col_width()`

# CHANGES IN styledTalbes VERSION 2.3.3

- fix minor bug in `create_html()`

# CHANGES IN styledTalbes VERSION 2.3.2

- remove outdated vignettes

# CHANGES IN styledTalbes VERSION 2.3.1

- remove obsolete code

# CHANGES IN styledTalbes VERSION 2.3.0

- add html export `create_html()`
- fix dependency problems for non-exported foreign functions