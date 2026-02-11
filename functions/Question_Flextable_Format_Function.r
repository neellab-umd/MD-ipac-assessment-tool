##Question Flextable Function
##
format_question_flextable <- function(df) {

  #flextable::set_flextable_defaults(font.size = 11)  run without this and see if it can be removed.
   flextable(df) %>%

    flextable::theme_booktabs() %>%
    flextable::font(fontname = "Aptos") %>%
    flextable::fontsize(size = 11, part = "all") %>%
    flextable::bold(bold = TRUE, part = "header") %>%
    bold( ## bold just the main questions
      i = ~ grepl("[0-9]$", `Question Number`, useBytes = TRUE),
      j = c("Body of Questions/Subquestions", "Grade"),
      bold = TRUE) %>%
    # Set column widths manually (in inches)
    flextable::width(j = 1, width = 0.85) %>%   # Question Number
    flextable::width(j = 2, width = 2.5) %>%   # Body of Questions/Subquestions
    flextable::width(j = 3, width = 0.65) %>%   # Grade
    flextable::width(j = 4, width = 2.5) %>%   # Evidence
    # Borders around all cells
    flextable::border_remove() %>%
    flextable::border_outer(part = "all", border = fp_border(color = "black", width = 2)) %>%
    flextable::border_inner_h(part = "all", border = fp_border(color = "black", width = 0.5)) %>%
    flextable::border_inner_v(part = "all", border = fp_border(color = "black", width = 0.5)) %>%
    # Formatting: left-align text, top vertical align
    flextable::align(align = "left", part = "body") %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::valign(valign = "top", part = "body") %>%
    flextable::valign(valign = "bottom", part = "header") %>%
    flextable::align(j = "Grade", align = "center", part = "body") %>%
    flextable::line_spacing(space = 1, part = "all") %>%
    flextable::set_table_properties(layout = "fixed",
                                    opts_word = list(split = FALSE,
                                                     repeat_headers = TRUE))

}