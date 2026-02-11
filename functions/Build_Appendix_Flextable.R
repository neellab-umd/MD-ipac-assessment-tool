library(readxl)
library(flextable)
library(officer)
library(tibble)

build_appendix_table <-function() {
# Read the Appendix Template Excel file
df <- read_excel(here("core_assets", "Appendix_template.xlsx"))  # Adjust sheet number/name as needed

##Create the Subrank Interval Grade Scale lookup table.

Subrank_Interval_grade_scale <- tribble(
  ~section,   ~grade, ~min, ~max,
  "Q1–Q5",    "A",     78,   102,
  "Q1–Q5",    "B",     52,    77,
  "Q1–Q5",    "C",     27,    51,
  "Q1–Q5",    "D",      0,    26,

  "Q6–Q9",    "A",     28,    36,
  "Q6–Q9",    "B",     19,    27,
  "Q6–Q9",    "C",     10,    18,
  "Q6–Q9",    "D",      0,     9,

  "Q10–Q13",  "A",     28,    36,
  "Q10–Q13",  "B",     19,    27,
  "Q10–Q13",  "C",     10,    18,
  "Q10–Q13",  "D",      0,     9,

  "Q14–Q18",  "A",     39,    51,
  "Q14–Q18",  "B",     27,    38,
  "Q14–Q18",  "C",     14,    26,
  "Q14–Q18",  "D",      0,    13
)

##Create the Subrank Interval Grade Scale lookup table.

Irank_Interval_grade_scale <- tribble(
  ~grade, ~min, ~max,
    "A",     76,   100,
    "B",     51,    75,
    "C",     26,    50,
    "D",      0,    25)

## Create a Terminology crosswalk

rank_terminology <-tribble(
  ~rank, ~term,
  "A",     "High",
  "B",     "Medium",
  "C",     "Low",
  "D",     "Insignificant")

## get the grades from the evidence_file list that includes all the assessor scores. That list is initially created by the Question_Evidence(). It is later updated by merge_word_evidence().  It will only be completed if the assessor input has been merged

df <- df %>%
  mutate(
    `Grade Assigned` = map_chr(Question, function(q) {
      if (grepl("[0-9]$", q, useBytes = TRUE)) {
        grade_key <- paste0(q, ".grade")
        if (grade_key %in% names(evidence_list)) {
          return(evidence_list[[grade_key]])
        }
      }
      return("")
    })
  )

## Start building df2 - add helper columns
df2 <- df %>%
  mutate(
    q_num = as.integer(sub("Q", "", Question)),
    section = case_when(
      q_num %in% 1:5   ~ "Q1–Q5",
      q_num %in% 6:9   ~ "Q6–Q9",
      q_num %in% 10:13 ~ "Q10–Q13",
      q_num %in% 14:18 ~ "Q14–Q18"
    )
  )

## Assign the number of points earned for each question given the grade
df2 <- df2 %>%
  rowwise() %>%
  mutate(
    points_earned = c_across(starts_with("Possible Points"))[
      match(`Grade Assigned`, c("A", "B", "C", "D"))
    ]
  )

## Compute Point Totals for all questions in each question group (signified by section)
section_summary <- df2 %>%
  group_by(section) %>%
  summarise(
    section_points = sum(points_earned, na.rm = TRUE),
    .groups = "drop"
  )

## Add the grade for each section to the section_summary
section_summary <- section_summary %>%
  left_join(Subrank_Interval_grade_scale, by = "section") %>%
  filter(section_points >= min, section_points <= max) %>%
  transmute(
    section,
    section_total = section_points,
    Subrank_grade = grade
  )

## Add section point totals and subrank grade to df2
df2 <- df2 %>%
  left_join(section_summary, by = "section")

## Populate Total Points in Section and Subrank columns (first row of each section)
df2 <- df2 %>%
  group_by(section) %>%
  mutate(
    `Total Points in Section` = if_else(row_number() == 1, section_total, NA_real_),
    Subrank = if_else(row_number() == 1, Subrank_grade, NA_character_)
  ) %>%
  ungroup()

## Assign Subrank points for each grade
df2 <- df2 %>%
  rowwise() %>%
  mutate(
    Subrank_points_earned = c_across(starts_with("Subrank Values"))[
      match(`Subrank`, c("A", "B", "C", "D"))
    ]
  ) %>%
  ungroup()

## Compute Total Subrank Points (ONE number)
total_subrank_points <- sum(df2$Subrank_points_earned, na.rm = TRUE)

## Get the IRank grade (ONE letter)
IRank_grade <- Irank_Interval_grade_scale %>%
  filter(total_subrank_points >= min, total_subrank_points <= max) %>%
  pull(grade)

## Add Total Subrank Points and I-Rank to first row only
df2 <- df2 %>%
  mutate(
    `Total Subrank Points` = if_else(row_number() == 1,
                                     as.numeric(total_subrank_points),
                                     NA_real_),
    `I-Rank` = if_else(row_number() == 1,
                       IRank_grade,
                       NA_character_)
  )


# Store the Actual Subrank Grades BEFORE removing columns
subrank_1 <- df2$Subrank[1]
subrank_6 <- df2$Subrank[6]
subrank_10 <- df2$Subrank[10]
subrank_14 <- df2$Subrank[14]

IRankInterval <- df2$`I-Rank`[1]

# Store the Subrank Point Values for each grade in each section
subrank_vals_A_1 <- as.character(df2$`Subrank Values A`[1])
subrank_vals_B_1 <- as.character(df2$`Subrank Values B`[1])
subrank_vals_C_1 <- as.character(df2$`Subrank Values C`[1])
subrank_vals_D_1 <- as.character(df2$`Subrank Values D`[1])

subrank_vals_A_6 <- as.character(df2$`Subrank Values A`[6])
subrank_vals_B_6 <- as.character(df2$`Subrank Values B`[6])
subrank_vals_C_6 <- as.character(df2$`Subrank Values C`[6])
subrank_vals_D_6 <- as.character(df2$`Subrank Values D`[6])

subrank_vals_A_10 <- as.character(df2$`Subrank Values A`[10])
subrank_vals_B_10 <- as.character(df2$`Subrank Values B`[10])
subrank_vals_C_10 <- as.character(df2$`Subrank Values C`[10])
subrank_vals_D_10 <- as.character(df2$`Subrank Values D`[10])

subrank_vals_A_14 <- as.character(df2$`Subrank Values A`[14])
subrank_vals_B_14 <- as.character(df2$`Subrank Values B`[14])
subrank_vals_C_14 <- as.character(df2$`Subrank Values C`[14])
subrank_vals_D_14 <- as.character(df2$`Subrank Values D`[14])

## Create objects for inline text values
inline.summary <- tibble(
  section = c("Ecological Impact", "Current Distribution and Abundance",
              "Trend in Distribution and Abundance", "Management Difficulty"),
  section_code = c("EI", "CDA", "TDA", "MD"),
  sum_section_points = c(
    df2$`Total Points in Section`[1],
    df2$`Total Points in Section`[6],
    df2$`Total Points in Section`[10],
    df2$`Total Points in Section`[14]
  ),
  subrank = c(
    df2$Subrank[1],
    df2$Subrank[6],
    df2$Subrank[10],
    df2$Subrank[14]
  ),
  section_subrank_points = c(
    df2$Subrank_points_earned[1],
    df2$Subrank_points_earned[6],
    df2$Subrank_points_earned[10],
    df2$Subrank_points_earned[14]
  ),
  irank = NA_character_        # Initialize as NA for all section rows
) %>%
  add_row(
    section = "Overall",
    section_code = "Total",
    sum_section_points = NA_real_,
    subrank = NA_character_,
    section_subrank_points = df2$`Total Subrank Points`[1],
    irank = df2$`I-Rank`[1],                          # Only Overall gets this value
    .before = 1
  ) %>%
  left_join(rank_terminology, by = c("subrank" = "rank")) %>%
  rename(subrank_term = term) %>%
  left_join(rank_terminology, by = c("irank" = "rank")) %>%
  rename(irank_term = term)

inline_values <- inline.summary %>%
  select(-section) %>%
  mutate(across(everything(), as.character)) %>%  # Convert all to character first
  pivot_longer(-section_code, names_to = "metric", values_to = "value") %>%
  mutate(name = paste((section_code), metric, sep = "_")) %>%
  select(name, value) %>%
  deframe()

## NOW remove all helper columns at the very end
df2 <- df2 %>%
  select(-c(q_num, section, points_earned, section_total, Subrank_grade, Subrank_points_earned))

# Add the two-level header structure
ft <- df2 %>%
  flextable(.) %>%
# Then do your merge_at() calls
  merge_at(i = 1:5, j = 9, part = "body") %>%
  # Set the second header row labels to just the letters
  set_header_labels(
    `Possible Points A` = "A",
    `Possible Points B` = "B",
    `Possible Points C` = "C",
    `Possible Points D` = "D",
    `Subrank Values A` = "A",
    `Subrank Values B` = "B",
    `Subrank Values C` = "C",
    `Subrank Values D` = "D"
  ) %>%
  add_header_row(
    values = c("Section", "Question", "Grade Assigned",
               "PossiblePoints",
               "Total Points in Section", "Subrank Interval", "Subrank",
               "Subrank\u00A0Values",
               "Total Subrank Points", "I-Rank Intervals", "I-Rank"),
    colwidths = c(1, 1, 1, 4, 1, 1, 1, 4, 1, 1, 1)
  ) %>%
  merge_h(part = "header", i = 1) %>%
  # Merge vertically for columns that don't have sub-headers
  merge_v(part = "header", j = c("Section", "Question", "Grade Assigned",
                                 "Total Points in Section", "Subrank Interval",
                                 "Subrank", "Total Subrank Points",
                                 "I-Rank Intervals", "I-Rank")) %>%
# format the combined content for Subrank Interval (column 9) before merging
flextable::compose(i = 1, j = 9,
                     value = as_paragraph(
                       as_chunk("78-102 (A)\n",
                                props = fp_text(shading.color =
                                                  ifelse(subrank_1 == "A",
                                                         "yellow", "transparent"))),
                       as_chunk("52-77 (B)\n",
                                props = fp_text(shading.color =
                                                  ifelse(subrank_1 == "B",
                                                         "yellow", "transparent"))),
                       as_chunk("27-51 (C)\n",
                                props = fp_text(shading.color =
                                                  ifelse(subrank_1 == "C",
                                                         "yellow", "transparent"))),
                       as_chunk("0-26 (D)",
                                props = fp_text(shading.color =
                                                  ifelse(subrank_1 == "D",
                                                         "yellow", "transparent")))
                     )) %>%
  # Q6-Q9 section (row 6)
  flextable::compose(i = 6, j = 9,
                     value = as_paragraph(
                       as_chunk("28-36 (A)\n",
                                props = fp_text(shading.color =
                                                  ifelse(subrank_6 == "A",
                                                         "yellow", "transparent"))),
                       as_chunk("19-27 (B)\n",
                                props = fp_text(shading.color =
                                                  ifelse(subrank_6 == "B",
                                                         "yellow", "transparent"))),
                       as_chunk("10-18 (C)\n",
                                props = fp_text(shading.color =
                                                  ifelse(subrank_6 == "C",
                                                         "yellow", "transparent"))),
                       as_chunk("0-9 (D)",
                                props = fp_text(shading.color =
                                                  ifelse(subrank_6 == "D",
                                                         "yellow", "transparent")))
                     )) %>%
  # Q10-Q13 section (row 10)
  flextable::compose(i = 10, j = 9,
                     value = as_paragraph(
                       as_chunk("28-36 (A)\n",
                                props = fp_text(shading.color =
                                                  ifelse(subrank_10 == "A",
                                                         "yellow", "transparent"))),
                       as_chunk("19-27 (B)\n",
                                props = fp_text(shading.color =
                                                  ifelse(subrank_10 == "B",
                                                         "yellow", "transparent"))),
                       as_chunk("10-18 (C)\n",
                                props = fp_text(shading.color =
                                                  ifelse(subrank_10 == "C",
                                                         "yellow", "transparent"))),
                       as_chunk("0-9 (D)",
                                props = fp_text(shading.color =
                                                  ifelse(subrank_10 == "D",
                                                         "yellow", "transparent")))
                     )) %>%
  # Q14-Q18 section (row 14)
  flextable::compose(i = 14, j = 9,
                     value = as_paragraph(
                       as_chunk("39-51 (A)\n",
                                props = fp_text(shading.color =
                                                  ifelse(subrank_14 == "A", "yellow", "transparent"))),
                       as_chunk("27-38 (B)\n",
                                props = fp_text(shading.color =
                                                  ifelse(subrank_14 == "B", "yellow", "transparent"))),
                       as_chunk("14-26 (C)\n",
                                props = fp_text(shading.color =
                                                  ifelse(subrank_14 == "C", "yellow", "transparent"))),
                       as_chunk("0-13 (D)",
                                props = fp_text(shading.color =
                                                  ifelse(subrank_14 == "D", "yellow", "transparent")))
                     )) %>%
  flextable::compose(i = 1, j = 9,
                     value = as_paragraph(
                       as_chunk("78-102 (A)\n",
                                props = fp_text(shading.color =
                                                  ifelse(subrank_1 == "A",
                                                         "yellow", "transparent"))),
                       as_chunk("52-77 (B)\n",
                                props = fp_text(shading.color =
                                                  ifelse(subrank_1 == "B",
                                                         "yellow", "transparent"))),
                       as_chunk("27-51 (C)\n",
                                props = fp_text(shading.color =
                                                  ifelse(subrank_1 == "C",
                                                         "yellow", "transparent"))),
                       as_chunk("0-26 (D)",
                                props = fp_text(shading.color =
                                                  ifelse(subrank_1 == "D",
                                                         "yellow", "transparent")))
                     )) %>%
  ## Compose I-rank intervals
  flextable::compose(i = 1, j = 16,
                     value = as_paragraph(
                       as_chunk("76-100 (A)\n",
                                props = fp_text(shading.color =
                                                  ifelse(IRankInterval == "A",
                                                         "yellow", "transparent"))),
                       as_chunk("51-75 (B)\n",
                                props = fp_text(shading.color =
                                                  ifelse(IRankInterval == "B",
                                                         "yellow", "transparent"))),
                       as_chunk("26-50 (C)\n",
                                props = fp_text(shading.color =
                                                  ifelse(IRankInterval == "C",
                                                         "yellow", "transparent"))),
                       as_chunk("0-25 (D)",
                                props = fp_text(shading.color =
                                                  ifelse(IRankInterval == "D",
                                                        "yellow", "transparent")))
                     )) %>%
# Merge body rows for Sections and columns 8-14
  merge_at(i = 1:5, j = 1, part = "body") %>%
  merge_at(i = 1:5, j = 8, part = "body") %>%
  merge_at(i = 1:5, j = 9, part = "body") %>%
  merge_at(i = 1:5, j = 10, part = "body") %>%
  merge_at(i = 1:5, j = 11, part = "body") %>%
  merge_at(i = 1:5, j = 12, part = "body") %>%
  merge_at(i = 1:5, j = 13, part = "body") %>%
  merge_at(i = 1:5, j = 14, part = "body") %>%
  merge_at(i = 1:18, j = 16, part = "body") %>%
# Merge rows 6-9
  merge_at(i = 6:9, j = 1, part = "body") %>%
  merge_at(i = 6:9, j = 8, part = "body") %>%
  merge_at(i = 6:9, j = 9, part = "body") %>%
  merge_at(i = 6:9, j = 10, part = "body") %>%
  merge_at(i = 6:9, j = 11, part = "body") %>%
  merge_at(i = 6:9, j = 12, part = "body") %>%
  merge_at(i = 6:9, j = 13, part = "body") %>%
  merge_at(i = 6:9, j = 14, part = "body") %>%
# Merge rows 10-13
  merge_at(i = 10:13, j = 1, part = "body") %>%
  merge_at(i = 10:13, j = 8, part = "body") %>%
  merge_at(i = 10:13, j = 9, part = "body") %>%
  merge_at(i = 10:13, j = 10, part = "body") %>%
  merge_at(i = 10:13, j = 11, part = "body") %>%
  merge_at(i = 10:13, j = 12, part = "body") %>%
  merge_at(i = 10:13, j = 13, part = "body") %>%
  merge_at(i = 10:13, j = 14, part = "body") %>%
# Merge rows 14-18
  merge_at(i = 14:18, j = 1, part = "body") %>%
  merge_at(i = 14:18, j = 8, part = "body") %>%
  merge_at(i = 14:18, j = 9, part = "body") %>%
  merge_at(i = 14:18, j = 10, part = "body") %>%
  merge_at(i = 14:18, j = 11, part = "body") %>%
  merge_at(i = 14:18, j = 12, part = "body") %>%
  merge_at(i = 14:18, j = 13, part = "body") %>%
  merge_at(i = 14:18, j = 14, part = "body") %>%
# DON'T merge columns 15-17, or use merge_v() to keep content from matching rows
  merge_at(i = 1:18, j = 15, part = "body") %>%
  merge_at(i = 1:18, j = 17, part = "body") %>%


#  Here's the complete code for all the Subrank Values compose calls that you can plug in:
# Q1-Q5 section (rows 1-5) - apply to row 1
flextable::compose(i = 1, j = 11,
                   value = as_paragraph(
                     as_chunk(subrank_vals_A_1,
                              props = fp_text(shading.color =
                                                ifelse(subrank_1 == "A",
                                                       "yellow", "transparent")))
                   )) %>%
flextable::compose(i = 1, j = 12,
                   value = as_paragraph(
                     as_chunk(subrank_vals_B_1,
                              props = fp_text(shading.color =
                                                ifelse(subrank_1 == "B",
                                                       "yellow", "transparent")))
                   )) %>%
flextable::compose(i = 1, j = 13,
                   value = as_paragraph(
                     as_chunk(subrank_vals_C_1,
                              props = fp_text(shading.color =
                                                ifelse(subrank_1 == "C",
                                                       "yellow", "transparent")))
                   )) %>%
flextable::compose(i = 1, j = 14,
                   value = as_paragraph(
                     as_chunk(subrank_vals_D_1,
                              props = fp_text(shading.color =
                                                ifelse(subrank_1 == "D",
                                                       "yellow", "transparent")))
                   )) %>%

# Q6-Q9 section (rows 6-9) - apply to row 6
flextable::compose(i = 6, j = 11,
                   value = as_paragraph(
                     as_chunk(subrank_vals_A_6,
                              props = fp_text(shading.color =
                                                ifelse(subrank_6 == "A",
                                                       "yellow", "transparent")))
                   )) %>%
flextable::compose(i = 6, j = 12,
                   value = as_paragraph(
                     as_chunk(subrank_vals_B_6,
                              props = fp_text(shading.color =
                                                ifelse(subrank_6 == "B",
                                                       "yellow", "transparent")))
                   )) %>%
flextable::compose(i = 6, j = 13,
                   value = as_paragraph(
                     as_chunk(subrank_vals_C_6,
                              props = fp_text(shading.color =
                                                ifelse(subrank_6 == "C",
                                                       "yellow", "transparent")))
                   )) %>%
flextable::compose(i = 6, j = 14,
                   value = as_paragraph(
                     as_chunk(subrank_vals_D_6,
                              props = fp_text(shading.color =
                                                ifelse(subrank_6 == "D",
                                                       "yellow", "transparent")))
                   )) %>%

# Q10-Q13 section (rows 10-13) - apply to row 10
flextable::compose(i = 10, j = 11,
                   value = as_paragraph(
                     as_chunk(subrank_vals_A_10,
                              props = fp_text(shading.color =
                                                ifelse(subrank_10 == "A",
                                                       "yellow", "transparent")))
                   )) %>%
flextable::compose(i = 10, j = 12,
                   value = as_paragraph(
                     as_chunk(subrank_vals_B_10,
                              props = fp_text(shading.color =
                                                ifelse(subrank_10 == "B",
                                                       "yellow", "transparent")))
                   )) %>%
flextable::compose(i = 10, j = 13,
                   value = as_paragraph(
                     as_chunk(subrank_vals_C_10,
                              props = fp_text(shading.color =
                                                ifelse(subrank_10 == "C",
                                                       "yellow", "transparent")))
                   )) %>%
flextable::compose(i = 10, j = 14,
                   value = as_paragraph(
                     as_chunk(subrank_vals_D_10,
                              props = fp_text(shading.color =
                                                ifelse(subrank_10 == "D",
                                                       "yellow", "transparent")))
                   )) %>%

# Q14-Q18 section (rows 14-18) - apply to row 14
flextable::compose(i = 14, j = 11,
                   value = as_paragraph(
                     as_chunk(subrank_vals_A_14,
                              props = fp_text(shading.color =
                                                ifelse(subrank_14 == "A",
                                                       "yellow", "transparent")))
                   )) %>%
flextable::compose(i = 14, j = 12,
                   value = as_paragraph(
                     as_chunk(subrank_vals_B_14,
                              props = fp_text(shading.color =
                                                ifelse(subrank_14 == "B",
                                                       "yellow", "transparent")))
                   )) %>%
flextable::compose(i = 14, j = 13,
                   value = as_paragraph(
                     as_chunk(subrank_vals_C_14,
                              props = fp_text(shading.color = ifelse(subrank_14 == "C", "yellow", "transparent")))
                   )) %>%
flextable::compose(i = 14, j = 14,
                   value = as_paragraph(
                     as_chunk(subrank_vals_D_14,
                              props = fp_text(shading.color =
                                                ifelse(subrank_14 == "D",
                                                       "yellow", "transparent")))
                   )) %>%
  theme_box() %>%
  flextable::align(part = "header", align = "center") %>%
  flextable::valign(part = "header", valign = "bottom") %>%
  flextable::align(part = "body", j = c(1,3:17), align = "center") %>%
  flextable::valign(part = "body", j = c(15, 17), valign = "center") %>%  # Explicitly center these columns
  flextable::valign(part = "body", valign = "center") %>%  # Then all body cells
  # Remove internal vertical borders for Possible Points (cols 4-7) within each group
  vline(j = 4:6, border = fp_border(width = 0), part = "header") %>%
  vline(i = 1:5, j = 4:6, border = fp_border(width = 0), part = "body") %>%
  vline(i = 6:9, j = 4:6, border = fp_border(width = 0), part = "body") %>%
  vline(i = 10:13, j = 4:6, border = fp_border(width = 0), part = "body") %>%
  vline(i = 14:18, j = 4:6, border = fp_border(width = 0), part = "body") %>%
  # Remove internal vertical borders for Subrank Values (cols 11-14) within each group
  vline(j = 11:13, border = fp_border(width = 0), part = "header") %>%
  vline(i = 1:5, j = 11:13, border = fp_border(width = 0), part = "body") %>%
  vline(i = 6:9, j = 11:13, border = fp_border(width = 0), part = "body") %>%
  vline(i = 10:13, j = 11:13, border = fp_border(width = 0), part = "body") %>%
  vline(i = 14:18, j = 11:13, border = fp_border(width = 0), part = "body") %>%
  flextable::width(j = c(1,9,16), width = .9) %>%
  flextable::width(j = c(2,3), width = 0.8) %>%
  flextable::width(j = c(8,15,17), width = 0.7) %>%
  flextable::width(j = c(4:7, 11:14), width = 0.3) %>%
  flextable::set_table_properties(layout = "fixed", width = 1) %>%
  flextable::font(fontname = "Aptos Narrow") %>%
  flextable::fontsize(size = 10, part = "all") %>%
  flextable::line_spacing(space = 0.9, part = "all") %>%
  flextable::padding(
    padding.top = 2,
    padding.bottom = 2,
    part = "all"
  ) %>%

  bg(i = 1:5, j = 1:14, bg = "#B7DAA2", part = "body") %>%
  # Apply background colors for rows 6-9 (purple)
  bg(i = 6:9, j = 1:14, bg = "#d18ed2", part = "body") %>%
  # Apply background colors for rows 10-13 (blue)
  bg(i = 10:13, j = 1:14, bg = "#94e4f0", part = "body") %>%
  # Apply background colors for rows 14-18 (orange)
  bg(i = 14:18, j = 1:14, bg = "#F2B591", part = "body") %>%
  bg(i = 1:18, j = 15:17, bg = "gray90", part = "body")%>%
  bg(i = ~ `Grade Assigned` == "A", j = 4, bg = "yellow") %>%
  bg(i = ~ `Grade Assigned` == "B", j = 5, bg = "yellow") %>%
  bg(i = ~ `Grade Assigned` == "C", j = 6, bg = "yellow") %>%
  bg(i = ~ `Grade Assigned` == "D", j = 7, bg = "yellow")





return(list(inline.scores = inline_values,
            appendix.table = ft))
}

