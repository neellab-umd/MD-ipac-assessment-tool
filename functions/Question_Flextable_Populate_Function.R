## This function populates the question tables with questions, evidence, and grades.
## It is called from within the Quarto document for each question table.


populate_question_flextable <- function(question_range, custom_bodies, evidence_list) {
  # Create the regex pattern for filtering
  q_numbers <- paste0("Q", question_range, collapse = "|")
  pattern <- paste0("^(", q_numbers, ")(\\D|$)")

  # Start with base flextable
  ft <- question_scaffold %>%
    filter(str_detect(`Question Number`, pattern)) %>%
    format_question_flextable(.)

  # Get the data
  ft_data <- ft$body$dataset

  # Apply body compositions using row indices
  for (q_num in names(custom_bodies)) {
    row_idx <- which(ft_data$`Question Number` == q_num)
    if (length(row_idx) > 0) {
      ft <- flextable::compose(
        ft,
        i = row_idx,
        j = "Body of Questions/Subquestions",
        value = custom_bodies[[q_num]]
      )
    }
  }

  # Apply Supporting Evidence
  evidence_questions <- names(evidence_list)[grepl("\\.evidence$", names(evidence_list),useBytes = TRUE)]
  for (ev_name in evidence_questions) {
    q_num <- sub("\\.evidence$", "", ev_name)
    row_idx <- which(ft_data$`Question Number` == q_num)
    if (length(row_idx) > 0) {
      ft <- flextable::compose(
        ft,
        i = row_idx,
        j = "Supporting Evidence",
        value = evidence_list[[ev_name]]
      )
    }
  }

  # Apply Grades
  grade_questions <- names(evidence_list)[grepl("\\.grade$", names(evidence_list),useBytes = TRUE)]
  for (gr_name in grade_questions) {
    q_num <- sub("\\.grade$", "", gr_name)
    row_idx <- which(ft_data$`Question Number` == q_num)
    if (length(row_idx) > 0) {
      ft <- flextable::compose(
        ft,
        i = row_idx,
        j = "Grade",
        value = as_paragraph(as_chunk(evidence_list[[gr_name]]))
      )
    }
  }

  return(ft)
}
