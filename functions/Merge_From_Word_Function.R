library(xml2)

# Helper function to extract tables with formatting from Word XML
extract_tables_with_formatting_from_xml <- function(word_file) {

  # Unzip the docx to access the XML
  temp_dir <- file.path(tempdir(), paste0("docx_extract_", format(Sys.time(), "%H%M%S")))
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

  tryCatch({
    unzip(word_file, exdir = temp_dir)

    # Read the main document XML
    doc_xml_path <- file.path(temp_dir, "word", "document.xml")

    if (!file.exists(doc_xml_path)) {
      warning("document.xml not found in Word file")
      return(list())
    }

    doc_xml <- xml2::read_xml(doc_xml_path)

    # Find all tables
    tables <- xml2::xml_find_all(doc_xml, ".//*[local-name()='tbl']")

    if (length(tables) == 0) {
      return(list())
    }

    tables_list <- list()

    for (tbl in tables) {
      # Get all rows
      rows <- xml2::xml_find_all(tbl, ".//*[local-name()='tr']")

      if (length(rows) == 0) next

      n_rows <- length(rows)

      # Determine number of columns from first row
      first_row_cells <- xml2::xml_find_all(rows[[1]], ".//*[local-name()='tc']")
      n_cols <- length(first_row_cells)

      if (n_cols == 0) next

      # Initialize structures
      data_matrix <- matrix("", nrow = n_rows, ncol = n_cols)
      formatted_cells <- vector("list", n_rows)
      for (i in 1:n_rows) {
        formatted_cells[[i]] <- vector("list", n_cols)
        for (j in 1:n_cols) {
          formatted_cells[[i]][[j]] <- list()
        }
      }

      # Process each row
      for (i in seq_along(rows)) {
        cells <- xml2::xml_find_all(rows[[i]], ".//*[local-name()='tc']")

        for (j in seq_along(cells)) {
          if (j > n_cols) break

          # Get all text runs in this cell
          runs <- xml2::xml_find_all(cells[[j]], ".//*[local-name()='r']")

          cell_text <- ""

          for (run in runs) {
            # Get text content
            text_nodes <- xml2::xml_find_all(run, ".//*[local-name()='t']")
            text <- paste(xml2::xml_text(text_nodes), collapse = "")

            if (text == "") next

            cell_text <- paste0(cell_text, text)

            # Check for italic
            italic_node <- xml2::xml_find_first(run, ".//*[local-name()='rPr']/*[local-name()='i']")
            is_italic <- !inherits(italic_node, "xml_missing")

            # Check for bold
            bold_node <- xml2::xml_find_first(run, ".//*[local-name()='rPr']/*[local-name()='b']")
            is_bold <- !inherits(bold_node, "xml_missing")

            # Store run with formatting
            run_info <- list(
              text = text,
              italic = is_italic,
              bold = is_bold
            )

            formatted_cells[[i]][[j]] <- c(formatted_cells[[i]][[j]], list(run_info))
          }

          data_matrix[i, j] <- cell_text
        }
      }

      # Convert to data frame
      data_df <- as.data.frame(data_matrix, stringsAsFactors = FALSE)

      # Use first row as column names
      if (n_rows > 1) {
        names(data_df) <- data_matrix[1, ]
        data_df <- data_df[-1, , drop = FALSE]
        formatted_cells <- formatted_cells[-1]
      }

      tables_list[[length(tables_list) + 1]] <- list(
        data = data_df,
        formatted_cells = formatted_cells
      )
    }

    return(tables_list)

  }, finally = {
    # Clean up temp files
    unlink(temp_dir, recursive = TRUE)
  })
}

# Main merge function
merge_word_evidence <- function(word_file, evidence_list, species_name, species_abb) {

  # Extract all tables with formatting preserved from XML
  tables_list <- extract_tables_with_formatting_from_xml(word_file)

  if (length(tables_list) == 0) {
    warning("No tables found in document")
    return(evidence_list)
  }

  # Identify question tables
  question_tables <- list()
  for (i in seq_along(tables_list)) {
    tbl <- tables_list[[i]]

    # Check if this is a question table by looking at column names
    col_names <- names(tbl$data)
    has_q_col <- any(grepl("Question.*Number", col_names, ignore.case = TRUE))
    has_ev_col <- any(grepl("Evidence", col_names, ignore.case = TRUE))
    has_gr_col <- any(grepl("Grade", col_names, ignore.case = TRUE))
    has_body_col <- any(grepl("Body.*Question", col_names, ignore.case = TRUE))

    if (has_q_col && has_ev_col && has_gr_col && has_body_col) {
      question_tables[[length(question_tables) + 1]] <- tbl
    }
  }

  if (length(question_tables) == 0) {
    warning("No question tables found in document")
    return(evidence_list)
  }

  # Process each question table
  for (tbl in question_tables) {
    # Get column indices
    col_names <- names(tbl$data)
    q_col_idx <- which(grepl("Question.*Number", col_names, ignore.case = TRUE))[1]
    ev_col_idx <- which(grepl("Evidence", col_names, ignore.case = TRUE))[1]
    gr_col_idx <- which(grepl("Grade", col_names, ignore.case = TRUE))[1]

    # Process each row
    for (i in 1:nrow(tbl$data)) {
      q_num <- trimws(tbl$data[i, q_col_idx])

      # Skip headers and empty rows
      if (is.na(q_num) || q_num == "" || grepl("^Question", q_num, ignore.case = TRUE)) {
        next
      }

      # Check if already processed
      grade_key <- paste0(q_num, ".grade")
      evidence_key <- paste0(q_num, ".evidence")

      if (grade_key %in% names(evidence_list) || evidence_key %in% names(evidence_list)) {
        next
      }

      # Add Grade
      grade_val <- tbl$data[i, gr_col_idx]
      if (!is.na(grade_val) && grade_val != "") {
        evidence_list[[grade_key]] <- grade_val
      }

      # Add Evidence with formatting preserved
      evidence_runs <- tbl$formatted_cells[[i]][[ev_col_idx]]
      if (!is.null(evidence_runs) && length(evidence_runs) > 0) {
        chunks <- list()
        for (run in evidence_runs) {
          if (!is.na(run$text) && run$text != "") {
            chunk <- flextable::as_chunk(
              run$text,
              props = officer::fp_text(
                italic = run$italic,
                bold = run$bold,
                font.size = 11,
                font.family = "Aptos"
              )
            )
            chunks <- append(chunks, list(chunk))
          }
        }
        if (length(chunks) > 0) {
          evidence_list[[evidence_key]] <- do.call(flextable::as_paragraph, chunks)
        }
      }
    }
  }

  ## Extract Literature Cited table
  lit_cited_table <- NULL
  for (i in seq_along(tables_list)) {
    tbl <- tables_list[[i]]
    col_names <- names(tbl$data)

    if (any(grepl("Literature.*Specific.*Assessed.*Species", col_names, ignore.case = TRUE))) {
      lit_cited_table <- tbl
      break
    }
  }

  # Process literature cited - returns character vector with asterisks for markdown
  if (!is.null(lit_cited_table) && nrow(lit_cited_table$data) > 0) {
    lit_citations <- character()

    for (i in 1:nrow(lit_cited_table$data)) {
      citation_text <- lit_cited_table$data[i, 1]

      # Skip empty rows
      if (is.na(citation_text) || citation_text == "") next

      # Get the formatted runs for this citation
      citation_runs <- lit_cited_table$formatted_cells[[i]][[1]]

      if (!is.null(citation_runs) && length(citation_runs) > 0) {
        # Rebuild citation with asterisks for italics (for markdown rendering)
        citation_parts <- character()
        for (run in citation_runs) {
          if (!is.na(run$text) && run$text != "") {
            if (run$italic) {
              citation_parts <- c(citation_parts, paste0("*", run$text, "*"))
            } else {
              citation_parts <- c(citation_parts, run$text)
            }
          }
        }
        lit_citations <- c(lit_citations, paste(citation_parts, collapse = ""))
      } else {
        # Fallback to plain text if no formatting info
        lit_citations <- c(lit_citations, citation_text)
      }
    }

    evidence_list$literature_cited <- lit_citations
  } else {
    warning("Literature cited table not found")
    evidence_list$literature_cited <- NULL
  }

  return(evidence_list)
}
