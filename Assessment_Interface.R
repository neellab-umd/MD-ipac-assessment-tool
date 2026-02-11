# Maryland Invasive Plant Assessment Protocol - Shiny Interface

library(shiny)
library(shinyjs)

# Source the functions
source("scripts/Query_Species_Location.R")
source("scripts/Render_Assessment.R")

# Read species list
all_species <- read.csv("./core_assets/List of species for MD IPSSA.csv")

# Find the species column
species_col <- if ("species_name" %in% names(all_species)) {
  "species_name"
} else if ("species" %in% names(all_species)) {
  "species"
} else {
  names(all_species)[1]
}

ui <- fluidPage(
  useShinyjs(),

  titlePanel("Maryland Invasive Plant Assessment Protocol"),

  fluidRow(
    column(6, offset = 3,
           wellPanel(
             h3("Species Selection"),

             radioButtons("assessment_type",
                          "Assessment Type:",
                          choices = list(
                            "Single Species" = "single",
                            "Multiple Species (Genus-level)" = "multi"
                          ),
                          selected = "single"),

             conditionalPanel(
               condition = "input.assessment_type == 'single'",
               selectInput("single_species",
                           "Select Species:",
                           choices = c("Select a species..." = "", all_species[[species_col]]),
                           selected = "")
             ),

             conditionalPanel(
               condition = "input.assessment_type == 'multi'",

               p(strong("Multi-species assessments are genus-level. Select a genus, then choose which species to include:")),

               selectInput("genus_select",
                           "Select Genus:",
                           choices = c("Select a genus..." = "",
                                       sort(unique(sub(" .*", "", all_species[[species_col]])))),
                           selected = ""),

               uiOutput("genus_species_checkboxes")
             ),

             actionButton("confirm", "Confirm Selection", class = "btn-primary btn-lg")
           )
    )
  ),

  # Task sections (only show after species confirmed)
  conditionalPanel(
    condition = "output.selection_confirmed",

    fluidRow(
      column(6,
             wellPanel(
               h3(icon("download"), " Query Species Locations"),
               p("Gather georeferenced records and occurrence data from GBIF, iNaturalist, FIA, and EDDMapS. Get native range and synonyms from Trefle."),
               p(strong("Expected time:"), " 5-10 minutes"),

               # File status check
               uiOutput("query_file_status"),

               actionButton("run_query",
                            "Query Species Data",
                            class = "btn-primary btn-lg",
                            icon = icon("play")),

               br(), br(),

               # Progress and output area
               div(style = "min-height: 300px; max-height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: #f5f5f5;",
                   uiOutput("query_progress"),
                   verbatimTextOutput("query_output")
               )
             )
      ),

      column(6,
             wellPanel(
               h3(icon("file-alt"), " Generate Assessment"),

               radioButtons("render_mode",
                            "Assessment Stage:",
                            choices = list(
                              "Draft (for assessor review)" = "generate",
                              "Final (merge with assessor evaluation)" = "merge"
                            ),
                            selected = "generate"),

               # File status checks
               uiOutput("render_file_status"),

               p(strong("Expected time:"), " 2-5 minutes"),

               actionButton("run_render",
                            "Generate Assessment",
                            class = "btn-info btn-lg",
                            icon = icon("play")),

               br(), br(),

               div(style = "min-height: 300px; max-height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: #f5f5f5;",
                   verbatimTextOutput("render_output")
               )
             )
      )
    ),

    fluidRow(
      column(12,
             wellPanel(
               h4("Selected Species Information"),
               verbatimTextOutput("species_info")
             )
      )
    )
  )
)

server <- function(input, output, session) {

  # Reactive values
  selection <- reactiveValues(
    species_for_query = NULL,
    species_for_render = NULL,
    display = NULL,
    confirmed = FALSE
  )

  # Output to control visibility
  output$selection_confirmed <- reactive({
    selection$confirmed
  })
  outputOptions(output, "selection_confirmed", suspendWhenHidden = FALSE)

  # Show checkboxes for species in the selected genus
  output$genus_species_checkboxes <- renderUI({
    req(input$genus_select)
    if (input$genus_select == "") return(NULL)

    genus_species <- all_species[[species_col]][
      grepl(paste0("^", input$genus_select, " "), all_species[[species_col]])
    ]

    div(
      h5(HTML(paste0("Select which species in <em>", input$genus_select, "</em> to include in the assessment:"))),
      checkboxGroupInput("selected_species",
                         NULL,
                         choices = genus_species,
                         selected = genus_species)
    )
  })

  # Confirm selection
  observeEvent(input$confirm, {
    if (input$assessment_type == "single") {
      req(input$single_species)
      if (input$single_species == "") {
        showNotification("Please select a species", type = "error")
        return()
      }

      selection$species_for_query <- input$single_species
      selection$species_for_render <- input$single_species
      selection$display <- input$single_species

    } else {
      req(input$genus_select, input$selected_species)
      if (input$genus_select == "" || length(input$selected_species) == 0) {
        showNotification("Please select a genus and at least one species", type = "error")
        return()
      }

      selection$species_for_query <- input$selected_species
      selection$species_for_render <- input$genus_select
      selection$display <- paste0(input$genus_select,
                                  " (", length(input$selected_species), " species)")
    }

    selection$confirmed <- TRUE
    showNotification("Selection confirmed! Choose a task below.", type = "message", duration = 3)
  })

  # Species info display
  output$species_info <- renderPrint({
    cat("Assessment Display:", selection$display, "\n\n")
    cat("Species for Query:", paste(selection$species_for_query, collapse = ", "), "\n\n")
    cat("Species for Render:", selection$species_for_render, "\n")
  })

  # Check for query data files
  query_files_exist <- reactive({
    req(selection$species_for_render)

    file_safe_name <- gsub(" ", "_", selection$species_for_render)
    points_file <- paste0("./combined_location_data/Points_sf_", file_safe_name, ".RData")

    list(
      points = file.exists(points_file),
      both = file.exists(points_file)  # Keep 'both' for backward compatibility
    )
  })

  # Display query file status
  output$query_file_status <- renderUI({
    req(selection$confirmed)

    files <- query_files_exist()

    if (files$both) {
      div(style = "background-color: #dff0d8; color: #3c763d; border: 1px solid #d6e9c6; padding: 10px; margin: 10px 0; border-radius: 4px;",
          icon("check-circle"),
          strong("You have the files you need to generate an assessment."),
          br(),
          "Rerun the query only if you need to update occurrence data."
      )
    } else {
      div(style = "background-color: #f2dede; color: #a94442; border: 1px solid #ebccd1; padding: 10px; margin: 10px 0; border-radius: 4px;",
          icon("exclamation-circle"),
          strong(" Query required."),
          br(),
          "Location data files not found. Run query before generating assessment."
      )
    }
  })

  # Assessor filename
  output$assessor_filename <- renderText({
    if (is.null(selection$species_for_render)) return("")
    paste0(selection$species_for_render, "_IPAC_Assessor_Draft.docx")
  })

  # Check for assessor file
  assessor_file_exists <- reactive({
    req(selection$species_for_render)

    assessor_file <- paste0("./Assessor_Completed_Ready_to_Merge/",
                            selection$species_for_render,
                            "_IPAC_Assessor_Draft.docx")

    file.exists(assessor_file)
  })

  # Display render file status
  output$render_file_status <- renderUI({
    req(selection$confirmed)

    files <- query_files_exist()
    mode <- input$render_mode

    if (mode == "generate") {
      # Check if query files exist
      if (!files$both) {
        div(style = "background-color: #f2dede; color: #a94442; border: 1px solid #ebccd1; padding: 10px; margin: 10px 0; border-radius: 4px;",
            icon("exclamation-circle"),
            strong(" Cannot generate draft."),
            br(),
            "Query species data first."
        )
      } else {
        # Check for existing draft
        draft_file <- paste0("./Assessments/",
                             selection$species_for_render,
                             "_IPAC_Assessor_Draft.docx")

        if (file.exists(draft_file)) {
          file_info <- file.info(draft_file)
          draft_date <- format(file_info$mtime, "%B %d, %Y at %I:%M %p")

          div(style = "background-color: #fcf8e3; color: #8a6d3b; border: 1px solid #faebcc; padding: 10px; margin: 10px 0; border-radius: 4px;",
              icon("exclamation-triangle"),
              strong(" Draft already exists."),
              br(),
              "Created: ", draft_date,
              br(),
              em("Generating a new draft will overwrite the existing file.")
          )
        } else {
          div(style = "background-color: #d9edf7; color: #31708f; border: 1px solid #bce8f1; padding: 10px; margin: 10px 0; border-radius: 4px;",
              icon("info-circle"),
              strong(" Ready to generate draft."),
              br(),
              "Create a draft assessment. An assessor will complete the literature review and scoring in this draft."
          )
        }
      }
    } else {
      # Merge mode - check for both query files and assessor file
      assessor_exists <- assessor_file_exists()

      issues <- c()
      if (!files$both) issues <- c(issues, "Query data files missing")
      if (!assessor_exists) issues <- c(issues, "Assessor file missing")

      if (length(issues) > 0) {
        div(style = "background-color: #f2dede; color: #a94442; border: 1px solid #ebccd1; padding: 10px; margin: 10px 0; border-radius: 4px;",
            icon("exclamation-circle"),
            strong(" Cannot generate final assessment:"),
            br(),
            tags$ul(
              lapply(issues, function(x) tags$li(x))
            ),
            if (!assessor_exists) {
              p("Expected file: ", tags$code(paste0(selection$species_for_render, "_IPAC_Assessor_Draft.docx")),
                br(), "Location: ", tags$code("./Assessor_Completed_Ready_to_Merge/"))
            }
        )
      } else {
        div(style = "background-color: #dff0d8; color: #3c763d; border: 1px solid #d6e9c6; padding: 10px; margin: 10px 0; border-radius: 4px;",
            icon("check-circle"),
            strong(" Ready to generate final assessment."),
            br(),
            "All required files found.",
            br(),
            em("Note: Consider re-querying data first to get current occurrence records.")
        )
      }
    }
  })

  # Query Species Data
  query_complete <- reactiveVal(FALSE)

  observeEvent(input$run_query, {
    req(selection$confirmed)
    shinyjs::disable("run_query")
    query_complete(FALSE)

    # Clear previous output
    output$query_output <- renderPrint({
      cat("")
    })

    # Run with progress bar
    withProgress(message = "Querying databases...", detail = "This may take 5-10 minutes", value = 0.3, {

      result <- tryCatch({
        setProgress(value = 0.5, detail = "Processing...")
        results <- species_location_query(selection$species_for_query)

        output$query_output <- renderPrint({
          cat("✓ Query Complete!\n")
          cat("Data saved to: ./combined_location_data/\n\n")
          cat("File created:\n")
          cat("  - Points_sf_", gsub(" ", "_", selection$species_for_render), ".RData\n", sep="")
          cat("\nAdditional files (for reference):\n")
          cat("  - database_source_total_numbers_", gsub(" ", "_", selection$species_for_render), ".csv\n", sep="")
          cat("  - all_records_", gsub(" ", "_", selection$species_for_render), ".csv\n", sep="")
        })

        query_complete(TRUE)
        setProgress(value = 1, message = "Complete!")
        list(success = TRUE)

      }, error = function(e) {
        output$query_output <- renderPrint({
          cat("✗ Error:\n")
          cat(e$message, "\n\n")

          if (grepl("Timeout", e$message)) {
            cat("Troubleshooting:\n")
            cat("- Check internet connection\n")
            cat("- If using VPN, try disconnecting\n")
          }
        })

        list(success = FALSE)

      }, finally = {
        shinyjs::enable("run_query")
      })
    })
  })

  # Progress indicator in query panel
  output$query_progress <- renderUI({
    if (query_complete()) {
      div(style = "background-color: #dff0d8; color: #3c763d; border: 1px solid #d6e9c6; padding: 10px; margin-bottom: 10px; border-radius: 4px;",
          icon("check-circle"),
          strong(" Query Complete!")
      )
    }
  })

  # ============================================================================
  # Generate Assessment Section
  # ============================================================================

  render_complete <- reactiveVal(FALSE)

  # Function to run the actual rendering
  run_render_assessment <- function(mode) {
    shinyjs::disable("run_render")
    render_complete(FALSE)

    # Clear previous output
    output$render_output <- renderPrint({
      cat("")
    })

    mode_label <- if (mode == "generate") "Draft" else "Final"

    # Check for assessor file if merging
    assessor_file <- NULL
    if (mode == "merge") {
      assessor_file <- paste0("./Assessor_Completed_Ready_to_Merge/",
                              selection$species_for_render,
                              "_IPAC_Assessor_Draft.docx")

      if (!file.exists(assessor_file)) {
        output$render_output <- renderPrint({
          cat("✗ Error: Assessor file not found!\n")
          cat("Expected: ", assessor_file, "\n\n")
          cat("Please ensure:\n")
          cat("1. You've completed your assessment\n")
          cat("2. File has the correct name\n")
          cat("3. File is in ./Assessor_Completed_Ready_to_Merge/\n")
        })

        shinyjs::enable("run_render")
        return()
      }
    }

    # Run with progress bar
    withProgress(message = paste("Generating", mode_label, "assessment..."),
                 detail = "This may take 2-5 minutes", value = 0.3, {

                   result <- tryCatch({
                     setProgress(value = 0.5, detail = "Rendering document...")

                     if (mode == "generate") {
                       render_assessment(selection$species_for_render, mode = "generate")
                     } else {
                       render_assessment(selection$species_for_render, mode = "merge", assessor_file = assessor_file)
                     }

                     output$render_output <- renderPrint({
                       if (mode == "generate") {
                         cat("✓ Draft Assessment Complete!\n")
                         cat("Saved to: ./Assessments/\n\n")
                         cat("Next Steps:\n")
                         cat("1. Complete literature review and scoring\n")
                         cat("2. Save as: ", selection$species_for_render, "_IPAC_Assessor_Draft.docx\n", sep="")
                         cat("3. Place in: ./Assessor_Completed_Ready_to_Merge/\n")
                         cat("4. Return here to generate final assessment\n")
                       } else {
                         cat("✓ Final Assessment Complete!\n")
                         cat("🎉 Saved to: ./Assessments/\n\n")
                         cat("Filename: ", selection$species_for_render, "_IPAC_Final_Assessment.docx\n", sep="")
                       }
                     })

                     render_complete(TRUE)
                     setProgress(value = 1, message = "Complete!")
                     list(success = TRUE)

                   }, error = function(e) {
                     output$render_output <- renderPrint({
                       cat("✗ Error:\n")
                       cat(e$message, "\n")
                     })

                     list(success = FALSE)

                   }, finally = {
                     shinyjs::enable("run_render")
                   })
                 })
  }

  # Button click handler
  observeEvent(input$run_render, {
    req(selection$confirmed)
    run_render_assessment(input$render_mode)
  })
}

shinyApp(ui = ui, server = server)
