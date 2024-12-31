## app.R
library(shiny)
library(bib2df)
library(dplyr)
library(DT)
library(shinyjs)

ui <- fluidPage(
    useShinyjs(),
    titlePanel("De-duplicate BibTeX by Title"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput(
                "bib_files", 
                "Upload .bib Files",
                multiple = TRUE,
                accept = ".bib"
            ),
            
            downloadButton("download_bib", "Download Cleaned .bib")
        ),
        
        mainPanel(
            h3("Detect & Resolve Duplicates by Title"),
            p("Below are the detected duplicates. For each group of duplicates, select which entry you wish to KEEP. You may also select 'None' to keep all entries in the group."),
            DTOutput("duplicates_table")
        )
    )
)

server <- function(input, output, session) {
    
    # Reactive to store the combined bib data
    combined_bib_data <- reactive({
        req(input$bib_files)
        
        # Read all .bib files and process them
        bib_dfs <- lapply(input$bib_files$datapath, function(path) {
            df <- bib2df::bib2df(path)
            # Ensure YEAR is numeric, replacing invalid entries with NA
            if ("YEAR" %in% names(df)) {
                df$YEAR <- suppressWarnings(as.numeric(df$YEAR))
            }
            df
        })
        
        # Combine all processed data frames into one
        combined <- do.call(bind_rows, bib_dfs)
        
        # Replace missing titles with an empty string
        combined <- combined %>%
            mutate(TITLE = ifelse(is.na(TITLE), "", TITLE))
        
        # Add ROW_ID for unique identification
        combined$ROW_ID <- seq_len(nrow(combined))
        
        combined
    })
    
    # Identify duplicates by TITLE
    # We'll create a grouping ID for each set of identical titles
    duplicates_info <- reactive({
        df <- combined_bib_data()
        
        # Group by TITLE, count how many in each group
        # We'll keep only groups with more than 1 row
        df %>%
            group_by(TITLE) %>%
            mutate(
                DUP_GROUP_ID = cur_group_id(),
                N_IN_GROUP = n()
            ) %>%
            ungroup() %>%
            filter(N_IN_GROUP > 1) %>%
            arrange(TITLE)
    })
    
    # We will store user selections (which row to keep) in a reactiveValues
    user_choices <- reactiveValues(
        keep_rows = integer()
    )
    
    # Initialize user choices once data is loaded or changes
    observeEvent(duplicates_info(), {
        duplicates <- duplicates_info()
        # By default, keep all rows for each group
        default_keeps <- duplicates %>%
            pull(ROW_ID)
        
        user_choices$keep_rows <- default_keeps
    })
    
    # Render the duplicates table with radio buttons for selection
    output$duplicates_table <- renderDT({
        dup_df <- duplicates_info()
        if (nrow(dup_df) == 0) {
            return(datatable(
                data.frame(Message = "No duplicates detected!"),
                options = list(dom = 't')
            ))
        }
        
        # For each DUP_GROUP_ID, we want the user to pick the single ROW_ID to keep or 'None'
        # We'll create radio buttons and a 'None' option for each group.
        dup_df <- dup_df %>%
            mutate(
                KEEP = paste0(
                    "<input type='radio' name='radio_group_", DUP_GROUP_ID,
                    "' value='", ROW_ID, "' ",
                    ifelse(ROW_ID %in% user_choices$keep_rows, "checked", ""),
                    " onclick=\"Shiny.setInputValue('chosen_", DUP_GROUP_ID,
                    "', this.value, {priority: 'event'}); event.preventDefault();\"/>"
                ),
                NONE = paste0(
                    "<input type='radio' name='radio_group_", DUP_GROUP_ID,
                    "' value='none' ",
                    ifelse(all(!(ROW_ID %in% user_choices$keep_rows)), "checked", ""),
                    " onclick=\"Shiny.setInputValue('chosen_", DUP_GROUP_ID,
                    "', this.value, {priority: 'event'}); event.preventDefault();\"/> Keep All"
                )
            )
        
        datatable(
            dup_df %>%
                select(DUP_GROUP_ID, TITLE, BIBTEXKEY, AUTHOR, YEAR, JOURNAL, KEEP, NONE),
            escape = FALSE,
            rownames = FALSE,
            options = list(
                pageLength = 10,
                autoWidth = TRUE,
                columnDefs = list(
                    list(targets = "KEEP", orderable = FALSE),
                    list(targets = "NONE", orderable = FALSE)
                )
            )
        )
    }, server = FALSE)
    
    # Observe for each group of duplicates, when the user checks a row
    # We catch the input dynamic name: chosen_{DUP_GROUP_ID}
    observe({
        dup_df <- duplicates_info()
        group_ids <- unique(dup_df$DUP_GROUP_ID)
        for (g_id in group_ids) {
            chosen_name <- paste0("chosen_", g_id)
            chosen_value <- input[[chosen_name]]
            if (!is.null(chosen_value)) {
                isolate({
                    if (chosen_value == "none") {
                        # Keep all rows in this group
                        group_rows <- dup_df$ROW_ID[dup_df$DUP_GROUP_ID == g_id]
                        user_choices$keep_rows <- unique(c(user_choices$keep_rows, group_rows))
                    } else {
                        chosen_value <- as.integer(chosen_value)
                        old_keep <- user_choices$keep_rows
                        
                        # Remove rows in the group from keep_rows
                        all_group_rows <- dup_df$ROW_ID[dup_df$DUP_GROUP_ID == g_id]
                        new_keep <- setdiff(old_keep, all_group_rows)
                        
                        # Add the chosen row
                        new_keep <- c(new_keep, chosen_value)
                        
                        user_choices$keep_rows <- new_keep
                    }
                })
            }
        }
    })
    
    # Prepare final deduplicated data
    cleaned_bib_data <- reactive({
        df_all <- combined_bib_data()
        df_dup <- duplicates_info()
        
        # The row IDs to keep
        keep_ids <- user_choices$keep_rows
        
        # For duplicates, keep only the chosen rows
        # For non-duplicates, keep everything
        # So effectively, remove any duplicates that are not in keep_ids
        df_removed_dups <- df_all %>%
            filter(
                !(ROW_ID %in% df_dup$ROW_ID) | (ROW_ID %in% keep_ids)
            ) %>%
            select(-ROW_ID)
        
        df_removed_dups
    })
    
    # Download handler for the cleaned .bib
    output$download_bib <- downloadHandler(
        filename = function() {
            paste0("cleaned_", Sys.Date(), ".bib")
        },
        content = function(file) {
            # Convert the cleaned data back to a .bib format
            bib2df::df2bib(cleaned_bib_data(), file)
        }
    )
}

shinyApp(ui, server)
