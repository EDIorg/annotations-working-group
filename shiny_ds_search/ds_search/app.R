#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)

#read in the data
df_sub <- read.csv(file = file.path("data", "df_sub.csv"))
df_terms_orig <- read.csv(file = file.path("data", "df_terms.csv"))
df_sub_long <- read.csv(file = file.path("data", "df_sub_long.csv"))
df_terms_empty <- read.csv(file = file.path("data", "df_terms_empty.csv"), 
                           colClasses = "character")

# Define UI for dataset search ----
ui <- fluidPage(

    # Application title
    titlePanel("Search Datasets"),

    # Sidebar ----
    sidebarLayout(
        sidebarPanel(
          # Dropdown menus (i.e., facets) for each category of terms
          selectInput(inputId = "ecosystem", label = "Ecosystem", 
                      choices = df_terms_orig$term[df_terms_orig$main == "ecosystem"]),
          
          selectInput(inputId = "discipline", label = "Discipline",
                      choices = df_terms_orig$term[df_terms_orig$main == "discipline"]),

          selectInput(inputId = "process", label = "Process",
                      choices = df_terms_orig$term[df_terms_orig$main == "process"],
                      multiple = FALSE),

          selectInput(inputId = "org_unit", label = "Organizational Unit",
                      choices = df_terms_orig$term[df_terms_orig$main == "org_unit"],
                      multiple = FALSE),

          selectInput(inputId = "organism", label = "Organism",
                      choices = df_terms_orig$term[df_terms_orig$main == "organism"],
                      multiple = FALSE),

          selectInput(inputId = "methods", label = "Methods",
                      choices = df_terms_orig$term[df_terms_orig$main == "methods"],
                      multiple = FALSE),

          selectInput(inputId = "events", label = "Events",
                      choices = df_terms_orig$term[df_terms_orig$main == "events"],
                      multiple = FALSE),

          selectInput(inputId = "substrate", label = "Substrate",
                      choices = df_terms_orig$term[df_terms_orig$main == "substrate"],
                      multiple = FALSE),
          
          actionButton(inputId = "searchButton", label = "Search"),

          actionButton(inputId = "resetButton", label = "Clear Search")
          
        ),

        # Main Panel ----
        mainPanel(
          textOutput(outputId = "numRows"),
          textOutput(outputId = "selected"),
          DTOutput(outputId = "view")
        )
    )
)

# Server ----
# Logic for transforming inputs to outputs
server <- function(input, output, session) {
  
  df_ds <- reactiveValues(df = df_sub)
  
  sel <- reactiveValues(terms = c(""))
  
  # Search button ----
  
  # On "Search" click, grab selected search terms
  observeEvent(input$searchButton, {
    
    ecosystem_t <- ifelse(test = input$ecosystem == "", yes = ".*", no = input$ecosystem)
    discipline_t <- ifelse(test = input$discipline == "", yes = ".*", no = input$discipline)
    process_t <- ifelse(test = input$process == "", yes = ".*", no = input$process)
    org_unit_t <- ifelse(test = input$org_unit == "", yes = ".*", no = input$org_unit)
    organism_t <- ifelse(test = input$organism == "", yes = ".*", no = input$organism)
    methods_t <- ifelse(test = input$methods == "", yes = ".*", no = input$methods)
    events_t <- ifelse(test = input$events == "", yes = ".*", no = input$events)
    substrate_t <- ifelse(test = input$substrate == "", yes = ".*", no = input$substrate)
    
    # Concatenate all terms (*not* just selected terms but all possibilities)
    sel$terms <- c(sel$terms, input$ecosystem,
                   input$discipline,
                   input$process,
                   input$org_unit,
                   input$organism,
                   input$methods,
                   input$events,
                   input$substrate)
    
    # Subset the table of datasets to only those that contain (one of) the keyword(s)
    df_ds$df <- df_ds$df %>%
      filter(str_detect(new_keywords, ecosystem_t),
             str_detect(new_keywords, discipline_t),
             str_detect(new_keywords, process_t),
             str_detect(new_keywords, org_unit_t),
             str_detect(new_keywords, organism_t),
             str_detect(new_keywords, methods_t),
             str_detect(new_keywords, events_t),
             str_detect(new_keywords, substrate_t))
    
    # Grab just the package IDs of the datasets that contained (one of) the keyword(s)
    df_ds_id <- df_ds$df %>%
      select(packageid)
    
    # Wrangle the more detailed dataset table
    df_terms <- df_sub_long %>%
      # Pare down to needed columns
      select(packageid, main, level, term) %>%
      # Retain only those datasets that contain (one of) the keyword(s)
      inner_join(y = df_ds_id, by = c("packageid")) %>%
      # Drop package ID now that we've used it to subset
      select(-packageid) %>%
      # Remove level 3
      filter(level != "level_3") %>%
      # Keep only unique rows
      distinct() %>%
      # Attach empty terms (but match column order)
      bind_rows(df_terms_empty) %>%
      # Group by term
      group_by(main) %>%
      # Sort
      arrange(main, level, term)
    
    # Re-set keyword inputs
    # isolate(updateSelectInput(session,
    #                   inputId = "ecosystem",
    #                   choices = df_terms$term[df_terms$main == 'ecosystem']))
    # 
    # isolate(updateSelectInput(session,
    #                   inputId = "discipline",
    #                   choices = df_terms$term[df_terms$main == 'discipline']))
    # 
    # isolate(updateSelectInput(session,
    #                   inputId = "process",
    #                   choices = df_terms$term[df_terms$main == 'process']))
    # 
    # isolate(updateSelectInput(session,
    #                   inputId = "org_unit",
    #                   choices = df_terms$term[df_terms$main == 'org_unit']))
    # 
    # isolate(updateSelectInput(session,
    #                   inputId = "organism",
    #                   choices = df_terms$term[df_terms$main == 'organism']))
    # 
    # isolate(updateSelectInput(session,
    #                   inputId = "methods",
    #                   choices = df_terms$term[df_terms$main == 'methods']))
    # 
    # isolate(updateSelectInput(session,
    #                   inputId = "events",
    #                   choices = df_terms$term[df_terms$main == 'events']))
    # 
    # isolate(updateSelectInput(session,
    #                   inputId = "substrate",
    #                   choices = df_terms$term[df_terms$main == 'substrate']))
    
    #output$selected <- renderText(paste(input$ecosystem, input$discipline))
    
    # Generate human-readable table of dataframes that contain (one of) the keyword(s)
    output$view <- renderDT({
      
      df_out <- df_ds$df %>%
        select(packageid, title)
      
    })
    
    # Generate some small diagnostic output text too
    output$numRows <- renderText(paste("Datasets found: ", nrow(df_ds$df)))
    output$selected <- renderText(paste("Search terms: ", paste(unique(sel$terms), 
                                                                collapse = " ")))
    
  }, ignoreInit = TRUE) # Search button closing parentheses (observer end)
  
  
  # Reset button ----
  
  observeEvent(input$resetButton, {
    
    df_ds$df <- df_sub
    
    sel$terms <- c("")
    
    # Clear search terms
    isolate(updateSelectInput(session, 
                      inputId = "ecosystem",
                      choices = df_terms_orig$term[df_terms_orig$main == 'ecosystem']))
    
    isolate(updateSelectInput(session, 
                      inputId = "discipline",
                      choices = df_terms_orig$term[df_terms_orig$main == 'discipline']))
    
    isolate(updateSelectInput(session, 
                      inputId = "process",
                      choices = df_terms_orig$term[df_terms_orig$main == 'process']))
    
    isolate(updateSelectInput(session, 
                      inputId = "org_unit",
                      choices = df_terms_orig$term[df_terms_orig$main == 'org_unit']))
    
    isolate(updateSelectInput(session, 
                      inputId = "organism",
                      choices = df_terms_orig$term[df_terms_orig$main == 'organism']))
    
    isolate(updateSelectInput(session, 
                      inputId = "methods",
                      choices = df_terms_orig$term[df_terms_orig$main == 'methods']))
    
    isolate(updateSelectInput(session, 
                      inputId = "events",
                      choices = df_terms_orig$term[df_terms_orig$main == 'events']))
    
    isolate(updateSelectInput(session, 
                      inputId = "substrate",
                      choices = df_terms_orig$term[df_terms_orig$main == 'substrate']))
    
    # Generate output text
    output$numRows <- renderText(paste("Datasets found: ", nrow(df_ds$df)))
    
    # Generate output table
    output$view <- renderDT({ 
      
      df_out <- df_ds$df %>%
        select(packageid, title)
      
    })
      
    
  }) # Reset button closing parentheses (observer end)
  
  
  
  output$numRows <- renderText({paste("Datasets found: " , nrow(df_ds$df))})

   output$view <- renderDT({ 
     
     df_out <- df_ds$df %>%
       select(packageid, title)
   })
   
   
}

# Create App ----
shinyApp(ui = ui, server = server)
