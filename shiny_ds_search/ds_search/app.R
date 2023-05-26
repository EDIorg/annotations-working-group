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
df_sub <- read.csv("data/df_sub.csv")
df_terms_orig <- read.csv("data/df_terms.csv")
df_sub_long <- read.csv("data/df_sub_long.csv")
df_terms_empty <- read.csv("data/df_terms_empty.csv", colClasses = "character")


# Define UI for dataset search
ui <- fluidPage(

    # Application title
    titlePanel("Search Datasets"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
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

        # Show a plot of the generated distribution
        mainPanel(
          textOutput("numRows"),
          textOutput("selected"),
          DTOutput("view")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  df_ds <- reactiveValues(df = df_sub)
  
  sel <- reactiveValues(terms = c(""))
  
  observeEvent(input$searchButton, {
    
    ecosystem_t <- ifelse(input$ecosystem == "", ".*", input$ecosystem)
    discipline_t <- ifelse(input$discipline == "", ".*", input$discipline)
    process_t <- ifelse(input$process == "", ".*", input$process)
    org_unit_t <- ifelse(input$org_unit == "", ".*", input$org_unit)
    organism_t <- ifelse(input$organism == "", ".*", input$organism)
    methods_t <- ifelse(input$methods == "", ".*", input$methods)
    events_t <- ifelse(input$events == "", ".*", input$events)
    substrate_t <- ifelse(input$substrate == "", ".*", input$substrate)
    
    sel$terms <- c(sel$terms, input$ecosystem,
                   input$discipline,
                   input$process,
                   input$org_unit,
                   input$organism,
                   input$methods,
                   input$events,
                   input$substrate)
    
    df_ds$df <- df_ds$df %>%
      filter(str_detect(new_keywords, ecosystem_t),
             str_detect(new_keywords, discipline_t),
             str_detect(new_keywords, process_t),
             str_detect(new_keywords, org_unit_t),
             str_detect(new_keywords, organism_t),
             str_detect(new_keywords, methods_t),
             str_detect(new_keywords, events_t),
             str_detect(new_keywords, substrate_t))
    
    df_ds_id <- df_ds$df %>%
      select(packageid)
    
    df_terms <- df_sub_long %>%
      select(packageid, main, level, term) %>%
      inner_join(df_ds_id) %>%
      select(-packageid) %>%
      filter(level != "level_3") %>%
      distinct() %>%
      rbind(df_terms_empty) %>%
      group_by(main) %>%
      arrange(main, level, term)
    
    
    isolate(updateSelectInput(session,
                      inputId = "ecosystem",
                      choices = df_terms$term[df_terms$main == 'ecosystem']))
    
    isolate(updateSelectInput(session,
                      inputId = "discipline",
                      choices = df_terms$term[df_terms$main == 'discipline']))
    
    isolate(updateSelectInput(session,
                      inputId = "process",
                      choices = df_terms$term[df_terms$main == 'process']))
    
    isolate(updateSelectInput(session,
                      inputId = "org_unit",
                      choices = df_terms$term[df_terms$main == 'org_unit']))
    
    isolate(updateSelectInput(session,
                      inputId = "organism",
                      choices = df_terms$term[df_terms$main == 'organism']))
    
    isolate(updateSelectInput(session,
                      inputId = "methods",
                      choices = df_terms$term[df_terms$main == 'methods']))
    
    isolate(updateSelectInput(session,
                      inputId = "events",
                      choices = df_terms$term[df_terms$main == 'events']))
    
    isolate(updateSelectInput(session,
                      inputId = "substrate",
                      choices = df_terms$term[df_terms$main == 'substrate']))
    
    #output$selected <- renderText(paste(input$ecosystem, input$discipline))
    
    output$view <- renderDT({
      
      df_out <- df_ds$df %>%
        select(packageid, title)
      
    })
    
    output$numRows <- renderText(paste("Datasets found: ", nrow(df_ds$df)))
    output$selected <- renderText(paste("Search terms: ", paste(sel$terms, collapse = " ")))
    
  }, ignoreInit = TRUE)
  
  
  # reset button
  
  observeEvent(input$resetButton, {
    
    df_ds$df <- df_sub
    
    sel$terms <- c("")
    

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
    
    
    output$numRows <- renderText(paste("Datasets found: " ,nrow(df_ds$df)))
    
    output$view <- renderDT({ 
      
      df_out <- df_ds$df %>%
        select(packageid, title)
      
    })
      
    
  })
  
  
  
  output$numRows <- renderText({paste("Datasets found: " , nrow(df_ds$df))})

   output$view <- renderDT({ 
     
     df_out <- df_ds$df %>%
       select(packageid, title)
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)
