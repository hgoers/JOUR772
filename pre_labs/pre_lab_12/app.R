library(shiny)
library(tidyverse)
library(janitor)
library(tabulizer)
library(glue)
library(lubridate)

# Create helper functions
get_new_rego_by_party <- function(month, year) {
  
  month_clean <- if_else(month < 10, paste0("0", month), as.character(month))
  
  report_url <- glue::glue("https://elections.maryland.gov/pdf/vrar/{ year }_{ month_clean }.pdf")
  
  table <- extract_tables(report_url)
  
  voter_reg <- table[[1]] |> 
    as_tibble(.name_repair = "unique") |> 
    clean_names() |> 
    slice(-(1:2)) |> 
    rename(method = x2, DEM = x3, REP = x4, GRN = x5, LIB = x6, WCP = x7, UNA = x8, OTH = x9) |> 
    transmute(method, 
              across(DEM:OTH, ~ as.numeric(str_remove_all(.x, ","))),
              month = month,
              year = year) |> 
    pivot_longer(DEM:OTH, names_to = "party") |> 
    filter(method != "TOTAL") |> 
    group_by(method) |> 
    mutate(percent = value / sum(value) * 100)
  
}

ui <- fluidPage(
  
  fluidRow(
    
    column(
      
      6,
      
      numericInput("month_1",
                   "Month:",
                   value = month(today()) - 1, 
                   min = 1,
                   max = 12,
                   width = "100%")
      
    ),
    
    column(
      
      6,
      
      numericInput("year_1",
                   "Year",
                   value = year(today()),
                   min = 2000,
                   max = 2022,
                   width = "100%")
      
    )
    
  ),
  
  fluidRow(
    
    plotOutput("registration")
    
  )
  
)

server <- function(input, output, session) {
  
  df <- reactive({
    req(input$month_1)
    get_new_rego_by_party(input$month_1, input$year_1)
  })
  
  output$registration <- renderPlot({
    
    df() |> 
      group_by(party) |> 
      summarise(total = sum(value)) |> 
      ggplot(aes(x = total, y = reorder(party, total))) + 
      geom_col() + 
      theme_minimal()
    
  })
  
}

shinyApp(ui, server)