library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)

# UI dalis
ui <- dashboardPage(
  dashboardHeader(title = "Kompiuterių programavimo veikla"),
  dashboardSidebar(
    selectizeInput(inputId = "imones_kodas", label = "Imonė", choices = NULL, selected = NULL)
  ),
  dashboardBody(tabsetPanel(
    tabPanel("Vidutinio atlyginimo kitimas metų eigoje", plotOutput("panel1")), 
    tabPanel("Apdraustų darbuotojų skaičius metų eigoje", plotOutput("panel2")), 
    tabPanel("Duomenys", tableOutput("panel3"))
  )
  )
)

#Serverio dalis
server <- function(input, output, session){
  Duomenys <- read_csv("../data/lab_sodra.csv")
  FiltruotiDuomenys <- Duomenys %>%
    filter(ecoActCode == 620100)
  
  updateSelectizeInput(session, "imones_kodas", choices = FiltruotiDuomenys$name, server = TRUE)
  
  output$panel1 <- renderPlot(
    FiltruotiDuomenys %>%
      filter(name == input$imones_kodas) %>%
      mutate(month = as.Date(paste0(month, "01"), format = "%Y%m%d")) %>%
      ggplot(aes(x = month, y = avgWage)) +
      geom_line() +
      geom_point() +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      theme_classic() +
      labs(x = "Month", y = "Average wage")
  )
  
  output$panel2 <- renderPlot(
    FiltruotiDuomenys %>%
      filter(name == input$imones_kodas) %>%
      mutate(month = as.Date(paste0(month, "01"), format = "%Y%m%d")) %>%
      ggplot(aes(x = month, y = numInsured)) +
      geom_col() +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      theme_classic() +
      labs(x = "Month", y = "Insured employees")
  )
  
  output$panel3 <- renderTable(
    FiltruotiDuomenys %>%
      filter(name == input$imones_kodas), digits = 0
  )
}

#Paleidimo kodas
shinyApp(ui, server)
