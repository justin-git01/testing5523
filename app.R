library(shiny)
library(tidyverse)
library(dplyr)
library(DT)
library(shinyWidgets)
library(shinydashboard)

ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')
ikea <- ikea %>% mutate(discount = ifelse(old_price == "No old price", "On sale", "NOT on sale"))
ikea <- ikea %>% mutate(depth = ifelse(is.na(depth), "NA", depth),
                        height = ifelse(is.na(height), "NA", height),
                        width = ifelse(is.na(width), "NA", width))
price_range <- range(ikea$price)

# Define UI for application using shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "IKEA Furniture App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Furniture", tabName = "furniture"),
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("furniture",
              sidebarLayout(
                sidebarPanel(
                  selectizeInput("category", 
                                 label = "Select Category", 
                                 choices = unique(ikea$category)),
                  sliderInput("slider",
                              label = "Price",
                              min = price_range[1], 
                              max = price_range[2],
                              value = price_range, 
                              step = 1),
                  radioGroupButtons("discount", "Select one", 
                                    choices = c("On sale", "NOT on sale", "Show all"),
                                    checkIcon = list(yes = icon("check"), empty = icon("times"))),
                  dataTableOutput("itemtable"),
                  selectizeInput("selected_items", label = "Select Items",
                                 choices = NULL, multiple = TRUE),
                  
                  # Select item ID based on the chosen category and item name
                  selectizeInput("selected_id", label = "Select ID",
                                 choices = NULL, multiple = TRUE)
                ),
              
                mainPanel(
                  dataTableOutput("comparison_table")
                )
              )
      ),
      tabItem("about",
              includeHTML("about.html")  # Content for the About tab
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # filter ikea data regarding to inputs
  filtered_data <- reactive({
    category_filtered <- ikea %>%
      filter(category == input$category) %>%
      filter(price > input$slider[1], price < input$slider[2]) %>%
      filter(discount == ifelse(input$discount == "Show all", discount, input$discount)) %>%
      arrange(desc(price)) %>%
      select(item_id, name, price, discount)
    return(category_filtered)
  })
  
  # Update item names only shown in filtered data
  observe({
    updateSelectizeInput(session, "selected_items",
                         choices = unique(filtered_data()$name),
                         selected = NULL)
  })
  
  observe({
    item_id_choices <- ikea %>%
      filter(category == input$category,
             name %in% input$selected_items) %>%
      filter(price > input$slider[1], price < input$slider[2]) %>%
      filter(discount == ifelse(input$discount == "Show all", discount, input$discount)) %>%
      pull(item_id)
    
    updateSelectizeInput(session, "selected_id",
                         choices = item_id_choices,
                         selected = NULL)
  })
  
  # Render the item table on side panel
  output$itemtable <- renderDataTable({
    datatable(filtered_data()
              ,
              options = list(pageLength = 5),
              colnames = c("Product ID", "Product Name", "Price", "Discount")
              )
  })
  
  # Render the comparison table based on selected items
  output$comparison_table <- renderDataTable({
    selected_items_filtered <- ikea %>%
      filter(category == input$category,
             name %in% input$selected_items,
             item_id %in% input$selected_id) %>%
      select(item_id, name, price, short_description, depth, height, width, link)
    
    datatable(selected_items_filtered
              ,
              extensions = 'Buttons',
              options = list(pageLength = 5),
              colnames = c("Product ID", "Product Name", "Price", "Description", "Depth", "Height", "Width", "Link to product")
              )
  })
}

# Run the application
shinyApp(ui, server)
