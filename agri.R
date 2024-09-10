library(shiny)
library(shinyjs)

# Load the data
df <- read.csv("Sample.csv")

ui <- fluidPage(
  useShinyjs(),  # Include shinyjs for showing/hiding messages
  titlePanel(title=div(img(src="logo.png", align='right', height = "100px"), "Agriculture Income")),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select State", 
                  choices = c("", unique(df$state_name)), selected = ""),
      uiOutput("district_ui"),
      uiOutput("center_ui"),
      uiOutput("category_ui"),
      uiOutput("type_ui"),
      downloadButton("downloadData", "Download CSV")
    ),
    
    mainPanel(
      tableOutput("table"),
      div(id = "warningMessage", style = "color: red; font-weight: bold;", "No data found for the selected criteria.", hidden = TRUE)
    )
  )
)

server <- function(input, output, session) {
  
  # Update District dropdown based on selected State
  output$district_ui <- renderUI({
    req(input$state) # Ensure that state is selected
    df_filtered <- df[df$state_name == input$state, ]
    selectInput("district", "Select District", choices = c("", unique(df_filtered$district_name)))
  })
  
  # Update Center dropdown based on selected District
  output$center_ui <- renderUI({
    req(input$district) # Ensure that district is selected
    df_filtered <- df[df$district_name == input$district, ]
    selectInput("center", "Select Center", choices = c("", unique(df_filtered$center_name)))
  })
  
  # Update Category dropdown based on selected Center
  output$category_ui <- renderUI({
    req(input$center) # Ensure that center is selected
    df_filtered <- df[df$center_name == input$center, ]
    selectInput("category", "Select Category", choices = c("", unique(df_filtered$labour_category)))
  })
  
  # Update Labour Type dropdown based on selected Category
  output$type_ui <- renderUI({
    req(input$category) # Ensure that category is selected
    df_filtered <- df[df$labour_category == input$category, ]
    selectInput("type", "Select Labour Type", choices = c("", unique(df_filtered$labour_type)))
  })
  
  # Filter and display the data based on all selections
  filtered_data <- reactive({
    req(input$type)  # Ensure the last dropdown (Labour Type) is selected
    df_filtered <- df[df$state_name == input$state &
                        df$district_name == input$district &
                        df$center_name == input$center &
                        df$labour_category == input$category &
                        df$labour_type == input$type, ]
    return(df_filtered)
  })
  
  output$table <- renderTable({
    df_filtered <- filtered_data()
    if (nrow(df_filtered) == 0 && !is.null(input$type)) {
      shinyjs::show("warningMessage")  # Show warning message only after the last dropdown is selected and no data is found
    } else {
      shinyjs::hide("warningMessage")  # Hide warning message if data is found or no selection made
    }
    df_filtered
  })
  
  # Allow the user to download the filtered data as a CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("agri_income_filtered-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
