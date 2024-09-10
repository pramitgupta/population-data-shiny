library(shiny)

# Load the data
df <- read.csv("data/demo.csv")

ui <- fluidPage(

    useShinyjs(),  # Include shinyjs for showing/hiding messages
    titlePanel(title=div(img(src="www/logo.png", align='right', height = "100px"), "Population Data")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("granularity", "Select Granularity", 
                  choices = unique(df$Granularity)),
      uiOutput("name_ui"),
      selectInput("type", "Select Type", 
                  choices = unique(df$Type)),
      downloadButton("downloadData", "Download CSV")
    ),
    
    mainPanel(
      tableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  
  # Update Name dropdown based on the selected Granularity, sorting names alphabetically
  output$name_ui <- renderUI({
    df_filtered <- df[df$Granularity == input$granularity, ]
    selectInput("name", "Select Name", choices = sort(unique(df_filtered$Name)))
  })
  
  # Filter and display the table based on the selections
  filtered_data <- reactive({
    if (input$type == "Total") {
      df_filtered <- df[df$Granularity == input$granularity & 
                          df$Name == input$name & 
                          df$Type %in% c("Rural", "Urban", "Total"), ]
    } else {
      df_filtered <- df[df$Granularity == input$granularity & 
                          df$Name == input$name & 
                          df$Type == input$type, ]
    }
    df_filtered
  })
  
  # Render the table based on the filtered data
  output$table <- renderTable({
    df_filtered <- filtered_data()
    df_filtered[, c("Granularity", "Name", "Type", "villages_Inhabited", 
                    "villages_Uninhabited", "towns", "households", 
                    "Persons", "Males", "Females", "Area", "Density")]
  })
  
  # Allow the user to download the filtered data as a CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("village_town_population_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      df_filtered <- filtered_data()
      write.csv(df_filtered[, c("Granularity", "Name", "Type", "villages_Inhabited", 
                                "villages_Uninhabited", "towns", "households", 
                                "Persons", "Males", "Females", "Area", "Density")], 
                file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
