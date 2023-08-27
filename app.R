library(shiny)
library(readxl)
library(openxlsx)
library(U.Taxonstand)
library(chron)

ui <- fluidPage(
  titlePanel("U_Taxonstand"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "Database", label = "Upload your database", accept = ".xlsx"),
      selectInput(inputId = "LocalDatabase", 
                  label = "OR choose a local database file:", 
                  choices = c("Plants_TPL_Part 1" = "Plants_TPL_database_part1.xlsx", 
                              "Plants_TPL_Part 2" = "Plants_TPL_database_part2.xlsx",
                              "Plants_TPL_Part 3" = "Plants_TPL_database_part3.xlsx",
                              "Amphibians" = "Amphibians_COL_database.xlsx",
                              "Birds" = "Birds_ITIS_database.xlsx",
                              "Fishes" = "Fishes_FishBase_database.xlsx",
                              "Mammals" = "Mammals_COL_database.xlsx",
                              "Plants LCVP Part 1" = "Plants_LCVP_database_part1.xlsx",
                              "Plants LCVP Part 2" = "Plants_LCVP_database_part2.xlsx",
                              "Plants LCVP Part 3" = "Plants_LCVP_database_part3.xlsx",
                              "Plants WCVP Part 1" = "Plants_WCVP_database_part1.xlsx",
                              "Plants WCVP Part 2" = "Plants_WCVP_database_part2.xlsx",
                              "Plants WCVP Part 3" = "Plants_WCVP_database_part3.xlsx",
                              "Plants WFO Part 1" = "Plants_WFO_database_part1.xlsx",
                              "Plants WFO Part 2" = "Plants_WFO_database_part2.xlsx",
                              "Plants WFO Part 3" = "Plants_WFO_database_part3.xlsx",
                              "Plants WP Part 1" = "Plants_WP_database_part1.xlsx",
                              "Plants WP Part 2" = "Plants_WP_database_part2.xlsx",
                              "Plants WP Part 3" = "Plants_WP_database_part3.xlsx",
                              "China Plants SP2000 2022 Edition" = "China_Plants_SP2000_2022Edition.xlsx",
                              "Reptiles" = "Reptiles_COL_database.xlsx"
                  )),
      fileInput(inputId = "genusPairs", label = "3. Genus pairs (optional)", accept = ".xlsx"),
      fileInput(inputId = "species", label = "4. Species list", accept = ".xlsx"),
      actionButton("RUN", "RUN"),
      actionButton("download", "Download")
    ),
    mainPanel(
      verbatimTextOutput("text"),
      tableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  ##define the size of file
  options(shiny.maxRequestSize=30*1024^2) 
  # Define reactive values
  spelist <- reactiveVal(NULL)
  databases <- reactiveVal(list())
  genusPairs_example <- reactiveVal(NULL)
  results <- reactiveVal(NULL)
  
  
  output$text <- renderPrint({
    req(input$species)
    input$species
  })
  # 初始化一个空数据框，用于展示结果数据
  result_data <- reactiveVal(data.frame())
  
  output$table <- renderTable({
    # 使用 result_data() 以获取反应式数据的值
     result_data()
    
  })
  
  observeEvent(input$RUN, {
    # Read input data
    spelist_data <- req(input$species)
    
    # Read excel data
    spelist <- read_excel(spelist_data$datapath)
    
    # Read the selected database file
    database_data <- NULL
    if (!is.null(input$LocalDatabase)) {
      database_data <- input$LocalDatabase
    } else {
      database_data <- req(input$Database1)
    }
    
    suppressWarnings({
      database <- read_xlsx(database_data)
      # Perform the name matching operation
     
    })
    withProgress(
      message = 'Running nameMatch...',
      detail = 'This may take a moment...',
      value = 0,
      {
        # Run the name matching operation
        if (!is.null(input$genusPairs)) {
          genusPairs_data <- req(input$genusPairs)
          genusPairs_example <- read_excel(genusPairs_data$datapath)
          
          # Run the name matching operation with genusPairs_example
          result_data(
            nameMatch(spelist, database, author = TRUE, max.distance = 1, 
                      genusPairs_example, Append = FALSE)
          )
          
        } else {
          # Run the name matching operation without genusPairs_example
          # 更新 result_data 为 namematch 的结果
          result_data(
            nameMatch(spelist, database, author = TRUE, max.distance = 1, Append = FALSE)
          )
        
        }
      }
    )
  })
  
  observeEvent(input$download, {
    req(results())
    
    # Get the current time
    current_time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    
    # Define the filename with the current time
    filename <- paste0(current_time, "_result.xlsx")
    
    # Write the results to an Excel file using openxlsx
    write.xlsx(results(), filename)
    
    # Download the file
    downloadHandler(
      filename = filename,
      content = function(file) {
        file.copy(filename, file)
      }
    )
  })
}

shinyApp(ui, server)