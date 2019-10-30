base_file<-read.csv("datadiscovery.csv",  header = TRUE, sep = ",", dec = ".", comment.char = "", strip.white = TRUE,
                    stringsAsFactors = FALSE, encoding="UTF-8")



saveRDS(base_file,file="data_sources.rds")


library(shiny)
library(DT)




saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("base_file")) {
    base_file <<- rbind(base_file, data)
    saveRDS(base_file,file="data_sources.rds")
  } else {
    base_file<-read.csv("datadiscovery.csv",  header = TRUE, sep = ",", dec = ".", comment.char = "", strip.white = TRUE,
                        stringsAsFactors = FALSE, encoding="UTF-8")
    base_file <<- rbind(base_file, data)
    saveRDS(base_file,file="data_sources.rds")
  }
}

loadData <- function() {
  if (exists("base_file")) {
    base_file
  }
  else{
    base_file<-read.csv("datadiscovery.csv",  header = TRUE, sep = ",", dec = ".", comment.char = "", strip.white = TRUE,
                        stringsAsFactors = FALSE, encoding="UTF-8")
  }
}

# Define the fields we want to save from the form
fields <- c("Source Category", "used_shiny", "r_num_years")
fields <- c("Source Category","Dataset(s) Name",	"Data Source (Who publishes the data?)",	"Link",	"Description of Dataset(s)",	"Description of Data Source",	"Data Type (Survey, Administrative, Opportunity), Stability, and Transparency",	"Time Period Coverage",	"Unit of Coverage",	"Geographical Coverage",	"Data Format",	"Accessibility",	"Category (Education, Employment)",	"Subpopulations and Geographies Coverage & Comparatibility",	"STW Inclusion",	"Comparability to National Survey Measures",	"Linkage to Other Sources",	"Data Dictionary",	"Aggregated Data Tools",	"Potential Use to Stakeholders",	"Potential Use to General Public",	"Notes")
# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
    DT::dataTableOutput("sources_data", width = 300), tags$hr(),
    textInput("Source Category", "Source Category", ""),
    textInput("Dataset(s) Name", "Dataset(s) Name", ""),
    textInput("Data Source (Who publishes the data?)","Data Source (Who publishes the data?)", ""),
    textInput("Link", "Link", ""),
    textInput("Description of Dataset(s)","Description of Dataset(s)", ""),
    textInput("Description of Data Source", "Description of Data Source", ""),
    textInput("Data Type (Survey, Administrative, Opportunity), Stability, and Transparency","Data Type (Survey, Administrative, Opportunity), Stability, and Transparency", ""),
    textInput("Time Period Coverage", "Time Period Coverage", ""),
    textInput("Unit of Coverage", "Unit of Coverage", ""),
    textInput("Geographical Coverage", "Geographical Coverage", ""),
    textInput("Data Format	Accessibility", "Data Format	Accessibility", ""),
    textInput("Category (Education, Employment)", "Category (Education, Employment)", ""),
    textInput("Subpopulations and Geographies Coverage & Comparatibility", "Subpopulations and Geographies Coverage & Comparatibility", ""),
    textInput("STW Inclusion", "STW Inclusion", ""),
    textInput("Comparability to National Survey Measures", "Comparability to National Survey Measures", ""),
    textInput("Linkage to Other Sources", "Linkage to Other Sources", ""),
    textInput("Data Dictionary	Aggregated Data Tools", "Data Dictionary	Aggregated Data Tools", ""),
    textInput("Potential Use to Stakeholders", "Potential Use to Stakeholders", ""),
    textInput("Potential Use to General Public", "Potential Use to General Public", ""),
    textInput("Notes", "Notes", ""),
    #checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
    #sliderInput("r_num_years", "Number of years using R",
    #           0, 25, 2, ticks = FALSE),
    actionButton("submit", "Submit")
  ),
  server = function(input, output, session) {
    if (!file.exists("data_sources.RDS")) {
      get_sources = function() {
        if (exists("base_file")) {
         return(base_file) 
        }
        else{
          base_file<-read.csv("datadiscovery.csv",  header = TRUE, sep = ",", dec = ".", comment.char = "", strip.white = TRUE,
                              stringsAsFactors = FALSE, encoding="UTF-8")
          return(base_file)
        }

      }
      sources_data <- get_sources()
      #crimes_data$Category <- crimes_data$crime_category
      
      saveRDS(sources_data, "data_sources.RDS")
    }
    
    print("Loading Data File...")
    sources_data <- readRDS("data_sources.RDS")
    
    make_datatable <- function(sources_data) {
      maditr::setDT(sources_data)
      dataset <- sources_data
      dt <-
        DT::datatable(
          dataset,
          extensions = 'Buttons',
          filter = "top",
          options = list(
            buttons = c('copy', 'csv', 'excel', 'print'),
            scrollX = TRUE,
            fixedColumns = TRUE
          )
        )
      dt
    }
    make_datatable(sources_data)  

    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
      
      
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$sources_data <- DT::renderDataTable(          extensions = 'Buttons',
                                                         filter = "top",
                                                         options = list(
                                                           buttons = c('copy', 'csv', 'excel', 'print'),
                                                           dom="lfrtipSB",
                                                           fixedColumns = TRUE
                                                         ),{
      input$submit
      loadData()
  })     


  })
