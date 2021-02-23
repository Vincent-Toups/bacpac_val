library(shiny);
library(val);
library(readr);

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Dataset Validator"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

      fileInput("dataset", "Upload a Data Set:",
                multiple = FALSE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    mainPanel(

      # Output: Histogram ----
      tableOutput("contents")

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

    output$contents <- renderTable({
        req(input$dataset);
        df <- val_read_csv(input$dataset$datapath);
        validate_generic(df)$messages;
    });
}

args <- commandArgs(trailingOnly=TRUE);

shinyApp(ui, server, options=list(port=as.numeric(args[[1]]),host="0.0.0.0"));
