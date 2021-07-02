library(shiny);
library(shinyjs);
library(val);
library(readr);
library(dplyr);

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
        checkboxGroupInput("checkOptions",
                           "",
                           c("Only Failed Validations?"="tidy")),
        disabled(downloadButton("download","Download Results")),
        tableOutput("contents")

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

    observe({
        if(!(length(input$dataset) == 0)){
            enable("download");
        } else {
            disable("download");
        }
    });

    output$contents <- renderTable({
        req(input$dataset);
        df <- val_read_csv(input$dataset$datapath);
        out <- validate_generic(df)$messages;
        print(names(out))
        print(input$checkOptions)
        out <- if("tidy" %in% input$checkOptions){
                   print("Filtering.")
                   out %>% filter(pass==FALSE)
               } else {
                   print(sprintf("No filter %d rows.", nrow(out)));
                   out
               }
        out %>% head(100)
    });

    output$download <- downloadHandler(
        filename="results.csv",
        contentType="text/csv",
        content=function(file){
            req(input$dataset);
            df <- val_read_csv(input$dataset$datapath);
            out <- validate_generic(df)$messages;
            print(names(out))
            print(input$checkOptions)
            out <- if("tidy" %in% input$checkOptions){
                       print("Filtering")
                       out %>% filter(pass==FALSE)
                   } else {
                       print(sprintf("No filter %d rows.", nrow(out)));
                       out
                   }
            write_csv(out, file);
        }
    );
}

args <- commandArgs(trailingOnly=TRUE);

shinyApp(ui, server, options=list(port=as.numeric(args[[1]]),host="0.0.0.0"));
