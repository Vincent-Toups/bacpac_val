library(tidyverse);
library(rjson);

files <- commandArgs(trailingOnly=T);

uniqueReductionPercentage <- function(vals){
    n <- length(vals);
    nu <- length(unique(vals));
    nu/n
}

buildMetaData <- function(file){
    dataframe_all <- read_csv(file);
    dataframe <- dataframe_all %>% filter(complete.cases(.));
    output <- list();
    output$columns <- names(dataframe);
    for(column in names(dataframe)){
        column_values <- dataframe[[column]];
        md <- mode(column_values);
        output[[column]] <- unique(column_values);
        output[[paste(column,"type",sep="_")]] <- mode(column_values);
        if(identical(md,"numeric")){            
            output[[paste(column,"range",sep="_")]] <- c(min(column_values),
                                     max(column_values));
        }
        
    }
    output$rows <- nrow(dataframe);
    output$source_file <- file;
    output$complete <- nrow(dataframe_all) == nrow(dataframe);
    output
}

last <- function(x){
    x[length(x)];
}

first <- function(x){
    x[1];
}

to_metadata_filename <- function(x){
    paste("meta_data/",x %>% str_split("/", simplify=T) %>% last() %>%
        str_split("\\.", simplify=T) %>% first() %>%
        paste(".json",sep=""),sep="");
}

dir.create("meta_data", showWarnings=F);

for (f in files) {
    mdf <- to_metadata_filename(f);
    md <- buildMetaData(f);
    json_str <- toJSON(md,indent=1);
    cat(json_str, file=mdf, append=FALSE);
}
