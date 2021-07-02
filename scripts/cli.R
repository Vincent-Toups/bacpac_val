#!/usr/local/bin/Rscript

library(val);
library(readr);
library(tidyverse);

args <- commandArgs(trailingOnly=TRUE);

print_usage <- function(){
    write(sprintf("bacpac_validate - input-file output-file"))
}

brief <- if(length(args)>=1 && (args[[1]] == "-b"||args[[1]] == "--brief")){
             TRUE;
         } else {
             args <- args[2:(length(args)-1)];
             FALSE;
         };

if(length(args)!=2 | !file.exists(args[[1]])){
    print_usage();
    stop("Error - bad args. Input file must exist and be a CSV file and output file must be explicitly provided.")    
}

input <- args[[1]];
output <- args[[2]];

if(file.exists(output)){
    file.remove(output);   
}

data <- val_read_csv(input);

result <- validate_generic(data);

out <- result$messages;

if(brief){
    out <- out %>% filter(pass==FALSE);
} else {
    out <- out;
}

readr::write_csv(out, output);
