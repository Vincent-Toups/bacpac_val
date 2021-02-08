#!/bin/Rscript

library(val);

args <- commandArgs(trailingOnly=TRUE);

print_usage <- function(){
    write(sprintf("bacpac_validate - input-file output-file"))
}

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

readr::write_csv(result$messages, output);
