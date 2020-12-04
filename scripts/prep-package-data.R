library(devtools);
library(readODS);
library(tidyverse);

pages <-
    str_split("Study
Datasets
Variables
ValueLevel
WhereClauses
Codelists
Dictionaries
Methods
Comments
Documents","
")[[1]];

data <- list();

for (page in pages){
    data[[page]] <- read_ods("source_data/STDSPECS_MinimumDataset.ods", page) %>% as_tibble();
}

specification <- data;

set_names <- c("DM","QSMD","SC");

test_data <- list();

for(ds in set_names){
    test_data[[ds]] <- read_file(sprintf("source_data/%s.csv", ds));    
}

use_data(test_data, specification, internal=TRUE);



