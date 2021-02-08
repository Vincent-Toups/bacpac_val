library(devtools);
library(readODS);
library(tidyverse);

# In order to find the codelist for a question response (QSSTRESC)
# we need to look up the QSTESTCD in the WhereClauses table
# and then from there take the ID field and look up
# the Codelist in the ValueLevel table.
#
# The Value column in the WhereClauses table contains multiple
# WhereClauses IDs so we expand them here. This makes the Comparator
# column redundant because IN reduces to EQ when the values are split
# like this.
expand_where_clauses <- function(where_clauses){
    do.call(rbind,
            Map(function(df){
                values <- str_split(df$Value,",") %>%
                    unlist() %>% 
                    str_trim();
                lst <- list();
                for(n in names(df)){
                    lst[[n]] <- rep(df[[n]][[1]], length(values));
                }
                lst[["Value"]] <- values;
                do.call(tibble, lst);
            },
            split(where_clauses, where_clauses$ID)) %>% unname())
}

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
specification$WhereClauses <- expand_where_clauses(specification$WhereClauses);

## We can precalculate the code lists for each QSTESTCD here

#' Calculate a data frame which maps QSTESTCD values to either a code
#' list or a type.
#'
#' @param where_clauses - the where clauses page from the spec
#' @param value_level - the value level page from the spec
#' @param codelist the codelist page from the spec
#' @return a data frame with three columns - QSTESTCD, codelist, type
precalculate_question_codelists_and_types <- function(where_clauses, value_level, codelist){
    where_clauses <- expand_where_clauses(where_clauses);
    get_column <- `[[`;
    extract_qstestcd_values <- function(codelist){
        codelist %>%
            filter(ID == "QSTESTCD") %>%
            get_column('Term');
    }
    get_codelist <- function(where_clause_id){
        value_level %>%
            filter(`Where Clause` == where_clause_id) %>%
            get_column("Codelist");
    }
    get_type <- function(qstestcd_value){
        codelist %>%
            filter(Term == qstestcd_value) %>%
            get_column("Data Type");
    }
    qstestcd_value_to_info <- function(qstestcd_value){
        where_clause_id <- where_clauses %>%
            filter(qstestcd_value == Value) %>%
            get_column("ID");
        codelist_found <- !identical(where_clause_id, character(0));
        codelist_value <- ifelse(codelist_found, get_codelist(where_clause_id), NA);
        type <- get_type(qstestcd_value);#ifelse(codelist_found, NA, );
        tibble(id=qstestcd_value, codelist=codelist_value, type=type);
    }
    do.call(rbind, Map(qstestcd_value_to_info,
                       extract_qstestcd_values(codelist)));
}

specification$qstestcd_codelists <- precalculate_question_codelists_and_types(specification$WhereClauses,
                                                                specification$ValueLevel,
                                                                specification$Codelists);

#Below creates SC codelist similar to QS one above
where <- specification$WhereClauses %>% 
    filter(Variable=="SCTESTCD") %>%
    rename(id=Value, clause=ID) %>% 
    select(id, clause)

#Create simplified value level dataset that show the codelist values
values <- data[[4]] %>% 
    rename(clause=`Where Clause`,
           type=`Data Type`) %>% 
    filter(Variable=="SCSTRESC") %>% 
    select(type, clause, Codelist)

#Join these together
specification$scstresc_codelists <- where %>% 
    inner_join(values) %>% 
    select(!clause) %>% 
    rename(codelist=Codelist) %>% 
    select(id, codelist, type)

#Need to do the same logic to create SC codelist lookups for:
#SCMETHOD, SCORRES, SCORRESU, SCSTRESN, SCSTRESU
#SCMETHOD
values <- data[[4]] %>% 
    rename(clause=`Where Clause`,
           type=`Data Type`) %>% 
    filter(Variable=="SCMETHOD") %>% 
    select(type, clause, Codelist)

#Join these together
specification$scmethod_codelists <- where %>% 
    inner_join(values) %>% 
    select(!clause) %>% 
    rename(codelist=Codelist) %>% 
    select(id, codelist, type)

#SCORRES
values <- data[[4]] %>% 
    rename(clause=`Where Clause`,
           type=`Data Type`) %>% 
    filter(Variable=="SCORRES") %>% 
    select(type, clause, Codelist)

#Join these together
specification$scscorres_codelists <- where %>% 
    inner_join(values) %>% 
    select(!clause) %>% 
    rename(codelist=Codelist) %>% 
    select(id, codelist, type)

#SCORRESU
values <- data[[4]] %>% 
    rename(clause=`Where Clause`,
           type=`Data Type`) %>% 
    filter(Variable=="SCORRESU") %>% 
    select(type, clause, Codelist)

#Join these together
specification$scscorresu_codelists <- where %>% 
    inner_join(values) %>% 
    select(!clause) %>% 
    rename(codelist=Codelist) %>% 
    select(id, codelist, type)

#SCSTRESU
values <- data[[4]] %>% 
    rename(clause=`Where Clause`,
           type=`Data Type`) %>% 
    filter(Variable=="SCSTRESU") %>% 
    select(type, clause, Codelist)

#Join these together
specification$scstresu_codelists <- where %>% 
    inner_join(values) %>% 
    select(!clause) %>% 
    rename(codelist=Codelist) %>% 
    select(id, codelist, type)

#SCSTRESN
values <- data[[4]] %>% 
    rename(clause=`Where Clause`,
           type=`Data Type`) %>% 
    filter(Variable=="SCSTRESN") %>% 
    select(type, clause, Codelist)

#Join these together
specification$scstresn_codelists <- where %>% 
    left_join(values) %>% 
    select(!clause) %>% 
    filter(!is.na(type)) %>% 
    rename(codelist=Codelist) %>% 
    select(id, codelist, type)


set_names <- c("DM","QSMD","SC");

test_data <- list()

for(ds in set_names){
    test_data[[ds]] <- read_file(sprintf("source_data/%s.csv", ds));    
}

use_data(test_data, specification, overwrite=TRUE);



