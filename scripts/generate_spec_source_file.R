
load_a_spec <- function(filename, pages=pages <-
                                      stringr::str_split("Study
Datasets
Variables
ValueLevel
WhereClauses
Codelists
Dictionaries
Methods
Comments
Documents","
")[[1]]){

    data <- list();

    for (page in pages){
        data[[page]] <- readODS::read_ods(filename, page) %>% as_tibble();
    }

    specification <- data;
    specification$WhereClauses <- expand_where_clauses(specification$WhereClauses);
    specification
}

specification <- load_a_spec("source_data/STDSPECS_MinimumDataset.ods");
specification_ex <- load_a_spec("source_data/STDSPECS_EX.ods");

create_data_source_file("R/prep-spec.R", list("bt_specification"=specification,
                                              "bt_specification_ex"=specification_ex));
for(v in c("bt_specification","bt_specification_ex")){
    cat(sprintf("for(n in names(%s)){
       %s[[n]] = tibble::as_tibble(%s[[n]])
}", v, v, v), file="R/prep-spec.R", append=TRUE, sep="\n");
}

