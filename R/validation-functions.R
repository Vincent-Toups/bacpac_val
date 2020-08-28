column_is_textual <- function(column){
    s <- sprintf;
    function(state){
        the_col <- state$data[[column]];
        if(class(the_col) %===% class("")){
            extend_state(state,
                         "ok",
                         check_report("Column type is text",
                                      T,
                                      "The column %s is text", column))
        } else {
            extend_state(state,
                         "continuable",
                         check_report("Column type is text",
                                      F,
                                      "The column %s must be text but it appears to be %s instead.", column, class(the_col)));
        }
    }
}

check_domain_presence <- function(state){
    data <- state$data;
    if ("DOMAIN" %in% names(data)){
        extend_state(state,
                     "ok",
                     check_report("DOMAIN column present.",
                                  T,
                                  "There is a DOMAIN column."));
    } else {
        extend_state(state,
                     "halted",
                     check_report("DOMAIN column present.",
                                  F,
                                  "There is not a DOMAIN column. Can't check a data set without knowing its DOMAIN."))
    }
}

check_domain_homogeneity <- function(state){
    data <- state$data;
    if(identical(length(unique(data$DOMAIN)),1)){
        extend_state(state,
                     "ok",
                     check_report("DOMAIN column homogeneous.",
                                  T,
                                  "There is only one unique value in the DOMAIN column."))
    } else {
        extend_state(state,
                     "halted",
                     check_report("DOMAIN column homogeneous.",
                                  F,
                                  "DOMAIN column not homogenous. These values found: %s.", collapse_commas(unique(data$DOMAIN))))
    }
}

check_domain_known <- function(domains=unique(specification$Datasets$Dataset)){
    function(state){
        domain <- unique(state$data$DOMAIN);
        if(domain %in% domains)
            extend_state(state, "ok",
                         check_report("Known DOMAIN", T, "Domain %s is valid.", domain))
        else extend_state(state, "halted",
                          check_report("Known DOMAIN", F, "Domain %s is not valid (valid domains %s).",domain,
                                       collapse_commas(domains)));
    }
}
