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

check_domain_known <- function(domains){
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


check_domain_known() %v% check_domain_homogeneity() %v% check_domain_known()

domain_check <- validation_chain(check_domain_presence,
                                 check_domain_homogeneity,
                                 check_domain_known);

## check_domain_validity <- function(state){
##     df <- state$df;
##     s <- sprintf;
##     if("domain" %not-in% tolower(names(df))){
##         update_state(state,
##                      "halted",
##                      check_report("domain column not present",
##                                   F,
##                                   "Domain column not present. Can't check data without knowing its domain."))
##     } else {
##         names(df) <- tolower(names(df));
##         unique_domain_entries <- unique(df$domain);
##         known_domains <- extract_domains(state$rules);
##         if(length(unique_domain_entries)!=1){
##             update_state(state,
##                          "halted",
##                          check_report("domain column does not contain more than one domain",
##                                       F,
##                                       "Domain column contains multiple distinct values. It must contain just one. Values: %s.", comcol(unique(df$domain))))
##         } else if(toupper(unique(df$domain)) %not-in% known_domains){
##             update_state(state,
##                          "halted",
##                          check_report(F,
##                                       s("incorrect or unknown domain %s", unique_domain_entries),
##                                       "The domain %s is not in the list of known domains %s",
##                                       unique_domain_entries, comcol(known_domains)))
##         } else {
##             st <- update_state(state,
##                                "ok",
##                                check_report(T,
##                                             s("domain %s ok.",unique_domain_entries),
##                                             "The domain %s is in the approved list.", unique_domain_entries));
##             st$domain <- unique_domain_entries;
##         }
##     }
## }
