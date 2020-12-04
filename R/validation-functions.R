column_is_iso8601_date <- function(column){
    s <- sprintf;
    p <- paste;
    sp <- function(a,b){
        paste(a,b,sep=" ");
    }
    function(state){
        whichcc <- function(a){
            which(a) %>% collapse_commas();
        }
        col <- state$data[[column]];
        col_unparsed <- state$data[[unparsed_column_name(column)]];
        simplified <- stringr::str_replace_all(col_unparsed,"-","");
        year_str <-  simplified %>% stringr::str_sub(1,4);
        simplified <- simplified %>% stringr::str_sub(5);
        month_str <- simplified %>% stringr::str_sub(1,2);
        simplified <- simplified %>% stringr::str_sub(3);
        day_str <- simplified %>% stringr::str_sub(1,2);

        year_len_checks <- stringr::str_length(year_str) == 4;
        month_len_checks <- stringr::str_length(month_str) == 2;
        day_len_checks <- stringr::str_length(day_str) == 2;

        message <- if(all_true(year_len_checks)) "" else s("Incorrectly formatted years on rows %s.", whichcc(!year_str));
        message <- if(all_true(month_len_checks))
                   {
                       message
                   } else {
                       sp(message, s("Incorrectly formatted months on rows %s.", whichcc(!month_len_checks)));
                   };
        message <- if(all_true(day_len_checks)){
                       message
                   } else {
                       sp(message, s("Incorrectly formatted days on rows %s.", whichcc(!day_len_checks)));
                   }
        if(message %!==% ""){
            return(extend_state(state,
                                "continuable",
                                check_report(s("Column %s is ISO8601 date compliant.", column),
                                             F,
                                             "There were these issues in %s: %s. Dates must be encoded per ISO8601 (YYYY-MM-DD or YYYYMMDD)", column, message)));
        }
        year <- as.numeric(year_str);
        month <- as.numeric(month_str);
        day <- as.numeric(day_str);

        day_ok <- valid_day(year, month, day);

        if(!all_true(day_ok)){
            return(extend_state(state,
                                "continuable",
                                check_report(s("Column %s is ISO8601 date compliant.", column),
                                             F,
                                             "Some dates, while syntactically valid, encode invalid calendar dates. %s rows: %s", column, which(!day_ok))))
        }
        extend_state(state,
                     "ok",
                     check_report(s("Column %s is ISO8601 date compliant.", column),
                                  T,
                                  "All dates in %s are valid.", column));
        
    }
}

column_exists <- function(column){
    function(state){
        ce <- ! is.null(state$data[[column]]);
        extend_state(state,
                     ifelse(ce,"ok","continuable"),
                     check_report(sprintf("Column %s exists.", column),
                                  ce,
                                  ifelse(ce,"Column %s exists.",
                                         "Column %s doesn't exist."),
                                  column));
    }
}

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

column_is_complete <- function(column){
    s <- sprintf;
    function(state){
        the_col <- state$data[[column]];
        nas <- is.na(the_col);
        n_na <- sum(nas);
        check <- identical(n_na, 0);
        extend_state(state,
                     ifelse(check,"ok","continuable"),
                     check_report(sprintf("Column %s is complete.", column),
                                  check,
                                  ifelse(check,
                                         sprintf("Column %s is complete.", column),
                                         sprintf("Column %s had missing elements at these indices: %s",
                                                 column,
                                                 collapse_commas(which(nas))))));
    }
}

column_is_numeric <- function(column){
    s <- sprintf;
    function(state){
        the_col <- state$data[[column]];
        if(class(the_col)=="numeric"){
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

column_is_integer <- function(column){
    s <- sprintf;
    function(state){
        the_col <- state$data[[column]];
        test_vec <- the_col %% 1 == 0
        falses <- which(test_vec==FALSE)
        if(FALSE %in% test_vec){
            extend_state(state,
                         "continuable",
                         check_report("Column contains only integers",
                                      F,
                                      "The column %s can only contain integers but it has non-integer values instead. Non-integer values appear at indeces %s.", 
                                      column,
                                      falses))
        } else {
            extend_state(state,
                         "ok",
                         check_report("Column contains only integers",
                                      T,
                                      "The column %s contains only integers", column));
        }
    }
}

column_in_integer_range <- function(column, values=c){
    function(state){
        check <- state$data[[column]] %in% values;
        n_good <- sum(check);
        n_bad <- sum(!check);
        ok <- n_bad == 0;
        extend_state(state,
                     ifelse(ok, "ok", "continuable"),
                     check_report(sprintf("Integer column draw from %s.", collapse_commas)(values),
                                  ok,
                                  ifelse(ok,"All values in the right range.".
                                         sprintf("These columns were out of range %s.",
                                                 collapse_commas(which(check))))));
    }    
}

column_is_float <- function(column){
  s <- sprintf;
  function(state){
    the_col <- state$data[[column]];
    if(typeof(the_col)=="double"){
      extend_state(state,
                   "ok",
                   check_report("Column type is floating point",
                                T,
                                "The column %s is floating point", column))
    } else {
      extend_state(state,
                   "continuable",
                   check_report("Column type is floating point",
                                F,
                                "The column %s must be a floating point but it appears to be %s instead.", column, typeof(the_col)));
    }
  }
}

column_in_codelist<-function(column, codelist){
    function(state){
        the_col <- state$data[[column]];
        the_col <- the_col[!is.na(the_col)]
        check <- the_col %in% codelist
        wrong <- the_col[!check] %>% unique()
        if(identical(sum(check), length(check))){
            extend_state(state,
                         "ok",
                         check_report("Column elements in codelist",
                                      T,
                                      "The column %s is in the codelist.", column))
        } else{
            extend_state(state,
                         "continuable",
                         check_report("Column elements in codelist",
                                      F,
                                      "The column %s has values that are not in the codelist. These values were not in the codelist: (%s)", 
                                      column, 
                                      collapse_commas(wrong)))
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

column_is_homogeneous <- function(column){
    s <- sprintf;
    function(state){
        col <- state$data[[column]];
        ucol <- unique(col);
        nu <- length(ucol);
        if(identical(nu==1)){
            extend_state(state,
                         "ok",
                         check_report(s("%s column is homogeneous.", column),
                                      T,
                                      "%s column is homogeneous.", column));
        } else {
            extend_state(state,
                         "continuable",
                         check_report(s("%s column is homogeneous.", column),
                                      F,
                                      "%s column has %d unique elements (%s)",
                                      nu,
                                      collapse_commas(ucol)));
        }
    }
}

check_domain_homogeneity <- column_is_homogeneous("DOMAIN");

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

