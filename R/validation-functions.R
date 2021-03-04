#' check_iso8601_durations - validate that a set of strings encode ISO 8601 durations
#' of the following sort: P3Y2M1DT1H3M5S
#'
#' @param s - a character array of values (NAs are propagated)
#' @return - a boolean array
check_iso8601_durations <- function(s){
    if(!identical(typeof(s),typeof(""))){
        stop("check_iso8601_durations requires a character array argument.");
    }
    s <- toupper(s); ## the standard is case insensitve but the parser
                     ## only validates uppercase for simplicity.
    check_ISO8601_durations_cpp(s);
}

column_is_iso8601_duration <- function(column){
    s <- sprintf;
    function(state){
        col <- state$data[[unparsed_column_name(column)]];
        check <- check_iso8601_durations(col);
        bad_count <- sum(check == FALSE, na.rm = TRUE);
        indexes <- state$data[["index__"]][which(check == FALSE)];
        ok = bad_count == 0;
        extend_state(state,
                     ifelse(ok, "ok", "continuable"),
                     check_report(s("Column (or column subset) %s contains ISO 8601 durations.", column),
                                  ok,
                                  ifelse(ok,
                                         sprintf("Column (or column subset) %s contains ISO 8601 durations.", column),
                                         s("Some values of column (or column subset) %s are not valid ISO 8601 Durations. The proper format is PnYnMnDTnHnMnS where n is an integer for all entries except the smallest non-zero one and where zero entries may be ellided unless the result would be P. If there are no non-zero hours, minutes or seconds, then these may be removed and if so, the trailing T must also be removed.", column)),
                                  collapse_commas(indexes %>% sort() %>% head(10))));
    }
}

#' column_is_iso8601_date - Returns a state function which checks
#' whether the column complies with the ISO8601 date formatting standard.
#'
#' @param column - the column to check
#' @return the state function which performs the check
#' @export 
column_is_iso8601_date <- function(column){
    s <- sprintf;
    p <- paste;
    sp <- function(a,b){
        paste(a,b,sep=" ");
    }
    function(state){
        whichcc <- function(a){
          state$data[["index__"]][which(a)] %>% sort() %>% head(10) %>% collapse_commas();
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

        message <- if(all_true(year_len_checks)) "" else s("Incorrectly formatted years on rows %s.", whichcc(!year_len_checks));
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
                                             s("There were these issues in %s: Dates must be encoded per ISO8601 (YYYY-MM-DD or YYYYMMDD)", column),
                                             message)));
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
                                             s("Some dates, while syntactically valid, encode invalid calendar dates in %s", column),
                                             whichcc(!day_ok))));
        }
        extend_state(state,
                     "ok",
                     check_report(s("Column %s is ISO8601 date compliant.", column),
                                  T,
                                  s("All dates in %s are valid.", column),
                                  NA));
        
    }
}

#' column_exists - Returns a state function which checks
#' whether the column is present within the state data.
#'
#' @param column - the column to check
#' @return the state function which performs the check
column_exists <- function(column){
    function(state){
        ce <- ! is.null(state$data[[column]]);
        extend_state(state,
                     ifelse(ce,"ok","continuable"),
                     check_report(sprintf("Column %s exists.", column),
                                  ce,
                                  ifelse(ce,
                                         sprintf("Column %s exists.", column),
                                         sprintf("Column %s doesn't exist.", column)),
                                  NA));
    }
}

#' column_is_textual - Returns a state function which checks
#' whether the column's class is character, i.e. textual.
#'
#' @param column - the column to check
#' @return the state function which performs the check
column_is_textual <- function(column){
    s <- sprintf;
    function(state){
        the_col <- state$data[[column]];
        if(class(the_col) %===% class("")){
            extend_state(state,
                         "ok",
                         check_report("Column type is text",
                                      T,
                                      s("The column %s is text", column),
                                      NA));
        } else {
            extend_state(state,
                         "continuable",
                         check_report("Column type is text",
                                      F,
                                      s("The column %s must be text but it appears to be %s instead.", column, class(the_col)),
                                      NA));
        }
    }
}

column_is_specified_decimal <- function(column, spec){
    if(!identical(length(spec),as.integer(1))){
        stop("column_is_specified_decimal can only accept one format to enforce.");
    }
    r <- stringr::str_match(spec, "^([0-9])+\\.([0-9]+)$");
    dim(r) <- 3;
    if(identical(is.na(r[[1]]),TRUE)){
        stop(sprintf("Invalid format for a specified decimal: %s", spec));
    }
    la <- as.numeric(r[[2]]);
    lb <- as.numeric(r[[3]]);
    function(state){
        colinfo <- state$data[[unparsed_column_name(column)]] %>%
            stringr::str_split("\\.");
        colinfo <- Map(stringr::str_length, colinfo);
        

        piece_length <- Map(length, colinfo) %>% unlist();
        valid_piece_lengths <- piece_length %in% c(1,2);
        a <- Map(function(x){x[[1]]}, colinfo);
        b <- Map(function(x){ifelse(length(x)>1,x[[2]],0)}, colinfo);
        check <- a <= la && b <= lb && valid_piece_lengths;
        bad_indices <- state$data[["index__"]][which(check == FALSE)];
        all_ok <- sum(check) == length(check);
        extend_state(state,
                     ifelse(all_ok, "ok", "continuable"),
                     check_report(sprintf("Column (or subset of a column) %s is consistent with a specified decimal like : %s.", column, spec),
                                  all_ok,
                                  ifelse(all_ok,
                                         "All values in spec.",
                                         "Some rows are out of spec."),
                                  ifelse(all_ok,
                                         NA,
                                         collapse_commas(bad_indices %>% sort() %>% head(10)))));
    }
}

valid_decimal_spec <- function(s){
    !is.na(stringr::str_match(s, "^([0-9])+\\.([0-9]+)$")[[1]])
}


#' text_column_matches_format parses the text column column and checks
#' it against `format`
#'
#' @param column - the column of interest
#' @param format - the text format of interest. Presently:
#'  (b).(a) - a number with b digits before a dot and a digits after
#'  ISO8601 - an ISO8601 duration.
text_column_matches_format <- function(column, format){
    if(is.na(format)){
        column_is_textual(column);
    } else if(format == "ISO8601"){
        column_is_iso8601_duration(column);
    } else if(valid_decimal_spec(format)) {
        column_is_specified_decimal(column, format);
    } else {
        column_is_textual(column);
    }
}

#' column_is_complete - Returns a state function which checks
#' whether the column is wholly without missing values, i.e. complete.
#'
#' @param column - the column to check
#' @return the state function which performs the check
column_is_complete <- function(column){
    s <- sprintf;
    function(state){
        the_col <- state$data[[column]];
        nas <- is.na(the_col);
        n_na <- sum(nas);
        check <- identical(n_na, 0L);
        extend_state(state,
                     ifelse(check,"ok","continuable"),
                     check_report(sprintf("Column %s is complete.", column),
                                  check,
                                  ifelse(check,
                                         sprintf("Column %s is complete.", column),
                                         sprintf("Column %s has missing elements.", column)),
                                  ifelse(check,
                                         NA,
                                         collapse_commas(state$data[["index__"]][which(nas)] %>% sort() %>% head(10)))));
    }
}

#' column_not_empty - Returns a state function which checks
#' whether the column has at least one non-missing value, i.e. not empty
#'
#' @param column - the column to check
#' @return the state function which performs the check
column_not_empty <- function(column){
  s <- sprintf;
  function(state){
    the_col <- state$data[[column]];
    check <- !all_true(is.na(the_col))
    extend_state(state,
                 ifelse(check,"ok","continuable"),
                 check_report(sprintf("Column %s is not empty", column),
                              check,
                              ifelse(check,
                                     sprintf("Column %s has at least one non-missing value.", column),
                                     sprintf("Column %s has zero non-missing values. It is empty.", column)),
                              NA));
  }
}

#' column_is_numeric - Returns a state function which checks
#' whether the column's class is numeric.
#'
#' @param column - the column to check
#' @return the state function which performs the check
column_is_numeric <- function(column){
    s <- sprintf;
    function(state){
        the_col <- state$data[[column]];
        if(class(the_col)=="numeric"){
            extend_state(state,
                         "ok",
                         check_report("Column type is numeric",
                                      T,
                                      s("The column %s is numeric", column),
                                      NA));
        } else {
            extend_state(state,
                         "continuable",
                         check_report("Column type is numeric",
                                      F,
                                      s("The column %s must be numeric but it appears to be %s instead.", column, class(the_col)),
                                      NA));
        }
    }
}

#' column_is_integer - Returns a state function which checks
#' whether all values of the column are integral.
#'
#' @param column - the column to check
#' @return the state function which performs the check
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
                                      s("The column %s should only contain integers but it has non-integer values.", column),
                                      collapse_commas(state$data[["index__"]][falses] %>% sort() %>% head(10))));
        } else {
            extend_state(state,
                         "ok",
                         check_report("Column contains only integers",
                                      T,
                                      s("The column %s contains only integers", column),
                                      NA));
        }
    }
}

#' column_in_integer_range - Returns a state function which checks
#' whether all entries of the column are contained within the set of values.
#'
#' @param column - the column to check
#' @param values - the set of values (defaults to the empty set)
#' @return the state function which performs the check
column_in_integer_range <- function(column, values=c()){
    function(state){
        check <- state$data[[column]] %in% values;
        n_good <- sum(check);
        n_bad <- sum(!check);
        ok <- identical(n_bad, 0L);
        extend_state(state,
                     ifelse(ok, "ok", "continuable"),
                     check_report(sprintf("Column %s drawn from the integers %s.", column, collapse_commas(values)),
                                  ok,
                                  ifelse(ok,
                                         "All values in the right range.",
                                         "Out of range values are present."),
                                  ifelse(ok,
                                         NA,
                                         collapse_commas(state$data[["index__"]][which(!check)] %>% sort() %>% head(10)))));
    }    
}

#' column_is_float - Returns a state function which checks
#' whether the column's type is double, i.e. floating point.
#'
#' @param column - the column to check
#' @return the state function which performs the check
column_is_float <- function(column){
  s <- sprintf;
  function(state){
    the_col <- state$data[[column]];
    if(typeof(the_col)=="double"){
      extend_state(state,
                   "ok",
                   check_report("Column type is floating point",
                                T,
                                s("The column %s is floating point", column),
                                NA));
    } else {
      extend_state(state,
                   "continuable",
                   check_report("Column type is floating point",
                                F,
                                s("The column %s must be a floating point but it appears to be %s instead.", column, typeof(the_col)),
                                NA));
    }
  }
}

#' column_covers_codelist - Returns a state function which checks
#' whether each value in the codelist appears at least once in the column.
#'
#' @param column - the column to check
#' @param codelist - the codelist to check - defaults to the codelist
#'     implied by the column
#' @param warn_only - when TRUE (default) only add a warning.
#' @return the state function which performs the check
column_covers_codelist <- function(column, codelist=column_to_codelist(column), warn_only=TRUE){
    function(state){
        unique_values < state$data[[column]] %>% unique();
        checks <- codelist %in% unique_values;
        check <- sum(checks) == length(codelist);
        missing_values <- codelist[!checks];
        ifelse(warn_only,
               extend_state(state,
                            "ok",
                            warnings=sprintf("Not every value in the codelist for column %s appears in the column.", column)),
               extend_state(state,
                            ifelse(check,"ok","continuable"),
                            check_report(sprintf("Column %s covers codelist", column),
                                         check,
                                         ifelse(check,
                                                sprintf("Column %s covers codelist.", column),
                                                sprintf("Column %s does not cover the entire codelist. These codelist values are missing: %s", column, collapse_commas(missing_values))),
                                         NA)));
    }
}

#' column_in_codelist - returns a state function which checks whether
#' a column's values are all in a given codelistq
#'
#' @param column - the column to check
#' @param codelist - the codelist to check against (defaults to the codelist implied by the column)
#' @return a state function to perform the check
column_in_codelist<-function(column, codelist=column_to_codelist(column), codelist_name=FALSE){
    function(state){
        the_col <- state$data[[column]];
        the_col <- the_col[!is.na(the_col)]
        check <- the_col %in% codelist
        wrong <- the_col[!check] %>% unique()
        check_name <- if(!identical(codelist_name,FALSE)){
                          sprintf("Column elements in codelist named %s.", codelist_name)
                      } else {
                          sprintf("Column elements in codelist.")
                      };
        if(identical(sum(check), length(check))){
            extended_message <- if(!identical(codelist_name,FALSE)){
                                    sprintf("The column %s is in the codelist %s.", column, codelist_name)
                                } else {
                                    sprintf("The column %s is in the codelist.", column)
                                }
            extend_state(state,
                         "ok",
                         check_report(check_name,
                                      T,
                                      extended_message,
                                      NA));
        } else{
            #print(codelist_name);
            msg <- if(!identical(codelist_name,FALSE)){
                       sprintf("The column %s has values that are not in the codelist named %s. These values were not in the codelist: (%s). The codelist: (%s)", 
                               column,
                               codelist_name,
                               collapse_commas(wrong),
                               collapse_commas(codelist))
                   } else {
                       sprintf("The column %s has values that are not in the codelist. These values were not in the codelist: (%s). The codelist: (%s)", 
                               column, 
                               collapse_commas(wrong),
                               collapse_commas(codelist))
                   };
            #print(msg);
            extend_state(state,
                         "continuable",
                         check_report(check_name,
                                      F,
                                      msg,
                                      collapse_commas(state$data[["index__"]][which(!check)] %>% sort() %>% head(10))));
        }
    }
}

#' check_domain_presence - Returns a state function which checks
#' whether the column 'DOMAIN' is present within the state data.
#'
#' @param state - the state to check
#' @return the state function which performs the check
check_domain_presence <- function(state){
    data <- state$data;
    if ("DOMAIN" %in% names(data)){
        extend_state(state,
                     "ok",
                     check_report("DOMAIN column present.",
                                  T,
                                  "There is a DOMAIN column.",
                                  NA));
    } else {
        extend_state(state,
                     "halted",
                     check_report("DOMAIN column present.",
                                  F,
                                  "There is not a DOMAIN column. Can't check a data set without knowing its DOMAIN.",
                                  NA));
    }
}

#' column_is_homogeneous - Returns a state function which checks
#' whether all values of the column are the same, i.e. homogeneous.
#'
#' @param column - the column to check
#' @return the state function which performs the check
column_is_homogeneous <- function(column){
    s <- sprintf;
    function(state){
        col <- state$data[[column]];
        ucol <- unique(col);
        nu <- length(ucol);
        if(identical(nu, 1L)){
            extend_state(state,
                         "ok",
                         check_report(s("%s column is homogeneous.", column),
                                      T,
                                      s("%s column is homogeneous.", column),
                                      NA));
        } else {
            extend_state(state,
                         "continuable",
                         check_report(s("%s column is homogeneous.", column),
                                      F,
                                      s("%s column has %d unique elements (%s).", column, nu, collapse_commas(ucol)),
                                      NA));
        }
    }
}

check_domain_homogeneity <- column_is_homogeneous("DOMAIN");

check_domain_known <- function(domains=c("QS","DM","SC")){
    function(state){
        domain <- unique(state$data$DOMAIN);
        if(domain %in% domains){
          extend_state(state, "ok",
                       check_report("Known DOMAIN",
                                    T,
                                    sprintf("Domain %s is valid.", domain),
                                    NA));
        }
        else{
          extend_state(state, "halted",
                       check_report("Known DOMAIN",
                                    F,
                                    sprintf("Domain %s is not valid (valid domains %s).", domain, collapse_commas(domains)),
                                    NA));
        } 
    }
}

mandatory_codelist_column <- function(col){
        bailout_validation_chain(
            column_exists(col),
            column_is_textual(col),
            column_is_complete(col),
            column_in_codelist(col));
}

#' build_subset_key_info given a validation_table return a useful
#' breakdown of information therein.
#'
#' @param validation_table - a table with N+2 (or N+1) columns. The
#'     first N are columns in a data set whose values form unique keys
#'     which map rows of the data set to a validation function. The
#'     next column (validation_function__) is the validation function
#'     to apply to the subset. The last (optional column) indicates
#'     whether the validation is required.
#' @return a list containing a table with just the keys, a character
#'     array of key names, and a list of key summaries - a string
#'     containing a text description of each column and its value. It
#'     also returns a validation_table which includes the optional
#'     required__ column, set to all FALSE if it was missing.
#' 
build_subset_key_info <- function(validation_table){
    if(column_missing(validation_table, "required__")){
        validation_table$required__ <- rep(FALSE, nrow(validation_table));
    }
    ncol <- length(names(validation_table));
    keys <- validation_table %>%
        dplyr::select(1:(ncol-2));
    key_names <- names(keys);
    key_summaries <- summarize_column_values(keys, key_names);
    list(validation_table=validation_table,
         key_names=key_names,
         key_summaries=key_summaries);
}

#' all_subsets_validated - Return a validation function which confirms
#' that each subset of rows indicated by the validation table keys
#' actually has a corresponding check.
#'
#' @param validation_table - the table of validation functions
#'     consisting of N+2 or N+1 columns. The first N columns consist
#'     of keys into the data set you are validating while the last two
#'     columns contain the validation functions for each subset and
#'     whether the validation is required.
#'
#' @param checked_column - the column to which the validation function
#'     corresponds. Technically any validation function whatsoever can
#'     be invoked on any subset but typically we are validating one
#'     column at a time. This is provided so the error message can be
#'     more informative.
#'
#' @return a state function which checks whether all subsets implied
#'     by the validation table in the source data have a corresponding
#'     validation function.
all_subsets_validated <- function(validation_table, checked_column){
    info <- build_subset_key_info(validation_table);
    keys <- info$keys;
    key_names <- info$key_names;
    key_summaries <- info$key_summaries;
    
    if(is.null(validation_table$validation_function__)){
        stop("Validate on subsets requires a table with a validation_function__ column in last or second to last position");
    }

    function(state){
        ## join the validation table to the data table so we can easily
        ## access each validation function from the split we are about
        ## to perform
        
        data <- state$data %>%
            dplyr::left_join(validation_table,by=dplyr::all_of(key_names));
        
        absent_validation_function_indices <- data %>%
            dplyr::filter(is.na(validation_function__)) %>%
            `[[`("index__");

        check <- length(absent_validation_function_indices)==0;

        error_message <- "This check ensures that values of the column %s are consistent with the values implied by the key columns: %s. There were failures.";

        extend_state(state,
                     ifelse(check,"ok","continuable"),
                     check_report("All values in column %s corresponding to these indexes %s have validations.",
                                  check,
                                  ifelse(check,
                                         "Check passed.",
                                         sprintf(error_message, checked_column, paste(key_names, collapse = ", "))),
                                  collapse_commas(absent_validation_function_indices %>% sort() %>% head(10))));        
    }
}

#' all_validations_applied - returns a validation function which
#' checks whether every validation in a validation table is actually
#' applied.
#'
#' This is a warning condition. It may be more clear and more
#' efficient to use column_covers_codelist for this purpose instead.
#'
#' @param validation_table - a table of N+2 values. N columns from the
#'     data set form the keys which map a validation (in the Nth
#'     column, validation_function__) to the appopriate subset. The
#'     Nth+2 colum indicates whether the validation is required. It is
#'     optional.
#' @param warning_text - the warning text to add to the validation
#'     state if this isn't true.
#' @return the validation function.
all_validations_applied <- function(validation_table, warning_text){
    info <- build_subset_key_info(validation_table);
    keys <- info$keys;
    key_names <- info$key_names;
    key_summaries <- info$key_summaries;
    
    if(is.null(validation_table$validation_function__)){
        stop("Validate on subsets requires a table with a validation_function__ column in last or second to last position");
    }

    function(state){
        ## join the validation table to the data table so we can easily
        ## access each validation function from the split we are about
        ## to perform

        key_data <- state$data %>%
            dplyr::select(dplyr::all_of(key_names)) %>%
            dplyr::distinct() %>% dplyr::mutate(dummy__=rep(TRUE, nrow(.)));

        keys %>% dplyr::left_join(key_data, by=dplyr::all_of(key_names));
        missing <- keys %>% dplyr::mutate(missing__=is.na(dummy__)) %>% `[[`("missing__");
        if(sum(missing__)==0){
            state
        } else {
            extend_state(state,
                         "ok",
                         warnings=warning_text);
        }        
    }
}

#' validate_on_subsets: accepts a table with N columns The first N-2
#' columns are the same as column names from the data set and are used
#' to split the data set into sub groups.
#'
#' The Nth column ('validation_function__')is a validation function to
#' be applied to each matching subset. The Nth + 1 column indicates
#' via a boolean 'required__' whether the validation function must be
#' tested. If there is no required column it is assumed that all
#' validation functions are required.
#'
#' The results are combined as if validation_chain. Furthermore, a
#' final check confirms that each subset of the data has been
#' validated by a validation function and that each validation
#' function has operated on a subset if required. 
#'
#' 
validate_on_subsets <- function(validation_table, check_name=""){
    info <- build_subset_key_info(validation_table);
    validation_table <- info$validation_table;
    keys <- info$keys;
    key_names <- info$key_names;
    key_summaries <- info$key_summaries;
    
    function(state){
        ## join the validation table to the data table so we can easily
        ## access each validation function from the split we are about
        ## to perform

        key_names <- key_names;
        validation_table <- validation_table;
        keys <- keys;
        key_summaries <- key_summaries;        
        
        data <- state$data %>%
            dplyr::inner_join(validation_table,by=dplyr::all_of(key_names));        
        
        the_splits <- split(data, data %>%
                                  dplyr::select(dplyr::all_of(key_names)));

        final_state <- Reduce(function(acc_state, sub_df){
            validation_function <- sub_df$validation_function__[[1]];
            if(typeof(validation_function)!="closure"){
                stop(sprintf("Can't find a validation function for the sub data frame (head) \n%s", (paste(capture.output(sub_df %>% head(10)),collapse="\n"))));
            }
            key_values <- sub_df %>% dplyr::select(dplyr::all_of(key_names)) %>% dplyr::distinct();
            if(nrow(key_values) != 1){
                .GlobalEnv$irritant <- key_values;
                .GlobalEnv$irritant_sub_df <- sub_df;
                stop("Each sub_df should have but one unique set of keys. See global irritant.")
            }
            prefix_message <- Map(function(nm){
                sprintf("%s = %s", nm, key_values[[nm]][[1]]);
            }, names(key_values)) %>% unlist() %>% paste(collapse = ", ") %>% sprintf("(context: %s)",.);            
            
            sub_data <- sub_df %>% 
                dplyr::select(-validation_function__,-required__);
            ## we create a fresh validation state for each subset so
            ## that we can harvest the messages, state and warning for
            ## each.            
            combine_states(acc_state, validation_function(fresh_state(sub_data)) %>% prefix_messages(prefix_message));               
        }, the_splits,
        init=state);
        
        
        final_state
    }
}


#' build a validator for the common case where one column of
#' codelisted and formatted values depends on the values of another.
#'
#' @param key_column - the column which determines the appropriate
#'     check
#' @param check_column - the column to check
#' @param check_name - an additional informative name for the whole
#'     check before performed
#' @return a validation function which splits its data by key_column
#'     and applies a standard set of checks based on the specification
#'     to the resulting sub data frames before merging the results.
check_simple_dependent_column <- function(key_column,
                                          check_column,
                                          check_name=sprintf("Check that %s is consistent with %s.", check_column, key_column)){
    key_column <- key_column;
    check_column <- check_column;
    rows <- key_column_to_codelists(key_column) %>%
        dplyr::filter(value_column == check_column) %>%
        dplyr::select(value, codelist, data_type, text_format) %>% 
        dplyr::distinct();
    validation_table <- do.call(rbind, Map(function(i){
        l <- list();
        l[[key_column]] <- rows$value[[i]];
        codelist <- rows$codelist[[i]];
        codelist_vals <- get_codelist(codelist);
        text_format <- rows$text_format[[i]];
        l$validation_function__ <- list(ifelse(!is.na(codelist),
                                               column_in_codelist(check_column, codelist_vals, codelist),
                                               text_column_matches_format(check_column,text_format)))
        as_tibble(l);        
    }, seq(nrow(rows))));
    validate_on_subsets(validation_table, check_name);
}


#' key_column_to_codelists - given a key column return the columns and
#' codelists corresponding to each unique value of the key.
#'
#' @param key_column - a column whose values indicate the appropriate
#'     codelist to evaluate other columns.
#' @return a data frame with keys
#'  key_column the column used to calculate the other values
#'  value the value type of the column (used to key the whereclause)
#'  value_column the column determined by the value above
#'  codelist the codelist for the value_column values
#'  data_type the data type for the value column
#'  text_format additional format information in the case of text fields
#'  term the term 
#'  order the order of the term
#'
#' @examples
#'
#' key_column_to_codelists("QSTESTCD") %>%
#'  filter(value=="EDANX01" & value_column=="QSSTRESC") %>%
#'    `[[`("term");
#' Fetches the codelist terms for the QSSTRESC column for value = 'EDANX01' in QSTESTCD.
#' 
key_column_to_codelists <- function(key_column){
    expand_where_clauses(bt_specification$WhereClauses) %>%
        dplyr::filter(Variable==key_column) %>%
        dplyr::left_join(bt_specification$ValueLevel, by=(c("ID"="Where Clause")), suffix=c("_wc","_vl")) %>%
        transmute(dataset=Dataset_wc,
                  key_column=Variable_wc,
                  value=Value,
                  value_level_datatype=`Data Type`,
                  value_column=Variable_vl,
                  text_format=Format,
                  codelist=Codelist) %>%
        dplyr::left_join(bt_specification$Codelists, by=c("codelist"="ID")) %>%
        transmute(dataset=dataset,
                  key_column=key_column,
                  value=value,
                  value_column=value_column,
                  codelist=codelist,
                  data_type=coalesce(`Data Type`, value_level_datatype),                  
                  term=Term,
                  text_format=text_format,
                  order=Order) %>% dplyr::distinct();
}

#' find values in key_column for which there is no where_clause
#' mapping to a type.
#'
#' @param key_column - the column to inspect
#' @return the values for which there is no where clause
missing_where_clauses <- function(key_column){
    terms <- bt_specification$Codelist %>% dplyr::filter(ID==key_column) %>% `[[`("Term");
    wc_terms <- expand_where_clauses(bt_specification$WhereClauses) %>% `[[`("Value");
    terms[!(terms %in% wc_terms)];
}
