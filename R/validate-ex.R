validate_qsmd <- block({
    build_validation_table <- function(key_column, check_column){
        key_column <- key_column;
        check_column <- check_column;
        rows <- key_column_to_codelists(key_column, specification = bt_specification_ex) %>%
            dplyr::filter(value_column == check_column) %>%
            dplyr::select(value, codelist, data_type, text_format) %>% 
            dplyr::distinct();
        do.call(rbind, Map(function(i){
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
    }

    bchain <- bailout_validation_chain;
    schain <- function(column, ...){
        simple_val_funs <- list(...);
        applied <- Map(function(f){
            f(column);
        }, simple_val_funs);
        do.call(bchain, applied);
    }

    textual_homogeneous <- function(column){
        schain(column,
               column_exists,
               column_is_textual,
               column_is_homogeneous);
    }

    textual_complete <- function(column){
        schain(column,
               column_exists,
               column_is_textual,
               column_is_complete)
    }

    textual_optional <- function(column){
        schain(column,
               column_exists,
               column_is_textual,
               column_is_complete)
    }

    float_complete <- function(column){
        schain(column,
               column_exists,
               column_is_float,
               column_is_complete);
    }


    integer_complete <- function(column){
        schain(column,
               column_exists,
               column_is_integer,
               column_is_complete);
    }

    float_optional <- function(column){
        schain(column,
               column_exists,
               column_is_float);
    }

    codelist_complete <- function(column){
        bchain(schain(column,
                      column_exists,
                      column_is_complete,
                      column_is_textual),
               column_in_codelist(column,
                                  column_to_codelist(column,
                                                     specification=bt_specification_ex)));
    };

    codelist_optional <- function(column){
        bchain(schain(column,
                      column_exists,
                      column_is_textual),
               column_in_codelist(column,
                                  column_to_codelist(column,
                                                     specification=bt_specification_ex)));
    }
    

    check_studyid  <- textual_homogeneous("STUDYID");
    check_domain   <- bchain(textual_homogeneous("DOMAIN"),
                             check_domain_known(domains="DEV"));
    check_usubjid  <- textual_complete("USUBJID");
    check_exseq    <- integer_complete("EXSEQ");
    check_excat    <- codelist_complete("EXCAT");
    check_extrt    <- codelist_complete("EXTRT");
    check_extrtoth <- textual_complete("EXTRTOTH");
    check_exacn    <- codelist_optional("EXACN");
    check_exadj    <- codelist_optional("EXADJ");
    
    ## NB - the spec indicates this is a codelist column but I believe
    ## this is in error
    check_exdose   <- float_optional("EXDOSE");
    ## NB - as a matter of exactness this check should only apply when
    ## EXDOSE is provided - its an error to leave off the units of the
    ## dose and an irregularity to indicate a unit without a dose
    check_exdoseu  <- codelist_optional("EXDOSEU");
    check_exroute  <- codelist_optional("EXROUTE");
    check_exdrfl   <- textual_optional("EXDRVFL");
    check_visitnum <- float_complete("VISITNUM");
    check_visit    <- textual_complete("VISIT");
    check_exdtc    <- column_is_iso8601_date("EXDTC");
    check_exdy     <- integer_complete("EXDY");
    check_exevlint <- textual_complete("EXEVLINT");

    validate_ex <- validation_chain(check_studyid,
                                    check_domain,
                                    check_usubjid,
                                    check_exseq,
                                    check_excat,
                                    check_extrt,
                                    check_extrtoth,
                                    check_exacn,
                                    check_exdose,
                                    check_exdoseu,
                                    check_exroute,
                                    check_exdrfl,
                                    check_visitnum,
                                    check_visit,
                                    check_exdtc,
                                    check_exdy,
                                    check_exevlint);    
})
