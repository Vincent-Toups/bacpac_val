validate_ft <- block({
    check_studyid <- block({
        col <- "STUDYID";
        bailout_validation_chain(
            column_exists(col),
            column_is_textual(col),
            column_is_homogeneous(col)
        )
    });
    
    check_domain <- block({
        col <- "DOMAIN";
        bailout_validation_chain(
            column_exists(col),
            column_is_textual(col),
            column_is_homogeneous(col),
            check_domain_known(domains='FT')
        )
    });

    check_usubjid <- block({
        col <- "USUBJID";
        bailout_validation_chain(
            column_exists(col),
            column_is_textual(col),
            column_is_complete(col)
        )
    });

    check_ftseq <- block({
        col <- "FTSEQ";
        ## this should be sequential by per subject ID
        bailout_validation_chain(
            column_exists(col),
            column_is_integer(col),
            column_is_complete(col),
            function(state){
                data <- state$data;
                checkdf <- data %>% group_by(USUBJID) %>%
                    arrange(FTSEQ) %>%
                    mutate(contiguity=FTSEQ==1:length(FTSEQ)) %>%
                    summarize(contiguous=length(contiguity)==sum(contiguity)) %>%
                    filter(!contiguous);
                n_failures <- row(checkdf);
                ok <- n_failures == 0;
                extend_state(state,
                             ifelse(ok,"ok","continuable"),
                             check_report("Column FTSEQ contains contiguous, increasing, integer values on a per USUBJID basis.",
                                          ok,
                                          ifelse(ok,
                                                 "The FTSEQ column contains contiguous integer values on a per USUBJID basis.",
                                                 sprintf("The FTSEQ column contains non-contigous values for USUBJIDs in the following list: %s", collapse_commas(checkdf %>% pull(USUBJIDs))))));                
            }
        );
    });
    check_ftcat <- mandatory_codelist_column("FTCAT",specification=bt_specification_ft);
    check_fttestcd <- mandatory_codelist_column("FTTESTCD",specification=bt_specification_ft);
    check_fttest <- mandatory_codelist_column("FTTEST",specification=bt_specification_ft);
    check_ftpos <- column_in_codelist("FTPOS",specification=bt_specification_ft);
    check_ftloc <- column_in_codelist("FTLOC",specification=bt_specification_ft);
    check_ftlat <- column_in_codelist("FTLAT",specification=bt_specification_ft);    
    check_ftstresc <- check_simple_dependent_column("FTTESTCD",
                                                    "FTSTRESC",
                                                    specification=bt_specification_ft);
    check_ftstresn <- column_is_numeric("FTSTRESN");
    check_ftstresu <- column_in_codelist("FTSTRESU",specification=bt_specification_ft);
    check_ftstat <- mandatory_codelist_column("FTSTAT",specification=bt_specification_ft);
    check_ftreasn <- column_is_textual("FTREASN");
    check_ftdrvl <- column_is_textual("FTDRVFL");
    check_ftrepnum <- column_is_integer("FTREPNUM");
    check_ftaid <- column_in_codelist("FTAID", specification=bt_specification_ft);
    check_ftaidoth <- column_is_textual("FTAIDOTH");
    check_visitnum <- bailout_validation_chain(column_is_complete("VISITNUM"),
                                               column_is_integer("VISITNUM"));
    check_visit <- bailout_validation_chain(column_is_complete("VISIT"),
                                            column_is_textual("VISIT"));
    check_ftdtc <- bailout_validation_chain(column_is_complete("FTDTC"),
                                            column_is_iso8601_date("FTDTC"));
    check_ftdy <- bailout_validation_chain(column_is_complete("FTDY"),
                                           column_is_integer("FTDY"));
    validation_chain(check_studyid,
                     check_domain,
                     check_usubjid,
                     check_ftseq,
                     check_ftcat,
                     check_fttestcd,
                     check_fttest,
                     check_ftpos,
                     check_ftloc,
                     check_ftlat,
                     check_ftstresc,
                     check_ftstresu,
                     check_ftstat,
                     check_ftreasn,
                     check_ftdrvl,
                     check_ftrepnum,
                     check_ftaid,
                     check_ftaidoth,
                     check_visitnum,
                     check_visit,
                     check_ftdtc,
                     check_ftdy);
    
});
