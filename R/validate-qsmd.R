validate_qsmd <- block({
    check_study_id <- block({
        col <- "STUDYID";
        bailout_validation_chain(
            column_exists(col),
            column_is_textual(col),
            column_is_homogeneous(col))
    });

    check_domain <- block({
        col <- "DOMAIN";
        bailout_validation_chain(
            column_exists(col),
            column_is_textual(col),
            column_is_homogeneous(col),
            check_domain_known(domains='QSMD'))
    });

    check_usubjid <- block({
        col <- "USUBJID";
        bailout_validation_chain(
            column_exists(col),
            column_is_textual(col),
            column_is_complete(col)
        )
    });

    check_qsseq <- block({
        col <- "QSSEQ";
        bailout_validation_chain(
            column_exists(col),
            column_is_integer(col),
            column_is_complete(col)
        )
    });


    check_qscat <- mandatory_codelist_column("QSCAT");
    check_qsscat <- mandatory_codelist_column("QSSCAT");
    check_qstestcd <- mandatory_codelist_column("QSTESTCD");
    check_qstest <- mandatory_codelist_column("QSTEST");

    check_qsstresc <- block({
        col <- "QSSTRESC"
        validation_table <- key_column_to_codelists("QSTESTCD") %>%
            filter(value_column == col) %>%
            transmute(QSTESTCD=value, codelist=codelist, text_format=text_format) %>%
            distinct() %>%
            rowwise() %>%
            transmute(QSTESTCD=QSTESTCD,
                      validation_function__ =
                          list(ifelse(!is.na(codelist),
                                      column_in_codelist(col, get_codelist(codelist)),
                                      text_column_matches_format(col,text_format)))) %>%
            ungroup();
                                                 
        validate_on_subsets(validation_table, "QSSTRESC column consistent with QSTESTCD.");
    })
    
    validation_chain(
        check_study_id,
        check_domain,
        check_usubjid,
        check_qsseq,
        check_qscat,
        check_qsscat,
        check_qstestcd,
        check_qstest,
        check_qsstresc);
});

