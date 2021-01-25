validate_sc <- block({
    
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
            check_domain_known(domains="SC")
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
    
    check_scseq <- block({
        col <- "SCSEQ";
        bailout_validation_chain(
            column_exists(col),
            column_is_numeric(col),
            column_is_integer(col),
            column_is_complete(col)
        )
    });
    
    check_sctestcd <- mandatory_codelist_column("SCTESTCD");
    check_sctest <- mandatory_codelist_column("SCTEST");

    sctestcd_dependent_column <- function(check_column){
        check_simple_dependent_column("SCTESTCD", check_column);
    }
    
    check_scmethod <- sctestcd_dependent_column("SCMETHOD");    
    check_scorres <- sctestcd_dependent_column("SCORRES")
    check_scorresu <- sctestcd_dependent_column("SCORRESU");       
    check_scstresc <- sctestcd_dependent_column("SCSTRESC");
    check_scstresn <- sctestcd_dependent_column("SCSTRESN");
    check_scstresu <- sctestcd_dependent_column("SCSTRESU");
    
    validation_chain(check_studyid,
                     check_domain,
                     check_usubjid,
                     check_scseq,
                     check_sctestcd,
                     check_sctest,
                     check_scmethod,
                     check_scorres,
                     check_scorresu,
                     check_scstresc,
                     check_scstresn,
                     check_scstresu);
                     
})
