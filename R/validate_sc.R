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
  
  check_scmethod <- block({
    validation_table <- specification$scmethod_codelists %>%
      rowwise() %>%
      transmute(SCTESTCD=id, validation_function__=
                  list(ifelse(!is.na(codelist),
                              column_in_codelist("SCMETHOD", get_codelist(codelist)),
                              column_is_textual("SCMETHOD")))) %>%
      ungroup();
    
    validate_on_subsets(validation_table, "SCMETHOD column consistent with SCTESTCD.");
  })
  
  check_scorres <- block({
    validation_table <- specification$scscorres_codelists %>%
      rowwise() %>%
      transmute(SCTESTCD=id, validation_function__=
                  list(ifelse(!is.na(codelist),
                              column_in_codelist("SCORRES", get_codelist(codelist)),
                              column_is_textual("SCORRES")))) %>%
      ungroup();
    
    validate_on_subsets(validation_table, "SCORRES column consistent with SCTESTCD.");
  })
  
  check_scorresu <- block({
    validation_table <- specification$scscorresu_codelists %>%
      rowwise() %>%
      transmute(SCTESTCD=id, validation_function__=
                  list(ifelse(!is.na(codelist),
                              column_in_codelist("SCORRESU", get_codelist(codelist)),
                              column_is_textual("SCORRESU")))) %>%
      ungroup();
    
    validate_on_subsets(validation_table, "SCORRESU column consistent with SCTESTCD.");
  })
  
  
  check_scstresc <- block({
    validation_table <- specification$scstresc_codelists %>%
      rowwise() %>%
      transmute(SCTESTCD=id, validation_function__=
                  list(ifelse(!is.na(codelist),
                              column_in_codelist("SCSTRESC", get_codelist(codelist)),
                              column_is_textual("SCSTRESC")))) %>%
      ungroup();
    
    validate_on_subsets(validation_table, "SCSTRESC column consistent with SCTESTCD.");
  })
  
  check_scstresn <- block({
    validation_table <- specification$scstresn_codelists %>%
      rowwise() %>%
      transmute(SCTESTCD=id, validation_function__=
                  list(ifelse(!is.na(codelist),
                              column_in_codelist("SCSTRESN", get_codelist(codelist)),
                              column_is_textual("SCSTRESN")))) %>%
      ungroup();
    
    validate_on_subsets(validation_table, "SCSTRESN column consistent with SCTESTCD.");
  })
  
  check_scstresu <- block({
    validation_table <- specification$scstresu_codelists %>%
      rowwise() %>%
      transmute(SCTESTCD=id, validation_function__=
                  list(ifelse(!is.na(codelist),
                              column_in_codelist("SCSTRESU", get_codelist(codelist)),
                              column_is_textual("SCSTRESU")))) %>%
      ungroup();
    
    validate_on_subsets(validation_table, "SCSTRESU column consistent with SCTESTCD.");
  })
  
  
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
                   check_scstresu)
  
})
