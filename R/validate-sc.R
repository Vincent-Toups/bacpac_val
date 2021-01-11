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
    col <- "SCMETHOD"
    validation_table <- key_column_to_codelists("SCTESTCD") %>% 
      filter(value_column==col) %>% 
      rowwise() %>%
      transmute(SCTESTCD=value, codelists=term, validation_function__=
                  list(ifelse(!is.na(codelists),
                              column_in_codelist(col, codelists),
                              column_is_textual(col)))) %>%
      ungroup();
    
    validate_on_subsets(validation_table, "SCMETHOD column consistent with SCTESTCD.");
  })
  
  check_scorres <- block({
    col <- "SCORRES"
    validation_table <- key_column_to_codelists("SCTESTCD") %>% 
      filter(value_column==col) %>% 
      rowwise() %>%
      transmute(SCTESTCD=value, codelists=term, validation_function__=
                  list(ifelse(!is.na(codelists),
                              column_in_codelist(col, codelists),
                              column_is_textual(col)))) %>%
      ungroup();
    
    validate_on_subsets(validation_table, "SCORRES column consistent with SCTESTCD.");
  })
  
  check_scorresu <- block({
    col <- "SCORRESU"
    validation_table <- key_column_to_codelists("SCTESTCD") %>% 
      filter(value_column==col) %>% 
      rowwise() %>%
      transmute(SCTESTCD=value, codelists=term, validation_function__=
                  list(ifelse(!is.na(codelists),
                              column_in_codelist(col, codelists),
                              column_is_textual(col)))) %>%
      ungroup();
    
    validate_on_subsets(validation_table, "SCORRESU column consistent with SCTESTCD.");
  })
  
  
  check_scstresc <- block({
    col <- "SCSTRESC"
    validation_table <- key_column_to_codelists("SCTESTCD") %>% 
      filter(value_column==col) %>% 
      rowwise() %>%
      transmute(SCTESTCD=value, codelists=term, validation_function__=
                  list(ifelse(!is.na(codelists),
                              column_in_codelist(col, codelists),
                              column_is_textual(col)))) %>%
      ungroup();
    
    validate_on_subsets(validation_table, "SCSTRESC column consistent with SCTESTCD.");
  })
  
  check_scstresn <- block({
    col <- "SCSTRESN"
    validation_table <- key_column_to_codelists("SCTESTCD") %>% 
      filter(value_column==col) %>% 
      rowwise() %>%
      transmute(SCTESTCD=value, codelists=term, validation_function__=
                  list(ifelse(!is.na(codelists),
                              column_in_codelist(col, codelists),
                              column_is_float(col)))) %>%
      ungroup();
    
    validate_on_subsets(validation_table, "SCSTRESN column consistent with SCTESTCD.");
  })
  
  check_scstresu <- block({
    col <- "SCSTRESU"
    validation_table <- key_column_to_codelists("SCTESTCD") %>% 
      filter(value_column==col) %>% 
      rowwise() %>%
      transmute(SCTESTCD=value, codelists=term, validation_function__=
                  list(ifelse(!is.na(codelists),
                              column_in_codelist(col, codelists),
                              column_is_textual(col)))) %>%
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
