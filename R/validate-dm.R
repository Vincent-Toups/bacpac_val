validate_dm <- block({
  
  check_studyid <- block({
    #col <- "STUDYID";
    bailout_validation_chain(
      column_exists("STUDYID"),
      column_is_textual("STUDYID"),
      column_is_homogeneous("STUDYID")
    )
  });
  
  check_domain <- block({
    #col <- "DOMAIN";
    bailout_validation_chain(
      column_exists("DOMAIN"),
      column_is_textual("DOMAIN"),
      column_is_homogeneous("DOMAIN"),
      check_domain_known(domains='DM')
    )
  });
  
  check_usubjid <- block({
    #col <- "USUBJID";
    bailout_validation_chain(
      column_exists("USUBJID"),
      column_is_textual("USUBJID"),
      column_is_complete("USUBJID")
    )
  });
  
  check_rfstdtc <- block({
    #col <- "RFSTDTC";
    bailout_validation_chain(
      column_exists("RFSTDTC"),
      column_is_iso8601_date("RFSTDTC"),
      column_is_complete("RFSTDTC")
    )
  });
  
  check_rfpendtc <- block({
    #col <- "RFPENDTC";
    bailout_validation_chain(
      column_exists("RFPENDTC"),
      column_is_iso8601_date("RFPENDTC"),
      column_is_complete("RFPENDTC")
    )
  });
  
  check_brthdtc <- block({
    #col <- "BRTHDTC";
    bailout_validation_chain(
      column_exists("BRTHDTC"),
      column_is_iso8601_date("BRTHDTC"),
      column_is_complete("BRTHDTC")
    )
  });
  
  check_age <- block({
    #col <- "AGE";
    #range <- 0:120;
    bailout_validation_chain(
      column_exists("AGE"),
      column_is_numeric("AGE"),      
      column_is_integer("AGE"),
      column_is_complete("AGE"),
      column_in_integer_range("AGE",0:120)
    )
  });
  
  check_sex  <- mandatory_codelist_column("SEX");
  check_race <- mandatory_codelist_column("RACE");
  
  check_racemult <- block({
    #col <- "RACEMULT";
    bailout_validation_chain(
      column_exists("RACEMULT"),
      column_is_textual("RACEMULT")
    )
  });
  
  check_ethnic <- mandatory_codelist_column("ETHNIC");  
  
  validation_chain(check_studyid,
                   check_domain,
                   check_usubjid,
                   check_rfstdtc,
                   check_rfpendtc,
                   check_brthdtc,
                   check_age,
                   check_sex,
                   check_race,
                   check_racemult,
                   check_ethnic);  
});
