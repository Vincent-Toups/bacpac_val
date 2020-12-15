validate_dm <- block({
  
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
      check_domain_known(domains='DM')
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
  
  check_rfstdtc <- block({
    col <- "RFSTDTC";
    bailout_validation_chain(
      column_exists(col),
      column_is_iso8601_date(col),
      column_is_complete(col)
    )
  });
  
  check_rfpendtc <- block({
    col <- "RFPENDTC";
    bailout_validation_chain(
      column_exists(col),
      column_is_iso8601_date(col),
      column_is_complete(col)
    )
  });
  
  check_brthdtc <- block({
    col <- "BRTHDTC";
    bailout_validation_chain(
      column_exists(col),
      column_is_iso8601_date(col),
      column_is_complete(col)
    )
  });
  
  check_age <- block({
    col <- "AGE";
    range <- 0:120;
    bailout_validation_chain(
      column_exists(col),
      column_is_numeric(col),      
      column_is_integer(col),
      column_is_complete(col),
      column_in_integer_range(col,range)
    )
  });
  
  check_sex  <- mandatory_codelist_column("SEX");
  check_race <- mandatory_codelist_column("RACE");
  
  check_racemult <- block({
    col <- "RACEMULT";
    bailout_validation_chain(
      column_exists(col),
      column_is_textual(col)
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
