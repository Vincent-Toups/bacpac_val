library(testthat)
library(val)
library(readr)
library(dplyr)

context("Column Validation Functions")

## NB - we encode our test data as a string to avoid the complexity of
## persisting it as a table of some kind.
## Here we read it back in.
## (Turns out read_csv doesn't read from textConnections)
## We use block here to create a temporary context so we don't
## make a mess with our tempnames
test_dm <- block({
  tempname <- tempfile();
  write(test_data$DM, file=tempname);
  val_read_csv(tempname);
});

#add a column to test_dm that is non-integer
test_dm <- test_dm %>% 
  mutate(non_integer_number=rnorm(150, 0, 1),
         empty_column=NA)

## test state functions are pure
## and thus we can reuse the fresh dm test state
## in all tests.
dm_state <- fresh_state(test_dm, "ok");

test_that("Test that the columns are in the codelist and the test function works.",
          {
            expect_identical("ok",
                             column_in_codelist("SEX", (specification$Codelists %>% filter(ID=="SEX")) %>% `[[`("Term"))(dm_state)$status);
          })

test_that("Test that the columns are textual and the test function works.",
          {
            expect_identical("ok",
                             column_is_textual("SEX")(dm_state)$status);
            expect_identical("continuable",
                             column_is_textual("AGE")(dm_state)$status)
          })

test_that("Test that the columns are numeric and the test function works.",
          {
            expect_identical("ok",
                             column_is_numeric("AGE")(dm_state)$status);
            expect_identical("continuable",
                             column_is_numeric("SEX")(dm_state)$status)
          })

test_that("Test that the columns are integers and the test function works.",
          {
            expect_identical("ok",
                             column_is_integer("AGE")(dm_state)$status);
            expect_identical("continuable",
                             column_is_integer("non_integer_number")(dm_state)$status)
          })

test_that("Test that the columns are float and the test function works.",
          {
            expect_identical("ok",
                             column_is_float("non_integer_number")(dm_state)$status);
            expect_identical("continuable",
                             column_is_float("SEX")(dm_state)$status)
          })

test_that("Test that date columns are ok.",
          {
            expect_identical("ok",
                            column_is_iso8601_date("RFSTDTC")(dm_state)$status);
          })

test_that("Test that the columns exist and the test function works.",
          {
            expect_identical("ok",
                             column_exists("USUBJID")(dm_state)$status);
            expect_identical("continuable",
                             column_exists("BIRTHDAY")(dm_state)$status)
          })

test_that("Test that the columns are complete and the test function works.",
          {
            expect_identical("ok",
                             column_is_complete("USUBJID")(dm_state)$status);
            expect_identical("continuable",
                             column_is_complete("RACEMULT")(dm_state)$status)
          })

test_that("Test that the columns are not empty and the test function works.",
          {
            expect_identical("ok",
                             column_not_empty("RACEMULT")(dm_state)$status);
            expect_identical("continuable",
                             column_not_empty("empty_column")(dm_state)$status)
          })

test_that("Test that the columns in integer range and the test function works.",
          {
            expect_identical("ok",
                             column_in_integer_range("AGE", 0:120)(dm_state)$status);
            expect_identical("continuable",
                             column_in_integer_range("AGE", 65:70)(dm_state)$status);
            expect_identical("continuable",
                             column_in_integer_range("empty_column", 65:70)(dm_state)$status)
          })

test_that("Test that the columns homogeneous and the test function works.",
          {
            expect_identical("ok",
                             column_is_homogeneous("DOMAIN")(dm_state)$status);
            expect_identical("ok",
                             column_is_homogeneous("empty_column")(dm_state)$status);
            expect_identical("continuable",
                             column_is_homogeneous("RACE")(dm_state)$status)
          })
