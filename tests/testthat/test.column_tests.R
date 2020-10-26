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
  read_csv(tempname);
});

#add a column to test_dm that is non-integer
test_dm <- test_dm %>% 
  mutate(non_integer_number=rnorm(150, 0, 1))

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

