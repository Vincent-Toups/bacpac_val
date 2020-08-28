library(testthat)
library(val)
library(readr)
library(dplyr)

context("Domain Validation Functions")

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

## test state functions are pure
## and thus we can reuse the fresh dm test state
## in all tests.
dm_state <- fresh_state(test_dm, "ok");

## Modify a state's data set. This should never
## happen in real life, thus we define the function here.
validation_dip_data <- function(validation_state,f){
    update_state(validation_state, data=f(validation_state$data));
}


test_that("That the DM domain is known and that the test function works.",
          {
              expect_identical("ok",
                        check_domain_known()(dm_state)$status);
          })

test_that("Test that the domain exists test function works.",
          {
              expect_identical("ok",
                        check_domain_presence(dm_state)$status);
              expect_identical("halted",
                               check_domain_presence(dm_state)$status)
          })



