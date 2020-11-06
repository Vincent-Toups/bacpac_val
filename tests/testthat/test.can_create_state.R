library(testthat)
library(val)
library(readr)

context("Validation Functions")

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



test_that("I can create a state object",
          {
              expect_identical("validation-state",
                        class(fresh_state(test_dm, "ok")));
          })


