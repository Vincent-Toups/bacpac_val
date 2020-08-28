library(testthat)
library(val)
library(readr)

context("Status Algebra")

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

test_that("That our status algebra holds.",
          {
              expect_identical("ok",
                        combine_statuses("ok","ok"));
              for (s in c("ok","continuable","halted")){                  
                  expect_identical("halted",
                                   combine_statuses("halted",s));
                  expect_identical("halted",
                                   combine_statuses(s,"halted"));
                  for(s1 in c("ok","continuable")){
                      for(s2 in c("continuable")){
                          expect_identical("continuable", combine_statuses(s1,s2));
                          expect_identical("continuable", combine_statuses(s2,s1));
                      }
                  }
                  TRUE
              }
          })

