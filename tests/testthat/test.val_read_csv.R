library(testthat)
library(val)
library(readr)
library(dplyr)

context("Test util.R val_read_csv() function")

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

test_that("Test that val_read_csv() creates unparsed columns of DM and creates as character.",
          {
            check <- test_dm %>%
              select(starts_with("unparsed__")) %>%
              sapply(., is.character) %>%
              unname() %>%
              all_true()
            expect_identical(TRUE, check);
          });

## QSMD
test_qsmd <- block({
  tempname <- tempfile();
  write(test_data$QSMD, file=tempname);
  val_read_csv(tempname);
});

test_that("Test that val_read_csv() creates unparsed columns of QSMD and creates as character.",
          {
            check <- test_qsmd %>%
              select(starts_with("unparsed__")) %>%
              sapply(., is.character) %>%
              unname() %>%
              all_true()
            expect_identical(TRUE, check);
          });

## SC
test_sc <- block({
  tempname <- tempfile();
  write(test_data$SC, file=tempname);
  val_read_csv(tempname);
});

test_that("Test that val_read_csv() creates unparsed columns of SC and creates as character.",
          {
            check <- test_qsmd %>%
              select(starts_with("unparsed__")) %>%
              sapply(., is.character) %>%
              unname() %>%
              all_true()
            expect_identical(TRUE, check);
          });