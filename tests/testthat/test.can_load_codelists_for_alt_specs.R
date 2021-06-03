library(testthat)
library(val)
library(readr)
library(stringr)

context("Validation Support Functions")


test_that("We can look up codelists from other specs.",{
    r <- column_to_codelist('EXCAT',specification=bt_specification_ex);
    should_be <-  c("Surgery", "Injection", 
                    "Medication", "PT, OT, or chiropractic",
                    "Alternative medicine", "Diet and exercise", 
                    "Mental health");
    expect_identical(sum(should_be == r), length(should_be))
})

