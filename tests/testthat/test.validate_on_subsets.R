library(testthat)
library(val)
library(readr)
library(dplyr)

context("Subset validation functions")

category_value_map = list("A"=c("x","y","z"),
                          "B"=c("q","r","s"),
                          "C"=c("i","j","k"));

cat_table <- function(category_name, n){
    tibble(category=rep(category_name,n), value=sample(category_value_map[[category_name]], n, replace=TRUE)) %>%
        mutate(unparsed__category=category,
               unparsed__value=value);
}

simple_data <-
    rbind(cat_table("A",100),
          cat_table("B",45),
          cat_table("C",55)) %>% mutate(index__=seq(nrow(.)));

category_validator <- function(category){
    function(state){
        checks <- state$data$value %in% category_value_map[[category]];
        all_good <- sum(checks) == length(checks);
        bad_indices <- state$data$index__[!checks];
        extend_state(state,
                     ifelse(all_good,"ok","continuable"),
                     check_report(sprintf("category %s in column value correct", category),
                                  all_good,
                                  ifelse(all_good, "All columns ok",
                                         sprintf("Errors at %s", bad_indices))))
    }
}

vft <- tibble(category=c("A","B","C"),
              validation_function__=c(category_validator("A"),
                                      category_validator("B"),
                                      category_validator("C")));



test_that("Validate on subsets runs.",{
    vf <- validate_on_subsets(vft)(fresh_state(simple_data))
    expect_identical(vf,vf);
});

test_that("validate_on_subsets validates a valid data set.",
          {
              vf <- validate_on_subsets(vft)(fresh_state(simple_data))
              expect_identical("ok",vf$status)
          });

bad_data <- simple_data %>% mutate(value=sample(value, nrow(.))) %>% mutate(unparsed__value=value);

test_that("validate_on_subsets fails to validate an invalid data set.",
          {
              vf <- validate_on_subsets(vft)(fresh_state(bad_data))
              expect_identical("continuable",vf$status)
          });




