library(testthat)
library(val)
library(readr)
library(stringr)

context("Validation Support Functions")

gen_random_correct_iso8601_duration <- function(n){
    gen_one <- function(){
    pastec <- function(...){
        paste(..., sep="");
    }
    out <- "P";
    if(!runif(1)<0.2){
        out <- pastec(out, sprintf("%dY", sample(seq(100),1)))
    }
    if(!runif(1)<0.2){
        out <- pastec(out, sprintf("%dM", sample(seq(100),1)))
    }
    if(!runif(1)<0.2){
        out <- pastec(out, sprintf("%dD", sample(seq(100),1)))
    }
    out <- pastec(out, "T");
    if(!runif(1)<0.2){
        out <- pastec(out, sprintf("%dH", sample(seq(100),1)))
    }
    if(!runif(1)<0.2){
        out <- pastec(out, sprintf("%dM", sample(seq(100),1)))
    }
    if(!runif(1)<0.2){
        out <- pastec(out, sprintf("%dS", sample(seq(100),1)))
    }
    str_replace_all(out, "T$", "") %>% 
        str_replace_all("P$","P0Y");
    };
    Map(function(ignored){
        gen_one();
    },seq(n)) %>% unlist();
    
}

test_that("Basic iso 8601 Duration checks work via the RCPP.",{
    example <- gen_random_correct_iso8601_duration(100);
    check <- check_iso8601_durations(example);
    bad_examples = example[!check];
    if(length(bad_examples)!=0){
        print(bad_examples);
    }
    expect_identical(character(0),bad_examples);
})

test_that("iso8601 Duration Checks work with NA values.", {
    example <- gen_random_correct_iso8601_duration(100);
    ii <- sample(seq(100), 20 , replace=F);
    example[ii] <- NA;
    expect_identical(rep(NA,20),check_iso8601_durations(example)[ii]);
});
