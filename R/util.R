library(dplyr);

#' Maps years to booleans indicating whether the year is a leap year
#'
#' @param year_number - numerical representation of the year
#' @return T if year is a leap year, F otherwise
leap_year <- function(year_number){
    by_four <- year_number %% 4 == 0;
    by_one_hundred <- year_number %% 100 == 0;
    by_four_hundred <- year_number %% 400 == 0;
    ly <- by_four & !by_one_hundred
    ly[by_four_hundred] <- T;
    ly
}

basic_month_map <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

#' Return the length of the given month for a given year
#'
#' @param month_number - the month as a number between inclusive 1 and 12
#' @param year_number - the year
#' @return the length of the given month
month_len <- function(month_number, year_number){
    basic <- basic_month_map[month_number];
    basic[leap_year(year_number) & month_number == 2] <- 29;
    basic    
}

#' maps vectors of year month day to booleans based on whether the day
#' is valid for the given year and month.
valid_day <- function(year, month, day){
    day >= 1 & day <= month_len(month, year);
}

#' Return the unparsed column name for "name"
#'
#' @param name (or names)
#' @return the unparsed name
unparsed_column_name <- function(name){
    paste("unparsed__",name,sep="");
}

#' Load a data frame but keep both a parsed and unparsed version of each column (unparsed columns are preceeded in their name by "unparsed__")
#'
#' @param filename - file to load
#' @return a data frame with twice the columns indicated in the file, half of which are unparsed duplicates.
val_read_csv <- function(filename){
    parsed <- read_csv(filename);
    args <- list();
    for(n in names(parsed)){
        args[n] <- col_character();
    }
    unparsed <- read_csv(filename, col_types=do.call(cols, args))
    names(unparsed) <- unparsed_column_name(names(unparsed));
    cbind(parsed, unparsed);
}

collapse_commas <- function(s){
    paste(s,collapse=",");
}

downcase_names <- function(o){
    names(o) <- tolower(names(o));
    o
}

dont_do <- function(block){
    NULL
}

read_json <- function(filename){
    jsonlite::fromJSON(filename) %>% as.data.frame();
}

block <- function(bod){
    eval(substitute(function()b,c(b=quote(bod))))()
}

`%not-in%` <- function(x,table){
    !(x %in% table);
}

last <- function(sq){
    sq[length(sq)];
}

file_extension <- function(fn){
    str_split(fn,"\\.") %>% last();
}


read_generic <- function(input_file_name){
    switch(file_extension(input_file_name),
           "csv" = read_csv(input_file_name),
           "xpt" = haven::read_xpt(input_file_name),
           "json"= )
}

to_map <- function(df, key_col, val_col=TRUE){
    o <- list();
    if(identical(val_col, TRUE)){
        nms <- names(df);
        nms <- nms[nms %not-in% key_col];
        to_map(df,key_col,nms);
    } else if (length(val_col)==1){
        for (i in seq(nrow(df))){
            o[[df[[key_col]][[i]]]] <- df[[val_col]][[i]];
        }
        o
    } else {
        for (i in seq(nrow(df))){
            target <- list();
            for(v in val_col){
                target[[v]] <- df[[v]][[i]];
            }
            o[[df[[key_col]][[i]]]] <- target;
        }
        o
    }
}

`%===%` <- identical;

`%!==%` <- function(a,b){
    !(a %===% b);
}

all_true <- function(s){
    s %===% rep(T,length(s));
}

all.equal_tf <- function(a,b){
    identical(T, all.equal(a,b));
}
