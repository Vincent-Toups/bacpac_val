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
    month >=1 & month <= 12 & day >= 1 & day <= month_len(month, year);
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
#' @export
val_read_csv <- function(filename){
    parsed <- readr::read_csv(filename);
    unparsed <- readr::read_csv(filename, col_types = readr::cols(.default = "c"))
    names(unparsed) <- unparsed_column_name(names(unparsed));
    cbind(parsed, unparsed) %>% dplyr::mutate(index__=seq(nrow(parsed)));
}

#' Returns the codelist (from the specification) for a given column id.
#'
#' @param column - the column ID to get the codelist for.
#' @return a character array of code list values.
column_to_codelist <- function(column){
    sf <- bt_specification$Codelists %>% dplyr::filter(ID==column) %>% dplyr::arrange(Order);
    lst <- sf$Term;
    if(identical(length(lst),0)){
        stop(sprintf("Tried to get the codelist for %s but it was empty.", column))
    } else {
        lst;
    }
}

#' get_codelist - fetch a codelist by ID.  NB. This is an alias for
#' column_to_codelist. Not all codelists correspond to a column but
#' the logic for fetching the codelist is identical.
#' 
#' @param ID - the id the codelist
#' @return the codelist
get_codelist <- column_to_codelist;

#' Returns the codelist order (from the specification) for a given column id.
#'
#' @param column - the column ID to get the codelist for.
#' @return a character array of code list values.
column_to_codelist_order <- function(code){
    sf <- bt_specification$Codelists %>% dplyr::filter(ID==code) %>% dplyr::arrange(Order);
    lst <- sf$Order;
    if(identical(length(lst),0)){
        stop(sprintf("Tried to get the codelist for %s but it was empty.", code))
    } else {
        lst;
    }
}

collapse_commas <- function(s){
    paste(s,collapse=", ");
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

has_column <- function(tbl,name){
    name %in% names(tbl);
}

column_missing <- function(tbl, name){
    !has_column(tbl, name);
}

#' return a string for each row which contains the column name/value
#' pairs for each column in cols.
summarize_column_values <- function(tbl, cols){
    tbl %>%
        dplyr::rowwise() %>%
        dplyr::mutate(sss=paste(sprintf("(column: %s: value %s)", dplyr::all_of(cols), dplyr::across(dplyr::all_of(cols), function(x)x)), collapse=", ")) %>%
        dplyr::ungroup() %>%
        `[[`("sss")
}

block <- block <- gtools::defmacro(bl,expr=(function()bl)());

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

# In order to find the codelist for a question response (QSSTRESC)
# we need to look up the QSTESTCD in the WhereClauses table
# and then from there take the ID field and look up
# the Codelist in the ValueLevel table.
#
# The Value column in the WhereClauses table contains multiple
# WhereClauses IDs so we expand them here. This makes the Comparator
# column redundant because IN reduces to EQ when the values are split
# like this.
expand_where_clauses <- function(where_clauses){
    where_clauses <- where_clauses;
    do.call(rbind,
            Map(function(df){
                values <- stringr::str_split(df$Value,",") %>%
                    unlist() %>% 
                    stringr::str_trim();
                lst <- list();
                for(n in names(df)){
                    lst[[n]] <- rep(df[[n]][[1]], length(values));
                }
                lst[["Value"]] <- values;
                do.call(tibble, lst);
            },
            split(where_clauses, where_clauses$ID)) %>% unname())
}
