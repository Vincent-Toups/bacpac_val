library(dplyr);

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
    names(unparsed) <- paste("unparsed__",names(unparsed),sep="");
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
