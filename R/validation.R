column_is_textual <- function(column){
    s <- sprintf;
    function(state){
        the_col <- state$df[[column]];
        if(class(the_col) %===% class("")){
            update_state(state,
                         "ok",
                         check_report(T,
                                      s("column %s is text", column),
                                      "The column %s is text", column))
        } else {
            update_state(state,
                         "continuable",
                         check_report(F,
                                      s("column %s must be textual", column),
                                      "The column %s must be text but it appears to be %s instead.", column, class(the_col)));
        }
    }
}
