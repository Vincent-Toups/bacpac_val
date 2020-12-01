library(tibble)

#' Create a new validation state
#'
#' @param data - the data set to validate
#' @param status - the status of the validation - defaults to "ok"
#' @param messages - data frame of messages about check status - defaults to empty
#' @param warnings - data frame of misc messages produced during validation - defaults to empty
#' @return a validation state
#' @export
#' @examples
#' fresh_state(data, rules);
fresh_state <- function(data, status="ok", messages=NULL, warnings=NULL){
    s <- list(status=status, messages=messages, warnings=warnings, data=data);
    class(s) <- "validation-state";
    s
}

#' Create a single row check report.
#'
#' @param check_name - the name of the check performed
#' @param pass - whether it passed
#' @param message - a longer status description
#' @param ... arguments with which to construct message as if with sprintf
#' @return a single row tibble
#' @export
#' @examples
#' check_report("all columns present",T,"All columns are present.")
check_report <- function(check_name, pass, message, ...){
    tibble(check_name=check_name, pass=pass, message=sprintf(message,...));
}

#' Create a new validation state with values from a previous
#'
#' @param st - current state object
#' @param data - the data set to validate
#' @param rules - object containing the rules to apply
#' @param status - the status of the validation - defaults to "ok"
#' @param messages - data frame of messages about check status - defaults to empty
#' @param warnings - data frame of misc messages produced during validation - defaults to empty
#' @return a validation state
#' @export
#' @examples
#' update_state(data, rules);
update_state <- function(st, data=st$data, status=st$status, messages=st$messages, warnings=st$warnings){
    fresh_state(data, status, messages, warnings);
}

#' Create a new validation state by updating it with new information and status
#'
#' @param st - current state object
#' @param status - the new status to combine 
#' @param messages - data frame of messages about check status
#' @param warnings - data frame of misc messages produced during validation
#' @return a validation state
#' @export
#' @examples
#' extend_state(st, "ok", messages=tibble(...), warnings=tibble(...))
extend_state <- function(st, status, messages=NULL, warnings=NULL){
    update_state(st,
                 status=combine_statuses(st$status, status),
                 messages=rbind(st$messages, messages),
                 warnings=cbind(st$warnings, warnings));
}

#' Test whether the status is valid
#'
#' @param s a putative status indicator
#' @return boolean - whether the state is valid
#' @export
#' @examples
#' valid_status_p("ok")
valid_status_p <- function(s){
    s %in% c("ok","continuable","halted");
}

#' Test whether the status is continuable
#'
#' @param s a putative status indicator
#' @return boolean - whether the state is continuable
#' @export
#' @examples
#' can_continue_p(st)
can_continue_p <- function(st){
    stopifnot(identical(class(st),"validation-state"));
    st$status %in% c("ok","continuable");
}

#' Combine two statusus appropriately
#'
#' @param sa a status indicator
#' @param sb another status indicator
#' @return sc the appropriate combination
#' @export
#' @examples
#' combine_statuses("ok","continuable") -> "continuable"
combine_statuses <- function(sa, sb){
    stopifnot(valid_status_p(sa),
               valid_status_p(sb));
    terms <- c(sa,sb);
    if("halted" %in% terms) {
        "halted"
    } else if (all.equal_tf(terms,c("ok","ok"))) {
        "ok"
    } else {
        "continuable"
    };
}

#' Get's the "pass" value of the last check performed
#'
#' @param state - the validation state object
#' @param if_none - the result to pass back if there are no previous states
#' @return the result of the last check
#' @examples
#' validation_last_check(state) -> T # when no checks performed or the last is T
validation_last_check <- function(state,if_none=T){
    df <- state$messages;
    last <- nrow(df);
    if(identical(last,0)){
        if_none
    } else {
        df$pass[[last]];
    }
}

validation_chain_builder <- function(short_circut_test){
    function(...){
        state_functions <- list(...);
        if(identical(length(state_functions), 0)){
                                        # return the identity function
            function(state) state
        } else {
            function(state) {
                for(sf in state_functions){
                    state <- sf(state);
                    if(short_circut_test(state)){
                        break
                    } 
                }
                state
            }
        }    
    }
}

#' bailout_validation_chain - like validation_chain except evaluate
#' short circuits on any failed check.
#'
#' @param ... - any number of validation functions to fuse together
#' @return a new validation function which performs the checks in ... until one fails or halts.
#' @examples
#' bailout_validation_chain(c1,c2,c3) - c # if c2 fails c3 is never evaluated.
bailout_validation_chain <- validation_chain_builder(short_circut_test=
                                                         function(state) {
                                                             identical(state$status,"halted") || identical(validation_last_check(state), F);
                                                         });

#' Sequence a series of validation functions
#'
#' @params ... 
#' @return a validation function
#' @export
#' @examples
#' combine_statuses("ok","continuable") -> "continuable"
bailout_validation_chain <- validation_chain_builder(short_circut_test=
                                                         function(state) {
                                                             identical(state$status,"halted");
                                                         });

