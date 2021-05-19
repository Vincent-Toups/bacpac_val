

#' Create a new validation state
#'
#' @param data - the data set to validate
#' @param status - the status of the validation - defaults to "ok"
#' @param messages - data frame of messages about check status - defaults to empty
#' @param warnings - data frame of misc messages produced during validation - defaults to empty
#' @return a validation state
#' @export
#' @examples
#' fresh_state(data, status, messages, warnings)
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
#' @param row_numbers - row indices where the test fails
#' @return a single row tibble
#' @export
#' @examples
#' check_report("all columns present",T,"All columns are present.")
check_report <- function(check_name, pass, message, row_numbers=""){
    tibble::tibble(check_name=check_name, pass=pass, message=message, row_numbers=row_numbers);
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

#' Combine two states by merging their status as in combine_statuses
#' and concatenating the tl checks and warnings onto the messages and
#' checks in the hd object.
#'
#' The data attribute of the hd state is kept.
#' @param hd the state to base the combination on
#' @param tl the state to concatenate onto the hd
#' @return a combined state object
#' combine_states(hd, tl) #-> new_state
combine_states <- function(hd, tl){
    extend_state(hd,
                 status=tl$status,
                 messages=tl$messages,
                 warnings=tl$warnings);
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
    if(identical(last, 0L)){
        if_none
    } else {
        df$pass[[last]];
    }
}

validation_chain_builder <- function(short_circut_test){
    function(...){
        state_functions <- list(...);
        if(identical(length(state_functions), 0L)){
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
#' @export
bailout_validation_chain <- validation_chain_builder(short_circut_test=
                                                         function(state) {
                                                             identical(state$status,"halted") || identical(validation_last_check(state), F);
                                                         });

#' Sequence a series of validation functions
#'
#' @param ... 
#' @return a validation function
#' @export
#' @examples
#' combine_statuses("ok","continuable") -> "continuable"
validation_chain <- validation_chain_builder(short_circut_test=
                                                 function(state) {
                                                     identical(state$status,"halted");
                                                 });

#' Prefix all the messages in a validation state with a prefix string
#' @param state - the validation state
#' @param prefix - the message to prefix the strings with
#' @return a new validation state
prefix_messages <- function(validation_state, prefix){
    validation_state$messages <- validation_state$messages %>% dplyr::mutate(message=paste(prefix,message,sep=" "));
    validation_state    
}
