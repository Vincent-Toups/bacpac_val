#' Create a new validation state
#'
#' @param data - the data set to validate
#' @param rules - object containing the rules to apply
#' @param status - the status of the validation - defaults to "ok"
#' @param messages - data frame of messages about check status - defaults to empty
#' @param warnings - data frame of misc messages produced during validation - defaults to empty
#' @return a validation state
#' @export
#' @examples
#' fresh_state(data, rules);
fresh_state <- function(data, rules, status="ok", messages=NULL, warnings=NULL){
    s <- list(status=status, messages=messages, warnings=warnings, data=data, rules=rules);
    class(s) <- "validation-state";
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
update_state <- function(st, data=st$data, rules=st$rules, status=st$status, messages=st$messages, warnings=st$NULL){
    fresh_state(data, rules, status, messages, warnings);
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
    return s %in% c("ok","continuable","halted");
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
    if("halted" %in% terms) "halted"
    else if (all.equal(terms,c("ok","ok"))) "ok"
    else "continuable";
}

#' Sequence a series of validation functions
#'
#' @params ... 
#' @return a validation function
#' @export
#' @examples
#' combine_statuses("ok","continuable") -> "continuable"
validation_chain <- function(...){
    rest <- function(lst){
        ll <- length(lst);
        if(ll==0 | ll==1) {
            list();
        } else {
            lst[2:ll];
        }
    }
    fs <- list(...);
    n <- length(fs);
    if(identical(n,0)) function(st) st
    else(identical(n,1)) fs[[1]]
    else {
        first <- fs[[1]];
        rst <- rest(fs);
        function(st){
            stt <- first(st);
            if(can_continue_p(stt)){
                do.call(validation_chain,rst)(stt);
            }
        }
    }
}

#' Extract a list of domains from the rules
#'
#' @param validation_state object
#' @return the list of known domains
#' @export
#' @examples
#' extract_domains(st)
extract_domains <- function(state){
    domains <- state$rules$Datasets$Dataset;
    domains <- domains[!is.na(domains)];
    domains
}
