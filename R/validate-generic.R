validation_function_dispatch_table <-
    list("DM"=validate_dm,
         "SC"=validate_sc,
         "QS"=validate_qsmd);

#' validate a dataset from the set of known set types
#' @param - dataset - a dataset (data frame) with a DOMAIN column
#'     indicating the domain validator to apply.
#' @return a validation state object containing $messages and
#'     $warnings. If $messages is empty then the set passes.
#' @export
validate_generic <- function(dataset){
    if(!("DOMAIN" %in% names(dataset))){
        stop("Can't validate a data set which does not have a DOMAIN column.")
    }
    domain <- dataset$DOMAIN[[1]];
    if(!(domain %in% names(validation_function_dispatch_table))){
        stop(sprintf("Can't validate a data set from DOMAIN: %s", domain));
    }
    validation_function_dispatch_table[[domain]](fresh_state(dataset));
}
