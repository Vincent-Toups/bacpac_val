#' Extract a list of domains from the rules
#'
#' @param rules object
#' @return the list of known domains
#' @export
#' @examples
#' extract_domains(st)
extract_domains <- function(rules){
    domains <- state$rules$Datasets$Dataset;
    domains <- domains[!is.na(domains)];
    domains
}
