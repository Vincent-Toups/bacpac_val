#include <Rcpp.h>
using namespace Rcpp;

#include"./pure-cpp.h"

//' @export
//[[Rcpp::export]]
LogicalVector check_ISO8601_durations_cpp(StringVector putative_duration_strings){
    
  LogicalVector out(putative_duration_strings.length());
  const int l = putative_duration_strings.length();
  for(int i = 0; i < l; i++){
    if(StringVector::is_na(putative_duration_strings[i])){
      out[i] = NA_LOGICAL;
    } else {
      out[i] =  parse_iso8601_duration(String(putative_duration_strings[i]).get_cstring()).ok;
    }
     
  }
  return out;
}
