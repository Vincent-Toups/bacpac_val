#include"stdlib.h"
#include"stdio.h"
#include"string.h"
#include"math.h"
#include"./pure-cpp.h"

//#define debugprintf(...) printf(__VA_ARGS__)
#define debugprintf(...)

#define TYPED_NUMBER_CASE(state_name, char_to_expect, parsed_field, next_state_name) state_name: \
r = read_typed_number(s, char_to_expect);\
debugprintf("Expecting %s\n", char_to_expect);\
state = next_state_name;\
s = s + r.count;\
if(r.count != 0){\
 debugprintf("-- satisfied\n");\
 debugprintf("-- rem: %s\n", s);\
 parsed.parsed_field = r.number;\
} else { \
  debugprintf("== failed\n");\
  debugprintf("== rem: %s\n", s);\
}\




void print_parsed_ISO8601DurationParsed(ISO8601DurationParsed p){
  if(p.ok){
    printf("P");
    if(p.y != 0.0) { printf("%0.2lfY", p.y); }
    if(p.mo != 0.0) { printf("%0.2lfM", p.mo); }
    if(p.d != 0.0) { printf("%0.2lfD", p.d); }
    if(p.h != 0.0 || p.mi != 0.0 || p.s != 0.0) {
      printf("T");
    }
    if(p.h != 0.0) { printf("%0.2lfH", p.h); }
    if(p.mi != 0.0) { printf("%0.2lfM", p.mi); }
    if(p.s != 0.0) { printf("%0.2lfS", p.s); }
    printf("\n");
  } else {
    printf("<Invalid>");
  }
}

NumReadResult read_typed_number(const char * s, const char * tags){  
  const int l = strlen(s);
  const int al = strlen(tags);

  NumReadResult r;

  r.count = 0;
  r.number = 0;
  r.tag = '\0';
  
  double tmp_number = 0;
  char tmp_tag = '\0';
  
  int n_read = 0;
  int c_read = 0;

  debugprintf("*** Trying to read from %s\n", s);
  
  sscanf(s, "%lf%n", &tmp_number, &n_read);
  if(n_read == 0){
    return r;
  }
  debugprintf("*** Read a number %lf - nread %d - rem: %s", tmp_number, n_read, s+n_read);
  sscanf(s+n_read, "%1c%n", &tmp_tag, &c_read);
  if(c_read == 0){
    return r;
  }
  
  for(int i = 0; i < al; i++){
    if(tags[i] == tmp_tag) {
      r.number = tmp_number;
      r.tag = tmp_tag;
      r.count = n_read + c_read;
      debugprintf("*** Success %d.\n", r.count);
      return r;
    }
  }
  debugprintf("*** Fell through.\n");
  return r;                                                             
}

int read_character(const char * s, const char * e, char * c){
  int n_read = 0;
  char tmp_c = '\0';
  int ne = strlen(e);
                                                                   
  n_read = sscanf(s, "%1c", &tmp_c);
  if(n_read == 0){
    return 0;
  } else {
    for(int i = 0; i < ne; i++){
      if(e[i] == tmp_c){
        *c = tmp_c;
        return n_read;
      }
    }
    return 0;
  }
}

ISO8601DurationParsed parse_iso8601_duration(const char * s){
  enum State {expecting_P,
              expecting_T,
              expecting_y,
              expecting_mo,
              expecting_d,
              expecting_h,
              expecting_mi,
              expecting_s,
              expecting_empty};
  enum ParsedIndex {Y, MO, D, H, MI, S};
  State state = expecting_P;
  const int s_len = strlen(s);
  int read_count = 0;
  int i = 0;
  char c = '\0';
  NumReadResult r;
  bool done = false; // When the parser finishes we exit the while
                     // loop.

  ISO8601DurationParsed parsed;
  parsed.ok = true; // we will detect if r.ok = false below as a means
                    // of bailing out.
  parsed.y = 0.0;
  parsed.mo = 0.0;
  parsed.d = 0.0;
  parsed.h = 0.0;
  parsed.mi = 0.0;
  parsed.s = 0.0;


  while(parsed.ok && !done){
    switch(state){
    case expecting_empty:
      if(strlen(s)!=0){
        done = true;
        parsed.ok = false;
      } else {
        done = true;
        parsed.ok = true;
      }
      break;
    case expecting_P:
      if(1 == read_character(s,"P",&c)){
        state = expecting_y;
        s = s + 1; // Move ahead in the string;
        // "P" alone is not a valid designator.
        if(strlen(s) == 0){
          parsed.ok = false;
          done = true;
        }
      } else {
        parsed.ok = false;
        done = true;
      }

      break;
    case TYPED_NUMBER_CASE(expecting_y, "Y", y, expecting_mo);
    break;
    case TYPED_NUMBER_CASE(expecting_mo, "M", mo, expecting_d);
    break;
    case TYPED_NUMBER_CASE(expecting_d, "D", d, expecting_T);
    break;
    case expecting_T:
      debugprintf("Expecting T\n");
      if(strlen(s) == 0){
        parsed.ok = true;
        done = true;
      } else if(1 == read_character(s,"T", &c)){
        s = s + 1;
        state = expecting_h;
        if(strlen(s) == 0){
          // A trailing T is not allowed.
          done = true;
          parsed.ok = false;
        }
      } else {
        state = expecting_empty; // If we don't have a T at this point
        // then this should be the end.
      }
      break;
    case TYPED_NUMBER_CASE(expecting_h, "H", h, expecting_mi);
    break;
    case TYPED_NUMBER_CASE(expecting_mi, "M", mi, expecting_s);
    break;
    case TYPED_NUMBER_CASE(expecting_s, "S", s, expecting_empty);
    break;
    }
  }  

  double n[6] = {parsed.y, parsed.mo, parsed.d, parsed.h, parsed.mi, parsed.s};
  bool seen_nz = false;
  // Only the smallest non-zero element can actually be a decimal.
  for(int k = 5; k >= 0; k--){
    if(seen_nz && round(n[k]) != n[k]){
      parsed.ok = false;
    } else {
      if(n[k] != 0){
        seen_nz = true;
      }
    }
  }

  return parsed;
  
}
