#include"stdlib.h"
#include"stdio.h"
#include"string.h"
#include"math.h"

typedef struct {
  int count;
  double number;
  char tag;
} NumReadResult;

typedef struct {
  double y;
  double mo;
  double d;
  double h;
  double mi;
  double s;
  bool ok;
} ISO8601DurationParsed;

void print_parsed_ISO8601DurationParsed(ISO8601DurationParsed p);
NumReadResult read_typed_number(const char * s, const char * tags);
int read_character(const char * s, const char * e, char * c);
ISO8601DurationParsed parse_iso8601_duration(const char * s);


