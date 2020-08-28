Val: A Minimum Dataset Validator (for BACPAC)
=============================================

This library builds validators.

This will eventually be a conforming R package but at present is in an
early development stage.

Development
===========

This repo comes with its own development environment in the form of a
Dockerfile.

    > source docker-aliases.sh
    > bu ## build 
    > r ## run r
    > rss ## run an r studio server on port 8787
    
Running Tests
-------------

Tests are defined in the tests/testthat directory. You can run them
like so:

    > r
    >> devtools::test()
    
Tests should be grouped by context in some semi-meaningful way. 

Concepts
========

The main idea here is chaining up little validation functions, which
have the signature:

    validation-state -> validation-state
    
That is, one validation state to another. See `R/validation-state.R`
for details.

A validation state is a collection of a data frame (to be validated),
a status (one of "ok", "halted" and "continuable"), messages
indicating which validations have been executed and how they went, and
finally a set of miscellaneous messages.

The idea here is to support chaining functions in a way that
validations can be continued even if the data set is already known to
be invalid.

The function `validation_chain` converts a list of validation
functions to a single validation function. However, this validation
function "short circuits" if any of the input functions returns
"halted". 

If a validation function return "continuable" then the rest of the
checks are run.

In this way, we can arrange tests sequentially so as to maximize the
number of tests run, even on bad data sets.

In other words, only return a validation status of "halted" if it
really is impossible to proceed.

Development
===========

The main task here is to map the rules (as represented in the
specification object automatically loaded in the package) to
validation functions.

For instance:

    check_domain_known <- function(domains=unique(specification$Datasets$Dataset)){
        function(state){
            domain <- unique(state$data$DOMAIN);
            if(domain %in% domains)
                extend_state(state, "ok",
                             check_report("Known DOMAIN", T, "Domain %s is valid.", domain))
            else extend_state(state, "halted",
                              check_report("Known DOMAIN", F, "Domain %s is not valid (valid domains %s).",domain,
                                           collapse_commas(domains)));
        }
    }

`check_domain_known` returns a validation function which ensures that
the domain in a given dataset is one of the known domains.

Note that for this validation function to be total it must be the case
that we've already checked the presence of the DOMAIN column in the
data set and that it is homogeneous. These functions already exist in
the repository: `check_domain_presence` and
`check_domain_homogeneity`.

Once we know a given domain we can then write checks for indvidual
columns using utility functions like `column_is_textual` which takes a
column name and returns a validation function to assert that columns
presence.

NB: because we can still check other columns if a column is missing or
otherwise malformed, this validation function returns "continuable" on
failure rather than "halted".

Todo
----

* check_column_entire
* check_column_type
* check_column_range
* check_column_string_lengths
* check_column_values_in_codelist
* code which maps the above utilities to validation functions based on
  the specification object for our known domains.
  
