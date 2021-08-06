**************************************************
*** Call data set validator from R within SAS
**************************************************;

*** Location where 'val' package installed;
%let libpath    = C:\\Users\\dgarb\\Documents\\R\\win-library\\3.6;

*** CSV to validate;
%let filepath   = J:\\BACPAC\\SC\\SASdata\\Simulated\\200706\\DM.csv;

**************************************************;
*** User should not need to edit this section;

*** ------------------------------ ***;
*** BEGIN CALL TO R FOR VALIDATION;
*** ------------------------------ ***;

proc iml;
    libpath     = "&libpath";
    filepath    = "&filepath";
    submit libpath filepath / R;
            R.home()        
            
            library(val, lib.loc = "&libpath")

            d <- val_read_csv("&filepath")

            state <- fresh_state(d, "ok")

            messages <- validate_generic(state$data)$messages
            head(messages)
    endsubmit;

    Call ImportDatasetFromR("WORK._messages_", "messages");
quit;

*** ------------------------------ ***;
*** END CALL TO R FOR VALIDATION;
*** ------------------------------ ***;

**************************************************;
*** View validator output messages;

proc print data = WORK._messages_;
run;

