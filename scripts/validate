#!/bin/bash

INFILE=$1
OUTFILE=$2
INFILENAME=`basename $INFILE`
TMPD=`mktemp -d`


cp $INFILE $TMPD/

ls $TMPD/

docker run -v $TMPD:/fileswap -it valcli sudo -H -u rstudio /bin/bash -c "cd /fileswap/; ls; validate $INFILENAME out.csv"

#if test -f out.csv; then
cp $TMPD/out.csv $OUTFILE
#else
#    docker run -v $TMPD:/fileswap -it valcli sudo -H -u rstudio /bin/bash -c "cd /fileswap/; ls; /bin/bash"
#fi


