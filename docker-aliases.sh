# handy aliases for working with the docker file
# and doing other stuff

source secrets.sh

function build_package(){
    TMPD=`mktemp -d`
    docker run -v $TMPD:/tmp/package-target -v `pwd`:/home/rstudio -e PASSWORD=$RPW -it val-dev sudo -H -u rstudio /bin/bash -c "cd ~/; R -e 'library(devtools); document(); build(path=\"/tmp/package-target\");'"
    FILE=`ls -1 $TMPD/  | head -n 1`
    cp $TMPD/$FILE ./`date | tr ' :' '_' | tr -s '_'`_$FILE
    cp $TMPD/$FILE ./$FILE
    rm -rf $TMPD
}

alias bu='docker build . --build-arg linux_user_pwd=$RPW -t val-dev'
alias dr='docker run -v `pwd`:/home/rstudio -e PASSWORD=$RPW -it val-dev'
alias r='docker run -v `pwd`:/home/rstudio -e PASSWORD=$RPW -it val-dev sudo -H -u rstudio /bin/bash -c "cd ~/; R"'
alias b='docker run -v `pwd`:/home/rstudio -e PASSWORD=$RPW -it val-dev sudo -H -u rstudio /bin/bash'
alias rss='docker run -p 8787:8787 -v `pwd`:/home/rstudio -e PASSWORD=$RPW -t val-dev'
alias dev='x11docker --clipboard --share ~/.ssh --share ~/.gitconfig --share ~/.emacs.d --share `pwd` val-dev xfce4-terminal'
