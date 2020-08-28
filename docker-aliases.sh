# handy aliases for working with the docker file
# and doing other stuff

source secrets.sh

alias bu='docker build . --build-arg linux_user_pwd=$RPW -t val-dev'
alias dr='docker run -v `pwd`:/home/rstudio -e PASSWORD=$RPW -it val-dev'
alias r='docker run -v `pwd`:/home/rstudio -e PASSWORD=$RPW -it val-dev sudo -H -u rstudio /bin/bash -c "cd ~/; R"'
alias b='docker run -v `pwd`:/home/rstudio -e PASSWORD=$RPW -it val-dev sudo -H -u rstudio /bin/bash'
alias build_package='docker run -v `pwd`:/home -v `pwd`:/home/rstudio -e PASSWORD=$RPW -it val-dev sudo -H -u rstudio /bin/bash -c "cd ~/; R -e \"library(devtools); build();\""'
alias rss='docker run -p 8787:8787 -v `pwd`:/home/rstudio -e PASSWORD=$RPW -t val-dev'
