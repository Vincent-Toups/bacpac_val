FROM rocker/verse
MAINTAINER Vincent Toups <toups@email.unc.edu>
ARG linux_user_pwd
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN echo "rstudio:$linux_user_pwd" | chpasswd
RUN adduser rstudio sudo
