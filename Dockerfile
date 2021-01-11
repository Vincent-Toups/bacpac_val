FROM rocker/verse
MAINTAINER Vincent Toups <toups@email.unc.edu>
RUN R -e "install.packages('xlsx')"
ARG linux_user_pwd
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN echo "rstudio:$linux_user_pwd" | chpasswd
RUN adduser rstudio sudo
RUN apt update && apt install -y emacs
RUN apt update && DEBIAN_FRONTEND=noninteractive apt install -y xfce4-terminal
RUN R -e "install.packages('readODS')"
RUN apt update && apt-get -y install build-essential wget
RUN wget https://root.cern.ch/download/cling/cling_2020-11-05_ROOT-ubuntu2004.tar.bz2
RUN tar xvf /cling_2020-11-05_ROOT-ubuntu2004.tar.bz2 -C /usr --strip-components=1
