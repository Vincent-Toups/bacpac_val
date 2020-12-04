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
