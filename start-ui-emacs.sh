docker build . --build-arg linux_user_pwd="$(cat .password)" -t val
docker run -p 8888:8888 \
       -p 8787:8787 \
       -v /home/toups/.emacs.d:/home/rstudio/.emacs.d \
       -v /home/toups/.emacs-trash:/home/rstudio/.emacs-trash \
       -v $(pwd):/home/rstudio/work \
       --user rstudio \
       --workdir /home/rstudio/work\
       --hostname val-eng\
       -e DISPLAY=:0\
       -v /tmp/.X11-unix/X0:/tmp/.X11-unix/X0\
       -it val\
       emacs 

