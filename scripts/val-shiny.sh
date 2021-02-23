#!/bin/bash 

PORT=$1

docker build . -f Dockerfile.shiny -t valshiny

docker run -p $1:$1 -e PORT=$1 -t valshiny 

