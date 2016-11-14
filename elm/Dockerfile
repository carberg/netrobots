
# TODO not complete: this should be a Docker file installing an Elm compiler.
# Up to date Elm is installed using Nix and `default.nix` file content.

FROM node:latest

RUN mkdir /data
RUN cd /data/ && npm install -g elm

ADD *.elm /data/
ADD *.json /data/
ADD *.sh /data/

VOLUME ["/data"]

