# TODO temporary (not tested) Dockerfile for creating a NetRobots server on a Docker container.

FROM ubuntu:16.04

RUN apt-get update && apt-get install -y \
    python-pip\
    git \
    nano

RUN pip install -r requirements.txt

