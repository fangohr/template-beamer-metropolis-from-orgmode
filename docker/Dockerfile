FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update -y && apt-get install -y \
    python3 python3-dev git pandoc texlive-xetex \
    latexmk python3-pip wget unzip emacs \
    && rm -rf /var/lib/apt/lists/*
RUN pip3 install pygments

# Following instructions to install Fira Sans font, taken from
# https://stevescott.ca/2016-10-20-installing-the-fira-font-in-ubuntu.html
RUN mkdir -p /usr/share/fonts/truetype/fira
RUN mkdir -p /usr/share/fonts/opentype/fira

RUN wget https://github.com/carrois/Fira/archive/master.zip
RUN unzip master.zip
RUN find FiraSans-master/ -name "*.otf" -exec cp {} /usr/share/fonts/opentype/fira/ \;
RUN find FiraSans-master/ -name "*.ttf" -exec cp {} /usr/share/fonts/truetype/fira/ \;

RUN mkdir -p /io
WORKDIR /io
CMD ["/bin/bash"]
