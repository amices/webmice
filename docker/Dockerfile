FROM rocker/r-ver:4.2.1
LABEL version="0.1"
LABEL description="Docker container running webmice (R, RestRserve)"

ARG R_VERSION=4.2.1
ARG OS_IDENTIFIER=ubuntu-2004

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8

RUN R -e "install.packages('mice', dependencies=TRUE)"
RUN R -e "install.packages('RestRserve', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('jsonlite')"

# Pull in webmice code
RUN mkdir /home/webmice
RUN rm -rf /home/webmice
RUN mkdir /home/webmice

COPY example_webmice.R /home/webmice/example_webmice.R
COPY ../openapi.yaml /home/webmice/openapi.yaml

WORKDIR /home/webmice
CMD ["Rscript", "example_webmice.R"]

EXPOSE 8080
