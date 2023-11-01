FROM rocker/r-ver:4.3.1
LABEL version="0.1"
LABEL description="Docker container running webmice (R, RestRserve)"

ARG R_VERSION=4.3.1
ARG OS_IDENTIFIER=ubuntu-2004

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8

# nloptr needs cmake
# haven needs zlib1g-dev
RUN apt update && \
    apt -y install cmake && \
    apt -y install zlib1g-dev

#RUN echo 'options(repos = c(CRAN = "https://cloud.r-project.org"))' >>"${R_HOME}/etc/Rprofile.site"
RUN R -e "install.packages('RestRserve', repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('jsonlite')"
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('amices/mice', ref='support_blocks')"

# Pull in webmice code
RUN mkdir /home/webmice
RUN rm -rf /home/webmice
RUN mkdir /home/webmice
ENV WEBMICE_LOC="/home/webmice"

COPY webmice.R /home/webmice/webmice.R
COPY webmice_handlers.R /home/webmice/webmice_handlers.R
COPY webmice_functions.R /home/webmice/webmice_functions.R
COPY sanitize_input.R /home/webmice/sanitize_input.R
COPY ../openapi.yaml /home/webmice/openapi.yaml

WORKDIR /home/webmice
CMD ["Rscript", "webmice.R"]

EXPOSE 8080
