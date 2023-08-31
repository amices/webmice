FROM rocker/r-ver:4.3.1
LABEL version="0.1.1"
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
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('amices/mice')"
RUN R -e "remotes::install_github('amices/micerocker')"  # 0.1.3

# Create working directory
RUN mkdir /home/webmice
ENV WEBMICE_LOC="/home/webmice"

# Create data directory
RUN mkdir /home/webmice/data_uploads
ENV MICEROCKER_DATA_UPLOADS="/home/webmice/data_uploads"

COPY webmice.R /home/webmice/webmice.R
COPY ../openapi.yaml /home/webmice/openapi.yaml

WORKDIR /home/webmice
CMD ["Rscript", "webmice.R"]

EXPOSE 8080
