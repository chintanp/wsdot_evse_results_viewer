FROM rocker/r-ver:4.0.3
RUN apt-get update && apt-get install -y  gdal-bin git-core libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libpng-dev libpq-dev libssh2-1-dev libssl-dev libudunits2-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.1")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.2")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("RColorBrewer",upgrade="never", version = "1.1-2")'
RUN Rscript -e 'remotes::install_version("stringi",upgrade="never", version = "1.5.3")'
RUN Rscript -e 'remotes::install_version("sp",upgrade="never", version = "1.4-4")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.0")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("curl",upgrade="never", version = "4.3")'
RUN Rscript -e 'remotes::install_version("httr",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("sf",upgrade="never", version = "0.9-6")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("data.table",upgrade="never", version = "1.13.2")'
RUN Rscript -e 'remotes::install_version("here",upgrade="never", version = "0.1")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_version("leafem",upgrade="never", version = "0.1.3")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.9.2.1")'
RUN Rscript -e 'remotes::install_version("pool",upgrade="never", version = "0.1.4.3")'
RUN Rscript -e 'remotes::install_version("dbplyr",upgrade="never", version = "1.4.4")'
RUN Rscript -e 'remotes::install_version("RPostgres",upgrade="never", version = "1.2.1")'
RUN Rscript -e 'remotes::install_version("shinyBS",upgrade="never", version = "0.61")'
RUN Rscript -e 'remotes::install_version("leaflet.extras",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("auth0",upgrade="never", version = "0.2.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.16")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("hash",upgrade="never", version = "2.2.6.1")'
RUN Rscript -e 'remotes::install_version("rgeos",upgrade="never", version = "0.5-5")'
RUN Rscript -e 'remotes::install_version("readr",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("rgdal",upgrade="never", version = "1.5-18")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.5.4")'
RUN Rscript -e 'remotes::install_github("rstudio/leaflet.mapboxgl@dec90d124daaf5af814584eecfba505b3882e4d8")'
RUN Rscript -e 'remotes::install_github("RinteRface/bs4Dash@2f3aedec89c38e2dd876f02f3cb11cb9c1745eac")'
RUN Rscript -e 'remotes::install_github("Thinkr-open/golem@06707a8bf33770bb6f08466923d06fbb4607b2cd")'
RUN mkdir /build_zone
RUN mkdir /build_zone/logs
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
# RUN rm -rf /build_zone
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');resview::run_app()"
