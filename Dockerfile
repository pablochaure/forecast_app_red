FROM rocker/shiny-verse:latest

RUN apt-get update -qq \
  && apt-get -y --no-install-recommends install \
    lbzip2 \
    libfftw3-dev \
    libgdal-dev \
    libgeos-dev \
    libgsl0-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libhdf4-alt-dev \
    libhdf5-dev \
    libjq-dev \
    libpq-dev \
    libproj-dev \
    libprotobuf-dev \
    libnetcdf-dev \
    libsqlite3-dev \
    libssl-dev \
    libudunits2-dev \
    netcdf-bin \
    postgis \
    protobuf-compiler \
    sqlite3 \
    tk-dev \
    unixodbc-dev \
    libsasl2-dev \
    libv8-dev \
    libsodium-dev \
    liblzma-dev \
    libglpk-dev \
    default-jre \
    default-jdk \
    r-cran-rjava \
    tesseract-ocr \
    libtesseract-dev \
    libpoppler-cpp-dev
    
RUN install2.r --error --deps TRUE \
    lubridate \      
    PerformanceAnalytics \
    dplyr \
    forcats \
    ggplot2 \
    plotly \
    rlang \
    stringr \
    tibble \
    tidyr \
    dplyr \
    purrr \
    berryFunctions \
    bit64 \
    data.table \
    DataExplorer \
    DT \
    factoextra \
    fresh \
    fs \
    imputeTS \
    lubridate \
    modeltime \
    modeltime.ensemble \
    modeltime.gluonts \
    modeltime.h2o \
    modeltime.resample \
    neuralprophet \
    plotly \
    reactable \
    readxl \
    remotes \
    reticulate \
    rlist \
    shiny \
    shinyalert \
    shinycssloaders \
    shinydashboard \
    shinydashboardPlus \
    shinyEffects \
    shinyjs \
    shinyWidgets \
    tidymodels \
    tidyquant \
    tidyverse \
    timetk \
    tools \
    writexl \
    htmltools \
    plotly \
    scales \
    shiny \
    shinyjs

RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site
COPY app.R /srv/shiny-server/forecast_app_red
COPY 00_scripts /srv/shiny-server/forecast_app_red/00_scripts
COPY www /srv/shiny-server/forecast_app_red/www
EXPOSE 3838
RUN sudo chown -R shiny:shiny /srv/shiny-server
CMD ["/usr/bin/shiny-server.sh"]