FROM registry.gitlab.com/couchbits/movestore/movestore-groundcontrol/co-pilot-v1-shiny:geospatial-4.1.2-2570

# install system dependencies required by this app
# no-op

WORKDIR /root/app

# clean up from packrat
RUN rm -rf .Rprofile packrat init-by-packrat.R
# the app
COPY ShinyModule.R .
# renv as R dependency manager
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/activate.R
RUN R -e "renv::restore()"
