FROM registry.gitlab.com/couchbits/movestore/movestore-groundcontrol/co-pilot-v1-shiny:geospatial-4.1.2-2809

# install system dependencies required by this app
# install phantomJS
RUN apt-get update \
    && apt-get install -qq -y --no-install-recommends \
      build-essential chrpath libssl-dev libxft-dev \
      libfreetype6 libfreetype6-dev \
      libfontconfig1 libfontconfig1-dev \
    && apt-get clean
ENV PHANTOM_JS phantomjs-2.1.1-linux-x86_64
RUN wget https://github.com/Medium/phantomjs/releases/download/v2.1.1/$PHANTOM_JS.tar.bz2 \
    && tar xvjf $PHANTOM_JS.tar.bz2 \
    && mv $PHANTOM_JS /usr/local/share \
    && ln -sf /usr/local/share/$PHANTOM_JS/bin/phantomjs /usr/local/bin \
    && echo phantomjs --version
# end install phantomJS

WORKDIR /root/app

# install the R dependencies this app needs
RUN R -e 'remotes::install_version("move")'
RUN R -e 'remotes::install_version("ggmap")'
RUN R -e 'remotes::install_version("adehabitatHR")'
RUN R -e 'remotes::install_version("fields")'
RUN R -e 'remotes::install_version("scales")'
RUN R -e 'remotes::install_github("tidyverse/lubridate")'

# copy the app as last as possible
# therefore following builds can use the docker cache of the R dependency installations
COPY ShinyModule.R .

# take a snapshot of all R dependencies
RUN R -e 'renv::snapshot()'
