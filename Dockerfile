# Use the official R Shiny image
FROM rocker/shiny:latest

# Install Linux dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libnetcdf-dev \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    unzip \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny', 'shinyjs', 'DT', 'zip', 'ncdf4', 'fields', 'viridisLite', 'lubridate', 'progress'), repos='https://cloud.r-project.org')"

# Create app directory
RUN mkdir -p /srv/shiny-server/app

# Copy your app files
COPY app/ /srv/shiny-server/app/

# Set working directory
WORKDIR /srv/shiny-server/app

# Expose Shiny default port
EXPOSE 3838

# Run the app
CMD ["/usr/bin/shiny-server"]
