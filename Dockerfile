FROM rocker/r-ver:4.4.1

# System libs
RUN apt-get update && apt-get install -y --no-install-recommends \
    git make g++ gfortran \
    libopenblas-dev liblapack-dev \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libfontconfig1-dev libfreetype6-dev libpng-dev libjpeg-dev libtiff5-dev libxt-dev \
    libharfbuzz-dev libfribidi-dev \
    pandoc && rm -rf /var/lib/apt/lists/*

# pak + BiocManager
RUN Rscript -e "install.packages(c('pak','BiocManager'), repos='https://cran.r-project.org')"

# Make pak use CRAN + Bioconductor (needed for limma, etc.)
RUN Rscript -e "options(repos = BiocManager::repositories()); pak::repo_add(CRAN='https://cran.r-project.org')"

# (optional) Pre-install heavy CRAN deps — NOTE: no version ranges here
RUN Rscript -e "pak::pak(c('caret','data.table','ggplot2','purrr','shiny'))"

# Install your package from GitHub (and remaining deps incl. Remotes)
RUN Rscript -e "pak::pak('CompBio-Lab/omicsBioAnalytics')"

EXPOSE 3838
CMD ["R", "-e", "shiny::runApp(system.file('app', package='omicsBioAnalytics'), host='0.0.0.0', port=3838, launch.browser=FALSE)"]
