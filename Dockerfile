## MODE trelliscope service, called from MODE shiny app.

FROM code-registry.emsl.pnl.gov/multiomics-analyses/rworker-docker:0.0.3

# Install new mapDataAccess
RUN R -e "install.packages('devtools', repos = 'https://cran.rstudio.com')"
RUN R -e "devtools::install_local('/srv/shiny-server/mapDataAccess')"

# load package to be installed from source
RUN R -e "devtools::install_github('hafen/trelliscopejs', repos = 'https://cran.rstudio.com', upgrade = 'always')"

# tell renv where to look for package sources to be installed locally
#ENV RENV_PATHS_LOCAL=/local_packages

## Install R packages ##

# we need only the lockfile to restore everything
COPY renv.lock .

# pre-install renv
RUN R -e "install.packages('renv', repos = 'https://cran.rstudio.com')"

# call restore() to install everything in the lock file.
RUN R -e 'renv::restore()'

COPY plot_functions /plot_functions
COPY preprocessing /preprocessing
COPY rworker.R /
CMD ["Rscript", "rworker.R", "&"]
