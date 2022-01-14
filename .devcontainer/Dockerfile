FROM ghcr.io/epiforecasts/forecast.vocs:main

WORKDIR /workspaces/ecdc-weekly-growth-forecasts
COPY renv.lock /workspaces/ecdc-weekly-growth-forecasts/renv.lock
RUN Rscript -e "install.packages('renv')"
RUN Rscript -e "renv::restore()"