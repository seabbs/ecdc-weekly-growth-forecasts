FROM ghcr.io/epiforecasts/forecast.vocs:main

COPY renv.lock /tmp/package/renv.lock
RUN Rscript -e "install.packages('renv')"
RUN Rscript -e "renv::restore('/tmp/package')"