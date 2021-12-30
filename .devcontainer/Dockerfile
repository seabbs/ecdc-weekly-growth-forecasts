FROM ghcr.io/epiforecasts/forecast.vocs:main

COPY DESCRIPTION /tmp/package/DESCRIPTION
RUN Rscript -e "devtools::install_deps('/tmp/package', dependencies = TRUE)"