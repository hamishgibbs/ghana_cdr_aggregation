FROM rocker/geospatial:4

RUN R -e 'devtools::install_github("hamishgibbs/ggutils")'
RUN R -e 'devtools::install_github("ropensci/rnaturalearthhires")'
RUN R -e 'install.packages("ggrepel")'
RUN R -e 'install.packages("cowplot")'
RUN R -e 'install.packages("odin")'

RUN echo "uiPrefs={\"theme\" : \"Solarized Dark\"}" >> \
  /home/rstudio/.rstudio/monitored/user-settings/user-settings
