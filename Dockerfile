FROM rocker/rstudio:4.3.0

RUN R -e "install.packages(c('tidyverse','readxl','rstatix','dunn.test','effectsize'), repos='https://cloud.r-project.org')"
