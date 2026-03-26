FROM rocker/verse:4.3.0

ENV RENV_CONFIG_REPOS_OVERRIDE="https://packagemanager.posit.co/cran/__linux__/jammy/latest"

RUN Rscript -e "install.packages(c('rstatix','dunn.test','effectsize'), repos='https://packagemanager.posit.co/cran/__linux__/jammy/latest')"
