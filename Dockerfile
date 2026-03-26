FROM rocker/verse:4.3.0

ENV RENV_CONFIG_REPOS_OVERRIDE="https://packagemanager.posit.co/cran/__linux__/jammy/latest"

RUN install2.r --error --skipinstalled \
    readxl rstatix dunn.test effectsize
