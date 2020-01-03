FROM rocker/tidyverse:latest

RUN Rscript -e 'devtools::install_github("Sage-Bionetworks/mhealthtools", threads = getOption("Ncpus",1))'