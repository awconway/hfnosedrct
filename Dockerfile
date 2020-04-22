FROM asachet/rocker-tidymodels:latest

# copy current directory into /mnt
COPY . /hfnosedrct

RUN install2.r --error \
    drake \
    diagram \
    ggpubr \
    gt \
    gtsummary \
    vroom \
    flexdashboard \
    distill \
    visNetwork \
    janitor \
    labelled \
    downloadthis \
    reactable \
    naniar \
    ggbeeswarm

RUN Rscript -e "install.packages('INLA', repos=c(getOption('repos'), INLA='https://inla.r-inla-download.org/R/testing'), dep=TRUE)"

RUN installGithub.r \
    julianfaraway/brinla \
    awconway/hfnosedrct
