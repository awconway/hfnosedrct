FROM asachet/rocker-tidymodels:latest

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
    naniar \
    ggbeeswarm \
    patchwork \
    captioner \
    flextable \
    officer

RUN Rscript -e "install.packages('INLA', repos=c(getOption('repos'), INLA='https://inla.r-inla-download.org/R/testing'), dep=TRUE)"

RUN installGithub.r \
    julianfaraway/brinla \
    awconway/hfnosedrct \
    davidgohel/officedown
