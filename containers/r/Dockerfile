FROM rocker/tidyverse

ENV OPENBLAS_NUM_THREADS=1
ENV NUMEXPR_NUM_THREADS=1
ENV MKL_NUM_THREADS=1
ENV OMP_NUM_THREADS=1

RUN echo 'utils::setRepositories(ind=1:4); options(echo = TRUE)' > ~/.Rprofile

RUN R -e 'devtools::install_github("dynverse/dynwrap")'
