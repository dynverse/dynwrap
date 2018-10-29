FROM rocker/tidyverse

ENV OPENBLAS_NUM_THREADS=1
ENV NUMEXPR_NUM_THREADS=1
ENV MKL_NUM_THREADS=1
ENV OMP_NUM_THREADS=1

RUN apt-get update && apt-get install -y libhdf5-dev libssh-dev

RUN R -e 'options(echo = TRUE); install.packages("minqa")' # weird problem where minqa can't install when .Rprofile isn't empty

RUN echo 'utils::setRepositories(ind=1:4); options(echo = TRUE)' > ~/.Rprofile

RUN R -e 'devtools::install_github("dynverse/dyndimred", dependencies = TRUE)'

RUN R -e 'devtools::install_github("dynverse/dynwrap@devel", dependencies = TRUE)'

RUN R -e 'devtools::install_cran(c("RcppEigen", "RSpectra", "RcppArmadillo"))' # preinstall certain rcpp libraries

RUN R -e 'devtools::install_cran("SingleCellExperiment")'

LABEL version 0.2.0.3
