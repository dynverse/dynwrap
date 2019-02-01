Bootstrap: docker

From: rocker/tidyverse

%labels
    version 0.2.0.8

%environment
    OPENBLAS_NUM_THREADS=1
    NUMEXPR_NUM_THREADS=1
    MKL_NUM_THREADS=1
    OMP_NUM_THREADS=1

    export OPENBLAS_NUM_THREADS NUMEXPR_NUM_THREADS MKL_NUM_THREADS OMP_NUM_THREADS

%post
    apt-get update && apt-get install -y libhdf5-dev libssh-dev
    echo 'utils::setRepositories(ind=1:4)' > ~/.Rprofile
    R -e 'devtools::install_cran("Rcpp")'
    R -e 'devtools::install_github("dynverse/dyndimred", dependencies = TRUE)'
    R -e 'devtools::install_github("dynverse/dynwrap", dependencies = TRUE)'
    R -e 'devtools::install_cran(c("RcppEigen", "RSpectra", "RcppArmadillo"))' # preinstall certain rcpp libraries
