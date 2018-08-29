Bootstrap: docker

From: rocker/tidyverse

%environment
    OPENBLAS_NUM_THREADS=1
    NUMEXPR_NUM_THREADS=1
    MKL_NUM_THREADS=1
    OMP_NUM_THREADS=1

    export OPENBLAS_NUM_THREADS NUMEXPR_NUM_THREADS MKL_NUM_THREADS OMP_NUM_THREADS

%post
    echo 'utils::setRepositories(ind=1:4); options(echo = TRUE)' > ~/.Rprofile
    R -e 'devtools::install_github("dynverse/dynwrap")'
