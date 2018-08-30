#!/bin/bash
docker build r -t dynverse/dynwrap:r
docker build bioc -t dynverse/dynwrap:bioc
docker build py3.6 -t dynverse/dynwrap:py3.6
docker build py2.7 -t dynverse/dynwrap:py2.7

# sudo singularity build r.simg bioc/Singularity.r
# sudo singularity build bioc.simg bioc/Singularity.bioc
# sudo singularity build py3.6.simg bioc/Singularity.py3.6
# sudo singularity build py2.7.simg bioc/Singularity.py2.7

# docker push dynverse/dynwrap
