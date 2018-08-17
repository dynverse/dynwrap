#!/bin/bash
docker build r -t dynverse/dynwrap:r
docker build bioc -t dynverse/dynwrap:bioc
docker build py3.6 -t dynverse/dynwrap:py3
docker build py2.7 -t dynverse/dynwrap:py2.7

docker push dynverse/dynwrap