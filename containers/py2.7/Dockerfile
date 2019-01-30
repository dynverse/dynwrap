FROM library/python:2.7

ENV OPENBLAS_NUM_THREADS=1
ENV NUMEXPR_NUM_THREADS=1
ENV MKL_NUM_THREADS=1
ENV OMP_NUM_THREADS=1

RUN apt-get update && apt-get install -y libhdf5-dev

RUN pip install scipy numpy pandas matplotlib sklearn h5py tables Cython

LABEL version 0.2.0.2
