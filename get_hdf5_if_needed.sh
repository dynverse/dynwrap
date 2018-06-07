#!/bin/bash

set -e

if [ -z ${HDF5_DIR+x} ]; then
    echo "Using OS HDF5"
else
    echo "Using downloaded HDF5"
    #python3 -m pip install requests
    #python3 ci/get_hdf5.py
    if [ -f $HDF5_DIR/lib/libhdf5.so ]; then
	    echo "using cached build"
    else
        pushd /tmp
        wget https://www.hdfgroup.org/ftp/HDF5/releases/hdf5-1.10/hdf5-$HDF5_VERSION/src/hdf5-$HDF5_VERSION.tar.gz
        tar -xzvf hdf5-$HDF5_VERSION.tar.gz
        pushd hdf5-$HDF5_VERSION
        chmod u+x autogen.sh
        ./configure --prefix $HDF5_DIR
        make -j 2
        make install
        popd
        popd
    fi

    echo "copying"
    sudo cp $HDF5_DIR/lib/* /usr/local/lib/
    sudo cp $HDF5_DIR/bin/* /usr/local/bin/
    sudo cp $HDF5_DIR/include/* /usr/local/include/
    sudo cp $HDF5_DIR/share/* /usr/local/share/

    R -e "install.packages('hdf5r')"
fi
