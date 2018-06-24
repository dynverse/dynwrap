#!/bin/bash

set -e

if [ "$TRAVIS_OS_NAME" == "osx" ]; then # use homebrew version
	echo "installing hdf5"
	brew update
	brew install hdf5 || true
	echo "brew install finished"
else 
	if [ -z ${HDF5_DIR+x} ]; then
	    echo "Using OS HDF5"
	else
	    echo "Using downloaded HDF5"
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
	fi
fi
