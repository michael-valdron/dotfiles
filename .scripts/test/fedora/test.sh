#!/bin/sh

if [ -z "${1}" ]
then
    echo "Please specify script to test."
    exit 1
fi

SCRIPT=$(readlink -f $1)
BUILD_PATH=$(dirname $(readlink -f $0))
IMG=$USER/$(basename $BUILD_PATH)
TAG="testing"

# Copy target script to pwd
cp $SCRIPT $BUILD_PATH/run.sh

# Test build image with target script
docker build --no-cache --force-rm -t $IMG:$TAG $BUILD_PATH

# Remove testing image
docker rmi $IMG:$TAG

# Remove target script from pwd
rm -f $BUILD_PATH/run.sh
