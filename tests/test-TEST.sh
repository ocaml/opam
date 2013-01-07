#!/bin/sh

if [ -f $1 ]; then
    . $1
fi

if [ x${TEST} != x$2 ]; then
    echo "Error: TEST=${TEST} instead of $2"
    exit 2
fi
