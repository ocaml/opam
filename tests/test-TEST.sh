#!/bin/sh

if [ -f $1 ]; then
    . $1
fi

if [ x${TEST} != x$2 ]; then
    exit 2
fi
