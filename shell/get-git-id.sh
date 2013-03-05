#!/bin/sh -e

if [ -e .git/logs/HEAD ]; then
    describe=$(git describe)
    echo let version = Some \"${describe}\"
else
    echo "let version = None"

fi