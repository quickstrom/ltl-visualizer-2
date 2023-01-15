#!/usr/bin/env bash

set -e

DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )


dune build
BUILD="$DIR/_build/default/bin"
STAGE=$(mktemp -d)

cp $BUILD/{index.html,main.bc.js,main.css} $STAGE/
ghp-import -p -f $STAGE
