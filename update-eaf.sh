#!/usr/bin/env sh

set -eu

git pull

python ./install-eaf.py
