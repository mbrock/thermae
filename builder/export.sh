#!/usr/bin/env bash
set -e
tmp=$(mktemp -d)
node index.mjs "$1" "$tmp"
cd "$tmp"
unzip -q Roam-Export*.zip
rm Roam-Export*.zip
realpath *.edn