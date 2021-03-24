#!/usr/bin/env bash
set -e
tmp=$(mktemp -d)
cd "$(dirname "$0")"
node index.mjs "$1" "$tmp"
cd "$tmp"
unzip -q Roam-Export*.zip
rm Roam-Export*.zip
cat *.edn