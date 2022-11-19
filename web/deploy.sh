#!/usr/bin/env nix-shell
#!nix-shell -i bash -p nodejs

set -ve

npx webpack

VERSION=0

for f in both.html party.html counterparty.html view.css controller.js src/secrets.js node_modules/renderjson/renderjson.js
do
  gsutil -h "Cache-Control:public, max-age=60" cp -a public-read $f gs://dl.bwbush.io/marlowe-swap/$VERSION/$f
done
