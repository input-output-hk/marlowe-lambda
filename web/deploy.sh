#!/usr/bin/env nix-shell
#!nix-shell -i bash -p nodejs

set -ve

npx webpack

VERSION=$(git rev-parse HEAD)

for f in index.html party.html counterparty.html view.css controller.js src/secrets.js node_modules/renderjson/renderjson.js
do
  gsutil -h "Cache-Control:public,max-age=600" cp $f gs://app.marlowe.run/swap/$VERSION/$f
done

set +v

echo
echo "http://app.marlowe.run/swap/$VERSION/index.html"
echo "http://app.marlowe.run/swap/$VERSION/party.html"
echo "http://app.marlowe.run/swap/$VERSION/counterparty.html"
