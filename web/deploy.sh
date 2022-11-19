#!/usr/bin/env bash

set -v

gsutil cp -a public-read both.html party.html counterparty.html view.css controller.html gs://dl.bwbush.io/marlowe-swap/
gsutil cp -a public-read src/controller.js src/lambda.js src/secrets.js gs://dl.bwbush.io/marlowe-swap/src/
gsutil cp -a public-read node_modules/renderjson/renderjson.js gs://dl.bwbush.io/marlowe-swap/node_modules/renderjson/
