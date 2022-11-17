#!/usr/bin/env nix-shell
#!nix-shell -i bash -p nodejs

set -ve

export DEBUG='express:*'
npx webpack
npx webpack-dev-server
