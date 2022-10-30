#!/usr/bin/env bash

cabal build exe:marlowe-lambda

cp -p dist-newstyle/build/x86_64-linux/ghc-8.10.7/marlowe-lambda-0.1.0.0/x/marlowe-lambda/noopt/build/marlowe-lambda/marlowe-lambda deploy/marlowe-lambda

pushd deploy/

mkdir -p lib

cp $(ldd marlowe-lambda | grep -F '=> /' | awk '{print $3}') lib/

chmod +w marlowe-lambda lib/*

patchelf --set-interpreter ./lib/ld-linux-x86-64.so.2 --set-rpath ./lib --force-rpath marlowe-lambda

for d in lib/lib{pq,gssapi_krb5}.so.*
do
  patchelf --set-rpath . --force-rpath $d
done

zip -9r ../marlowe-lambda.zip .

popd
