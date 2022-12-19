#!/usr/bin/env bash

cabal install exe:marlowe-lambda --installdir=./deploy --overwrite-policy=always

rm marlowe-lambda.zip

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
