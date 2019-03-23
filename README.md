# evy

the app

## building

(I use https://docs.haskellstack.org/en/stable/README/),
installing from apt-get did not work.

stack build

### openssl

cql-io depends on HsOpenSSL, to build HsOpenSSL I needed to:

stack build --extra-lib-dirs=/usr/local/Cellar/openssl/1.0.2o_1/lib/ --extra-include-dirs=/usr/local/Cellar/openssl/1.0.2o_1/include/

## executing

stack exec evy

## cqlsh

cqlsh --cqlversion="3.4.4"
