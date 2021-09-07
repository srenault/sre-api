#!/bin/bash

ROOT=$(pwd)
cd src/test/resources/
tar hcvf resources.tar application.conf icompta.cdb transactions
cd $ROOT
mv src/test/resources/resources.tar .
gpg --symmetric --cipher-algo AES256 resources.tar
rm resources.tar
