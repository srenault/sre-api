#!/bin/bash

ROOT=$(pwd)
cd src/test/resources/
tar hcvf resources.tar application.conf icompta.cdb transactions
cd $ROOT
mv src/test/resources/resources.tar .
travis encrypt-file resources.tar --add
rm resources.tar
