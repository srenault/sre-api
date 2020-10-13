#!/bin/bash

ROOT=$(pwd)

cd src/test/resources/
rm resources.*
tar cvf resources.tar application.conf icompta.cdb sre.db transactions
cd $ROOT
mv src/test/resources/resources.tar .
travis encrypt-file resources.tar --add
rm resources.tar
