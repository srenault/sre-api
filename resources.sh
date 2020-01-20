#!bin/bash

cd src/test/resources/
rm resources.*
tar cvf resources.tar application.conf icompta.cdb sre.db transactions
travis encrypt-file resources.tar --add
