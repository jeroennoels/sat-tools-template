#!/bin/bash
find ../sat-tools-template/ -name "*.o"  -print0 | xargs -r -0 /bin/rm
find ../sat-tools-template/ -name "*.hi" -print0 | xargs -r -0 /bin/rm
find ../sat-tools-template/ -name "*~"   -print0 | xargs -r -0 /bin/rm
/bin/rm ../sat-tools-template/test/Main
