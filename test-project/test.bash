#!/usr/bin/env bash

set -e

psc-package install
purp build
purp test
purp test -m Test.Main
purp make-module
purp make-module -m Main -o index.js
purp bundle
purp bundle -m Main -o index.js
node index.js
