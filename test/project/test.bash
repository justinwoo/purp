#!/usr/bin/env bash

set -e

purp version
psc-package install
purp build
purp test
purp test -m Test.Main
purp make-module
purp bundle
node index.js
