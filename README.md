# purp

[![Build Status](https://travis-ci.org/justinwoo/purp.svg?branch=master)](https://travis-ci.org/justinwoo/purp)

[![npm](https://img.shields.io/npm/v/purp-bin-simple.svg)](https://www.npmjs.com/package/purp-bin-simple)

Some kind of CLI for building PureScript projects with Psc-Package

## Usage

Run `purp` once to see the options:

```
Usage: purp (build | test | bundle | make-module | version)

Commands:
  build [[passthrough args]]
    Build the project, with passthrough args to purp.
  test [-m Your.Test.Module.Name]
    Test the project with some module, default Test.Main.
  bundle [bundle options]
    Bundle the project, with optional main and target path arguments
  make-module [bundle options]
    Make a CommonJS module by running bundle first
Bundle options (for bundle and make-module):
  -m
    Specify main module e.g. Main
  -o
    Specify output path e.g. index.js
```

## FAQ

### Where do I get "do action on watch files?"

Maybe one is better off using something like <https://github.com/watchexec/watchexec>?
