# Quickstart

## Installation

### from Github releases

Travis builds the binaries for Linux and OSX for every tag release, so grab the binaries and place them on your path from <https://github.com/justinwoo/purp/releases>.

### npm

You can also install the simple binary distributions from the npm package: `npm i -g purp-bin-simple`

[![npm](https://img.shields.io/npm/v/purp-bin-simple.svg)](https://www.npmjs.com/package/purp-bin-simple)

## Building your project

You can use the `purp build` command to build your project and its dependencies.

```
> purp build
Compiling ...
Build succeeded
```

## Building a bundle

To get an executable bundle, you can use the `purp bundle` command, with optional `-m` main module and `-t` target arguments.

```
> purp bundle -m Main -t index.js
Bundle succeeded and output file to index.js

> node .
```

## Building a module

Sometimes, you will want to build a module that has been "dead code eliminated" if you plan to make a single module of your PS exports, which can be required from JS. However, you probably will often prefer to use the `output/` directory directly.

```
> purp make-module -m Main -t module.js
Bundling first...
Bundle succeeded and output file to module.js
Make module succeeded and output file to module.js

> node -e "console.log(require('./module').main)"
[Function]
```

## Running tests

You can also run tests

```
> purp test -m Test.Main
→ Suite: nameParser
  ✓ Passed: works with valid names
  ✓ Passed: fails with invalid names

All 2 tests passed! 🎉
Tests succeeded.
```

## Actions on watch

Please use `watchexec`. It works and is simple: <https://github.com/watchexec/watchexec>
