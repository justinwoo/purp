# purp

[![Build Status](https://travis-ci.org/justinwoo/purp.svg?branch=master)](https://travis-ci.org/justinwoo/purp)

Some kind of CLI for building PureScript projects with Psc-Package

## Features

```
purp

Usage: purp (build | test | bundle | make-module)

Available options:
  -h,--help                Show this help text

Available commands:
  build                    Build the project
  test                     Test the project with Test.Main
  bundle                   Bundle the project, with optional main and target path arguments
  make-module              Make a CommonJS module by running bundle first
```

## FAQ

### Where do I get "do action on watch files?"

Maybe one is better off using something like <https://github.com/watchexec/watchexec>?
