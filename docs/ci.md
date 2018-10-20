# Installing Purp on CI

You can use the same setup as in the [installation](./intro.html#Installation), otherwise, but you can download the binaries directly from Github easily.

## Azure Pipelines

From the [vidtracker example](https://raw.githubusercontent.com/justinwoo/vidtracker/master/azure-pipelines.yml):

```yml
pool:
  vmImage: 'Ubuntu 16.04'

steps:
- script: |
    PURESCRIPT_TAG=v0.12.0
    PSC_PACKAGE_TAG=v0.4.2
    PURP_TAG=0.3.0.0

    PURESCRIPT=https://github.com/purescript/purescript/releases/download/$PURESCRIPT_TAG/linux64.tar.gz
    PSC_PACKAGE=https://github.com/purescript/psc-package/releases/download/$PSC_PACKAGE_TAG/linux64.tar.gz
    PURP=https://github.com/justinwoo/purp/releases/download/$PURP_TAG/linux.tar.gz

    wget -O $HOME/purescript.tar.gz $PURESCRIPT
    wget -O $HOME/psc-package.tar.gz $PSC_PACKAGE
    wget -O $HOME/purp.tar.gz $PURP

    tar -xvf $HOME/psc-package.tar.gz -C $HOME/
    tar -xvf $HOME/purescript.tar.gz -C $HOME/
    tar -xvf $HOME/purp.tar.gz -C $HOME/bin

    mv $HOME/purescript/* $HOME/bin
    mv $HOME/psc-package/* $HOME/bin

    chmod a+x $HOME/bin
  displayName: 'Install deps'
- script: |
    export PATH=./bin:$HOME/bin:$PATH

    which purs
    which psc-package
    which purp

    make
  displayName: 'Make'
```

## Travis CI

From the [vidtracker example](https://raw.githubusercontent.com/justinwoo/vidtracker/master/.travis.yml)

```yml
language: node_js
sudo: required
dist: trusty
node_js: stable
env:
  - PATH=./bin:$HOME/bin:$PATH
install:
  - PURESCRIPT_TAG=v0.12.0
  - PSC_PACKAGE_TAG=v0.4.0
  - PURP_TAG=0.3.0.0

  - PURESCRIPT=https://github.com/purescript/purescript/releases/download/$PURESCRIPT_TAG/linux64.tar.gz
  - PSC_PACKAGE=https://github.com/purescript/psc-package/releases/download/$PSC_PACKAGE_TAG/linux64.tar.gz
  - PURP=https://github.com/justinwoo/purp/releases/download/$PURP_TAG/linux.tar.gz

  - wget -O $HOME/purescript.tar.gz $PURESCRIPT
  - wget -O $HOME/psc-package.tar.gz $PSC_PACKAGE
  - wget -O $HOME/purp.tar.gz $PURP

  - tar -xvf $HOME/psc-package.tar.gz -C $HOME/
  - tar -xvf $HOME/purescript.tar.gz -C $HOME/
  - tar -xvf $HOME/purp.tar.gz -C $HOME/bin

  - mv $HOME/purescript/* $HOME/bin
  - mv $HOME/psc-package/* $HOME/bin

  - chmod a+x $HOME/bin
script:
  - which purs
  - which psc-package
  - which purp
  - make
```
