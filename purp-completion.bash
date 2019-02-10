#!/usr/bin/env bash

_purp() {
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    opts="build test bundle make-module"

    case $prev in
        "test")
            COMPREPLY=("-m");
            return 0;;
        "bundle")
            COMPREPLY=("-m -o");
            return 0;;
        "make-module")
            COMPREPLY=("-m -o");
            return 0;;
        *)
            # shellcheck disable=SC2207
            # shellcheck disable=SC2086
            COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) );
            return 0;;
    esac
}

complete -F _purp purp
