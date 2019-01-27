#!/bin/sh
if [ "$STACK_FILE" != stack-8.6.3.yaml ] && [ "$STACK_FILE" != stack.yaml ]; then
    rm -r ~/.stack/precompiled/*/ghc-8.6.3
    rm -r ~/.stack/programs/*/ghc-8.6.3*
    rm -r ~/.stack/loaded-snapshot-cache/*/ghc-8.6.3
    rm -r .stack-work/install/*/*/8.6.3
fi