#!/bin/sh
if [ "$STACK_FILE" != stack-8.6.3.yaml ] && [ "$STACK_FILE" != stack.yaml ]; then
    # use -f so it doesn't error if it doesn't exist
    rm -rf ~/.stack/precompiled/*/ghc-8.6.3
    rm -rf ~/.stack/programs/*/ghc-8.6.3*
    rm -rf ~/.stack/loaded-snapshot-cache/*/ghc-8.6.3
    rm -rf .stack-work/install/*/*/8.6.3
fi