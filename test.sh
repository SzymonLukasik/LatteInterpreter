#!/bin/bash

for file in tests/*/*; do
  if [[ -f "$file" ]]; then
    cabal run latte -- "$file" > /dev/null
    echo "Return code for $file: $?"
  fi
done