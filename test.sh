#!/bin/bash

for file in tests/*/*; do
  if [[ -f "$file" ]]; then
    ./interpreter "$file" #> /dev/null
    echo "Return code for $file: $?"
  fi
done