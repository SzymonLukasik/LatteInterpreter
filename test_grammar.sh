#!/bin/bash

for file in tests/*; do
  if [[ -f "$file" ]]; then
    ./TestLatte "$file" > /dev/null
    echo "Return code for $file: $?"
  fi
done