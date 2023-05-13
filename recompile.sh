#!/bin/bash

$BNFC -m latte.cf
make
happy -gca --info=latte.txt ParLatte.y
