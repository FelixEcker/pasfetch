#!/bin/sh
rm -rf out
mkdir out

fpc -O4 src/PasFetch.pas -FE"out/"

mv out/PasFetch PasFetch
