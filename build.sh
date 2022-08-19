rm -rf out
mkdir out

fpc -O3 -CX src/PasFetch.pas -FE"out/"

mv out/PasFetch PasFetch