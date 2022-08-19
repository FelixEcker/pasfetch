rm -rf out
mkdir out

fpc src/PasFetch.pas -FE"out/"

mv out/PasFetch PasFetch