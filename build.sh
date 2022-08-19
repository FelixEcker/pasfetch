rm -rf out
mkdir out

fpc -O3 -CpCOREAVX2 -OpCOREAVX2 -CfAVX2 -Xs src/PasFetch.pas -FE"out/"

mv out/PasFetch PasFetch