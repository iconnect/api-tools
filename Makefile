OD = dist/build/test
ID = /usr/hs/bin
HC = mkdir -p $(OD); ghc -XHaskell2010 --make -O1 -outputdir build -Wall


all: api-tools docs


api-tools: .prep
	cabal build

docs:
	runghc gen_docs.hs

.prep: Data/API/Scan.hs
	hub load    api-tools <api-tools.har
	hub comment api-tools "api-tools build"
	hub set     api-tools
	cabal configure --enable-tests
	touch .prep

alex: Data/API/Scan.hs

Data/API/Scan.hs: Data/API/Scan.x
	alex $<

save-hub:
	hub save api-tools >api-tools.har

test: .prep
	cabal test

clean:
	cabal clean
	rm -rf build .prep
