CFLAGS = -lproj
HFLAGS = -lproj --make

all: .capri
	capri install

#all: t2
#
#t2: t2.hs c_proj4.o Proj4.hs
#	ghc $(HFLAGS) t2.hs -o t2 c_proj4.o

.capri:
	capri bootstrap
	capri import ../..

dist-clean:
	rm -Rf dist .capri
