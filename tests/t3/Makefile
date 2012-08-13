NAME = t3

CFLAGS = -lproj
HFLAGS = -lproj --make

all: .capri
	capri install

.capri:
	capri bootstrap
	capri import ../..

dist-clean:
	rm -Rf dist .capri

run: all
	.capri/install/bin/$(NAME)
