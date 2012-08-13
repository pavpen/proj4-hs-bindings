NAME = t4

CFLAGS = -lproj
HFLAGS = -lproj --make

all: .capri
	capri install

.capri:
	capri bootstrap
	capri clone array bytestring deepseq containers filepath old-locale \
		old-time pretty split text blaze-builder blaze-markup dlist \
		template-haskell
	capri import ../..

dist-clean:
	rm -Rf dist .capri $(NAME).svg

run: all
	.capri/install/bin/$(NAME) -o $(NAME).svg
