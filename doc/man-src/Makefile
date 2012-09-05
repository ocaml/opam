SRC=$(wildcard *.md)
DST=$(SRC:.md=.1)


all: mkdir $(SRC) $(DST)

mkdir: 
	mkdir -p ../man

%.1: %.md
	pandoc -s -t man -o ../man/$@ $^

.PHONY: clean

clean:
	rm -rf ../man
