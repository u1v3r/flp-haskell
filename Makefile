#     Makefile pre projekt flp-basic

CC=ghc
SRCDIR=./src
ONAME=xdvors08
HADDOCK=haddock
HTITLE=flp-basic
HPARAMS= -h -U -w --title $(HTITLE) -p ../doc/prologue

build:	
	cd $(SRCDIR);$(CC) --make Main -o ../$(ONAME)

.PHONY: doc
doc:
	cd $(SRCDIR);$(HADDOCK) $(HPARAMS) -o ../doc/ ./*.hs

.PHONY: clean
clean:  
	rm -f $(SRCDIR)/*.o $(SRCDIR)/*.hi ./$(ONAME)