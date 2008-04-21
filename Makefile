# TARGETS
#
# local:	Build and install into the local machine's R archive
# check:	Ask R to check that the package looks okay
# html:		Build the HTML documents and view in a browser
# install:	Build and copy the package across to the public www
# access:	Have a look at who might be downloading rattle

PACKAGE=package/rattle
DESCRIPTION=$(PACKAGE)/DESCRIPTION
DESCRIPTIN=support/DESCRIPTION.in
NAMESPACE=$(PACKAGE)/NAMESPACE

PPACKAGE=package/pmml
PDESCRIPTION=$(PPACKAGE)/DESCRIPTION

RVER=$(shell R --version | head -1 | cut -d" " -f3 | sed 's|\..$||')
REPOSITORY=repository

# Canonical version information from rattle.R
MAJOR=$(shell egrep '^MAJOR' src/rattle.R | cut -d\" -f 2)
MINOR=$(shell egrep '^MINOR' src/rattle.R | cut -d\" -f 2)
SVNREVIS=$(shell svn info | egrep 'Revision:' |  cut -d" " -f 2)
REVISION=$(shell svn info | egrep 'Revision:' |  cut -d" " -f 2\
            | awk '{print $$1-230}')
VERSION=$(MAJOR).$(MINOR).$(REVISION)
VDATE=$(shell svn info |grep 'Last Changed Date'| cut -d"(" -f2 | sed 's|)||'\
	   | sed 's|^.*, ||')

PVERSION=$(shell egrep ' VERSION <-' src/pmml.R | cut -d \" -f 2)

DATE=$(shell date +%F)

R_SOURCE = \
	src/rattle.R \
	src/ada.R \
	src/ada_gui.R \
	src/associate.R \
	src/cluster.R \
	src/execute.R \
	src/export.R \
	src/log.R \
	src/model.R \
	src/nnet.R \
	src/paradigm.R \
	src/projects.R \
	src/random_forest.R \
	src/rpart.R \
	src/textview.R \
	src/zzz.R

# Eventually remove pmml.R from above and put into own package.

PSOURCE = \
	src/pmml.R \
	src/pmml.arules.R

GLADE_SOURCE = src/rattle.glade

SOURCE = $(R_SOURCE) $(GLADE_SOURCE) $(NAMESPACE)

#temp:
#	@echo  $(VDATE)
#temp:
#	@echo rattle_$(VERSION).tar.gz $(VERSION) $(REVISION) $(PVERSION)
#temp:
#	grep REVISION src/rattle.R

default: local plocal

# This one checks the R installations for overlap of packages
# installed. If they are in both local and lib, should remove the
# local one.

checkR:
	ls /usr/local/lib/R/site-library/ > TMPlocal
	ls /usr/lib/R/site-library/ > TMPlib
	meld TMPlocal TMPlib
	rm -f TMPloca TMPlib

revision:
	perl -pi -e "s|Revision: \d* |Revision: $(REVISION) |" src/rattle.R

update:
	svn update

status:
	svn status -q

meld:
	@for i in $(shell svn status -q | awk '{print $$2}'); do\
	  meld $$i;\
	done

install: build pbuild zip # check pcheck
	cp changes.html.in /home/gjw/projects/togaware/www/
	cp todo.html.in /home/gjw/projects/togaware/www/
	(cd /home/gjw/projects/togaware/www/;\
	 perl -pi -e "s|Latest version [0-9\.]* |Latest version $(VERSION) |" \
			rattle.html.in;\
	 perl -pi -e "s|released [^\.]*\.|released $(VDATE).|" \
			rattle.html.in;\
	 perl -pi -e "s|\(revision [0-9]*\)|(revision $(SVNREVIS))|" \
			rattle.html.in;\
	 perl -pi -e "s|rattle_[0-9\.]*zip|rattle_$(VERSION).zip|g" \
			rattle.html.in;\
	 perl -pi -e "s|rattle_[0-9\.]*tar.gz|rattle_$(VERSION).tar.gz|g" \
			rattle.html.in;\
	 perl -pi -e "s|pmml_[0-9\.]*zip|pmml_$(PVERSION).zip|g" \
			rattle.html.in;\
	 perl -pi -e "s|pmml_[0-9\.]*tar.gz|pmml_$(PVERSION).tar.gz|g" \
			rattle.html.in;\
	 perl -pi -e "s|rattle_[0-9\.]*zip|rattle_$(VERSION).zip|g" \
			rattle-download.html.in;\
	 perl -pi -e "s|rattle_[0-9\.]*tar.gz|rattle_$(VERSION).tar.gz|g" \
			rattle-download.html.in;\
	 perl -pi -e "s|pmml_[0-9\.]*zip|pmml_$(PVERSION).zip|g" \
			rattle-download.html.in;\
	 perl -pi -e "s|pmml_[0-9\.]*tar.gz|pmml_$(PVERSION).tar.gz|g" \
			rattle-download.html.in;\
	 make local; lftp -f .lftp-rattle)
	(cd /home/gjw/projects/dmsurvivor/;\
	 perl -pi -e "s|rattle_.*zip|rattle_$(VERSION).zip|g" \
			dmsurvivor.tex;\
	 perl -pi -e "s|rattle_.*tar.gz|rattle_$(VERSION).tar.gz|g" \
			dmsurvivor.tex)
	mv rattle_$(VERSION).tar.gz pmml_$(PVERSION).tar.gz $(REPOSITORY)
	mv rattle_$(VERSION).zip pmml_$(PVERSION).zip $(REPOSITORY)
	-R --no-save < support/repository.R
	chmod go+r $(REPOSITORY)/*
	lftp -f .lftp

check: build
	R CMD check $(PACKAGE)

pcheck: pbuild
	R CMD check $(PPACKAGE)

# For development, temporarily remove the NAMESPACE so all is exposed.

devbuild:
	mv package/rattle/NAMESPACE .
	cp rattle.R package/rattle/R/
	cp rattle.glade package/rattle/inst/etc/
	R CMD build package/rattle
	mv NAMESPACE package/rattle/

build: data rattle_$(VERSION).tar.gz

pbuild: data pmml_$(PVERSION).tar.gz

rattle_$(VERSION).tar.gz: revision $(SOURCE)
	rm -f package/rattle/R/*
	perl -pi -e "s|^VERSION.DATE <- .*$$|VERSION.DATE <- \"Released $(VDATE)\"|" \
             src/rattle.R
	cp $(R_SOURCE) package/rattle/R/
	cp $(GLADE_SOURCE) package/rattle/inst/etc/
	perl -p -e "s|^Version: .*$$|Version: $(VERSION)|" < $(DESCRIPTIN) |\
	perl -p -e "s|^Date: .*$$|Date: $(DATE)|" > $(DESCRIPTION)
	R CMD build $(PACKAGE)
	chmod -R go+rX $(PACKAGE)

pmml_$(PVERSION).tar.gz: $(PSOURCE)
	cp $(PSOURCE) package/pmml/R/
	perl -pi -e "s|^Version: .*$$|Version: $(PVERSION)|" $(PDESCRIPTION)
	perl -pi -e "s|^Date: .*$$|Date: $(DATE)|" $(PDESCRIPTION)
	R CMD build $(PPACKAGE)
	chmod -R go+rX $(PPACKAGE)

data: package/rattle/data/audit.RData

package/rattle/data/audit.RData: support/audit.R Makefile
	R --no-save --quiet < support/audit.R
	cp audit.RData package/rattle/data/
	cp data/audit.csv package/rattle/inst/csv/audit.csv
	cp data/audit.arff package/rattle/inst/arff/audit.arff
	cp data/audit.RData data/audit.csv data/audit.arff src/
	cp data/audit_missing.csv data/audit_auto.csv src/
	cp data/audit.RData data/audit.csv data/audit.arff .
	cp data/audit_missing.csv data/audit_auto.csv .
	chmod go+r $@

zip: local plocal
	(cd /home/gjw/R/x86_64-pc-linux-gnu-library/2.7; zip -r9 - rattle) \
	>| rattle_$(VERSION).zip
	(cd /home/gjw/R/x86_64-pc-linux-gnu-library/2.7; zip -r9 - pmml) \
	>| pmml_$(PVERSION).zip

txt:
	R CMD Rd2txt package/rattle/man/rattle.Rd

html:
	for m in rattle evaluateRisk genPlotTitleCmd plotRisk; do\
	  R CMD Rdconv -t=html -o=$$m.html package/rattle/man/$$m.Rd;\
	  epiphany -n $$m.html;\
	  rm -f $$m.html;\
	done

local: rattle_$(VERSION).tar.gz
	R CMD INSTALL $^

plocal: pmml_$(PVERSION).tar.gz
	R CMD INSTALL $^

access:
	grep 'rattle' /home/gjw/projects/ilisys/log

python:
	python rattle.py

test:
	R --no-save --quiet < regression.R 

ptest:
	r ptest.R

clean:
	rm -f rattle_*.tar.gz rattle_*.zip
	rm -f package/rattle/R/rattle.R package/rattle/inst/etc/rattle.glade
	rm -f package/rattle/DESCRIPTION
	rm -f pmml_*.tar.gz pmml_*.zip

realclean: clean
	rm -f package/rattle/data/audit.RData package/rattle/inst/csv/audit.csv
	rm -rf rattle.Rcheck rattle_$(VERSION).tar.gz
