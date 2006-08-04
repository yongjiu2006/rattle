# TARGETS
#
# check:	As R to check that the package looks okay
# html:		Build the HTML documents and view in a browser
# local:	Build and install in the local machine's R archive
# install:	Build and copy the package across to the public www
# access:	Have a look at who might be downloading rattle

# Canonical version information from rattle.R
RELEASE=8
VERSION:=$(shell egrep '^VERSION' src/rattle.R | cut -d\" -f 2)-$(RELEASE)
DATE:=$(shell date +%F)

install: build zip check
	cp src/rattle.R src/rattle.glade /var/www/access/
	chmod go+r /var/www/access/rattle*
	cp rattle_$(VERSION).tar.gz rattle_$(VERSION).zip repository
	chmod go+r repository/*
	R --no-save < repository.R
	lftp -f .lftp

check: build
	R CMD check rattle

# For development, temporarily remove the NAMESPACE so all is exposed.

devbuild:
	mv rattle/NAMESPACE .
	cp rattle.R rattle/R/
	cp rattle.glade rattle/inst/etc/
	R CMD build rattle
	mv NAMESPACE rattle/

build: data
	cp rattle.R rattle/R/
	cp rattle.glade rattle/inst/etc/
	cp audit.RData rattle/data
	cp audit.csv rattle/inst/csv/
	perl -pi -e "s|^Version: .*$$|Version: $(VERSION)|" rattle/DESCRIPTION
	perl -pi -e "s|^Date: .*$$|Date: $(DATE)|" rattle/DESCRIPTION
	(cd /home/gjw/projects/togaware/www/;\
	 perl -pi -e "s|rattle_......zip|rattle_$(VERSION).zip|g" \
			rattle.html.in;\
	 perl -pi -e "s|rattle_......tar.gz|rattle_$(VERSION).tar.gz|g" \
			rattle.html.in;\
	 make local; lftp -f .lftp-rattle)
	(cd /home/gjw/projects/dmsurvivor/;\
	 perl -pi -e "s|rattle_......zip|rattle_$(VERSION).zip|g" \
			dmsurvivor.tex;\
	 perl -pi -e "s|rattle_......tar.gz|rattle_$(VERSION).tar.gz|g" \
			dmsurvivor.tex)
	R CMD build rattle
	chmod -R go+rX rattle/

data: package/rattle/data/audit.RData

package/rattle/data/audit.RData: support/audit.R
	R --no-save --quiet < support/audit.R
	mv audit.RData package/rattle/data/
	mv audit.csv package/rattle/inst/csv/audit.csv
	mv survey.data survey.csv archive
	chmod go+r $@

zip: local
	(cd /usr/local/lib/R/site-library; zip -r9 - rattle) >| \
	rattle_$(VERSION).zip

txt:
	R CMD Rd2txt rattle/man/rattle.Rd

html:
	for m in rattle evaluateRisk genPlotTitleCmd plotRisk; do\
	  R CMD Rdconv -t=html -o=$$m.html rattle/man/$$m.Rd;\
	  epiphany -n $$m.html;\
	  rm -f $$m.html;\
	done

local: build
	R CMD INSTALL rattle_$(VERSION).tar.gz

access:
	grep 'rattle' /home/gjw/projects/ilisys/log

python:
	python rattle.py

