# See INSTRUCTIONS

#-----------------------------------------------------------------------
# General Locations

RSITELIB=/usr/local/lib/R/site-library

#-----------------------------------------------------------------------
# Rattle Package

PACKAGE=package/rattle
DESCRIPTION=$(PACKAGE)/DESCRIPTION
DESCRIPTIN=support/DESCRIPTION.in
NAMESPACE=$(PACKAGE)/NAMESPACE

#-----------------------------------------------------------------------
# PMML Package

PPACKAGE=package/pmml
PRDEST=$(PPACKAGE)/R
PDESCRIPTION=$(PPACKAGE)/DESCRIPTION
PVERSION=$(shell egrep ' VERSION <-' src/pmml.R | cut -d \" -f 2)

PSOURCE = \
	src/pmml.R \
	src/pmml.arules.R \
	src/pmml.kmeans.R \
	src/pmml.hclust.R \
	src/pmml.ksvm.R \
	src/pmml.lm.R \
	src/pmml.multinom.R \
	src/pmml.nnet.R \
	src/pmmltoc.R \
	src/pmml.randomForest.R \
	src/pmml.rpart.R \
	src/pmml.rsf.R \
	src/pmml.coxph.R \
	src/pmml.survreg.R

#-----------------------------------------------------------------------

IPACKAGE=package/rstat
IDESCRIPTION=$(IPACKAGE)/DESCRIPTION

RVER=$(shell R --version | head -1 | cut -d" " -f3 | sed 's|\..$||')
REPOSITORY=repository

# Canonical version information from rattle.R
MAJOR=$(shell egrep '^MAJOR' src/rattle.R | cut -d\" -f 2)
MINOR=$(shell egrep '^MINOR' src/rattle.R | cut -d\" -f 2)
SVNREVIS=$(shell svn info | egrep 'Revision:' |  cut -d" " -f 2)
#REVISION=$(shell svn info | egrep 'Revision:' |  cut -d" " -f 2\
#            | awk '{print $$1-480}')
REVISION=$(shell egrep '^REVISION' src/rattle.R | cut -d\" -f 2)
VERSION=$(MAJOR).$(MINOR).$(REVISION)
VDATE=$(shell svn info |grep 'Last Changed Date'| cut -d"(" -f2 | sed 's|)||'\
	   | sed 's|^.*, ||')
IDATE=$(shell date +%m%d%y)

IVERSION=$(shell egrep 'VERSION <- "' src/rstat.R | cut -d \" -f 2)

DATE=$(shell date +%F)

# 080524 add data.R and remove paradigm.R

R_SOURCE = \
	src/rattle.R \
	src/ada.R \
	src/ada_gui.R \
	src/audit.R \
	src/associate.R \
	src/biclust.R \
	src/clara.R \
	src/cluster.R \
	src/ctree.R \
	src/data.R \
	src/evaluate.R \
	src/execute.R \
	src/explore.R \
	src/export.R \
	src/hclust.R \
	src/help.R \
	src/kmeans.R \
	src/log.R \
	src/model.R \
	src/nnet.R \
	src/projects.R \
	src/random_forest.R \
	src/report.R \
	src/rpart.R \
	src/survival.R \
	src/test.R \
	src/textview.R \
	src/textminer.R \
	src/tooltips.R \
	src/transform.R \
	src/zzz.R

ISOURCE = \
	src/rstat.R \
	src/pmmltocibi.R \
	src/pmml.transforms.R

GLADE_SOURCE = src/rattle.glade src/rattle.ui src/tooltips.xml src/textviews.xml

SOURCE = $(R_SOURCE) $(GLADE_SOURCE) $(NAMESPACE)

#temp:
#	@echo  $(VDATE)
#temp:
#	@echo rattle_$(VERSION).tar.gz $(VERSION) $(REVISION) $(PVERSION)
#temp:
#	grep REVISION src/rattle.R

default: local plocal ilocal

.PHONY: ibidiff
ibidiff: zip
	-echo "Diff in rstat.R" >| updates
	-diff ibi/rstat.R src >> updates
	-echo "Diff in pmml.transforms.R" >> updates
	-diff ibi/pmml.transforms.R src >> updates
	-echo "Diff in pmmltocibi.R" >> updates
	-diff ibi/pmmltocibi.R src >> updates
	-echo "Diff in R-en.po" >> updates
	-diff ibi/R-en.po po >> updates
	most updates
	rm -f updates

.PHONY: ibirstat
ibirstat: zip
	mv $@??????????.zip archive
	-echo "Diff in rstat.R" >| ibi/updates
	-diff ibi/rstat.R src >> ibi/updates
	-echo "Diff in pmml.transforms.R" >> ibi/updates
	-diff ibi/pmml.transforms.R src >> ibi/updates
	-echo "Diff in pmmltocibi.R" >> ibi/updates
	-diff ibi/pmmltocibi.R src >> ibi/updates
#	-echo "Diff in R-en.po" >> ibi/updates
#	-diff ibi/R-en.po po >> ibi/updates
	-cp ibi/updates archive/updates`date +%y%m%d%H%M`
	most ibi/updates
	zip $@`date +%y%m%d%H%M`.zip \
	rattle_$(VERSION).zip pmml_$(PVERSION).zip rstat_$(IVERSION).zip \
	src/rstat.R src/pmml.transforms.R src/pmmltocibi.R ibi/updates \
	po/R-en.po
	cp src/rstat.R src/pmml.transforms.R src/pmmltocibi.R ibi/
	cp po/R-en.po ibi/

.PHONY: tom
tom:
	tar zcvf tom`date +%y%m%d%H%M`.tar.gz \
	src/ada_gui.R \
	src/ada.R \
	src/all.R \
	src/associate.R \
	src/audit.R \
	src/cluster.R \
	src/ctree.R \
	src/data.R \
	src/evaluate.R \
	src/execute.R \
	src/explore.R \
	src/export.R \
	src/gbm.R \
	src/hclust.R \
	src/help.R \
	src/kmeans.R \
	src/log.R \
	src/model.R \
	src/nnet.R \
	src/pmml.arules.R \
	src/pmml.hclust.R \
	src/pmml.kmeans.R \
	src/pmml.ksvm.R \
	src/pmml.lm.R \
	src/pmml.multinom.R \
	src/pmml.nnet.R \
	src/pmml.R \
	src/pmml.randomForest.R \
	src/pmml.read.R \
	src/pmml.rpart.R \
	src/pmml.rsf.R \
	src/pmmltoc.R \
	src/pmml.transforms.R \
	src/pmml.coxph.R \
	src/pmml.survreg.R \
	src/projects.R \
	src/random_forest.R \
	src/rattle.R \
	src/report.R \
	src/rpart.R \
	src/survival.R \
	src/test.R \
	src/textminer.R \
	src/textview.R \
	src/tooltips.R \
	src/transform.R \
	src/utils.R \
	src/zzz.R \
	src/rattle.glade \
	src/rattle.ui \
	src/tooltips.xml \
	src/textviews.xml

# This one checks the R installations for overlap of packages
# installed. If they are in both local and lib, should remove the
# local one.

checkR:
	ls /usr/local/lib/R/site-library/ > TMPlocal
	ls /usr/lib/R/site-library/ > TMPlib
	meld TMPlocal TMPlib
	rm -f TMPloca TMPlib

#revision:
#	perl -pi -e "s|Revision: \d* |Revision: $(REVISION) |" src/rattle.R

.PHONY: update
update:
	svn update
	svn update

status:
	svn status -q

meld:
	@for i in $(shell svn status -q | awk '{print $$2}'); do\
	  meld $$i;\
	done

diff:
	svn diff

.PHONY: install
install: # build pbuild ibuild zip rattle_src.zip # check pcheck
	perl -pi -e "s|version is [0-9\.]*\.|version is $(VERSION).|"\
			changes.html.in
	cp changes.html.in /home/gjw/Projects/Togaware/www/
	cp ToDo /home/gjw/Projects/Togaware/www/todo.html.in
	(cd /home/gjw/Projects/Togaware/www/;\
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
	(cd /home/gjw/Projects/dmsurvivor/;\
	 perl -pi -e "s|rattle_.*zip|rattle_$(VERSION).zip|g" \
			dmsurvivor.Rnw;\
	 perl -pi -e "s|rattle_.*tar.gz|rattle_$(VERSION).tar.gz|g" \
			dmsurvivor.Rnw)
	#mv rattle_$(VERSION).tar.gz pmml_$(PVERSION).tar.gz $(REPOSITORY)
	#mv rattle_$(VERSION).zip pmml_$(PVERSION).zip $(REPOSITORY)
	-R --no-save < support/repository.R
	chmod go+r $(REPOSITORY)/*
	lftp -f .lftp

# 100123 Updated the build process

check: #build
	R CMD check $(PACKAGE)

pcheck: #pbuild
	R CMD check $(PPACKAGE)

icheck: #ibuild
	R CMD check $(IPACKAGE)

ucheck: #build
	sh ./upload_uwe.sh

pucheck: #pbuild
	sh ./uploadp_uwe.sh

cran: #build
	sh ./upload_cran.sh

pcran: #pbuild
	sh ./uploadp_cran.sh

# For development, temporarily remove the NAMESPACE so all is exposed.

devbuild:
	mv package/rattle/NAMESPACE .
	cp rattle.R package/rattle/R/
	cp rattle.glade package/rattle/inst/etc/
	cp rattle.ui package/rattle/inst/etc/
	R CMD build package/rattle
	mv NAMESPACE package/rattle/

########################################################################
# BUILD Targets
#
# 100123 Updated the build process

.PHONY: build
build: weather translations \
	rattle_$(VERSION).tar.gz \
	rattle_$(VERSION).zip
	chmod go+r $(REPOSITORY)/*

.PHONY: translations
translations:
	(cd src; make R-rattle.pot)
	(cd po; make updates; make all)

.PHONY: pbuild
pbuild: pmml_$(PVERSION).tar.gz  pmml_$(PVERSION).zip
	chmod go+r $(REPOSITORY)/*

ibuild: rstat_$(IVERSION).tar.gz rstat_$(IVERSION).zip 
	chmod go+r $(REPOSITORY)/*

########################################################################
# OLD

rattle_src.zip:
	cp $(R_SOURCE) zipsrc
	cp $(PSOURCE) zipsrc
	cp src/pmmltocibi.R src/all.R zipsrc
	cp $(GLADE_SOURCE) zipsrc
	zip -r9 rattle_src.zip zipsrc
	mv rattle_src.zip /var/www/access/
	chmod go+r /var/www/access/rattle_src.zip

########################################################################
# Create .tar.z and .zip packages for R and copy to REPOSITORY
#
# 100123 Updated the build process.

#-----------------------------------------------------------------------
# Rattle

rattle_$(VERSION).tar.gz: $(SOURCE) translations
	rm -f package/rattle/R/*
	perl -pi -e "s|^VERSION.DATE <- .*$$|VERSION.DATE <- \"Released $(VDATE)\"|" \
		src/rattle.R
	perl -pi -e "s|Revision: [0-9]*|Revision: $(SVNREVIS)|" \
		src/rattle.R
	perl -pi -e "s|^PACKAGEID <- \"11_.*$$|PACKAGEID <- \"11_$(IDATE)\"|" \
		src/rstat.R
	cp $(R_SOURCE) package/rattle/R/
	cp $(GLADE_SOURCE) package/rattle/inst/etc/
	cp ChangeLog INSTALL package/rattle/inst/
	cp rattle.Rnw package/rattle/inst/doc/
	cp odf/data_summary.odt package/rattle/inst/odt/
	perl -p -e "s|^Version: .*$$|Version: $(VERSION)|" < $(DESCRIPTIN) \
	| perl -p -e "s|^Date: .*$$|Date: $(DATE)|" > $(DESCRIPTION)
	chmod -R go+rX $(PACKAGE)
	R CMD build $(PACKAGE)
	R CMD INSTALL --library=$(RSITELIB) rattle_$(VERSION).tar.gz
	mv $@ $(REPOSITORY)

#-----------------------------------------------------------------------
# PMML
#
# 101203 Cleanup and test.

pmml_$(PVERSION).tar.gz: $(PSOURCE)
	cp $(PSOURCE) $(PRDEST)
	perl -pi -e "s|^Version: .*$$|Version: $(PVERSION)|" $(PDESCRIPTION)
	perl -pi -e "s|^Date: .*$$|Date: $(DATE)|" $(PDESCRIPTION)
	chmod -R go+rX $(PPACKAGE)
	R CMD build $(PPACKAGE)
	R CMD INSTALL --library=$(RSITELIBRARY) $@
	mv $@ $(REPOSITORY)

pmml_$(PVERSION).zip: pmml_$(PVERSION).tar.gz
	(cd $(RSITELIB); zip -r9 - rstat) > $@
	mv $@ $(REPOSITORY)

#-----------------------------------------------------------------------
# RStat

rstat_$(IVERSION).tar.gz: $(ISOURCE)
	cp $(ISOURCE) package/rstat/R/
	perl -pi -e "s|^Version: .*$$|Version: $(IVERSION)|" $(IDESCRIPTION)
	perl -pi -e "s|^Date: .*$$|Date: $(DATE)|" $(IDESCRIPTION)
	R CMD build $(IPACKAGE)
	chmod -R go+rX $(IPACKAGE)
	R CMD build $(IPACKAGE)
	R CMD INSTALL --library=/usr/local/lib/R/site-library rstat_$(IVERSION).tar.gz
	# mv rstat_$(IVERSION).tar.gz $(REPOSITORY)

rstat_$(IVERSION).zip: rstat_$(IVERSION).tar.gz
	(cd /usr/local/lib/R/site-library; zip -r9 - rstat) \
	>| rstat_$(VERSION).zip

R4X:
	R CMD build support/r4x/pkg/R4X

# 090201 Keep the weather data static - keeps changing my book details
# otherwise! 090326 Modify the R script to keep a static version and
# then regualrly updated versions.

.PHONY: data
data: package/rattle/data/audit.RData weather

package/rattle/data/audit.RData: support/audit.R src/audit.R Makefile
	R --no-save --quiet < support/audit.R
	chmod go+r audit.RData audit.csv audit.arff audit_missing.csv
	cp audit.RData audit.csv audit.arff audit_missing.csv data/
	cp audit.RData audit.csv audit.arff audit_missing.csv src/
	cp audit.RData package/rattle/data/
	cp audit.csv package/rattle/inst/csv/
	cp audit.arff package/rattle/inst/arff/
	cp audit.csv /home/gjw/Projects/Togaware/www/site/rattle/

.PHONY: weather
weather: package/rattle/data/weather.RData
package/rattle/data/weather.RData: support/weather.R src/weather.R Makefile
	R --no-save --quiet < support/weather.R
	chmod go+r weather*.RData weather.csv weather.arff weather_missing.csv
	cp weather.RData weather.csv weather.arff weather_missing.csv data/
	cp weather.RData weather.csv weather.arff weather_missing.csv src/
	cp locationsAUS.RData weather.RData weatherAUS.RData package/rattle/data/
	cp weather.csv package/rattle/inst/csv/
	cp weather.arff package/rattle/inst/arff/
	cp weather.csv /home/gjw/Projects/Togaware/www/site/rattle/
	cp weatherAUS.csv /home/gjw/Projects/Togaware/www/site/rattle/
	mv locationsAUS.RData weather*.RData archive
	mv weather.csv weather.arff weather_missing.csv archive

rattle_$(VERSION).zip: $(REPOSITORY)/rattle_$(VERSION).tar.gz
	(cd /usr/local/lib/R/site-library; zip -r9 - rattle) \
	>| rattle_$(VERSION).zip
	mv rattle_$(VERSION).zip $(REPOSITORY)

zip: build plocal ilocal
	(cd /usr/local/lib/R/site-library; zip -r9 - rattle) \
	>| rattle_$(VERSION).zip
	#mv rattle_$(VERSION).zip $(REPOSITORY)
	(cd /usr/local/lib/R/site-library; zip -r9 - pmml) \
	>| pmml_$(PVERSION).zip
	(cd /usr/local/lib/R/site-library; zip -r9 - rstat) \
	>| rstat_$(IVERSION).zip

txt:
	R CMD Rd2txt package/rattle/man/rattle.Rd

html:
	for m in rattle evaluateRisk genPlotTitleCmd plotRisk; do\
	  R CMD Rdconv -t=html -o=$$m.html package/rattle/man/$$m.Rd;\
	  epiphany -n $$m.html;\
	  rm -f $$m.html;\
	done

locals: clean translations local plocal ilocal zip

local: rattle_$(VERSION).tar.gz
	R CMD INSTALL --library=/usr/local/lib/R/site-library $^

plocal: pmml_$(PVERSION).tar.gz
	R CMD INSTALL --library=/usr/local/lib/R/site-library repository/$^

ilocal: rstat_$(IVERSION).tar.gz
	R CMD INSTALL --library=/usr/local/lib/R/site-library $^

access:
	grep 'rattle' /home/gjw/Projects/ilisys/log

python:
	python rattle.py

test: $(REPOSITORY)/rattle_$(VERSION).tar.gz
	LANGUAGE=ja r test.R
#	read -p "Press Enter to continue: "
#	LANGUAGE=es r test.R
#	read -p "Press Enter to continue: "
#	LANGUAGE=de r test.R
#	read -p "Press Enter to continue: "

#	R --no-save --quiet < regression.R 

ptest:
	r ptest.R

clean:
	rm -f rattle_*.tar.gz rattle_*.zip
	rm -f package/rattle/R/rattle.R package/rattle/inst/etc/rattle.glade
	rm -f package/rattle/inst/etc/rattle.ui
	rm -f package/rattle/DESCRIPTION
	rm -f pmml_*.tar.gz pmml_*.zip
	rm -f rstat_*.tar.gz rstat_*.zip

realclean: clean
	rm -f package/rattle/data/audit.RData package/rattle/inst/csv/audit.csv
	rm -f package/rattle/data/weather.RData package/rattle/inst/csv/weather.csv
	rm -f package/rattle/data/weatherCanberra.RData
	rm -f package/rattle/inst/csv/weatherCanberra.csv
	rm -f package/rattle/data/weatherSydney.RData
	rm -f package/rattle/data/weatherSydney.csv
	rm -rf rattle.Rcheck rattle_$(VERSION).tar.gz

.PHONY: backup
backup:
	rsync -a src "/home/gjw/Ubuntu One/dmsurvivor"

.Phony: ja
ja: build
	LANGUAGE=ja R CMD BATCH rattle_ja.R

.Phony: no
no: locals
	LANGUAGE=no R CMD BATCH rattle_ja.R

.Phony: cn
cn: locals
	LANGUAGE=zh_CN R CMD BATCH rattle_ja.R

.PHONY: rstat
rstat: locals
	R CMD BATCH rstat.R