# See INSTRUCTIONS

PACKAGE=package/rattle
DESCRIPTION=$(PACKAGE)/DESCRIPTION
DESCRIPTIN=support/DESCRIPTION.in
NAMESPACE=$(PACKAGE)/NAMESPACE

PPACKAGE=package/pmml
PDESCRIPTION=$(PPACKAGE)/DESCRIPTION

IPACKAGE=package/rstat
IDESCRIPTION=$(IPACKAGE)/DESCRIPTION

RVER=$(shell R --version | head -1 | cut -d" " -f3 | sed 's|\..$||')
REPOSITORY=repository

# Canonical version information from rattle.R
MAJOR=$(shell egrep '^MAJOR' src/rattle.R | cut -d\" -f 2)
MINOR=$(shell egrep '^MINOR' src/rattle.R | cut -d\" -f 2)
SVNREVIS=$(shell svn info | egrep 'Revision:' |  cut -d" " -f 2)
REVISION=$(shell svn info | egrep 'Revision:' |  cut -d" " -f 2\
            | awk '{print $$1-480}')
VERSION=$(MAJOR).$(MINOR).$(REVISION)
VDATE=$(shell svn info |grep 'Last Changed Date'| cut -d"(" -f2 | sed 's|)||'\
	   | sed 's|^.*, ||')
IDATE=$(shell date +%m%d%y)

PVERSION=$(shell egrep ' VERSION <-' src/pmml.R | cut -d \" -f 2)

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

ISOURCE = \
	src/rstat.R \
	src/pmmltocibi.R \
	src/pmml.transforms.R

GLADE_SOURCE = src/rattle.glade src/tooltips.xml src/textviews.xml

SOURCE = $(R_SOURCE) $(GLADE_SOURCE) $(NAMESPACE)

#temp:
#	@echo  $(VDATE)
#temp:
#	@echo rattle_$(VERSION).tar.gz $(VERSION) $(REVISION) $(PVERSION)
#temp:
#	grep REVISION src/rattle.R

default: local plocal ilocal

.PHONY: ibirstat
ibirstat: zip
	mv $@??????????.zip archive
	-diff ibi/rstat.R src >| ibi/updates
	-diff ibi/pmml.transforms.R src >> ibi/updates
	-diff ibi/pmmltocibi.R src >> ibi/updates
	zip $@`date +%y%m%d%H%M`.zip \
	rattle_$(VERSION).zip pmml_$(PVERSION).zip rstat_$(IVERSION).zip \
	src/rstat.R src/pmml.transforms.R src/pmmltocibi.R ibi/updates
	cp src/rstat.R src/pmml.transforms.R src/pmmltocibi.R ibi/

.PHONY: jie
jie:
	tar zcvf jie`date +%y%m%d%H%M`.tar.gz \
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
	src/pmmltocibi.R \
	src/pmmltoc.R \
	src/pmml.transforms.R \
	src/pmml.coxph.R \
	src/pmml.survreg.R \
	src/projects.R \
	src/random_forest.R \
	src/rattle.R \
	src/report.R \
	src/rpart.R \
	src/rstat.R \
	src/survival.R \
	src/test.R \
	src/textminer.R \
	src/textview.R \
	src/tooltips.R \
	src/transform.R \
	src/utils.R \
	src/zzz.R \
	src/rattle.glade \
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

.PHONY: translations
translations:
	(cd package/rattle/po; make)

.PHONY: install
install: build pbuild ibuild zip rattle_src.zip # check pcheck
	perl -pi -e "s|version is [0-9\.]*\.|version is $(VERSION).|"\
			changes.html.in
	cp changes.html.in /home/gjw/Projects/Togaware/www/
	cp todo.html.in /home/gjw/Projects/Togaware/www/
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
	# 100123 Do I still want to keep these updated? lftp -f .lftp

# 100123 Updated the build process

check: build
	R CMD check $(PACKAGE)

pcheck: pbuild
	R CMD check $(PPACKAGE)

icheck: ibuild
	R CMD check $(IPACKAGE)

ucheck: build
	sh ./upload_uwe.sh

cran: build
	sh ./upload_cran.sh

# For development, temporarily remove the NAMESPACE so all is exposed.

devbuild:
	mv package/rattle/NAMESPACE .
	cp rattle.R package/rattle/R/
	cp rattle.glade package/rattle/inst/etc/
	R CMD build package/rattle
	mv NAMESPACE package/rattle/

# 100123 Updated the build process

.PHONY: build
build: $(REPOSITORY)/rattle_$(VERSION).tar.gz $(REPOSITORY)/rattle_$(VERSION).zip

pbuild: data pmml_$(PVERSION).tar.gz

ibuild: data rstat_$(IVERSION).tar.gz

rattle_src.zip:
	cp $(R_SOURCE) zipsrc
	cp $(PSOURCE) zipsrc
	cp src/pmmltocibi.R src/all.R zipsrc
	cp $(GLADE_SOURCE) zipsrc
	zip -r9 rattle_src.zip zipsrc
	mv rattle_src.zip /var/www/access/
	chmod go+r /var/www/access/rattle_src.zip

# 100123 Updated the build process.

$(REPOSITORY)/rattle_$(VERSION).tar.gz: $(SOURCE) weather
	rm -f package/rattle/R/*
	perl -pi -e "s|^VERSION.DATE <- .*$$|VERSION.DATE <- \"Released $(VDATE)\"|" \
		src/rattle.R
	perl -pi -e "s|Revision: [0-9]*|Revision: $(SVNREVIS)|" \
		src/rattle.R
	perl -pi -e "s|^PACKAGEID <- \"11_.*$$|PACKAGEID <- \"11_$(IDATE)\"|" \
		src/rstat.R
	cp $(R_SOURCE) package/rattle/R/
	cp $(GLADE_SOURCE) package/rattle/inst/etc/
	cp ChangeLog NEWS INSTALL package/rattle/inst/
	cp odf/data_summary.odt package/rattle/inst/odt/
	perl -p -e "s|^Version: .*$$|Version: $(VERSION)|" < $(DESCRIPTIN) \
	| perl -p -e "s|^Date: .*$$|Date: $(DATE)|" > $(DESCRIPTION)
	chmod -R go+rX $(PACKAGE)
	R CMD build $(PACKAGE)
	R CMD INSTALL --library=/usr/local/lib/R/site-library rattle_$(VERSION).tar.gz
	mv rattle_$(VERSION).tar.gz $(REPOSITORY)

pmml_$(PVERSION).tar.gz: $(PSOURCE)
	cp $(PSOURCE) package/pmml/R/
	perl -pi -e "s|^Version: .*$$|Version: $(PVERSION)|" $(PDESCRIPTION)
	perl -pi -e "s|^Date: .*$$|Date: $(DATE)|" $(PDESCRIPTION)
	R CMD build $(PPACKAGE)
	chmod -R go+rX $(PPACKAGE)
	mv pmml_$(PVERSION).tar.gz $(REPOSITORY)
	#mv pmml_$(PVERSION).zip $(REPOSITORY)

rstat_$(IVERSION).tar.gz: $(ISOURCE)
	cp $(ISOURCE) package/rstat/R/
	perl -pi -e "s|^Version: .*$$|Version: $(IVERSION)|" $(IDESCRIPTION)
	perl -pi -e "s|^Date: .*$$|Date: $(DATE)|" $(IDESCRIPTION)
	R CMD build $(IPACKAGE)
	chmod -R go+rX $(IPACKAGE)

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
package/rattle/data/weather.RData: support/weather.R src/weather.R Makefile weather
	R --no-save --quiet < support/weather.R
	chmod go+r weather*.RData weather.csv weather.arff weather_missing.csv
	cp weather.RData weather.csv weather.arff weather_missing.csv data/
	cp weather.RData weather.csv weather.arff weather_missing.csv src/
	cp weather.RData weatherAUS.RData package/rattle/data/
	cp weather.csv package/rattle/inst/csv/
	cp weather.arff package/rattle/inst/arff/
	cp weather.csv /home/gjw/Projects/Togaware/www/site/rattle/
	mv weather*.RData archive
	mv weather.csv weather.arff weather_missing.csv archive

$(REPOSITORY)/rattle_$(VERSION).zip: $(REPOSITORY)/rattle_$(VERSION).tar.gz
	(cd /usr/local/lib/R/site-library; zip -r9 - rattle) \
	>| rattle_$(VERSION).zip
	mv rattle_$(VERSION).zip $(REPOSITORY)

zip: build plocal ilocal
	(cd /usr/local/lib/R/site-library; zip -r9 - rattle) \
	>| rattle_$(VERSION).zip
	#mv rattle_$(VERSION).zip $(REPOSITORY)
	#(cd /usr/local/lib/R/site-library; zip -r9 - pmml) \
	#>| pmml_$(PVERSION).zip
	#(cd /usr/local/lib/R/site-library; zip -r9 - rstat) \
	#>| rstat_$(IVERSION).zip

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

test:
	R --no-save --quiet < regression.R 

ptest:
	r ptest.R

clean:
	rm -f rattle_*.tar.gz rattle_*.zip
	rm -f package/rattle/R/rattle.R package/rattle/inst/etc/rattle.glade
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
ja: locals
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