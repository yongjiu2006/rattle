#options(error=recover) # Any errors will invoke recover() for debugging
#options(error=browser) # Any errors will invoke browser() for debugging
#options(error=NULL) # Turn it off
#options(warn=2) # For development treat any warnings as errors

source("rattle.R")
source("zzz.R")
source("associate.R")
source("cluster.R")
source("evaluate.R")
source("execute.R")
source("explore.R")
source("export.R")
source("hclust.R")
source("kmeans.R")
source("ada.R")
source("ada_gui.R")
source("nnet.R")
source("log.R")
source("model.R")
source("projects.R")
source("rpart.R")
source("ctree.R")
source("random_forest.R")
source("textview.R")
source("data.R")
source("report.R")
source("test.R")
source("textminer.R")
source("tooltips.R")
source("transform.R")
source("help.R")
source("survival.R")
source("biclust.R")
source("clara.R")
source("ewkm.R")

source("pmml.R")
source("pmml.arules.R")
source("pmml.kmeans.R")
source("pmml.hclust.R")
source("pmml.ksvm.R")
source("pmml.lm.R")
source("pmml.multinom.R")
source("pmml.nnet.R")
source("pmmltoc.R")
source("pmml.randomForest.R")
source("pmml.rpart.R")
source("pmml.rsf.R")
source("pmml.coxph.R") # 091015
source("pmml.survreg.R") # 091123

if (! exists("crv"))
{
  .onLoad()
#110202 require(RGtk2)
  .onAttach()
}

#source("rstat.R")
library(XML)
source("pmml.transforms.R")
source("pmmltocibi.R")

crv$export.to.c.available <- TRUE
