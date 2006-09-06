## Regression test Rattle

testCNT <- 0; testYES <- 0
testIT <- function(code, target)
{
  testCNT <<- testCNT + 1
  if (all(code == target)) testYES <<- testYES + 1
  return(code == target)
}
  
source("src/rattle.R")
setwd("src"); rattle(); setwd("..")

VERSION

## DATA

load("audit.RData")

rattleWidget("rdataset_radiobutton")$setActive(TRUE)

rattleWidget("rdataset_combobox")$getModel()$clear()
rattleWidget("rdataset_combobox")$appendText("audit")
rattleWidget("rdataset_combobox")$setActive(0)

executeDataTab()

testIT(dim(crs$dataset), c(2000, 13))

testIT(sum(crs$dataset == audit, na.rm=TRUE), 25756) # NAs are ignored.

testIT(sum(is.na(crs$dataset))+sum(crs$dataset == audit, na.rm=TRUE), 26000)

sprintf("Tested: %d; Correct %d", testCNT, testYES)

quit()
