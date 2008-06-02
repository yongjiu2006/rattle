# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2008-06-01 21:41:42 Graham Williams>
#
# MODEL TAB
#
# Copyright (c) 2008 Togaware Pty Ltd
#
# This file is part of Rattle.
#
# Rattle is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# Rattle is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Rattle. If not, see <http://www.gnu.org/licenses/>.

########################################################################
# CALLBACKS

# When radio button is selected, display appropriate tab page

on_regression_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    crv$MODEL$setCurrentPage(crv$MODEL.GLM.TAB)
    setTextview("confusion_textview")
  }
  setStatusBar()
}

on_dtree_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    crv$MODEL$setCurrentPage(crv$MODEL.RPART.TAB)
    setTextview("confusion_textview")
  }
  setStatusBar()
}

on_boost_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    ## crv$MODEL$setCurrentPage(crv$MODEL.GBM.TAB)
    crv$MODEL$setCurrentPage(crv$MODEL.ADA.TAB)
    setTextview("confusion_textview")
  }
  setStatusBar()
}

on_nnet_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    crv$MODEL$setCurrentPage(crv$MODEL.NNET.TAB)
    setTextview("confusion_textview")
  }
  setStatusBar()
}

on_rf_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    crv$MODEL$setCurrentPage(crv$MODEL.RF.TAB)
    setTextview("confusion_textview")
  }
  setStatusBar()
}

on_svm_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    crv$MODEL$setCurrentPage(crv$MODEL.SVM.TAB)
    setTextview("confusion_textview")
  }
  setStatusBar()
}

on_e1071_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    .SVMNB$setCurrentPage(.SVMNB.ESVM.TAB)
  }
  setStatusBar()
}

on_kernlab_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    .SVMNB$setCurrentPage(.SVMNB.KSVM.TAB)
  }
  setStatusBar()
}

########################################################################
# UTILITIES

commonName <- function(mtype)
{
  name.map <- data.frame(rpart="Tree",
                         ada="Boost",
                         rf="Forest",
                         ksvm="SVM",
                         glm="Linear",
                         multinom="Neural Net",
                         nnet="Neural Net")
  return(as.character(name.map[[mtype]]))
}

numericTarget <- function()
{
  if (length(getSelectedVariables("target")) == 0)
    return(FALSE)
  else
    return(theWidget("target_numeric_radiobutton")$getActive())
}

categoricTarget <- function()
{
  if (length(getSelectedVariables("target")) == 0)
    return(FALSE)
  else
    return(theWidget("target_categoric_radiobutton")$getActive())
}

binomialTarget <- function()
{
  return(categoricTarget() &&
         length(levels(as.factor(crs$dataset[[crs$target]]))) == 2)
}

multinomialTarget <- function()
{
  return(categoricTarget() &&
         length(levels(as.factor(crs$dataset[[crs$target]]))) > 2)
}

currentModelTab <- function()
{
  lb <- getCurrentPageLabel(crv$MODEL)
  if (lb == .SVM && theWidget("kernlab_radiobutton")$getActive()) lb <- .KSVM
  return(lb)
}

deactivateROCRPlots <- function()
{
  theWidget("lift_radiobutton")$setSensitive(FALSE)
  theWidget("roc_radiobutton")$setSensitive(FALSE)
  theWidget("precision_radiobutton")$setSensitive(FALSE)
  theWidget("sensitivity_radiobutton")$setSensitive(FALSE)
  theWidget("risk_radiobutton")$setSensitive(FALSE)

  if (numericTarget())
    theWidget("confusion_radiobutton")$setSensitive(FALSE)
}

activateROCRPlots <- function()
{
  theWidget("confusion_radiobutton")$setSensitive(TRUE)
  theWidget("lift_radiobutton")$setSensitive(TRUE)
  theWidget("roc_radiobutton")$setSensitive(TRUE)
  theWidget("precision_radiobutton")$setSensitive(TRUE)
  theWidget("sensitivity_radiobutton")$setSensitive(TRUE)
  theWidget("risk_radiobutton")$setSensitive(TRUE)
}

########################################################################
#
# EXECUTE MODEL TAB
#

executeModelTab <- function()
{
  ## Check for prerequisites.

  # Can not build a model without a dataset.

  if (noDatasetLoaded()) return()

  # If VARIABLES has some ignores but crs$ignore is NULL, complain.

  if (variablesHaveChanged("building a model")) return()

  # If the WeightCalculator has changed but it is not the same as
  # crs$weight, complain. This doesn't work any more since we add
  # crs$dataset to the variable names in the Weights Calculator, so
  # they are different! But, let's remove the crs$dataset and compare.

  weights.display <- gsub('crs\\$dataset\\$', '', crs$weights)

  if (not.null(crs$weights)
      && weights.display != theWidget("weight_entry")$getText())
  {
    errorDialog("You appear to have changed the formula for calculating the",
                "weights on the Data tab without executing the tab.",
                "The previous formula",
                sprintf('was "%s" and it is now "%s".', crs$weights,
                        theWidget("weight_entry")$getText()),
                "Please be sure to execute the Data tab",
                "before continuing.")
    return()
  }
    
  # Retrieve the target and make sure there is one.

  if (length(crs$target) == 0)
  {
    errorDialog("No target has been specified.",
                "Please identify the target using the Data tab.",
                "Be sure to Execute the tab once the target has",
                "been identified.")
    return()
  }

  # Check if sampling needs executing.

  if (sampleNeedsExecute()) return()
    
  # If the target is a categorical and has more than 2 levels then
  # disable the ROCR and Risk plots, and place a message on the first
  # textview of the Evaluate tab. We make this word wrap here and then
  # turn that off once the tab is Executed.

##  paradigm <- getParadigm()
  
  if (multinomialTarget())
  {
    deactivateROCRPlots()
    theWidget("confusion_textview")$setWrapMode("word")
    resetTextview("confusion_textview")
    appendTextview("confusion_textview",
                   "Note that the target you have chosen has more than",
                   "2 classes. Some functionality on the Evaluate tab",
                   "will not be available. In particular, the ROCR",
                   "package (Lift, ROC, Precision, and Sensitivity",
                   "charts) and the Risk Chart only handle binary",
                   "classification.", sep=" ")
  }
  else if (numericTarget())
  {
    deactivateROCRPlots()
    setTextview("confusion_textview") # Clear any confusion table
  }
  else
  {
    activateROCRPlots()
    setTextview("confusion_textview") # Clear any confusion table
  }

  # DISPATCH

  build.all <- theWidget("all_models_radiobutton")$getActive()

  # Reset all Evaluate options to unchecked.
  
  theWidget("rpart_evaluate_checkbutton")$setActive(FALSE)
  theWidget("ada_evaluate_checkbutton")$setActive(FALSE)
  theWidget("rf_evaluate_checkbutton")$setActive(FALSE)
  theWidget("ksvm_evaluate_checkbutton")$setActive(FALSE)
  theWidget("glm_evaluate_checkbutton")$setActive(FALSE)
  theWidget("nnet_evaluate_checkbutton")$setActive(FALSE)
  
  ## The following work for ada, do they work for the rest?
  formula <- paste(crs$target, "~ .")
  included <- getIncludedVariables()
  sampling <- not.null(crs$sample)
  including <- not.null(included)
  subsetting <- sampling || including
  dataset <- paste("crs$dataset",
                   if (subsetting) "[",
                   if (sampling) "crs$sample",
                   if (subsetting) ",",
                   if (including) included,
                   if (subsetting) "]",
                   sep="")

  
  ## This order of execution should correspond to the order in the
  ## GUI as this makes most logical sense to the user.

  start.time <- Sys.time()

  if (build.all || currentModelTab() == .RPART)
  {
    if (theWidget("rpart_build_radiobutton")$getActive())
    {
      setStatusBar("Building", .RPART, "model ...")
      if (executeModelRPart())
        theWidget("rpart_evaluate_checkbutton")$setActive(TRUE)
      else
        setStatusBar("Building", .RPART, "model ... failed.")
    }
    else if (theWidget("rpart_tune_radiobutton")$getActive())
    {
      setStatusBar("Tuning", .RPART, "model ...")
      if (! executeModelRPart("tune"))
        setStatusBar("Tuning", .RPART, "model ... failed.")
    }
    else if (theWidget("rpart_best_radiobutton")$getActive())
    {
      setStatusBar("Building best", .RPART, "model ...")
      if (! executeModelRPart("best"))
        setStatusBar("Building best", .RPART, "model ... failed.")
    }
    else # That's all the radio buttons - we should not be here.
    {
      errorDialog("Tried building an rpart model with option not",
                  "one of build/tune/best. This should not be possible.",
                  "Let support@togaware.com know.")
      return(FALSE)
      
    }
  }
  if ((binomialTarget() && build.all)
      || currentModelTab() == .ADA)
  {
    setStatusBar("Building", .ADA, "model ...")
    crs$ada <<-
      buildModelAda(formula,
                    dataset,
                    tv=theWidget("ada_textview"),
                    maxdepth=theWidget("ada_maxdepth_spinbutton")$getValue(),
                    minsplit=theWidget("ada_minsplit_spinbutton")$getValue(),
                    cp=theWidget("ada_cp_spinbutton")$getValue(),
                    xval=theWidget("ada_xval_spinbutton")$getValue(),
                    ntree=theWidget("ada_ntree_spinbutton")$getValue())
    if (not.null(crs$ada))
    {
      showModelAdaExists()
      theWidget("ada_evaluate_checkbutton")$setActive(TRUE)
    }
    else
      setStatusBar("Building", .ADA, "model ... failed.")

  }
  if ((categoricTarget() && build.all)
      || currentModelTab() == .RF)
  {
    setStatusBar("Building", .RF, "model ...")
    if (executeModelRF())
      theWidget("rf_evaluate_checkbutton")$setActive(TRUE)
    else
      setStatusBar("Building", .RF, "model ... failed.")
  }
  if ((categoricTarget() && build.all)
      || currentModelTab() %in% c(.SVM, .KSVM))
  {
    setStatusBar("Building", .KSVM, "model ...")
    if (executeModelSVM())
      theWidget("ksvm_evaluate_checkbutton")$setActive(TRUE)
    else
      setStatusBar("Building", .KSVM, "model ... failed.")

  }
  if (build.all || currentModelTab() == crv$GLM)
  {
    setStatusBar("Building", crv$GLM, "model ...")
    if (executeModelGLM())
      theWidget("glm_evaluate_checkbutton")$setActive(TRUE)
    else
      setStatusBar("Building", crv$GLM, "model ... failed.")
  }
  if (build.all || currentModelTab() == crv$NNET)
  {
    setStatusBar("Building", crv$NNET, "model ...")
    if (executeModelNNet())
      theWidget("nnet_evaluate_checkbutton")$setActive(TRUE)
    else
      setStatusBar("Building", crv$NNET, "model ... failed.")
  }
  
  if (build.all)
  {
    time.taken <- Sys.time()-start.time
    time.msg <- sprintf("Time taken: %0.2f %s", time.taken,
                        attr(time.taken, "units"))
    setStatusBar("All models have been generated.", time.msg)
  }
  
}

#----------------------------------------------------------------------
#
# MODEL GLM
#

executeModelGLM <- function()
{
  # Initial setup. 
  
  TV <- "glm_textview"

##  paradigm <- getParadigm()
  
  # Currently only handling binary classification.
  
##   num.classes <- length(levels(as.factor(crs$dataset[[crs$target]])))
##   if (num.classes > 2)
##   {
##     errorDialog("Currently Rattle only handles logistic regression for",
##                  "binary classification.",
##                  sprintf("The %s dataset has %d classes.",
##                          crs$dataname, num.classes))
##     return(FALSE)
##   }

  ## Obtain the family

  family <- theWidget("glm_family_comboboxentry")$getActiveText()
  
  # Build the formula for the model.

  frml <- paste(crs$target, "~ .")

  # List, as a string, the variables to be included. 
  
  included <- getIncludedVariables()
  
  # Some convenience booleans.

  sampling  <- not.null(crs$sample)
  including <- not.null(included)
  subsetting <- sampling || including
  
  # Assume logistic regression for binary classification for now.

  if (categoricTarget())
    glm.cmd <- paste("crs$glm <<- glm(", frml, ", data=crs$dataset",
                     if (subsetting) "[",
                     if (sampling) "crs$sample",
                     if (subsetting) ",",
                     if (including) included,
                     if (subsetting) "]",
                     ", family=", family,
                     ")", sep="")
  else if (numericTarget())
    glm.cmd <- paste("crs$glm <<- lm(", frml, ", data=crs$dataset",
                     if (subsetting) "[",
                     if (sampling) "crs$sample",
                     if (subsetting) ",",
                     if (including) included,
                     if (subsetting) "]",
                     #", family=", family,
                     ")", sep="")
  
  summary.cmd <- paste("print(summary(crs$glm))",
                       "cat('==== ANOVA ====\n\n')",
                       "print(anova(crs$glm))", sep="\n")
  
  # Build the model.

  startLog("REGRESSION")
  appendLog("Build a regression model using lm.",
            gsub("<<-", "<-", glm.cmd), sep="")
  start.time <- Sys.time()
  eval(parse(text=glm.cmd))
  
  # Summarise the model.

  appendLog("Summary of the resulting LM model", summary.cmd)
  
  resetTextview(TV)
  setTextview(TV, sprintf(paste("Summary of the %s model",
                                "(built using %s):\n"),
                          commonName("glm"),
                          ifelse(categoricTarget(),
                                 "glm", "lm")),
              collectOutput(summary.cmd))

  if (sampling) crs$smodel <<- union(crs$smodel, crv$GLM)
  
  ## Finish up.
  
  time.taken <- Sys.time()-start.time
  time.msg <- sprintf("Time taken: %0.2f %s", time.taken,
                      attr(time.taken, "units"))
  addTextview(TV, "\n", time.msg, textviewSeparator())
  appendLog(time.msg)
  setStatusBar("A lm model has been generated.", time.msg)
  return(TRUE)
}

exportRegressionTab <- function()
{
  # Make sure we have a model first!
  
  if (is.null(crs$glm))
  {
    errorDialog("No regression model is available. Be sure to build",
                "the model before trying to export it! You will need",
                "to press the Execute button (F5) in order to build the",
                "model.")
    return()
  }

  # Require the pmml package
  
  lib.cmd <- "require(pmml, quietly=TRUE)"
  if (! packageIsAvailable("pmml", "export regression model")) return(FALSE)
  appendLog("Load the PMML package to export a regression model.", lib.cmd)
  eval(parse(text=lib.cmd))
  
  # Obtain filename to write the PMML to.
  
  dialog <- gtkFileChooserDialog("Export PMML", NULL, "save",
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-save", GtkResponseType["accept"])

  if(not.null(crs$dataname))
    dialog$setCurrentName(paste(get.stem(crs$dataname), "_glm", sep=""))

  ff <- gtkFileFilterNew()
  ff$setName("PMML Files")
  ff$addPattern("*.xml")
  dialog$addFilter(ff)

  ff <- gtkFileFilterNew()
  ff$setName("All Files")
  ff$addPattern("*")
  dialog$addFilter(ff)
  
  if (dialog$run() == GtkResponseType["accept"])
  {
    save.name <- dialog$getFilename()
    dialog$destroy()
  }
  else
  {
    dialog$destroy()
    return()
  }

  if (get.extension(save.name) == "") save.name <- sprintf("%s.xml", save.name)
    
  if (file.exists(save.name))
    if (is.null(questionDialog("An XML file of the name", save.name,
                                "already exists. Do you want to overwrite",
                                "this file?")))
      return()
  

  pmml.cmd <- "pmml(crs$glm)"
  appendLog("Export a regression model as PMML.", pmml.cmd)
  saveXML(eval(parse(text=pmml.cmd)), save.name)

  # Be less chatty infoDialog("The PMML file", save.name, "has been written.")

  setStatusBar("The PMML file", save.name, "has been written.")
  
}

#------------------------------------------------------------------------
# MODEL SVM - SUPPORT VECTOR MACHINE
#
# Eventually the following will go into svm_gui.R, using the same
# conventions as in ada.R.

on_svm_kernel_comboboxentry_changed <- function(action, window)
{
  krnl <- theWidget("svm_kernel_comboboxentry")$getActiveText()
  krnl <- gsub(").*$", "", gsub("^.*\\(", "",  krnl))
  setGuiDefaultsSVM(krnl)
}

setGuiDefaultsSVM <- function(kernel=NULL)
{
  if (is.null(kernel))
  {
    theWidget("svm_kernel_comboboxentry")$setActive(0)
    theWidget("svm_classweights_entry")$setText("")
    theWidget("svm_poly_degree_spinbutton")$setValue(1)
  }
  else
  {
    if (kernel=="polydot")
    {
      theWidget("svm_poly_degree_label")$show()
      theWidget("svm_poly_degree_spinbutton")$show()
    }
    else
    {
      theWidget("svm_poly_degree_label")$hide()
      theWidget("svm_poly_degree_spinbutton")$hide()
    }
  }
}

## Eventually the following will go into svm.R, using the same
## conventions as in ada.R.

executeModelSVM <- function()
{
  ## DESCRIPTION
  ## Build a support vector machine predictor.
  ##
  ## RETURNS
  ## Ignored.
  ##
  ## DETAILS There are two model builders for SVMs: The e1071 version
  ## is older and is supported by tune, and the kernlab version is
  ## much more extensive. I did move back to e1071 because I thought
  ## issues around the handling of NAs in kernlab a problem, but
  ## essentially I think it is an issue with svm using all variables,
  ## so I had to clean up my handling of NAs.
  
  useKernlab <- theWidget("kernlab_radiobutton")$getActive()

  TV <- ifelse(useKernlab, "ksvm_textview", "esvm_textview")
  
  startLog("SUPPORT VECTOR MACHINE")

  ## Library.

  if (useKernlab)
  {
    if (packageIsAvailable("kernlab", "build an SVM model using ksvm"))
    {
      libCmd <- "require(kernlab, quietly=TRUE)"
      appendLog("The kernlab package supplies the ksvm function.", libCmd)
    }
    else
      return(FALSE)
  }
  else
  {
    if (packageIsAvailable("e1071", "build an SVM model using svm"))
    {
      libCmd <- "require(e1071, quietly=TRUE)"
      appendLog("The e1071 package supplies the svm function.", libCmd)
    }
    else
      return(FALSE)
   }
  eval(parse(text=libCmd))

  ## Formula. TODO For kernlab we assume we will always do
  ## classification rather than regression, at least for now.

  if (useKernlab)
    frml <- paste(ifelse(numericTarget(),
                         crs$target,
                         sprintf("as.factor(%s)", crs$target)),
                  "~ .")
  else
    frml <- paste(crs$target, "~ .")

  ## Interface options.

  krnl <- theWidget("svm_kernel_comboboxentry")$getActiveText()
  krnl <- gsub(").*$", "", gsub("^.*\\(", "",  krnl))

  if (krnl == "polydot")
    degree <- theWidget("svm_poly_degree_spinbutton")$getValue()
  
  cweights <- theWidget("svm_classweights_entry")$getText()
  
  ## Included variables.

  included <- getIncludedVariables()
  
  ## Convenience booleans.

  sampling   <- not.null(crs$sample)
  including  <- not.null(included)
  subsetting <- sampling || including

  ## Parameters.

  parms <- ""
  if (krnl != "")
    parms <- sprintf('%s, kernel="%s"', parms, krnl)
  if (cweights != "")
    parms <- sprintf('%s, class.weights=%s', parms, cweights)
  if (krnl == "polydot")
    parms <- sprintf('%s, kpar=list("degree"=%s)', parms, degree)
  
  ## Build the model.

  if (useKernlab)
    svmCmd <- paste("crs$ksvm <<- ksvm(", frml, ", data=crs$dataset", sep="")
  else
    svmCmd <- paste("crs$svm <<- svm(", frml, ", data=crs$dataset", sep="")
  svmCmd <- paste(svmCmd,
                   if (subsetting) "[",
                   if (sampling) "crs$sample",
                   if (subsetting) ",",
                   if (including) included,
                   if (subsetting) "]",
                   parms, sep="")
  if (useKernlab)
    svmCmd <- paste(svmCmd, ", prob.model=TRUE", sep="")  # Probabilities
  else
    svmCmd <- paste(svmCmd, ", probability=TRUE", sep="")  # Probabilities
  svmCmd <- paste(svmCmd, ")", sep="")
  start.time <- Sys.time()
  appendLog("Build a support vector machine model.", gsub("<<-", "<-", svmCmd))
  result <- try(eval(parse(text=svmCmd)), silent=TRUE)
  if (inherits(result, "try-error"))
  {
    if (any(grep("cannot allocate vector", result)))
    {
      errorDialog("The call to svm appears to have failed.",
                  "This is often due, as in this case,",
                  "to running out of memory",
                  "as svm is rather memory hungry.",
                  "A quick solution is to sample the dataset, through the",
                  "Transform tab. On 32 bit machines you may be limited to",
                   "less than 10000 entities.")
      setTextview(TV)
    }
    else
      errorDialog("The call to svm appears to have failed.",
                  "The error message was:", result,
                  "I am not familiar with this error, and you may",
                  "want to report it to",
                  "at support@togaware.com")
    return(FALSE)
  }

  ## Display the resulting model.

  if (useKernlab)
    summaryCmd <- "crs$ksvm"
  else
    summaryCmd <- "crs$svm"
  appendLog("Generate textual output of the svm model.", summaryCmd)
  resetTextview(TV)
  setTextview(TV, sprintf(paste("Summary of the %s model",
                                "(built using ksvm):\n\n"),
                          commonName("ksvm")),
              collectOutput(summaryCmd, TRUE), "\n")

  if (sampling)
    if (useKernlab)
      crs$smodel <<- union(crs$smodel, .KSVM)
    else
      crs$smodel <<- union(crs$smodel, .SVM)

  ## Finish up.

  time.taken <- Sys.time()-start.time
  time.msg <- sprintf("Time taken: %0.2f %s", time.taken,
                      attr(time.taken, "units"))
  addTextview(TV, "\n", time.msg, textviewSeparator())
  appendLog(time.msg)
  setStatusBar(sprintf("A %s model has been generated.",
                       ifelse(useKernlab, .KSVM, .SVM)), time.msg)
  return(TRUE)
}

exportSVMTab <- function()
{
  # Make sure we have a model first!
  
  if (is.null(crs$ksvm))
  {
    errorDialog("No SVM model is available. Be sure to build",
                "the model before trying to export it! You will need",
                "to press the Execute button (F5) in order to build the",
                "model.")
    return()
  }

  # Require the pmml package
  
  lib.cmd <- "require(pmml, quietly=TRUE)"
  if (! packageIsAvailable("pmml", "export SVM model")) return(FALSE)
  appendLog("Load the PMML package to export a SVM model.", lib.cmd)
  eval(parse(text=lib.cmd))
  
  # Obtain filename to write the PMML to.
  
  dialog <- gtkFileChooserDialog("Export PMML", NULL, "save",
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-save", GtkResponseType["accept"])

  if(not.null(crs$dataname))
    dialog$setCurrentName(paste(get.stem(crs$dataname), "_ksvm", sep=""))

  ff <- gtkFileFilterNew()
  ff$setName("PMML Files")
  ff$addPattern("*.xml")
  dialog$addFilter(ff)

  ff <- gtkFileFilterNew()
  ff$setName("All Files")
  ff$addPattern("*")
  dialog$addFilter(ff)
  
  if (dialog$run() == GtkResponseType["accept"])
  {
    save.name <- dialog$getFilename()
    dialog$destroy()
  }
  else
  {
    dialog$destroy()
    return()
  }

  if (get.extension(save.name) == "") save.name <- sprintf("%s.xml", save.name)
    
  if (file.exists(save.name))
    if (is.null(questionDialog("An XML file of the name", save.name,
                                "already exists. Do you want to overwrite",
                                "this file?")))
      return()
  

  pmml.cmd <- 'pmml(crs$ksvm, data.name=crs$dataset)'
  appendLog("Export a SVM model as PMML.", pmml.cmd)
  saveXML(eval(parse(text=pmml.cmd)), save.name)

  # Be less chatty infoDialog("The PMML file", save.name, "has been written.")

  setStatusBar("The PMML file", save.name, "has been written.")
  
}

##----------------------------------------------------------------------
##
## MARS
##
## y <- pchresp[, c(1)]
## x <- pchresp[, -c(1)]
##
## m1 <- mars(x, y)
##
## showcuts <- function(obj)
## {
##   tmp <- obj$cuts[obj$sel, ]
##   dimnames(tmp) <- list(NULL, dimnames(x)[[2]])
##   tmp
## }
##
## m2 <- mars(x, y, degree=2)

##----------------------------------------------------------------------
##
## SVM
##
## > >
## > > I am using the "svm" command in the e1071 package.
## > >
## > > Does it have an automatic way of setting the "cost" parameter?
## >
## > See ?best.svm in that package.
## >
## > > I changed a few values for the "cost" parameter but I hope there is a
## > > systematic way of obtaining the best "cost" value.
## > >
## > > I noticed that there is a "cross" (Cross validation)
## > > parameter in the "svm"
## > > function.
## > >
## > > But I did not see how it can be used to optimize the "cost" parameter.
## > >
## > > By the way, what does a 0 training error and a high testing
## > > error mean?
## > > Varying "cross=5", or "cross=10", etc. does not change the
## > > training error
## > > and testing error at all. How to improve?
## >
## > Overfitting, which varying different validation method will not solve.


## You might find http://www.csie.ntu.edu.tw/~cjlin/papers/guide/guide.pdf
## <http://www.csie.ntu.edu.tw/~cjlin/papers/guide/guide.pdf>  helpful.

## Parameter tuning is essential for avoiding overfitting.


########################################################################
#
# EXPORT
#

exportModelTab <- function()
{

  if (noDatasetLoaded()) return()

  if (theWidget("rpart_radiobutton")$getActive())
  {
    exportRpartTab()
  }
  else if (theWidget("regression_radiobutton")$getActive())
  {
    exportRegressionTab()
  }
  else if (theWidget("svm_radiobutton")$getActive())
  {
    exportSVMTab()
  }
  else if (theWidget("nnet_radiobutton")$getActive())
  {
    exportNNetTab()
  }
  else
  {
    errorDialog("PMML export for this model is not yet implemented.")
    return()
  }
}

