## Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2006-11-07 22:29:55 Graham Williams>
##
## MODEL TAB
##
## Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2

########################################################################
##
## CALLBACKS
##

## When radio button is selected, display appropriate tab page

on_regression_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    MODEL$setCurrentPage(MODEL.GLM.TAB)
    setTextview("confusion_textview")
  }
  setStatusBar()
}

on_dtree_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    MODEL$setCurrentPage(MODEL.RPART.TAB)
    setTextview("confusion_textview")
  }
  setStatusBar()
}

on_boost_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    MODEL$setCurrentPage(MODEL.GBM.TAB)
    setTextview("confusion_textview")
  }
  setStatusBar()
}

on_rf_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    MODEL$setCurrentPage(MODEL.RF.TAB)
    setTextview("confusion_textview")
  }
  setStatusBar()
}

on_svm_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    MODEL$setCurrentPage(MODEL.SVM.TAB)
    setTextview("confusion_textview")
  }
  setStatusBar()
}

on_e1071_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    SVMNB$setCurrentPage(SVMNB.ESVM.TAB)
  }
  setStatusBar()
}

on_kernlab_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    SVMNB$setCurrentPage(SVMNB.KSVM.TAB)
  }
  setStatusBar()
}

########################################################################
##
## GENERAL SUPPORT
##

currentModelTab <- function()
{
  lb <- getCurrentPageLabel(MODEL)
  if (lb == SVM && rattleWidget("kernlab_radiobutton")$getActive())
    lb <- KSVM
  return(lb)
}

########################################################################
##
## EXECUTION
##

executeModelTab <- function()
{
  ## Can not build a model without a dataset.

  if (noDatasetLoaded()) return()

  ## If VARIABLES has some ignores but crs$ignore is NULL, complain.

  if (variablesHaveChanged("building a model")) return()

  ## If WeightCalculator has changed but not same as crs$weight,
  ## complain. This doesn't work any more since we add crs$dataset to
  ## the variable names in the Weights Calculator, so they are
  ## different! But, let's remove the crs$dataset and compare.

  weights.display <- gsub('crs\\$dataset\\$', '', crs$weights)

  if (! is.null(crs$weights)
      && weights.display != rattleWidget("weight_entry")$getText())
  {
    errorDialog("You appear to have changed the formula for calculating the",
                 "weights on the Variables tab, but have not executed the tab.",
                 "The previous formula",
                 sprintf('was "%s" and is now "%s".', crs$weights,
                         rattleWidget("weight_entry")$getText()),
                 "Please be sure to execute the tab before continuing.")
    return()
  }
    
  ## Retrieve the target and make sure there is one.

  if (length(crs$target) == 0)
  {
    errorDialog("No target has been specified.",
                 "Please identify the target using the Variables tab.",
                 "Be sure to Execute the tab once the target has",
                 "been identified.")
    return()
  }

  ## Check if sampling needs executing.

  if (sampleNeedsExecute()) return()
    
  ## If the target has more than 2 levels, disable the ROCR and Risk
  ## plots, and place a message on the first textview of the Evaluate
  ## tab. We make this word wrap here and then turn that off once the
  ## tab is Executed.
  
  if (length(levels(as.factor(crs$dataset[[crs$target]]))) > 2)
  {
    deactivate.rocr.plots()
    rattleWidget("confusion_textview")$setWrapMode("word")
    clearTextview("confusion_textview")
    appendTextview("confusion_textview",
                   "Note that the target you have chosen has more than",
                   "2 classes. Some functionality on the Evaluate tab",
                   "will not be available. In particular, the ROCR",
                   "package (Lift, ROC, Precision, and Sensitivity",
                   "charts) and the Risk Chart only handle binary",
                   "classification.")
  }
  else
  {
    activate.rocr.plots()
    setTextview("confusion_textview") # Clear any confusion table
  }

  ## DISPATCH

  if (rattleWidget("all_models_radiobutton")$getActive())
  {
    if (executeModelRPart())
      rattleWidget("rpart_evaluate_checkbutton")$setActive(TRUE)
    if (executeModelRF())
      rattleWidget("rf_evaluate_checkbutton")$setActive(TRUE)
    if (executeModelSVM())
      rattleWidget("ksvm_evaluate_checkbutton")$setActive(TRUE)
    if (executeModelGLM())
      rattleWidget("glm_evaluate_checkbutton")$setActive(TRUE)
    if (executeModelGBM())
      rattleWidget("gbm_evaluate_checkbutton")$setActive(TRUE) 

    setStatusBar("All models have been generated.")
  }
  else if (currentModelTab() == GLM)
    executeModelGLM()
  else if (currentModelTab() == RPART)
    executeModelRPart()
  else if (currentModelTab() == GBM)
    executeModelGBM()
  else if (currentModelTab() == RF)
    executeModelRF()
  else if (is.element(currentModelTab(), c(SVM, KSVM)))
    executeModelSVM()
}

##----------------------------------------------------------------------
##
## MODEL GLM
##

executeModelGLM <- function()
{

  ## Currently only handling binary classification.
  
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

  family <- rattleWidget("glm_family_comboboxentry")$getActiveText()
  
  ## Build the formula for the model.

  frml <- paste(crs$target, "~ .")

  ## List, as a string, the variables to be included. 
  
  included <- getIncludedVariables()
  
  ## Some convenience booleans.

  sampling  <- ! is.null(crs$sample)
  including <- ! is.null(included)
  subsetting <- sampling || including
  
  ## Assume logistic regression for binary classification for now.
  
  glm.cmd <- paste("crs$glm <<- glm(", frml, ", data=crs$dataset",
                       if (subsetting) "[",
                       if (sampling) "crs$sample",
                       if (subsetting) ",",
                       if (including) included,
                       if (subsetting) "]",
                       ", family=", family,
                       ")", sep="")

  summary.cmd <- "summary(crs$glm)"
  
  ## Build the model.

  addLogSeparator("LOGISTIC REGRESSION")
  addToLog("Build a logistic regression model using glm.",
          gsub("<<-", "<-", glm.cmd), sep="")
  startTime <- Sys.time()
  eval(parse(text=glm.cmd))
  
  ## Summarise the model.

  addToLog("Summary of the resulting GLM model", summary.cmd)
          
  clearTextview("glm_textview")
  setTextview("glm_textview",
              "Summary of the model built using glm.\n",
              collectOutput(summary.cmd, TRUE),
              textviewSeparator())

  if (sampling) crs$smodel <<- union(crs$smodel, GLM)
  
  timeTaken <- Sys.time()-startTime
  addToLog(sprintf("Time taken: %0.2f %s", timeTaken, timeTaken@units))
  setStatusBar("GLM model has been generated.")
  return(TRUE)
}

##------------------------------------------------------------------------
##
## MODEL SVM - SUPPORT VECTOR MACHINE
##

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
  
  useKernlab <- rattleWidget("kernlab_radiobutton")$getActive()

  TV <- ifelse(useKernlab, "ksvm_textview", "esvm_textview")
  
  addLogSeparator("SUPPORT VECTOR MACHINE")

  ## Library.

  if (useKernlab)
  {
    if (packageIsAvailable("kernlab", "build an SVM model using ksvm"))
    {
      libCmd <- "require(kernlab, quietly=TRUE)"
      addToLog("The kernlab package supplies the ksvm function.", libCmd)
    }
    else
      return(FALSE)
  }
  else
  {
    if (packageIsAvailable("e1071", "build an SVM model using svm"))
    {
      libCmd <- "require(e1071, quietly=TRUE)"
      addToLog("The e1071 package supplies the svm function.", libCmd)
    }
    else
      return(FALSE)
   }
  eval(parse(text=libCmd))

  ## Formula. TODO For kernlab we assume we will always do
  ## classification rather than regression, at least for now.

  if (useKernlab)
    frml <- paste(ifelse(is.factor(crs$dataset[[crs$target]]),
                         crs$target,
                         sprintf("as.factor(%s)", crs$target)),
                  "~ .")
  else
    frml <- paste(crs$target, "~ .")

  ## Included variables.

  included <- getIncludedVariables()
  
  ## Convenience booleans.

  sampling   <- ! is.null(crs$sample)
  including  <- ! is.null(included)
  subsetting <- sampling || including

  ## Parameters.

  parms <- ""
  
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
  startTime <- Sys.time()
  addToLog("Build a support vector machine model.", gsub("<<-", "<-", svmCmd))
  result <- try(eval(parse(text=svmCmd)), silent=TRUE)
  if (inherits(result, "try-error"))
  {
    errorDialog("The call to svm appears to have failed.",
                 "The error message was:", result,
                 "I am not familiar with this error, and you may",
                 "want to report it to the Rattle author",
                 "at Graham.Williams@togaware.com")
    return(FALSE)
  }

  ## Display the resulting model.

  if (useKernlab)
    summaryCmd <- "crs$ksvm"
  else
    summaryCmd <- "crs$svm"
  addToLog("Generate textual output of the svm model.", summaryCmd)
  clearTextview(TV)
  setTextview(TV,
              "Summary of the svm model:\n\n",
              collectOutput(summaryCmd, TRUE),
              textviewSeparator())

  if (sampling)
    if (useKernlab)
      crs$smodel <<- union(crs$smodel, KSVM)
    else
      crs$smodel <<- union(crs$smodel, SVM)

  timeTaken <- Sys.time()-startTime
  addToLog(sprintf("Time taken: %0.2f %s", timeTaken, timeTaken@units))
  setStatusBar(sprintf("A %s model has been generated.",
                       ifelse(useKernlab, KSVM, SVM)))

  return(TRUE)
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
##
## EXPORT
##

exportModelTab <- function()
{

  if (noDatasetLoaded()) return()

  if (rattleWidget("rpart_radiobutton")$getActive())
  {
    exportRpartTab()
  }
  else
  {
    errorDialog("PMML export for this model is not yet implemented.")
    return()
  }
}

