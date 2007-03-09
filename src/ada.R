## Rattle TwoClass Ada
##
## This is a model or template "module" for rattle.
##
## Time-stamp: <2007-03-10 08:55:07 Graham>
##
## Copyright (c) 2007 Graham Williams, Togaware.com, GPL Version 2
##
## This implements a generic interface for interacting with the ada
## modeller and ada models. It can be used independent of the Rattle
## GUI, but is designed for use by it.

buildModelAda <- function(formula,
                          dataset,
                          tv=NULL,
                          seed=123,
                          maxdepth=30,
                          minsplit=20,
                          cp=0.01,
                          xval=10,
                          ntree=50)
{
  ## If tv is not null, then we will be updating the textview object
  ## as we proceed, as well as sending information to the log. The aim
  ## is for this function to run totally independent of the GUI, but
  ## to also support it. A developer can use this function, supply
  ## their own textview object and their own implementations of
  ## resetTextview and appendTextview for the modelling output, and
  ## startLog and appendLog for a log of the commands, and
  ## setStatusBar for a summary of what has been done.

  gui <- not.null(tv)
  if (gui) startLog("ADA BOOST")

  ## Load the required package into the library.

  lib.cmd <-  "require(ada, quietly=TRUE)"
  if (! packageIsAvailable("ada", "build an AdaBoost model")) return(FALSE)
  if (gui) appendLog("Require the ada package.", lib.cmd)
  eval(parse(text=lib.cmd))

  ## Construct the appropriate rpart control.
  
  control <- sprintf(paste(", control=rpart.control(maxdepth=%d,",
                           "cp=%f, minsplit=%d, xval=%d)"),
                     maxdepth, cp, minsplit, xval)
  
  ## Build a model. Note that there is randomness in this
  ## implementation of AdaBoost, so set the seed to get the same
  ## result each time.

  model.cmd <- paste(sprintf("set.seed(%d)\n", seed),
                     "ada(", formula, ", data=", dataset,
                     control, ", iter=", ntree, ")",
                     sep="")

  if (gui) appendLog("Build the adaboost model.", "crs$ada <-", model.cmd)

  ## Note that this crs$ada is not the global crs$ada! We use it here
  ## to be consistent in terms of the commands that are reported to
  ## the log, but we return this value and in the outer call we
  ## globally assign to crs$ada, at least in the context of the Rattle
  ## GUI.
  
  start.time <- Sys.time()
  crs$ada <- try(eval(parse(text=model.cmd)), silent=TRUE)
  time.taken <- Sys.time()-start.time

  if (inherits(crs$ada, "try-error"))
  {
    msg <- paste("An error occured in the call to ada and modelling failed.",
                  "The error was:", crs$ada)
    if (gui)
    {
      errorDialog(msg)
      return(FALSE)
    }
    stop(msg)
  }
  
  ## Print the results of the modelling.

  if (gui)
  {
    print.cmd <- paste("print(crs$ada)", "summary(crs$ada)", sep="\n")
    appendLog("Print the results of the modelling.", print.cmd)
    resetTextview(tv, tvsep=FALSE,
                  "Summary of the adaboost modelling:\n\n",
                  collectOutput(print.cmd))
  }

  ## Finish up.
  
  if (gui)
  {
    time.msg <- sprintf("Time taken: %0.2f %s", time.taken, time.taken@units)
    appendTextview(tv, "\n", time.msg)
    appendLog(time.msg)
    setStatusBar("An adaboost model has been generated.", time.msg)
  }
  return(crs$ada)
}

genPredictAda <- function(dataset)
{
  ## Generate a command to obtain the prediction results when applying
  ## the model to new data.
  
  return(sprintf("crs$pr <<- predict(crs$ada, %s)", dataset))
}

genResponseAda <- function(dataset)
{
  ## Generate a command to obtain the response when applying the model
  ## to new data.
  
  return(genPredictAda(dataset))
}

genProbabilityAda <- function(dataset)
{
  ## Generate a command to obtain the probability when applying the
  ## model to new data.
  
  return(sprintf("%s[,2]", gsub(")$", ', type="prob")',
                                genPredictAda(dataset))))
}

plotImportanceAda <- function()
{
  ## Generate a plot of the variable importances.
  
  ## Make sure there is a model object first.

  if (is.null(crs$ada))
  {
    errorDialog("E135: Should not be here.",
                "There is no ADA model and attempting to plot importance.",
                "The button should not be active.",
                "Please report this to Graham.Williams@togaware.com")
    return()
  }

  ## Plot the variable importance.
  
  newPlot()
  plot.cmd <- "varplot(crs$ada)"
  appendLog("Plot the relative importance of the variables.", plot.cmd)
  eval(parse(text=plot.cmd))

  setStatusBar("ADA Variable Importance has been plotted.")
}
  
plotErrorsAda <- function()
{
  ## Generate a plot of the error rate as we add more models to the
  ## ensemble.
  
  ## Make sure there is a model object first.

  if (is.null(crs$ada))
  {
    errorDialog("E136: Should not be here.",
                "There is no ADA model and attempting to plot error.",
                "The button should not be active.",
                "Please report this to Graham.Williams@togaware.com")
    return()
  }

  ## Plot the error rates.
  
  newPlot()
  plot.cmd <- "plot(crs$ada)" #, kappa=TRUE)"
  appendLog("Plot the error rate as we increase the number of trees.", plot.cmd)
  eval(parse(text=plot.cmd))

  setStatusBar("Ada errors has been plotted.")
}

listTreesAda <- function(model, trees=0)
{
  stopifnot(require(ada, quietly=TRUE))
  ntrees <- length(model$model$trees)
  if (trees == 0) trees=1:ntrees
  for (i in trees)
  {
    cat(sprintf("\nTree %d of %d: \n", i, ntrees))
    print(model$model$trees[[i]])
  }
}

drawTreesAda <- function(model,
                         trees=0,
                         title="")
{
  stopifnot(require(ada, quietly=TRUE))
  ntrees <- length(model$model$trees)
  if (length(trees) == 1 && trees == 0) trees=1:ntrees
  for (i in trees)
  {
    newPlot()
    drawTreeNodes(model$model$trees[[i]])
    eval(parse(text=genPlotTitleCmd(sprintf("Tree %d of %d%s", i,
                 ntrees, title))))
  }
}

displayHelpAda <- function()
{
  if (showHelpPlus("Boosting builds multiple, but generally simple, models.
The models might be decision trees that have just one split - these
are often called decision stumps. After building each model any
training entities that the model misclassifies are boosted - they are
given more weight or more importance in the next model building
step. The resulting model is then the weighted sum of the ensemble of
models built.
<<>>
The ada package is used to build the boosted model."))
    {
      require(ada, quietly=TRUE)
      popupTextviewHelpWindow("ada")
    }
}
