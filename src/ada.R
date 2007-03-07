## Rattle TwoClass Ada
##
## This is becoming the model "module" for rattle.
##
## TODO
##   Split GUI stuff into ada_gui.R
##   Remove all references to crs and gui stuff - use function parameters
##
## Time-stamp: <2007-03-08 07:24:03 Graham>
##
## Copyright (c) 2007 Graham Williams, Togaware.com, GPL Version 2

########################################################################
##
## CALLBACKS - This is where we have a bunch of Rattle GUI specific
## functions. Perhaps these need to be in a separate ada_gui.R
##

on_ada_stumps_button_clicked <- function(button)
{
  set.defaults.ada(stumps=TRUE)
}

on_ada_defaults_button_clicked <- function(button)
{
  set.defaults.ada()
}

on_ada_importance_button_clicked <- function(button)
{
  plot.importance.ada()
}

on_ada_errors_button_clicked <- function(button)
{
  plot.errors.ada()
}

on_ada_list_button_clicked <- function(button)
{
  list.trees.ada.gui()
}

on_ada_draw_button_clicked <- function(button)
{
  draw.trees.ada.gui()
}

on_help_ada_activate <- function(action, window)
{
  display.help.ada()
}

list.trees.ada.gui <- function()
{
  ## Initial setup. 
  
  TV <- "ada_textview"

  ## Obtain user interface options.

  tree.num <- theWidget("ada_draw_spinbutton")$getValue()

  ## Command to run.

  display.cmd <- sprintf("list.trees.ada(crs$ada, %d)", tree.num)

  ## Perform the action.

  addToLog(sprintf("Display tree number %d.", tree.num), display.cmd)
  addTextview(TV, collectOutput(display.cmd, TRUE), textviewSeparator())
  setStatusBar(paste("Tree", tree.num, "has been added to the textview.",
                     "You may need to scroll the textview to see it."))
}

draw.trees.ada.gui <- function()
{
  ## Obtain user interface options.

  tree.num <- theWidget("ada_draw_spinbutton")$getValue()

  ## Command to run.

  draw.cmd <- sprintf('draw.trees.ada(crs$ada, %d, ": %s")', tree.num,
                      paste(crs$dataname, "$", crs$target))

  ## Perform the action.

  addToLog(sprintf("Display tree number %d.", tree.num), draw.cmd)
  eval(parse(text=draw.cmd))
  setStatusBar("Tree", tree.num, "has been drawn.")
}

########################################################################
##
## ADA - This implements a generic interface for interacting with the
## ada modeller and ada models. Need to remove all reference to crs
## and pass them through as parameters, where needed.
##

execute.model.ada <- function()
{
  ## Initial setup. 
  
  TV <- "ada_textview"

  ## Obtain user interface model options.

  maxdepth <- theWidget("ada_maxdepth_spinbutton")$getValue()
  minsplit <- theWidget("ada_minsplit_spinbutton")$getValue()
  cp       <- theWidget("ada_cp_spinbutton")$getValue()
  xval     <- theWidget("ada_xval_spinbutton")$getValue()
  ntree    <- theWidget("ada_ntree_spinbutton")$getValue()

  if (ntree != .ADA.NTREE.DEFAULT)
    ntree <- sprintf(", iter=%d", ntree)
  else
    ntree <- ""

  ## Construct the appropriate rpart control.
  
  control <- sprintf(paste(", control=rpart.control(maxdepth=%d,",
                           "cp=%f, minsplit=%d, xval=%d)"),
                     maxdepth, cp, minsplit, xval)
  
  ## Load the required package into the library

  addLogSeparator("ADA BOOST")

  lib.cmd <-  "require(ada, quietly=TRUE)"
  if (! packageIsAvailable("ada", "build an AdaBoost model")) return(FALSE)
  addToLog("Build an adaboost model using the ada package.", lib.cmd)
  eval(parse(text=lib.cmd))

  ## Build the formula for the model.

  frml <- paste(crs$target, "~ .")

  ## Variables to be included --- a string of indicies.
  
  included <- getIncludedVariables()

  ## Some convenience booleans

  sampling <- not.null(crs$sample)
  including <- not.null(included)
  subsetting <- sampling

  ## Time the model building.
  
  start.time <- Sys.time()
  
  ## Build a model. Note that there seems to be some randomness in
  ## this implementation of AdaBoost, so set the seed to get the same
  ## result each time. Experiment, once I have the rule printing
  ## working, to test this assumption.

  model.cmd <- paste("set.seed(123)\n",
                     "crs$ada <<- ada(", frml, ", data=crs$dataset",
                     if (subsetting) "[",
                     if (sampling) "crs$sample",
                     if (subsetting) ",",
                     if (including) included,
                     if (subsetting) "]",
                     control, ntree,
                     ")", sep="")

  addToLog("Build the adaboost model.", gsub("<<-", "<-", model.cmd))
  result <- try(eval(parse(text=model.cmd)), silent=TRUE)
  if (inherits(result, "try-error"))
  {
    errorDialog("An error occured in the call to ada and modelling failed.",
                "The error was:", result)
    return(FALSE)
  }
  
  ## Print the results of the modelling.

  print.cmd <- paste("print(crs$ada)", "summary(crs$ada)", sep="\n")
  addToLog("Print the results of the modelling.", print.cmd)
  clearTextview(TV)
  setTextview(TV, "Summary of the adaboost modelling:\n\n",
              collectOutput(print.cmd))

  ## Now that we have a model, make sure appropriate actions are sensitive.

  make.sensitive.ada()
  
  ## Finish up.
  
  time.taken <- Sys.time()-start.time
  time.msg <- sprintf("Time taken: %0.2f %s", time.taken, time.taken@units)
  addTextview(TV, "\n", time.msg, textviewSeparator())
  addToLog(time.msg)
  setStatusBar("An adaboost model has been generated.", time.msg)
  return(TRUE)
}

gen.cmd.predict.ada <- function(dataset)
{
  return(sprintf("crs$pr <<- predict(crs$ada, %s)", dataset))
}

gen.cmd.response.ada <- function(dataset)
{
  return(gen.cmd.predict.ada(dataset))
}

gen.cmd.probability.ada <- function(dataset)
{
  return(sprintf("%s[,2]", gsub(")$", ', type="prob")',
                                gen.cmd.predict.ada(dataset))))
}

make.sensitive.ada <- function(state=TRUE)
{
  theWidget("ada_importance_button")$setSensitive(state)
  theWidget("ada_errors_button")$setSensitive(state)
  theWidget("ada_list_button")$setSensitive(state)
  theWidget("ada_draw_button")$setSensitive(state)
  theWidget("ada_draw_spinbutton")$setSensitive(state)
}

set.defaults.ada <- function(stumps=FALSE)
{
  if (stumps)
  {
    theWidget("ada_maxdepth_spinbutton")$setValue(1)
    theWidget("ada_minsplit_spinbutton")$setValue(0)
    theWidget("ada_cp_spinbutton")$setValue(-1)
    theWidget("ada_xval_spinbutton")$setValue(0)
  }
  else
  {
    theWidget("ada_maxdepth_spinbutton")$setValue(30)
    theWidget("ada_minsplit_spinbutton")$setValue(20)
    theWidget("ada_cp_spinbutton")$setValue(0.01)
    theWidget("ada_xval_spinbutton")$setValue(10)
  }
}

plot.importance.ada <- function()
{

  ## Make sure there is a model object first.

  if (is.null(crs$ada))
  {
    errorDialog("E131: Should not be here.",
                "There is no ADA model and attempting to plot importance.",
                "The button should not be active.",
                "Please report this to Graham.Williams@togaware.com")
    return()
  }

  ## Plot the variable importance.
  
  newPlot()
  plot.cmd <- "varplot(crs$ada)"
  addToLog("Plot the relative importance of the variables.", plot.cmd)
  eval(parse(text=plot.cmd))

  setStatusBar("ADA Variable Importance has been plotted.")
}
  
plot.errors.ada <- function()
{

  ## Make sure there is a model object first.

  if (is.null(crs$ada))
  {
    errorDialog("E132: Should not be here.",
                "There is no ADA model and attempting to plot error.",
                "The button should not be active.",
                "Please report this to Graham.Williams@togaware.com")
    return()
  }

  ## Plot the error rates.
  
  newPlot()
  plot.cmd <- "plot(crs$ada)" #, kappa=TRUE)"
  addToLog("Plot the error rate as we increase the number of trees.", plot.cmd)
  eval(parse(text=plot.cmd))

  setStatusBar("Ada errors has been plotted.")
}

plot.importance.ada <- function()
{

  ## Make sure there is a model object first.

  if (is.null(crs$ada))
  {
    errorDialog("E131: Should not be here.",
                "There is no ADA model and attempting to plot importance.",
                "The button should not be active.",
                "Please report this to Graham.Williams@togaware.com")
    return()
  }

  ## Plot the variable importance.
  
  newPlot()
  plot.cmd <- "varplot(crs$ada)"
  addToLog("Plot the relative importance of the variables.", plot.cmd)
  eval(parse(text=plot.cmd))

  setStatusBar("ADA Variable Importance has been plotted.")
}  

list.trees.ada <- function(model, trees=0)
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

draw.trees.ada <- function(model,
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

display.help.ada <- function()
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
