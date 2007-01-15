## Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2006-12-30 17:20:26 Graham>
##
## NNET TAB 061230
##
## Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2

########################################################################
##
## TODO
##
## Figure out the actual framework for two class prediction

########################################################################
##
## CALLBACKS
##

## on_ada_importance_button_clicked <- function(button)
## {
##   plotAdaImportance()
## }

## on_ada_errors_button_clicked <- function(button)
## {
##   plotAdaErrors()
## }

## on_ada_list_button_clicked <- function(button)
## {
##   doListAdaTrees()
## }

## on_ada_draw_button_clicked <- function(button)
## {
##   doDrawAdaTrees()
## }

########################################################################
##
## NNET
##

executeModelNNet <- function()
{
  ## Initial setup. 
  
  TV <- "nnet_textview"

  ## Obtain user interface model options.

##   if (theWidget("ada_stumps_checkbutton")$getActive())
##     stumps <- ", control=rpart.control(maxdepth=1,cp=-1,minsplit=0,xval=0)"
##   else
##     stumps <- ""
  
##   ntree <- theWidget("ada_ntree_spinbutton")$getValue()
##   if (ntree != ADA.NTREE.DEFAULT)
##     ntree <- sprintf(", iter=%d", ntree)
##   else
##     ntree <- ""

  ## Load the package into the library

  addLogSeparator("NEURAL NETWORK")
  lib.cmd <-  "require(nnet, quietly=TRUE)"
  if (! packageIsAvailable("nnet", "build a neural network")) return(FALSE)
  addToLog("Build a neural network model using the nnet package.", lib.cmd)
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
  
  ## Build a model.

  model.cmd <- paste("crs$nnet <<- nnet(", frml, ", data=crs$dataset",
                     if (subsetting) "[",
                     if (sampling) "crs$sample",
                     if (subsetting) ",",
                     if (including) included,
                     if (subsetting) "]",
                     ", size=10", # How to choose a good value here.
                     ")", sep="")

  addToLog("Build the nnet model.", gsub("<<-", "<-", model.cmd))
  result <- try(eval(parse(text=model.cmd)), silent=TRUE)
  if (inherits(result, "try-error"))
  {
    errorDialog("An error occured in the call to nnet and modelling failed.",
                "The error was:", result)
    return(FALSE)
  }
  
  ## Print the results of the modelling.

  print.cmd <- paste("print(crs$nnet)", "summary(crs$nnet)", sep="\n")
  addToLog("Print the results of the modelling.", print.cmd)
  clearTextview(TV)
  setTextview(TV, "Summary of the nnet modelling:\n\n",
              collectOutput(print.cmd))

  ## Now that we have a model, make sure appropriate actions are sensitive.
  
##   theWidget("ada_importance_button")$setSensitive(TRUE)
##   theWidget("ada_errors_button")$setSensitive(TRUE)
##   theWidget("ada_list_button")$setSensitive(TRUE)
##   theWidget("ada_draw_button")$setSensitive(TRUE)
##   theWidget("ada_draw_spinbutton")$setSensitive(TRUE)

  ## Finish up.
  
  time.taken <- Sys.time()-start.time
  time.msg <- sprintf("Time taken: %0.2f %s", time.taken, time.taken@units)
  addTextview(TV, "\n", time.msg, textviewSeparator())
  addToLog(time.msg)
  setStatusBar("A neural network model has been generated.", time.msg)
  return(TRUE)
}

## plotAdaImportance <- function()
## {

##   ## Make sure there is a model object first.

##   if (is.null(crs$ada))
##   {
##     errorDialog("E131: Should not be here.",
##                 "There is no ADA model and attempting to plot importance.",
##                 "The button should not be active.",
##                 "Please report this to Graham.Williams@togaware.com")
##     return()
##   }

##   ## Plot the variable importance.
  
##   newPlot()
##   plot.cmd <- "varplot(crs$ada)"
##   addToLog("Plot the relative importance of the variables.", plot.cmd)
##   eval(parse(text=plot.cmd))

##   setStatusBar("ADA Variable Importance has been plotted.")
## }
  
## plotAdaErrors <- function()
## {

##   ## Make sure there is a model object first.

##   if (is.null(crs$ada))
##   {
##     errorDialog("E132: Should not be here.",
##                 "There is no ADA model and attempting to plot error.",
##                 "The button should not be active.",
##                 "Please report this to Graham.Williams@togaware.com")
##     return()
##   }

##   ## Plot the error rates.
  
##   newPlot()
##   plot.cmd <- "plot(crs$ada)" #, kappa=TRUE)"
##   addToLog("Plot the error rate as we increase the number of trees.", plot.cmd)
##   eval(parse(text=plot.cmd))

##   setStatusBar("Ada errors has been plotted.")
## }

## plotAdaImportance <- function()
## {

##   ## Make sure there is a model object first.

##   if (is.null(crs$ada))
##   {
##     errorDialog("E131: Should not be here.",
##                 "There is no ADA model and attempting to plot importance.",
##                 "The button should not be active.",
##                 "Please report this to Graham.Williams@togaware.com")
##     return()
##   }

##   ## Plot the variable importance.
  
##   newPlot()
##   plot.cmd <- "varplot(crs$ada)"
##   addToLog("Plot the relative importance of the variables.", plot.cmd)
##   eval(parse(text=plot.cmd))

##   setStatusBar("ADA Variable Importance has been plotted.")
## }  

## doListAdaTrees <- function()
## {
##   ## Initial setup. 
  
##   TV <- "ada_textview"

##   ## Obtain user interface options.

##   tree.num <- theWidget("ada_draw_spinbutton")$getValue()

##   ## Command to run.

##   display.cmd <- sprintf("listAdaTrees(crs$ada, %d)", tree.num)

##   ## Perform the action.

##   addToLog(sprintf("Display tree number %d.", tree.num), display.cmd)
##   addTextview(TV, collectOutput(display.cmd, TRUE), textviewSeparator())
##   setStatusBar(paste("Tree", tree.num, "has been added to the textview.",
##                      "You may need to scroll the textview to see it."))
## }

## listAdaTrees <- function(model, trees=0)
## {
##   stopifnot(require(ada, quietly=TRUE))
##   ntrees <- length(model$model$trees)
##   if (trees == 0) trees=1:ntrees
##   for (i in trees)
##   {
##     cat(sprintf("\nTree %d of %d: \n", i, ntrees))
##     print(model$model$trees[[i]])
##   }
## }

## doDrawAdaTrees <- function()
## {
##   ## Obtain user interface options.

##   tree.num <- theWidget("ada_draw_spinbutton")$getValue()

##   ## Command to run.

##   draw.cmd <- sprintf('drawAdaTrees(crs$ada, %d, ": %s")', tree.num,
##                       paste(crs$dataname, "$", crs$target))

##   ## Perform the action.

##   addToLog(sprintf("Display tree number %d.", tree.num), draw.cmd)
##   eval(parse(text=draw.cmd))
##   setStatusBar("Tree", tree.num, "has been drawn.")
## }

## drawAdaTrees <- function(model,
##                          trees=0,
##                          title="")
## {
##   stopifnot(require(ada, quietly=TRUE))
##   ntrees <- length(model$model$trees)
##   if (length(trees) == 1 && trees == 0) trees=1:ntrees
##   for (i in trees)
##   {
##     newPlot()
##     drawTreeNodes(model$model$trees[[i]])
##     eval(parse(text=genPlotTitleCmd(sprintf("Tree %d of %d%s", i,
##                  ntrees, title))))
##   }
## }
