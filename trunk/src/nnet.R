## Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2007-03-09 06:28:43 Graham>
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
##   plot.importance.ada()
## }

## on_ada_errors_button_clicked <- function(button)
## {
##   plotErrorsAda()
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

  startLog("NEURAL NETWORK")
  lib.cmd <-  "require(nnet, quietly=TRUE)"
  if (! packageIsAvailable("nnet", "build a neural network")) return(FALSE)
  appendLog("Build a neural network model using the nnet package.", lib.cmd)
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

  appendLog("Build the nnet model.", gsub("<<-", "<-", model.cmd))
  result <- try(eval(parse(text=model.cmd)), silent=TRUE)
  if (inherits(result, "try-error"))
  {
    errorDialog("An error occured in the call to nnet and modelling failed.",
                "The error was:", result)
    return(FALSE)
  }
  
  ## Print the results of the modelling.

  print.cmd <- paste("print(crs$nnet)", "summary(crs$nnet)", sep="\n")
  appendLog("Print the results of the modelling.", print.cmd)
  resetTextview(TV)
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
  appendLog(time.msg)
  setStatusBar("A neural network model has been generated.", time.msg)
  return(TRUE)
}

