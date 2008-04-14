## Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2008-04-14 20:57:23 Graham Williams>
##
## NNET TAB 061230
##
## Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2

#######################################################################
#
# TODO
#
# Figure out the actual framework for two class prediction
#
# use multinom from nnet package - but that is just same as glm.
#

########################################################################
##
## CALLBACKS
##

########################################################################
##
## NNET
##

executeModelNNet <- function()
{
  ## Initial setup. 
  
  TV <- "nnet_textview"

  ## Obtain user interface model options.

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
                     ", linout=TRUE", # How to get prob output?
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
  
  ## Finish up.
  
  time.taken <- Sys.time()-start.time
  time.msg <- sprintf("Time taken: %0.2f %s", time.taken,
                      attr(time.taken, "units"))
  addTextview(TV, "\n", time.msg, textviewSeparator())
  appendLog(time.msg)
  setStatusBar("A neural network model has been generated.", time.msg)
  return(TRUE)
}

