# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2008-04-27 16:42:35 Graham Williams>
#
# NNET OPTION 061230
#
# Copyright (c) 2006-2008 Togaware Pty Ltd, GPL Version 2

#######################################################################
#
# TODO
#
# Figure out the actual framework for two class prediction
#
# use multinom from nnet package - but that is just same as glm.
#
##        mymn <- multinom(TARGET_Adjusted ~ .,
##                         data=crs$dataset[crs$sample,c(2:10,13)])
##        crs$pr <- predict(mymn, crs$dataset[-crs$sample, c(2:10,13)]) # (0,1)
##        table(crs$pr, crs$dataset[-crs$sample, c(2:10,13)]$TARGET_Adjusted,
##              dnn=c("Predicted", "Actual"))
##        crs$pr <- predict(mymn, crs$dataset[-crs$sample, c(2:10,13)],
##                          type="prob")
##        crs$eval <- evaluateRisk(crs$pr,
##                                 crs$dataset[-crs$sample,
##                                             c(2:10,13)]$TARGET_Adjusted,
##                                 crs$dataset[-crs$sample,
##                                             c(2:10,13,12)]$RISK_Adjustment)
##        plotRisk(crs$eval$Caseload, crs$eval$Precision,
##                 crs$eval$Recall, crs$eval$Risk)
#

########################################################################
#
# CALLBACKS
#

########################################################################
#
# NNET
#

executeModelNNet <- function()
{
  # Initial setup. 
  
  TV <- "nnet_textview"

  # Obtain user interface model options.

  # Load the package into the library

  startLog("NEURAL NETWORK")
  lib.cmd <-  "require(nnet, quietly=TRUE)"
  if (! packageIsAvailable("nnet", "build a neural network")) return(FALSE)
  appendLog("Build a neural network model using the nnet package.", lib.cmd)
  eval(parse(text=lib.cmd))

  # Build the formula for the model.

  frml <- paste(crs$target, "~ .")

  # Variables to be included --- a string of indicies.
  
  included <- getIncludedVariables()

  # Some convenience booleans

  sampling <- not.null(crs$sample)
  including <- not.null(included)
  subsetting <- sampling
  paradigm <- getParadigm()

  # Time the model building.
  
  start.time <- Sys.time()
  
  # Build a model.

  model.cmd <- paste("crs$nnet <<- ",
                     ifelse(paradigm == "regression", "nnet", "multinom"),
                     "(", frml, ", data=crs$dataset",
                     if (subsetting) "[",
                     if (sampling) "crs$sample",
                     if (subsetting) ",",
                     if (including) included,
                     if (subsetting) "]",
                     # TODO 080427 How to choose a good value for size?
                     if (paradigm == "regression")
                     ", size=10, linout=TRUE, skip=TRUE",
                     ", trace=FALSE",
                     ")", sep="")

  appendLog("Build the nnet model.", gsub("<<-", "<-", model.cmd))
  result <- try(eval(parse(text=model.cmd)), silent=TRUE)
  if (inherits(result, "try-error"))
  {
    errorReport(model.cmd, result)
    return(FALSE)
  }
  
  # Print the results of the modelling.

  print.cmd <- paste("print(crs$nnet)", "print(summary(crs$nnet))", sep="\n")
  appendLog("Print the results of the modelling.", print.cmd)
  resetTextview(TV)
  setTextview(TV, "Summary of the nnet modelling:\n\n",
              collectOutput(print.cmd))

  # Now that we have a model, make sure appropriate actions are sensitive.
  
  # Finish up.
  
  time.taken <- Sys.time()-start.time
  time.msg <- sprintf("Time taken: %0.2f %s", time.taken,
                      attr(time.taken, "units"))
  addTextview(TV, "\n", time.msg, textviewSeparator())
  appendLog(time.msg)
  setStatusBar("A neural network model has been generated.", time.msg)
  return(TRUE)
}

exportNNetTab <- function()
{
  # Make sure we have a model first!
  
  if (is.null(crs$nnet))
  {
    errorDialog("No neural net model is available. Be sure to build",
                "the model before trying to export it! You will need",
                "to press the Execute button (F5) in order to build the",
                "model.")
    return()
  }

  # Require the pmml package
  
  lib.cmd <- "require(pmml, quietly=TRUE)"
  if (! packageIsAvailable("pmml", "export neural net")) return(FALSE)
  appendLog("Load the PMML package to export a neural net.", lib.cmd)
  eval(parse(text=lib.cmd))
  
  # Obtain filename to write the PMML to.
  
  dialog <- gtkFileChooserDialog("Export PMML", NULL, "save",
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-save", GtkResponseType["accept"])

  if(not.null(crs$dataname))
    dialog$setCurrentName(paste(get.stem(crs$dataname), "_nnet", sep=""))

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
  

  pmml.cmd <- "pmml(crs$nnet)"
  appendLog("Export a neural net as PMML.", pmml.cmd)
  saveXML(eval(parse(text=pmml.cmd)), save.name)

  # Be less chatty infoDialog("The PMML file", save.name, "has been written.")

  setStatusBar("The PMML file", save.name, "has been written.")
  
}
