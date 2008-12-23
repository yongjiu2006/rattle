# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2008-12-22 16:25:41 Graham Williams>
#
# NNET OPTION 061230
#
# Copyright (c) 2008 Togaware Pty Ltd
#
# This files is part of Rattle.
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

#######################################################################
#
# COMMENTS
#
# 080514 For classification tasks we use multinom which calls nnet. As
# in the documentation in R, multinom fits multinomial log-linear
# models via neural networks. If you use glm instead for the case of
# binary classificatio we get only a very slightly different
# model. See Wikipedia for details of GLM.
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

  size <- theWidget("nnet_hidden_nodes_spinbutton")$getValue()
  
  # Load the package into the library

  startLog("NEURAL NETWORK")
  lib.cmd <-  "require(nnet, quietly=TRUE)"
  if (! packageIsAvailable("nnet", "build a neural network")) return(FALSE)
  appendLog("Build a neural network model using the nnet package.", lib.cmd)
  eval(parse(text=lib.cmd))

  # Build the formula for the model.

  if (binomialTarget() && ! is.numeric(crs$dataset[[crs$target]]))
    # 081010 Needs to be numeric, but I also subtract 1 so we get a
    # 0/1 target?
    frml <- sprintf("as.numeric(%s)-1 ~ .", crs$target)
  else
    frml <- sprintf("%s ~ .", crs$target)
  

  # Variables to be included --- a string of indicies.
  
  included <- getIncludedVariables()

  # Some convenience booleans

  sampling <- not.null(crs$sample)
  including <- not.null(included)
  subsetting <- sampling || including

  # Time the model building.
  
  start.time <- Sys.time()
  
  # Build a model.

  model.cmd <- paste("crs$nnet <<- ",
                     ifelse(numericTarget() || binomialTarget(),
                            "nnet", "multinom"),
                     "(", frml, ", data=crs$dataset",
                     if (subsetting) "[",
                     if (sampling) "crs$sample",
                     if (subsetting) ",",
                     if (including) included,
                     if (subsetting) "]",
                     # TODO 080427 How to choose a good value for size?
                     if (numericTarget() || binomialTarget())
                     sprintf(", size=%d, linout=TRUE, skip=TRUE", size),
                     ", trace=FALSE, maxit=1000",
                     ")", sep="")

  appendLog("Build the nnet model.", gsub("<<-", "<-", model.cmd))
  result <- try(eval(parse(text=model.cmd)), silent=TRUE)
  if (inherits(result, "try-error"))
  {
    errorReport(model.cmd, result)
    return(FALSE)
  }
  
  # Print the results of the modelling.

  if (numericTarget() || binomialTarget())
    print.cmd <- paste("print(crs$nnet)", 'print("\n\nNetwork Weights:\n\n")',
                       "print(summary(crs$nnet))", sep="\n")
  else
    print.cmd <- "print(crs$nnet)"

  appendLog("Print the results of the modelling.", print.cmd)
  resetTextview(TV)
  setTextview(TV,
              paste("Summary of the Neural Net model (built using ",
                    ifelse(numericTarget() || binomialTarget(),
                           "nnet", "multinom"), "):\n\n",
                    sep=""),
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
  dialog$setDoOverwriteConfirmation(TRUE)
  
  if(not.null(crs$dataname))
    dialog$setCurrentName(paste(get.stem(crs$dataname), "_nnet.xml", sep=""))

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

  #if (get.extension(save.name) == "") save.name <- sprintf("%s.xml", save.name)
    
  pmml.cmd <- "pmml(crs$nnet)"
  appendLog("Export a neural net as PMML.",
            sprintf('saveXML(%s, "%s")', pmml.cmd, save.name))
  saveXML(eval(parse(text=pmml.cmd)), save.name)

  # Be less chatty infoDialog("The PMML file", save.name, "has been written.")

  setStatusBar("The PMML file", save.name, "has been written.")
  
}
