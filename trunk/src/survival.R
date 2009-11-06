# Rattle Survival
#
# Time-stamp: <2009-11-06 19:09:00 Graham Williams>
#
# Copyright (c) 2009 Togaware Pty Ltd
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

########################################################################
# GUI

setGuiDefaultsSurvival <- function()
{
  theWidget("model_survival_time_var_label")$setText("No time variable selected")
  theWidget("model_survival_status_var_label")$setText("No status variable selected")
  theWidget("model_survival_coxph_radiobutton")$setActive(TRUE)
  theWidget("model_survival_plots_button")$hide()
}

on_model_survival_plots_button_clicked <- function(button)
{
  plotSurvivalModel()
}


########################################################################
# Model Tab

buildModelSurvival <- function(formula, dataset, tv=NULL, method=c("para", "coxph"))
{
  # If tv is not NULL, then we will be updating the textview object as
  # we proceed, as well as sending information to the log. The aim is
  # for this function to run totally independent of the GUI, but to
  # also support it. A developer can use this function, supply their
  # own textview object and their own implementations of resetTextview
  # and appendTextview for the modelling output, and startLog and
  # appendLog for a log of the commands, and setStatusBar for a
  # summary of what has been done.

  gui <- not.null(tv)
  if (gui) startLog("Survival Model")

  # Load the required package into the library.

  lib.cmd <-  "require(survival, quietly=TRUE)"
  if (! packageIsAvailable("survival", "build a Survival model")) return(NULL)
  if (gui) appendLog("Require the survival package.", lib.cmd)
  eval(parse(text=lib.cmd))

  # Build a model. 

  method <- ifelse(method=="para", "survreg", "coxph")
  model.cmd <- sprintf("%s(%s, data=%s)", method, formula, dataset)

  if (gui) appendLog("Build the Survival model.",
                     sprintf('crs$survival <- %s', model.cmd))

  # Note that this crs$survival is not the global crs$survival! We use
  # it here to be consistent in terms of the commands that are
  # reported to the log, but we return this value and in the outer
  # call we globally assign to crs$survival, at least in the context
  # of the Rattle GUI.
  
  start.time <- Sys.time()
  crs$survival <- try(eval(parse(text=model.cmd)), silent=TRUE)
  time.taken <- Sys.time()-start.time

  if (inherits(crs$survival, "try-error"))
  {
    msg <- sprintf(paste("An error occured in the call to %s and modelling failed.",
                         "The error was: %s"), method, crs$survival)
    if (gui)
    {
      errorDialog(msg)
      return(NULL)
    }
    stop(msg)
  }
  
  # Print the results of the modelling.

  if (gui)
  {
    print.cmd <- "summary(crs$survival)"
    appendLog("Print the results of the modelling.", print.cmd)
    resetTextview(tv, tvsep=FALSE,
                  sprintf("Summary of the Survival model (built using %s):\n\n", method),
                  collectOutput(print.cmd))
  }

  # Finish up.
  
  if (gui)
  {
    time.msg <- sprintf("Time taken: %0.2f %s", time.taken,
                        attr(time.taken, "units"))
    appendTextview(tv, "\n", time.msg)
    appendLog(time.msg)
    setStatusBar("A survival model has been generated.", time.msg)
  }
  return(crs$survival)
}

showModelSurvivalExists <- function(state=!is.null(crs$survival))
{
  # If a survival model exists then show the relevant buttons that
  # require the model to exist. For the Survival model this will be
  # some plot functions.

  if (state)
  {
    theWidget("model_survival_plots_button")$show()
    if (class(crs$survival) == "coxph")
      theWidget("model_survival_plots_button")$setSensitive(TRUE)
    else
      # Show it but indicate it is not yet implemented
      theWidget("model_survival_plots_button")$setSensitive(FALSE)
  }
  else
  {
    theWidget("model_survival_plots_button")$hide()
  }
}

plotSurvivalModel <- function()
{
  plot.cmd <- paste('plot(survfit(crs$survival), xlab=crs$target,',
                    'ylab="Survival Probability", col=3)\n',
                    genPlotTitleCmd('Survival Chart', crs$target, 'to',
                                    crs$risk))
  appendLog("Plot the survival chart for the most recent survival model.", plot.cmd)
  newPlot()
  eval(parse(text=plot.cmd))
}



########################################################################
# Export

exportSurvivalTab <- function()
{
  # Make sure we have a model first!

  if (noModelAvailable(crs$survival, crv$SURVIVAL)) return(FALSE)

  if (class(crs$survival) == "survreg")
  {
    infoDialog("The Parametric Survival Regression model (survreg) can not",
               "currently be exported. Perhaps try a Cox Proportional",
               "Hazards model.")
    return(FALSE)
  }
  
  startLog("Export Survival Model")

  save.name <- getExportSaveName(crv$SURVIVAL)
  if (is.null(save.name)) return(FALSE)
  ext <- tolower(get.extension(save.name))

  # Generate appropriate code.
  
  pmml.cmd <- sprintf("pmml(crs$survival%s)",
                      ifelse(length(crs$transforms) > 0,
                             ", transforms=crs$transforms", ""))

  if (ext == "xml")
  {
    appendLog("Export regression as PMML.",
              sprintf('saveXML(%s, "%s")', pmml.cmd, save.name))
    saveXML(eval(parse(text=pmml.cmd)), save.name)
  }
  else if (ext == "c")
  {
    # 090103 gjw Move to a function: saveC(pmml.cmd, save.name, "regression")

    # 090223 Why is this tolower being used? Under GNU/Linux it is
    # blatantly wrong. Maybe only needed for MS/Widnows
    
    if (isWindows()) save.name <- tolower(save.name)
    
    model.name <- sub("\\.c", "", basename(save.name))
    appendLog("Export a regression model as C code for WebFocus.",
              sprintf('cat(pmmltoc(toString(%s), "%s", %s, %s, %s), file="%s")',
                      pmml.cmd, model.name, 
                      attr(save.name, "includePMML"),
                      attr(save.name, "includeMetaData"),
                      attr(save.name, "exportClass"),
                      save.name))
    cat(pmmltoc(toString(eval(parse(text=pmml.cmd))), model.name,
                attr(save.name, "includePMML"),
                attr(save.name, "includeMetaData"),
                attr(save.name, "exportClass")), file=save.name)
  }
  
  setStatusBar("The", toupper(ext), "file", save.name, "has been written.")
}

########################################################################
# Evaluate

genPredictSurvival <- function(dataset, coxph=FALSE)
{
  # Generate a command to obtain the prediction results when applying
  # the model to new data. 091002 Don't use the coxph yet until it is
  # better understood.
  
  return(sprintf("crs$pr <- predict(crs$survival, %s%s)", dataset,
                 ifelse(coxph, ', type="risk"', "")))
}

genResponseSurvival <- function(dataset, coxph=FALSE)
{
  # Generate a command to obtain the response when applying the model
  # to new data.
  
  return(genPredictSurvival(dataset, coxph))
}

genProbabilitySurvival <- function(dataset, coxph=FALSE)
{
  # Generate a command to obtain the probability when applying the
  # model to new data.
  
  return(sprintf("%s[,2]", gsub(")$", ', type="prob")',
                                genPredictSurvival(dataset, coxph))))
}

