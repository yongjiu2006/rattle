# Rattle Survival
#
# Time-stamp: <2009-10-12 20:07:20 Graham Williams>
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
  if (gui) startLog("SURVIVAL MODEL")

  # Load the required package into the library.

  lib.cmd <-  "require(survival, quietly=TRUE)"
  if (! packageIsAvailable("survival", "build a Survival model")) return(FALSE)
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
  # some plot functions, but as of 090909 they are not yet
  # implemented.
  
  if (state)
  {
  }
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

