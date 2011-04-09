## Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2011-04-08 06:02:33 Graham Williams>
##
## GBM TAB
##
## Copyright (c) 2009 Togaware Pty Ltd
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

# 110408 The target code might be something like the following, but it
# does not quite work.

## require(gbm, quietly=TRUE)

## # Build the Ada Boost model.

## set.seed(crv$seed)
## crs$ada <- gbm(as.numeric(RainTomorrow)-1 ~ .,
##                data=crs$dataset[crs$train,c(crs$input, crs$target)],
##                distribution="adaboost")

########################################################################
##
## CALLBACKS
##

on_gbm_importance_button_clicked <- function(button)
{
  plotGBMImportance()
}

########################################################################
##
## GBM - BOOSTING
##

executeModelGBM <- function()
{
  num.classes <- length(levels(as.factor(crs$dataset[[crs$target]])))

  ## Check for ignored variables

  indicies <- NULL
  if (not.null(crs$input))
    indicies <- getVariableIndicies(crs$input)

  ## Build the formula for the model - why can't GBM use "."?

  target.index <- which(colnames(crs$dataset)==crs$target)
##   frml <- paste(target, "~",
##                 paste(colnames(crs$dataset)[-c(indicies, target.index)],
##                       collapse="+"))

  ## Some convenience booleans

  sampling <- not.null(crs$sample)
  subsetting <- sampling

  ## Greg Ridgway's advice is to use bernoulli, not adaboost or gaussian

  if (is.factor(crs$dataset[[crs$target]]) || num.classes > 2)
    distribution <- "gaussian"
  else
    #distribution <- "adaboost"
    distribution <- "bernoulli"
  
  ## Required library

  if (! packageIsAvailable("gbm", "build an AdaBoost model"))
    return(FALSE)
  
  lib.cmd <- paste(sprintf("\n\n## Build a GBM (%s) model.",
                                   distribution),
                           "\n\nrequire(gbm, quietly=TRUE)")

  ## Boost command

  ## Use gbm.fit rather than gbm for more efficiency.

  included <- simplifyNumberList(indicies)
  
  boost.cmd <- paste("crs$gbm <- gbm.fit(crs$dataset[",
                         if (sampling) "crs$sample",
                         ",", included, "], ",
                         "crs$dataset$", crs$target,
                         if (sampling) "[crs$sample]",
                         ", ",
                         'distribution="', distribution, '"',
                         ")", sep="")

  ## Summary command

  summary.cmd <- "summary(crs$gbm, cBars=5, plotit=FALSE)"
  show.cmd <- "gbmShowRules(crs$gbm)"
 
  ## Log

  appendLog(lib.cmd, "\n",
          boost.cmd, "\n",
          summary.cmd, "\n",
          show.cmd, sep="")

  ## Run model and show results.

  startTime <- Sys.time()
  eval(parse(text=lib.cmd))
  resetTextview("gbm_textview")
  setTextview("gbm_textview",
               "Output from GBM model builder:\n\n",
               collectOutput(boost.cmd),
               "\n\nSummary of relative influence of each variable:\n\n",
               collectOutput(paste("print(",summary.cmd, ")")),
               "\n\nRules making up the model:\n\n",
               collectOutput(show.cmd),
               sep="")

  if (sampling) crs$smodel <- union(crs$smodel, GBM)
  
  timeTaken <- Sys.time()-startTime
  appendLog(sprintf("Time taken: %0.2f %s", timeTaken,
                    attr(timeTaken, "units")))
  setStatusBar("Boosted model has been generated.")
  return(TRUE)
}

gbmShowRules <- function(object, rules=1:object$n.trees)
{
  stopifnot(require(gbm, quietly=TRUE))
  cat(sprintf("Number of models: %d\n", object$n.trees))
  for (i in rules)
  {
    cat(sprintf("\nTree %d: \n", i))

    tmp.frame <- data.frame(object$trees[[i]])
    split.var <- tmp.frame[1,1]
    split.var.name <- object$var.names[split.var+1]
    left.predict <- if (tmp.frame[2,8] < 0) 0 else 1
    right.predict <- if (tmp.frame[3,8] < 0) 0 else 1
    miss.predict <- if  (tmp.frame[4,8] < 0) 0 else 1

    ## TODO Should get if it is a factor from object - perhaps saver to do so.
    ## object$var.names is the variable names. object$var.type != 0 => factor
    if (is.factor(crs$dataset[[split.var.name]]))
    {
      val.index <- tmp.frame[1,2]
      categories <- levels(crs$dataset[[split.var.name]])
      lf <- paste(categories[object$c.split[[val.index+1]]==-1], collapse=",")
      rh <- paste(categories[object$c.split[[val.index+1]]==1], collapse=",")

      ## TODO Replace the predict values with object$data$y if Factor.
      cat(sprintf("  %s == %s : %.0f \n",
                  split.var.name, lf, left.predict))
      cat(sprintf("  %s == %s : %.0f \n",
                  split.var.name, rh, right.predict))
      cat(sprintf("  %s missing : %.0f \n",
                  split.var.name, miss.predict))
    }
    else
    {
      split.val <- tmp.frame[1,2]

      ## TODO Replace the predict values with object$data$y if Factor.
      cat(sprintf("  %s < %.2f : %.0f \n",
                  split.var.name, split.val, left.predict))
      cat(sprintf("  %s >= %.2f : %.0f \n",
                  split.var.name,split.val,right.predict))
      cat(sprintf("  %s missing : %.0f \n",
                  split.var.name, miss.predict))
    }
  }
}

plotGBMImportance <- function()
{

  ## Make sure there is a gbm object first.

  if (is.null(crs$gbm))
  {
    errorDialog("E134: Should not be here.",
                "There is no GBM and attempting to plot importance.",
                crv$support.msg)
    return()
  }
  
  newPlot()
  plot.cmd <- paste("summary(crs$gbm, cBars=5, plotit=TRUE)\n",
                    genPlotTitleCmd("Relative Importance of Variables"),
                    sep="")

  appendLog("Plot the relative importance of the variables.", plot.cmd)
  eval(parse(text=plot.cmd))

  setStatusBar("GBM Importance has been plotted.")
}
  
