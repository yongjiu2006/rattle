## Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2006-12-08 13:01:56 Graham>
##
## ADA TAB
##
## Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2

########################################################################
##
## TODO
##
## Add a Error button to "plot(crs$ada)"
## Add a Var Import button to "varplot(crs$ada)"

########################################################################
##
## CALLBACKS
##

on_ada_importance_button_clicked <- function(button)
{
  plotADAImportance()
}

########################################################################
##
## ADA - BOOSTING
##

executeModelADA <- function()
{
  ## Initial setup. 
  
  TV <- "ada_textview"
  
  ## Load the package into the library

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

  sampling <- ! is.null(crs$sample)
  including <- ! is.null(included)
  subsetting <- sampling

  ## Time the model building.
  
  start.time <- Sys.time()
  
  ## Build a model.

  model.cmd <- paste("crs$ada <<- ada(", frml, ", data=crs$dataset",
                     if (subsetting) "[",
                     if (sampling) "crs$sample",
                     if (subsetting) ",",
                     if (including) included,
                     if (subsetting) "]",
                     #ifelse(is.null(crs$weights), "",
                     #       sprintf(", weights=(%s)%s",
                     #               crs$weights,
                     #               ifelse(sampling, "[crs$sample]", ""))),
                     #', method="class"',
                     #ifelse(is.null(parms), "", parms),
                     #ifelse(is.null(control), "", control),
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

  ## Finish up.
  
  time.taken <- Sys.time()-start.time
  time.msg <- sprintf("Time taken: %0.2f %s", time.taken, time.taken@units)
  addTextview(TV, "\n", time.msg, textviewSeparator())
  addToLog(time.msg)
  setStatusBar("An adaboost model has been generated.")
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
    errorDialog("E129: Should not be here.",
                "There is no GBM and attempting to plot importance.",
                "Please report to",
                "Graham.Williams@togaware.com")
    return()
  }
  
  newPlot()
  plot.cmd <- paste("summary(crs$gbm, cBars=5, plotit=TRUE)\n",
                    genPlotTitleCmd("Relative Importance of Variables"),
                    sep="")

  addToLog("Plot the relative importance of the variables.", plot.cmd)
  eval(parse(text=plot.cmd))

  setStatusBar("GBM Importance has been plotted.")
}
  
