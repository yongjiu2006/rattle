## Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2007-02-25 21:32:15 Graham>
##
## Implement associations functionality.
##
## Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2

########################################################################
##
## CALLBACKS
##

on_tools_associate_activate <- function(action, window)
{
  .NOTEBOOK$setCurrentPage(getNotebookPage(.NOTEBOOK, .NOTEBOOK.ASSOCIATE.NAME))
  switchToPage(.NOTEBOOK.ASSOCIATE.NAME)
}

on_associate_plot_frequency_button_clicked <-  function(action, window)
{
  plotAssociateFrequencies()
}

on_associate_rules_button_clicked <-  function(action, window)
{
  listAssociateRules()
}

########################################################################
##
## SUPPORT
##

generateAprioriSummary <- function(ap)
{
 result <- sprintf("Number of Rules: %d\n\n", length(ap))
 result <- paste(result, "Rule Length Distribution (LHS + RHS):\n\n", sep="")
 pp <- table(size(ap@lhs)+size(ap@rhs))
 result <- paste(result, paste(sprintf("\tRules of length %s:  %-4d",
                                           names(pp), pp), collapse="\n"),
                 "\n\n", sep="")
 return(result)
}

########################################################################
##
## EXECUTION
##

executeAssociateTab <- function()
{

  ## We require a dataset

  if (noDatasetLoaded()) return()

  ## If it looks like the VARIABLES page has not been executed, complain..

  if (variablesHaveChanged()) return()

  ## Check if sampling needs executing.

  if (sampleNeedsExecute()) return()

  ## Determine whether we want basket analysis of transactions or
  ## rules from the categorical variables. The former is indicated if
  ## there is a single IDENT variable and a TARGET with multiple
  ## values. Perhaps we just see if the GUI checkbutton is set and if
  ## so, check that the variables meet these criteria, and if not the
  ## return. Also, on  executeSelectTab, if there is one ID and the
  ## TARGET is factor or integer and there are no inputs then set the
  ## default to Baskets.

  baskets <- theWidget("associate_baskets_checkbutton")$getActive()
  if (baskets && length(crs$ident) != 1)
  {
    errorDialog("Exactly one variable must be identified as an Ident",
                "in the Variables tab to be used as",
                "the identifier of the transactions.",
                "I found", length(crs$ident), "variables.",
                "The entities need to be aggregated by the Ident to",
                "create the baskets for association analysis.")
    return()
  }
  if (baskets && length(crs$target) != 1)
  {
    errorDialog("You need to specify a Target variable in the Variables tab.",
                "This vairable then identifies the items associated with each",
                "basket or transaction in the analysis. Each basket or",
                "transaction is uniquely identified using the Ident variable.")
    return()
  }
      
  ## Check that we have only categorical attributes.

  include <- getCategoricalVariables()
  if (!baskets && length(include) == 0)
  {
    errorDialog("Associations are calculated only for categorical data.",
                "No categorical variables were found in the dataset",
                "from amongst those having an input role.",
                "If you wanted a basket analysis with the Target variable",
                "listing the items, and the Ident variable identifying",
                "the baskets, then please check the Baskets button.")
    return()
  }

  ## Ensure the arules library is available and loaded.

  if (! packageIsAvailable("arules", "generate associations")) return()
  startLog("ASSOCIATION RULES GENERATION")
  lib.cmd <- "require(arules, quietly=TRUE)"
  appendLog("Association rules are implemented in the arules package.", lib.cmd)
  eval(parse(text=lib.cmd))
 
  ## Initialise the textview.
  
  TV <- "associate_textview"
  resetTextview(TV)
  
  ## Required information
  
  sampling   <- not.null(crs$sample)
  support    <- theWidget("associate_support_spinbutton")$getValue()
  confidence <- theWidget("associate_confidence_spinbutton")$getValue()

  ## Transform data into a transactions dataset for arules.

  if (baskets)
    transaction.cmd <- paste("crs$transactions <<- as(split(",
                             sprintf('crs$dataset%s$%s, crs$dataset%s$%s',
                                     ifelse(sampling, "[crs$sample,]", ""),
                                     crs$target,
                                     ifelse(sampling, "[crs$sample,]", ""),
                                     crs$ident),
                             '), "transactions")', sep="") 
  else
    transaction.cmd <- paste("crs$transactions <<- as(",
                             sprintf('crs$dataset[%s,%s], "transactions")',
                                     ifelse(sampling, "crs$sample", ""),
                                     include), sep="")
  appendLog("Generate a transactions dataset.",
           gsub("<<-", "<-", transaction.cmd))
  eval(parse(text=transaction.cmd))

  ## Now generate the association rules.

  apriori.cmd <- paste("crs$apriori <<- apriori(crs$transactions, ",
                       "parameter = list(",
                       sprintf("support=%.3f, confidence=%.3f",
                               support, confidence),
                       "))", sep="")
  appendLog("Generate the association rules.",
           gsub("<<-", "<-", apriori.cmd))
  cmd.output <- collectOutput(apriori.cmd)

  ## Add a summary of the rules.

  mysummary.cmd <- "generateAprioriSummary(crs$apriori)"
  appendLog("Summarise the resulting rule set.", mysummary.cmd)

  summary.cmd <- "summary(crs$apriori@quality)"
  appendTextview(TV, "Summary of the Apriori Association Rules\n\n",
                 collectOutput(mysummary.cmd, use.cat=TRUE),
                 "\nSummary of the Intersting Measures\n\n",
                 collectOutput(summary.cmd, use.print=TRUE))
  
  appendTextview(TV, "Summary of the execution of the apriori command.\n",
                 cmd.output)
  
  setStatusBar("Generated the association rules.")
}

plotAssociateFrequencies <- function()
{
  ## We require a dataset

  if (noDatasetLoaded()) return()

  ## If it looks like the VARIABLES page has not been executed, complain..

  if (variablesHaveChanged()) return()

  ## Check if sampling needs executing.

  if (sampleNeedsExecute()) return()
    
  baskets <- theWidget("associate_baskets_checkbutton")$getActive()
  if (baskets && length(crs$ident) != 1)
  {
    errorDialog("Exactly one variable must be identified as an Ident",
                "in the Variables tab to be used as",
                "the identifier of the transactions.",
                "I found", length(crs$ident), "variables.",
                "The entities need to be aggregated by the Ident to",
                "create the baskets for association analysis.")
    return()
  }
  if (baskets && length(crs$target) != 1)
  {
    errorDialog("You need to specify a Target variable in the Variables tab.",
                "This vairable then identifies the items associated with each",
                "basket or transaction in the analysis. Each basket or",
                "transaction is uniquely identified using the Ident variable.")
    return()
  }

  ## Check that we have categorical attributes.

  include <- getCategoricalVariables()
  if (! baskets && length(include) == 0)
  {
    errorDialog("Associations are calculated only for categorical data.",
                "No categorical variables were found in the dataset",
                "from amongst those having an input role.",
                "If you wanted a basket analysis with the Target variable",
                "listing the items, and the Ident variable identifying",
                "the baskets, then please check the Baskets button.")
    return()
  }

  ## Ensure the arules library is available and loaded.

  if (! packageIsAvailable("arules", "generate associations")) return()
  startLog("RELATIVE FREQUENCIES PLOT")
  lib.cmd <- "require(arules, quietly=TRUE)"
  appendLog("Association rules are implemented in the arules package.", lib.cmd)
  eval(parse(text=lib.cmd))
 
  ## Required information
  
  sampling  <- not.null(crs$sample)
  support <- theWidget("associate_support_spinbutton")$getValue()

  ## Transform data into a transactions dataset for arules.
  
  if (baskets)
    transaction.cmd <- paste("crs$transactions <<- as(split(",
                             sprintf('crs$dataset%s$%s, crs$dataset%s$%s',
                                     ifelse(sampling, "[crs$sample,]", ""),
                                     crs$target,
                                     ifelse(sampling, "[crs$sample,]", ""),
                                     crs$ident),
                             '), "transactions")', sep="")
  else
    transaction.cmd <- paste("crs$transactions <<- as(",
                             sprintf('crs$dataset[%s,%s], "transactions")',
                                     ifelse(sampling, "crs$sample", ""),
                                     include), sep="")
  appendLog("Generate a transactions dataset.",
           gsub("<<-", "<-", transaction.cmd))
  eval(parse(text=transaction.cmd))

  ## Now plot the relative frequencies.

  plot.cmd <- paste("itemFrequencyPlot(crs$transactions, support=",
                    support, ", cex=0.8)", sep="")
  newPlot()
  appendLog("Plot the relative frequecies.", plot.cmd)
  eval(parse(text=plot.cmd))

  setStatusBar("Generated the relative frequency plot.")
}

listAssociateRules <- function()
{
  ## We require a dataset

  if (noDatasetLoaded()) return()

  ## Also make sure we have already generated the association rules.
  
  if (is.null(crs$apriori))
  {
    errorDialog("You first need to generate the association rules.",
                "Perhaps you need to click the Execute button.")
    return()
  }

  ## Note the textview.
  
  TV <- "associate_textview"
  
  ## Required information
  
  lift    <- theWidget("associate_lift_spinbutton")$getValue()

#  appendTextview(TV, "Top Rules\n\n",
#                 "For now, run the following command in the console:\n\n",
#                 paste('inspect(SORT(subset(crs$apriori, lift >',
#                       lift, '), by="confidence"))'))

  ## I wanted to use the subset function to list just the top rules,
  ## but the use of "lift" has issues when run within a library, even
  ## though it is fine when loaded using source.
  ##
  summary1.cmd <- paste('inspect(SORT(subset(crs$apriori, lift > ',
                       lift, '),  by="confidence"))')
  ## This suceeds.
  ##result <- eval(parse(text=summary1.cmd))
  ## This fails
  ##result <- collectOutput(summary1.cmd)
  ## This succeeds
  zz <- textConnection("commandsink", "w", TRUE)
  sink(zz)
  cat(eval(parse(text=summary1.cmd)))
  sink()
  close(zz)
  result <- paste(commandsink, collapse="\n")
  appendTextview(TV, "Top Rules\n\n", result, "\n\n",
                 "If nothing appears above, ",
                 "past the following into the console:\n\n",
                 paste('inspect(SORT(subset(crs$apriori, lift >',
                       lift, '), by="confidence"))'))
  
  ## This works but it lists all rules.
  summary.cmd <- 'inspect(SORT(crs$apriori, by="confidence"))'
  appendLog("List all rules.", summary.cmd)
  appendTextview(TV, "All Rules\n\n", collectOutput(summary.cmd))

  setStatusBar("Finished listing the rules.")
}
