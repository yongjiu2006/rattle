# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2008-04-30 20:37:01 Graham Williams>
#
# Implement associations functionality.
#
# Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2

########################################################################
#
# CALLBACKS
#

on_tools_associate_activate <- function(action, window)
{
  crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                              crv$NOTEBOOK.ASSOCIATE.NAME))
  switchToPage(crv$NOTEBOOK.ASSOCIATE.NAME)
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
#
# SUPPORT
#

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
#
# EXECUTION
#

executeAssociateTab <- function()
{
  # We require a dataset

  if (noDatasetLoaded()) return()

  # If it looks like the VARIABLES page has not been executed, complain..

  if (variablesHaveChanged()) return()

  # Check if sampling needs executing.

  if (sampleNeedsExecute()) return()

  # Determine whether we want basket analysis of transactions or
  # rules from the categorical variables. The former is indicated if
  # there is a single IDENT variable and a TARGET with multiple
  # values. Perhaps we just see if the GUI checkbutton is set and if
  # so, check that the variables meet these criteria, and if not the
  # return. Also, on  executeSelectTab, if there is one ID and the
  # TARGET is factor or integer and there are no inputs then set the
  # default to Baskets.

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
      
  # Check that we have only categorical attributes.

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

  # Ensure the arules library is available and loaded.

  if (! packageIsAvailable("arules", "generate associations")) return()
  startLog("ASSOCIATION RULES GENERATION")
  lib.cmd <- "require(arules, quietly=TRUE)"
  appendLog("Association rules are implemented in the arules package.",
            lib.cmd)
  eval(parse(text=lib.cmd))
 
  # Initialise the textview.
  
  TV <- "associate_textview"
  resetTextview(TV)
  
  # Required information
  
  sampling   <- not.null(crs$sample)
  support    <- theWidget("associate_support_spinbutton")$getValue()
  confidence <- theWidget("associate_confidence_spinbutton")$getValue()

  # Transform data into a transactions dataset for arules.

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

  # Now generate the association rules.

  apriori.cmd <- paste("crs$apriori <<- apriori(crs$transactions, ",
                       "parameter = list(",
                       sprintf("support=%.3f, confidence=%.3f",
                               support, confidence),
                       "))", sep="")
  appendLog("Generate the association rules.",
           gsub("<<-", "<-", apriori.cmd))
  cmd.output <- collectOutput(apriori.cmd)

  # Add a summary of the rules.

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
 
  # Required information
  
  sampling  <- not.null(crs$sample)
  support <- theWidget("associate_support_spinbutton")$getValue()

  # Transform data into a transactions dataset for arules.

  # TODO 080425 Note that aprior does "as(..., 'transactions')" so we
  # don't need this here do we?
  
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

  # Now plot the relative frequencies.

  plot.cmd <- paste("itemFrequencyPlot(crs$transactions, support=",
                    support, ", cex=0.8)", sep="")
  newPlot()
  appendLog("Plot the relative frequecies.", plot.cmd)
  eval(parse(text=plot.cmd))

  setStatusBar("Generated the relative frequency plot.")
}

listAssociateRules <- function()
{
  # We require a dataset

  if (noDatasetLoaded()) return()

  # Also make sure we have already generated the association rules.
  
  if (is.null(crs$apriori))
  {
    errorDialog("You first need to generate the association rules.",
                "Perhaps you need to click the Execute button.")
    return()
  }

  # Note the textview.
  
  TV <- "associate_textview"
  
  # Required information
  
#  lift    <- theWidget("associate_lift_spinbutton")$getValue()

#  appendTextview(TV, "Top Rules\n\n",
#                 "For now, run the following command in the console:\n\n",
#                 paste('inspect(SORT(subset(crs$apriori, lift >',
#                       lift, '), by="confidence"))'))

  # I wanted to use the subset function to list just the top rules,
  # but the use of "lift" has issues when run within a library, even
  # though it is fine when loaded using source. The problems seems to
  # have been fixed by 080419 so refine this to do the right
  # thing. Some more testing 080421 indicates I'm back to the old
  # problem...

#  if (lift == 0)
    summary1.cmd <- 'inspect(SORT(crs$apriori, by="confidence"))'
#  else
#    summary1.cmd <- paste('SORT(subset(crs$apriori, lift > ',
#                          lift, '),  by="confidence")')
#ORIG    summary1.cmd <- paste('inspect(SORT(subset(crs$apriori, lift > ',
#                          lift, '),  by="confidence"))')
  appendLog("List rules.", summary1.cmd)
  # print(summary1.cmd)
  ## This returns "" 080429 when "lift > 1.3" is included in the
  ## subset command.
  result <- collectOutput(summary1.cmd)
  ## This 
##   zz <- textConnection("commandsink", "w", TRUE)
##   sink(zz)
##   cat(eval(parse(text=summary1.cmd)))
##   sink()
##   close(zz)
##   result <- paste(commandsink, collapse="\n")
  # print(result) # DEBUG
  appendTextview(TV, "Top Rules\n\n", result, "\n")
                 #"\n\nKnown Bug: If nothing appears above, ",
                 #"set the Lift to 0.0\n")
#                 paste('inspect(SORT(subset(crs$apriori, lift >',
#                       lift, '), by="confidence"))'))
  
  # This works but it lists all rules.

#  summary.cmd <- 'inspect(SORT(crs$apriori, by="confidence"))'
#  appendLog("List all rules.", summary.cmd)
#  appendTextview(TV, "All Rules\n\n", collectOutput(summary.cmd))

  setStatusBar(paste("Finished listing the rules",
                     "- scroll the text window to view the rules."))
}

########################################################################
#
# EXPORT
#

exportAssociateTab <- function()
{
  # Make sure we have already done something in Rattle.
  
  if (noDatasetLoaded()) return()

  # Make sure we have a model first!
  
  if (is.null(crs$apriori))
  {
    errorDialog("No association rules model is available. Be sure to build",
                "the model before trying to export it! You will need",
                "to press the Execute button (F5) in order to build the",
                "model.")
    return()
  }

  # Require the pmml package
  
  lib.cmd <- "require(pmml, quietly=TRUE)"
  if (! packageIsAvailable("pmml", "export associate rules")) return(FALSE)
  appendLog("Load the PMML package to export association rules.", lib.cmd)
  eval(parse(text=lib.cmd))
  
  # Obtain filename to write the PMML to.
  
  dialog <- gtkFileChooserDialog("Export PMML", NULL, "save",
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-save", GtkResponseType["accept"])

  if(not.null(crs$dataname))
    dialog$setCurrentName(paste(get.stem(crs$dataname), "_arules", sep=""))

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
  
  pmml.cmd <- "pmml(crs$apriori)"
  appendLog("Export association rules as PMML.", pmml.cmd)
  saveXML(eval(parse(text=pmml.cmd)), save.name)

  # Reduce chatter infoDialog("The PMML file", save.name, "has been written.")

  setStatusBar("The PMML file", save.name, "has been written.")
  
}
