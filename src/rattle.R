# Gnome R Data Miner: GNOME interface to R for Data Mining
##
## Time-stamp: <2007-02-21 22:04:15 Graham>
##
## Copyright (c) 2006 Graham Williams, Togaware.com, GPL Version 2
##
## The Rattle package is made of of the following R source files:
##
## execute.R	The Execute functionality.
## paradigm.R	Display and hide tabs depending on paradigm radio buttons
##

MAJOR <- "2"
MINOR <- "1"
REVISION <- unlist(strsplit("$Revision$", split=" "))[2]
VERSION <- paste(MAJOR, MINOR, REVISION, sep=".")
COPYRIGHT <- "Copyright (C) 2006 Graham.Williams@togaware.com, GPL"

## Acknowledgements: Frank Lu has provided much feedback and has
## extensively tested the application. Many colleagues at the
## Australian Taxation Office have used Rattle and made many and
## varied suggestions. These include Anthony Nolan, Stuart Hamilton,
## Liyin Zue, Weiqiang Lin, Robert Williams, Shawn Wicks, Ray Lindsay.

## LICENSE
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version. See the file gpl-license.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

## STYLE GUIDE
##
##    Use the "_" convention only for Glade variables and functions.
##    Use capitalised verbs for own functions: displayPlotAgain
##    Use dot spearated words for variables: list.of.frames, lib.cmd
##    RGtk2 uses the capitalised word convention.
##    Use same names in R code as for the Glade objects.
##    Hide global variables, all capitalised, by beginning with "."

## INTERFACE STYLE
##
##    Should the philosophy be to have them active, and check
##    conditions on execute, rather than messing around turning things
##    on and off?
##
##    If the functionality is not yet implemented, full stop, then
##    have the interface item(s) greyed out, as an indication that the
##    functionality is to come. Probably not a good idea. The
##    expectation is that perhaps there is some way within the
##    interface of getting it not to be greyed out! But doing this
##    also encourages those with an interest in the greyed out bits to
##    either complain (i.e., I get to know what is wanted) or else
##    help implement them!
##
##    If the functionality is not appropriate in a particular
##    circumstance then don't grey it out. Simply check, in the one
##    place in the code (e.g., when the button is pushed) and pop up
##    an error dialogue.
##
##    This doesn't always work, as in the case of sample where you do
##    want greyed out functionality, but you don't want it to mean not
##    yet implemented.

## BUGS
##
##   Tooltips are not working on GNU/Linux. Just fine on MS/Windows.
##
##   The RGtk2 author, Michael Lawrence, notes that most of the GUI
##   functionality in Gnome (i.e., libgnome and libgnomeui) will soon
##   be merged into GTK. At that time, that functionality will be part
##   of RGtk2.

########################################################################
##
## INITIALISATIONS

rattle <- function()
{

  require(RGtk2, quietly=TRUE) # From http://www.ggobi.org/rgtk2/

  ## Keep the loading of Hmisc quiet.
  options(Hverbose=FALSE)

  ## Load the Rattle GUI specification. The three commands here
  ## represent an attempt to be independent of where R is running and
  ## where rattle.R is located by finding out from the system calls the
  ## actual call to source rattle.R, and then point to this same
  ## location for finding rattle.glade. Assumes the call to source is
  ## something like: source("abc/def/rattle.R"). The better alternative
  ## might be to tell people to use the chdir=TRUE option in source.
  
  ##s <- as.character(sys.calls())
  ##n <- grep("source", s)
  ##p <- gsub("\.R..$", ".glade", gsub("source..", "", s[n]))

  ## Try firstly to load the glade file from the installed rattle
  ## package, if it exists. Otherwise, look locally.
  
  result <- try(etc <- file.path(.path.package(package="rattle")[1], "etc"),
                silent=TRUE)
  if (inherits(result, "try-error"))
    rattleGUI<<-gladeXMLNew("rattle.glade",root="rattle_window")
  else
    rattleGUI<<-gladeXMLNew(file.path(etc,"rattle.glade"),root="rattle_window")

  ## Some default GUI settings

  #id.string <- sprintf("<i>Rattle  Version %s  togaware.com</i>", VERSION)
  id.string <- paste('<span foreground="blue">',
                     '<i>Rattle</i> ',
                     '<i>Version ', VERSION, '</i> ',
                     '<i><span underline="single">togaware.com</span></i>',
                     '</span>')
  rattle.menu <- theWidget("rattle_menu")
  rattle.menu$SetRightJustified(TRUE)
  #rattle.menu$getChild()$setText(id.string)
  #rattle.menu$getChild()$setUseMarkup(TRUE)
  rattle.menu$getChild()$setMarkup(id.string)
  #rattle.menu$getChild()$setText(id.string)

########################################################################

  ## Constants: I would like these available within this package, but
  ## not outside? Do I use assign in some way? That is, how to keep
  ## these constants within the package only.
  
  ## TODO Put these constants into the top level of this file, defined
  ## as NULL. Then keep these double arrow assignments here. I think
  ## then that they will stay with the package, but not be in
  ## .GlobalEnv because the package scope will be found before the top
  ## level.
  
########################################################################

  ## PACKAGE GLOBAL CONSTANTS

  ## These are double arrow assigned here to place them in .GlobalEnv. I
  ## couldn't figure out an easy way to keep them scoped locally.
  
  ## Various Treeview Columns

  .COLUMN <<- c(number = 0, variable = 1, type = 2, input = 3,
               target = 4, risk = 5, ident = 6, ignore = 7, comment = 8)

  .IMPUTE <<- c(number=0, variable=1, zero=2, comment=3)
  
  .CATEGORICAL <<- c(number = 0, variable = 1, barplot = 2,
                    dotplot = 3, comment = 4)

  .CONTINUOUS <<-  c(number = 0, variable = 1, boxplot = 2,
                    hisplot = 3, cumplot = 4, benplot = 5, comment = 6)
  
  ## MODELLERS

  .GLM   <<- "glm"
  .RPART <<- "rpart"
  ##GBM <<- "gbm"
  .ADA   <<- "ada"
  .RF    <<- "rf"
  .SVM   <<- "svm"
  .KSVM  <<- "ksvm"
  .NNET  <<- "nnet"

  .MODELLERS <<- c(.RPART, .ADA, .RF, .KSVM, .GLM, .NNET)
  
  ## RPART
  
  .RPART.CP.DEFAULT        <<- 0.010
  .RPART.MINSPLIT.DEFAULT  <<- 20
  .RPART.MINBUCKET.DEFAULT <<- 7
  .RPART.MAXDEPTH.DEFAULT  <<- 30

  .ADA.NTREE.DEFAULT   <<- 50
  
  .RF.NTREE.DEFAULT    <<- 500
  .RF.MTRY.DEFAULT     <<- 10
  .RF.SAMPSIZE.DEFAULT <<- ""
  
  ## MISC
  
  .START.LOG.COMMENT <<- "\n\n## "	# Assume paste with sep=""
  .LOG.COMMENT       <<- "\n## "	# Assume paste with sep=""
  .END.LOG.COMMENT   <<- "\n\n"	# Assume paste with sep=""
  
########################################################################
  
  ## PACKAGE STATE VARIABLE
  
  ## Global variables are generally a bad idea, but until a better idea
  ## comes to mind.
  
  crs <<- list(dataset=NULL,
               dataname=NULL,
               cwd=getwd(),
               input=NULL,
               target=NULL,
               weights=NULL,
               risk=NULL,
               ident=NULL,
               ignore=NULL,
               sample=NULL,
               sample.seed=NULL,
               kmeans=NULL,
               kmeans.seed=NULL,
               hclust=NULL,
               page="",
               smodel=NULL, # Record whether the sample has been modelled
               glm=NULL,
               rpart=NULL,
               ada=NULL,
               rf=NULL,
               svm=NULL,
               ksvm=NULL,
               perf=NULL,
               eval=NULL,
               testset=NULL,
               testname=NULL,
               alog=NULL	# Record of interaction - useful?
               )

  ## Main notebook related constants and widgets.  Track the widgets
  ## that are needed for removing and inserting tabs in the notebook,
  ## depending on the selected paradigm.
  
  .NOTEBOOK               <<- theWidget("notebook")

  .NOTEBOOK.DATA.NAME      <<- "Data"

  .NOTEBOOK.EXPLORE.NAME   <<- "Explore"

  .NOTEBOOK.VARIABLES.NAME <<- "Variables"

  .NOTEBOOK.SAMPLE.NAME    <<- "Transform"

  .NOTEBOOK.CLUSTER.NAME    <<- "Cluster"
  .NOTEBOOK.CLUSTER.WIDGET <<- theWidget("cluster_tab_widget")
  .NOTEBOOK.CLUSTER.LABEL  <<- theWidget("cluster_tab_label")

  .NOTEBOOK.ASSOCIATE.NAME    <<- "Associate"
  .NOTEBOOK.ASSOCIATE.WIDGET <<- theWidget("associate_tab_widget")
  .NOTEBOOK.ASSOCIATE.LABEL  <<- theWidget("associate_tab_label")

  .NOTEBOOK.MODEL.NAME     <<- "Model"
  .NOTEBOOK.MODEL.WIDGET  <<- theWidget("model_tab_widget")
  .NOTEBOOK.MODEL.LABEL   <<- theWidget("model_tab_label")

  .NOTEBOOK.EVALUATE.NAME    <<- "Evaluate"
  .NOTEBOOK.EVALUATE.WIDGET <<- theWidget("evaluate_tab_widget")
  .NOTEBOOK.EVALUATE.LABEL  <<- theWidget("evaluate_tab_label")

  .NOTEBOOK.LOG.NAME       <<- "Log"

  ## Pages that are common to all paradigms.

  .NOTEBOOK.COMMON.NAMES <<- c(.NOTEBOOK.DATA.NAME,
                              .NOTEBOOK.SAMPLE.NAME,
                              .NOTEBOOK.VARIABLES.NAME,
                              .NOTEBOOK.LOG.NAME)
  
  ## DATA tab pages.

  .DATA              <<- theWidget("data_notebook")
  .DATA.CSV.TAB      <<- getNotebookPage(.DATA, "csv")
  .DATA.RDATA.TAB    <<- getNotebookPage(.DATA, "rdata")
  .DATA.RDATASET.TAB <<- getNotebookPage(.DATA, "rdataset")
  .DATA.ODBC.TAB     <<- getNotebookPage(.DATA, "odbc")

  .TRANSFORM               <<- theWidget("transform_notebook")
  .TRANSFORM.SAMPLE.TAB    <<- getNotebookPage(.TRANSFORM, "sample")
  .TRANSFORM.IMPUTE.TAB    <<- getNotebookPage(.TRANSFORM, "impute")
  .TRANSFORM.FACTORISE.TAB <<- getNotebookPage(.TRANSFORM, "factorise")
  .TRANSFORM.OUTLIER.TAB   <<- getNotebookPage(.TRANSFORM, "outlier")
  
  .EXPLORE                 <<- theWidget("explore_notebook")
  .EXPLORE.SUMMARY.TAB     <<- getNotebookPage(.EXPLORE, "summary")
  .EXPLORE.PLOT.TAB        <<- getNotebookPage(.EXPLORE, "explot")
  .EXPLORE.GGOBI.TAB       <<- getNotebookPage(.EXPLORE, "ggobi")
  .EXPLORE.CORRELATION.TAB <<- getNotebookPage(.EXPLORE, "correlation")
  .EXPLORE.HIERCOR.TAB     <<- getNotebookPage(.EXPLORE, "hiercor")
  .EXPLORE.PRCOMP.TAB      <<- getNotebookPage(.EXPLORE, "prcomp")
  
  .CLUSTER            <<- theWidget("cluster_notebook")
  .CLUSTER.KMEANS.TAB <<- getNotebookPage(.CLUSTER, "kmeans")
  .CLUSTER.HCLUST.TAB <<- getNotebookPage(.CLUSTER, "hclust")
  
  .MODEL           <<- theWidget("model_notebook")
  .MODEL.RPART.TAB <<- getNotebookPage(.MODEL, .RPART)
  .MODEL.GLM.TAB   <<- getNotebookPage(.MODEL, .GLM)
  .MODEL.ADA.TAB   <<- getNotebookPage(.MODEL, .ADA)
  ## .MODEL.GBM.TAB   <<- getNotebookPage(.MODEL, .GBM)
  .MODEL.RF.TAB    <<- getNotebookPage(.MODEL, .RF)
  .MODEL.SVM.TAB   <<- getNotebookPage(.MODEL, .SVM)
  .MODEL.NNET.TAB   <<- getNotebookPage(.MODEL, .NNET)

  .SVMNB           <<- theWidget("svm_notebook")
  .SVMNB.ESVM.TAB  <<- getNotebookPage(.SVMNB, "esvm")
  .SVMNB.KSVM.TAB  <<- getNotebookPage(.SVMNB, "ksvm")
  
  .EVALUATE                 <<- theWidget("evaluate_notebook")
  .EVALUATE.CONFUSION.TAB   <<- getNotebookPage(.EVALUATE, "confusion")
  .EVALUATE.RISK.TAB        <<- getNotebookPage(.EVALUATE, "risk")
  .EVALUATE.LIFT.TAB        <<- getNotebookPage(.EVALUATE, "lift")
  .EVALUATE.ROC.TAB         <<- getNotebookPage(.EVALUATE, "roc")
  .EVALUATE.PRECISION.TAB   <<- getNotebookPage(.EVALUATE, "precision")
  .EVALUATE.SENSITIVITY.TAB <<- getNotebookPage(.EVALUATE, "sensitivity")
  
########################################################################
  
  ## Now connect the callbacks
  
  gladeXMLSignalAutoconnect(rattleGUI)
  
  ## A friendly startup message in the status bar
  
  ##setStatusBar("Select a CSV filename to load into Rattle to get started.")
  
  ## Some initialisations
  
  initialiseVariableViews()
  
  ## Turn off the sub-notebook tabs.
  
  .DATA$setShowTabs(FALSE)
  .TRANSFORM$setShowTabs(FALSE)
  .EXPLORE$setShowTabs(FALSE)
  .CLUSTER$setShowTabs(FALSE)
  .MODEL$setShowTabs(FALSE)
  .EVALUATE$setShowTabs(FALSE)
  
  ## Set glm_family_comboboxentry to default value.
  
  theWidget("glm_family_comboboxentry")$setActive(0)
  
  ## Tell MS/Windows to use 2GB (TODO - What's needed under Win64?)
  
  if (isWindows())
  {
    require(utils) # In case rattle is loaded in .Rprofile, before utils.
    memory.limit(2073)
  }
  
  ##

  addInitialLogMessage()
  
  ## By default the CLUSTER page is not showing.

  ## Don't turn this off until we move away from using the numeric tab
  ## variables above, since a Execute on the Model tab runs the
  ## Cluster tab :-)

  .NOTEBOOK$removePage(getNotebookPage(.NOTEBOOK, .NOTEBOOK.CLUSTER.NAME))
  .NOTEBOOK$removePage(getNotebookPage(.NOTEBOOK, .NOTEBOOK.ASSOCIATE.NAME))

  while (gtkEventsPending()) gtkMainIteration() # Make sure window is displayed

}

resetRattle <- function()
{
  ## Cleanup various bits of Rattle, as when a new dataset is loaded
  ## or a project is loaded. Might also be useful for the New button.

  ## Initialise CRS

  crs$dataset  <<- NULL
  crs$dataname <<- NULL
  crs$input    <<- NULL
  crs$target   <<- NULL
  crs$weights  <<- NULL
  crs$risk     <<- NULL
  crs$ident    <<- NULL
  crs$ignore   <<- NULL
  crs$sample   <<- NULL
  crs$sample.seed <<- NULL
  crs$kmeans   <<- NULL
  crs$kmeans.seed <<- NULL
  crs$hclust   <<- NULL
  crs$page     <<- ""
  crs$smodel   <<- NULL
  crs$glm      <<- NULL
  crs$rpart    <<- NULL
  crs$ada      <<- NULL
  crs$rf       <<- NULL
  crs$svm      <<- NULL
  crs$ksvm     <<- NULL
  crs$perf     <<- NULL
  crs$eval     <<- NULL
  crs$testset  <<- NULL
  crs$testname <<- NULL

  ## Clear all now outdated text views

  setTextview("data_textview")
  setTextview("summary_textview")
  setTextview("impute_textview")
  setTextview("correlation_textview")
  setTextview("prcomp_textview")
  setTextview("kmeans_textview")
  setTextview("hclust_textview")
  setTextview("rpart_textview")
  setTextview("glm_textview")
  ##setTextview("gbm_textview")
  setTextview("ada_textview")
  setTextview("rf_textview")
  setTextview("esvm_textview")
  setTextview("ksvm_textview")
  setTextview("confusion_textview")
  setTextview("roc_textview")

  ## Reset some textviews back to standard text.

  setTextview("impute_textview",
  gsub("\n", " ",
       "Click the Execute Button (or Menu or F5) to obtain a summary of the
missing values in the data. In the resulting summary, we will see a
matrix with headings corresponding to the variables in the data. The
body of the matrix includes 1's and 0's, with a 0 indicating missing
values. The rows of the matrix show the missing values for each
variable in combination with the other variables."))
  
  ## Set all sub tabs back to the default tab page and reflect this in
  ## the appropriate radio button.

  .TRANSFORM$setCurrentPage(.TRANSFORM.SAMPLE.TAB)
  theWidget("sample_radiobutton")$setActive(TRUE)
  
  .EXPLORE$setCurrentPage(.EXPLORE.SUMMARY.TAB)
  theWidget("summary_radiobutton")$setActive(TRUE)

  .CLUSTER$setCurrentPage(.CLUSTER.KMEANS.TAB)
  theWidget("kmeans_radiobutton")$setActive(TRUE)

  .MODEL$setCurrentPage(.MODEL.RPART.TAB)
  theWidget("rpart_radiobutton")$setActive(TRUE)
  #theWidget("all_models_radiobutton")$setActive(TRUE)

  .EVALUATE$setCurrentPage(.EVALUATE.CONFUSION.TAB)
  theWidget("confusion_radiobutton")$setActive(TRUE)

  ## Reset the VARIABLES tab.
  
  theWidget("variables_treeview")$getModel()$clear()
  theWidget("impute_treeview")$getModel()$clear()
  theWidget("categorical_treeview")$getModel()$clear()
  theWidget("continuous_treeview")$getModel()$clear()

  theWidget("weight_entry")$setText("")
  theWidget("rpart_weights_label")$setText("Weights:")
  
  ## Reset MODEL:RPART
  
  theWidget("rpart_priors_entry")$setText("")
  theWidget("rpart_loss_entry")$setText("")
  theWidget("rpart_minsplit_spinbutton")$setValue(.RPART.MINSPLIT.DEFAULT)
  theWidget("rpart_maxdepth_spinbutton")$setValue(.RPART.MAXDEPTH.DEFAULT)
  theWidget("rpart_cp_spinbutton")$setValue(.RPART.CP.DEFAULT)
  theWidget("rpart_minbucket_spinbutton")$setValue(.RPART.MINBUCKET.DEFAULT)
  makeRPartSensitive(FALSE)

  ## Reset MODEL:ADA
  
  makeAdaSensitive(FALSE)

  ## Reset MODEL:RF
  
  makeRandomForestSensitive(FALSE)
  
  ## Update EXPLORE, MODEL and EVALUATE targets

  theWidget("explot_target_label")$setText("No target selected")

  theWidget("glm_target_label")$setText("No target selected")
  theWidget("rpart_target_label")$setText("No target selected")
  ##theWidget("gbm_target_label")$setText("No target selected")
  theWidget("ada_target_label")$setText("No target selected")
  theWidget("rf_target_label")$setText("No target selected")
  theWidget("svm_target_label")$setText("No target selected")
  theWidget("evaluate_risk_label")$setText("No risk variable selected")
  
  theWidget("evaluate_training_radiobutton")$setActive(TRUE)
  theWidget("evaluate_filechooserbutton")$setFilename("")
  theWidget("evaluate_rdataset_combobox")$setActive(-1)

  theWidget("rpart_evaluate_checkbutton")$setActive(FALSE)
  theWidget("rf_evaluate_checkbutton")$setActive(FALSE)
  theWidget("ksvm_evaluate_checkbutton")$setActive(FALSE)
  theWidget("glm_evaluate_checkbutton")$setActive(FALSE)
  ## theWidget("gbm_evaluate_checkbutton")$setActive(FALSE)
  theWidget("ada_evaluate_checkbutton")$setActive(FALSE)

  theWidget("rpart_evaluate_checkbutton")$setSensitive(FALSE)
  theWidget("rf_evaluate_checkbutton")$setSensitive(FALSE)
  theWidget("ksvm_evaluate_checkbutton")$setSensitive(FALSE)
  theWidget("glm_evaluate_checkbutton")$setSensitive(FALSE)
  ## theWidget("gbm_evaluate_checkbutton")$setSensitive(FALSE)
  theWidget("ada_evaluate_checkbutton")$setSensitive(FALSE)

}

## UTILITIES

"%notin%" <- function(x,y) ! x %in% y

not.null <- function(x) ! is.null(x)

## Common Dialogs

debugDialog <- function(...)
{
  dialog <- gtkMessageDialogNew(NULL, "destroy-with-parent", "info", "ok",
                                "Debug Message:", ...)
  connectSignal(dialog, "response", gtkWidgetDestroy)
}

infoDialog <- function(...)
{
  dialog <- gtkMessageDialogNew(NULL, "destroy-with-parent", "info", "close",
                                ...)
  connectSignal(dialog, "response", gtkWidgetDestroy)
}

warnDialog <- function(...)
{
  dialog <- gtkMessageDialogNew(NULL, "destroy-with-parent", "warn", "close",
                                ...)
  connectSignal(dialog, "response", gtkWidgetDestroy)
}

errorDialog <- function(...)
{
  dialog <- gtkMessageDialogNew(NULL, "destroy-with-parent", "error", "close",
                                ...)
  connectSignal(dialog, "response", gtkWidgetDestroy)
}

questionDialog <- function(...)
{
  dialog <- gtkMessageDialogNew(NULL, "destroy-with-parent", "question",
                                "yes-no",
                                ...)
  result <- dialog$run()
  dialog$destroy()
  if (result == GtkResponseType["yes"])
    return("yes")
  else
    return(NULL)
}

notImplemented <- function(action, window)
{
  ## Popup a little information window for non-implemented functions.

  aname <- action$getName()
  result <- try(atype <- action$typeName(), silent=TRUE)
  if (inherits(result, "try-error")) atype <- NULL
  
  infoDialog(sprintf(paste("The function you activated (via %s)",
                            "%s is not yet implemented."),
                      aname,
                     ifelse(is.null(atype), "", sprintf("of type %s", atype))))
#  infoDialog(sprintf(paste("The function you activated (via %s)",
#                            "of type %s is not yet implemented."),
#                      action$getName(), action$typeName()))
}

noDatasetLoaded <- function()
{
  ## Popup an error dialog if no dataset has been loaded, and return
  ## TRUE, otherwise return FALSE.

  if (is.null(crs$dataset))
  {
    errorDialog("No dataset has been loaded at this time.",
                 "At a minimum, please load a dataset from the Data tab",
                 "before attempting any other operation.",
                 "Be sure to Execute the Data tab once the",
                 "data source has been specified.")
    return(TRUE)
  }
  else
    return(FALSE)
}

variablesHaveChanged <- function(action)
{
  if (length(crs$ignore) != length(getSelectedVariables("ignore")) ||
      length(crs$ident) != length(getSelectedVariables("ident")) ||
      length(crs$input) != length(getSelectedVariables("input")))
  {
    errorDialog("You have made changes to the selected variables in the",
                 "Variables tab, but have not Executed the Variables tab.",
                 "Please do so before", paste(action, ".", sep=""))
    return(TRUE)
  }
  else
    return(FALSE)
}

packageIsAvailable <- function(pkg, msg=NULL)
{
  if (pkg %notin% rownames(installed.packages()))
  {
    if (not.null(msg))
      infoDialog("The package", pkg, "is required to",
                 paste(msg, ".", sep=""),
                 "It does not appear to be installed.",
                 "Please consider installing it, perhaps with the",
                 "R command",
                 sprintf('install.packages("%s")', pkg),
                 "to use the full",
                 "functionality of Rattle.")
    return(FALSE)
  }
  else
    return(TRUE)
}

sampleNeedsExecute <- function()
{
  ## Popup an error dialog if sampling needs to be executed and return
  ## TRUE.

  ## If sampling is active, make sure there is a sample.
  
  if (theWidget("sample_checkbutton")$getActive()
      && is.null(crs$sample))
  {
    errorDialog("Sampling is active but has not been Executed.",
                    "Either ensure you Execute the sampling by clicking",
                    "the Execute button on the Sample tab,",
                    "or else de-activate Sampling on the Sample tab.")
    return(TRUE)
  }

  ## If sampling is inactive, make sure there is no sample.

  if (! theWidget("sample_checkbutton")$getActive()
      && not.null(crs$sample))
  {
    errorDialog("Sampling is inactive but has not been Executed",
                 "since being made inactive.",
                 "Please ensure you Execute the Sample tab",
                 "after de-activating the Sampling.")
        return(TRUE)
  }

  return(FALSE)
}

########################################################################
##
## Simplify updates to status bar
##

setRattleTitle <- function(title)
{
  standard <- "Rattle: Effective Data Mining with R"
  if (is.null(title))
    theWidget("rattle_window")$setTitle(standard)
  else
    theWidget("rattle_window")$setTitle(sprintf("%s: %s", standard, title))
}

setStatusBar <- function(..., sep=" ")
{
  msg <- paste(sep=sep, ...)
  if (length(msg) == 0) msg <-""
  theWidget("statusbar")$push(1, msg)
  while (gtkEventsPending()) gtkMainIteration() # Refresh status and windows
  invisible(NULL)
}

collectOutput <- function(command, use.print=FALSE, use.cat=FALSE,
                          width=getOption("width"))
{
  ## TODO Should this use cat or print? Cat translates the \n to a
  ## newline and doesn't precede the output by [1].  For pretty output
  ## with sprintf() you probably want cat(), but if you have a vector
  ## of formatted text and you want to look at it (as data), print()
  ## would be better.

  owidth <- getOption("width")
  options(width=width)
  if (use.print)
    command <- paste("print(", command, ")", sep="")
  else if (use.cat)
    command <- paste("cat(", command, ")", sep="")
  zz <- textConnection("commandsink", "w", TRUE)
  sink(zz)
  result <- try(eval(parse(text=command)))
  sink()
  close(zz)
  if (inherits(result, "try-error"))
  {
    errorDialog(sprintf("A Rattle command has failed: %s.", command),
                 "The action you requested has not been completed.",
                 "Refer to the R Console for details.")
    commandsink <- "FAILED"
  }
  options(width=owidth)
  return(paste(commandsink, collapse="\n"))
}

########################################################################
##
## Miscellaneous Support
##

theWidget <- function(widget)
{
  return(rattleGUI$getWidget(widget))
}

getNotebookPage <- function(notebook, label)
{
  ## Obtain the notebook page number given its tab's label's text.
  ## Return NULL if the label is not found.

  for (i in 0:(notebook$getNPages()-1))
   if (notebook$getTabLabelText(notebook$getNthPage(i)) == label)
     return(i)
  return(NULL)
}

getCurrentPageLabel <- function(nb)
{
  return(nb$getTabLabelText(nb$getNthPage(nb$getCurrentPage())))
}

isWindows <- function()
{
  return(.Platform$OS.type == "windows")
}

listBuiltModels <- function()
{
#  return(! (is.null(crs$glm) && is.null(crs$rpart) &&
#            is.null(crs$gbm) && is.null(crs$rf) &&
#            is.null(crs$svm)))
  models <- c()
  for (m in .MODELLERS)
    if (not.null(eval(parse(text=sprintf("crs$%s", m)))))
      models <- c(models, m)
  return(models)
}

setDefaultPath <- function(filename)
{
  if (not.null(filename))
  {
    crs$cwd <<- dirname(filename)
    setwd(crs$cwd)
  }
}

newPlot <- function(pcnt=1)
{
  ## Trial the use of the Cairo device. This was the only place I
  ## needed to change to switch over to the Cairo device. As backup,
  ## revert to the x11() or windows() device.

  if (require("cairoDevice", quietly=TRUE))
  {
    plotGUI <- gladeXMLNew("rattle.glade", root="plot_window")
    gladeXMLSignalAutoconnect(plotGUI)
    da <- plotGUI$getWidget("drawingarea")
    asCairoDevice(da)
    plotGUI$getWidget("plot_window")$setTitle(paste("Rattle: Plot", dev.cur()))
  }
  else if (.Platform$GUI %in% c("X11", "unknown"))
  {
    ## Add "unknown" to handle the case with the littler script
    ## interface which runs with an "unknown" GUI.

    x11()
  }
  else if (isWindows())
    windows()

  if (pcnt==1)
    layout(matrix(c(1), 1, 1, byrow=TRUE))
  else if (pcnt==2)
    layout(matrix(c(1,2), 2, 1, byrow=TRUE))
  else if (pcnt==3)
    layout(matrix(c(1,1,2,3), 2, 2, byrow=TRUE))
  else if (pcnt==4)
    layout(matrix(c(1,2,3,4), 2, 2, byrow=TRUE))
  else if (pcnt==5)
    layout(matrix(c(1,1,2,3,4,5), 2, 3, byrow=TRUE))
  else if (pcnt==6)
    layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow=TRUE))
  else if (pcnt==7)
    layout(matrix(c(1,1,2,3,3,4,5,6,7), 3, 3, byrow=TRUE))
  else if (pcnt==8)
    layout(matrix(c(1,1,2,3,4,5,6,7,8), 3, 3, byrow=TRUE))
  else if (pcnt==9)
    layout(matrix(c(1,2,3,4,5,6,7,8,9), 3, 3, byrow=TRUE))
}

########################################################################

on_plot_save_button_clicked <- function(action)
{
  ## To know which window we are called from we extract the plot
  ## number from the window title!!!. This then ensures we save the
  ## right device.
  ##
  ## Also, export to pdf (from Cairo) is not too good it seems. Gets a
  ## grey rather than white background. PNG and JPEG look just fine.
  
  ttl <- action$getParent()$getParent()$getParent()$getParent()$getTitle()
  devnum <- as.integer(sub("Rattle: Plot ", "", ttl))
  savePlot(devnum)
}

on_plot_print_button_clicked <- function(action)
{
  infoDialog("The Print button is not yet implemented.")
}


on_plot_close_button_clicked <- function(action)
{
  ttl <- action$getParent()$getParent()$getParent()$getParent()$getTitle()
  devnum <- as.integer(sub("Rattle: Plot ", "", ttl))
  dev.off(devnum)
  pw <- action$getParentWindow()
  pw$destroy()
}

savePlot <- function(device=NULL, name="plot")
{
  if (is.null(dev.list()))
  {
    warnDialog("There are currently no active graphics devices.",
               "So there is nothing to export!",
               "Please Execute (F5) to obtain a plot to export.")
    return()
  }

  # Obtain a filename to save to. Ideally, this would also prompt for
  # the device to export, and the fontsize, etc.

  dialog <- gtkFileChooserDialog("Export Graphics (pdf, png, jpg)",
                                 NULL, "save",
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-save", GtkResponseType["accept"])

  if(not.null(crs$dataname))
    dialog$setCurrentName(paste(get.stem(crs$dataname),
                                "_", name, ".pdf", sep=""))

  ff <- gtkFileFilterNew()
  ff$setName("Graphics Files")
  ff$addPattern("*.pdf")
  ff$addPattern("*.png")
  ff$addPattern("*.jpg")
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

  if (get.extension(save.name) == "") save.name <- sprintf("%s.pdf", save.name)
    
  if (file.exists(save.name))
    if (is.null(questionDialog("A Graphics file of the name", save.name,
                                "already exists. Do you want to overwrite",
                                "this file?")))
      return()
  
  cur <- dev.cur()
  if (! is.null(device)) dev.set(device)
  ext <- get.extension(save.name)
  if (ext == "pdf")
    ## Set version to 1.4 since dev.copy from a Cairo device needs
    ## this.  It is done automatically with a warning anyhow, but
    ## might as well avoid the warning so as not to worry anyone.
    dev.copy(pdf, file=save.name, width=7, height=7, version="1.4")
  else if (ext == "png")
    dev.copy(png, file=save.name, width=700, height=700)
  else if (ext == "jpg")
    dev.copy(jpeg, file=save.name, width=700, height=700)
  dev.off()
  dev.set(cur)
  
  infoDialog(sprintf("Rattle: Plot %d", ifelse(is.null(device), cur, device)),
             "has been exported to", save.name)
}
  
########################################################################

genPlotTitleCmd <- function(..., vector=FALSE)
{
  main = paste(...)
  if(vector)
  {
    sub = sprintf("Rattle %s %s", Sys.time(), Sys.info()["user"])
    return(c(main, sub))
  }
  else
  {  
    sub = sprintf('paste("Rattle", Sys.time(), Sys.info()["user"])')
    return(sprintf('title(main="%s", sub=%s)', main, sub))
  }
}

set.cursor <- function(cursor="left-ptr")
{
  theWidget("rattle_window")$getWindow()$
  setCursor(gdkCursorNew(cursor))
}

simplifyNumberList <- function(nums)
{
  ## Convert 3 4 6 7 8 9 10 12 14 16 17 18 19 21 to
  ## "3:4,6:10,12,14,16:19,21"

  if (length(nums) == 1)
    return(sprintf("%s", nums))
  else if (is.null(nums) || length(nums) == 0)
    return(NULL)

  result <- ""
  start <- nums[1]
  len <- 1

  for (i in 2:length(nums))
  {
    if (nums[i] != start + len)
    {
      if (len == 1)
        result <- sprintf("%s,%d", result, start)
      else
        result <- sprintf("%s,%d:%d", result, start, nums[i-1])
      start <- nums[i]
      len <- 1
    }
    else
      len <- len + 1
  }

  if (len == 1)
    result <- sprintf("%s,%d", result, start)
  else
    result <- sprintf("%s,%d:%d", result, start, nums[i])

  result <- sub('c\\(,', 'c(', sprintf("c(%s)", result))
  return(result)
}
    
get.extension <- function(path)
{
  ## Extract and return the extension part of a filename
  
  parts <- strsplit(path, "\\.")[[1]]
  if (length(parts) > 1)
    last <- parts[length(parts)]
  else
    last <- ""
  last
}

get.stem <- function(path)
{
  parts <- strsplit(basename(path), "\\.")[[1]]
  last <- paste(parts[1:length(parts)-1], collapse=".")
  last
}

plotNetwork <- function(flow)
{
  if (! packageIsAvailable("network", "draw the network plot")) return()
  require(network, quietly=TRUE)
  
  flow.net <- network(as.matrix(flow))

  ## Change the line widths to represent the magnitude of the flow.
  ## Use a log transform to get integers for the line widths.

  flow.log <- log10(flow) # Log 10 to get magnitude
  flow.log[flow.log==0] <- 1 # Set any 0's to 1 as the base case
  flow.log[flow.log==-Inf] <- 0 # Set resulting -Infinty (log10(0)) values to 0
  flow.mag <- round(flow.log) # Round them to 

  ## Add color to indicate the magnitude.  Use heat colours to
  ## indicate the magnitude of the flow, from yellow to red.

  heat <- rev(heat.colors(max(flow.mag)))
  flow.col <- flow.mag
  for (i in 1:length(heat)) flow.col[flow.col==i] <- heat[i]
  flow.col <- sapply(flow.col, as.character)
  
  ## Record the magnitude of flow coming into any label and use this to
  ## scale the entity labels. 

  entity.sizes <- round(log10(apply(flow, 2, sum)))
  entity.sizes[entity.sizes==-Inf] <- 0
  entity.sizes <- 1 + entity.sizes-min(entity.sizes)
  entity.sizes <- 1 + entity.sizes/max(entity.sizes)

  ## A warning that "par()$cxy * label.cex" have missmatched
  ## dimensions. par()$cxy is of length 2? Should be 1?
  
  suppressWarnings(plot(flow.net, displaylabels=TRUE, usecurve=TRUE,
                        mode="circle",
                        edge.lwd=flow.mag*1.5, edge.col=flow.col,
                        label.cex=entity.sizes, label.border=0))

  eval(parse(text=genPlotTitleCmd("Network Map of Flows")))

}

########################################################################
##
## Shared callbacks
##

## Update a combo box with just the available data frames

update_comboboxentry_with_dataframes <- function(action, window)
{
  #cat("XXX Update Combobox XXX\n")
  current <- theWidget("rdataset_combobox")$getActiveText()
  
  dl <- unlist(sapply(ls(sys.frame(0)),
                      function(x)
                      {
                        cmd <- sprintf("is.data.frame(%s)", x)
                        var <- try(ifelse(eval(parse(text=cmd), sys.frame(0)),
                                          x, NULL), silent=TRUE)
                        if (inherits(var, "try-error"))
                          var <- NULL
                        return(var)
                      }))
  if (not.null(dl))
  {
    action$getModel()$clear()
    lapply(dl, action$appendText)
    ## Set the selection to that which was already selected, if possible.
    if (not.null(current) && current %in% dl)
      action$setActive(which(sapply(dl, function(x) x==current))[1]-1)
  }
}

quit_rattle <- function(action, window)
{
  ## Don't remove the graphics for now. In moving to the Cairo device,
  ## this blanks the device, but does not destroy the containing
  ## window. I wonder if there is some way to get a list of the plot
  ## windows, and destroy each one?

  ## graphics.off() # for (i in dev.list()) dev.off(i)

  theWidget("rattle_window")$destroy()
  
  ## Communicate to R that Rattle has finished. This is used by the
  ## rattle script on GNU/Linux using the littler package which allows
  ## one to use R as a scripting language. But rattle dispatches
  ## itself from R, and so normally the script immediately
  ## terminates. Instead we can have a loop that checks if rattleGUI
  ## is NULL, and when it is we finish! Seems to work.

  rattleGUI <<- NULL
}

########################################################################
##
## DATA TAB
##

##----------------------------------------------------------------------
##
## Interface Actions
##
display_click_execute_message <- function(button)
{
  #cat("XXX Display Click Execute message XXX\n")
  theWidget("data_textview")$setWrapMode("word")
  setTextview("data_textview",
               "Now click the Execute button to load the dataset.",
               "\n\nAny R errors will be displayed in the R Console. ",
               "Check the R Console if nothing seems to happen ",
               "after clicking the Execute button. ",
               "Be aware that large datasets do take some time to load.")
  setStatusBar()
}

on_csv_radiobutton_toggled <- function(button)
{
  #cat("XXX CSV Radio Toggle XXX\n")
  if (button$getActive())
  {
    .DATA$setCurrentPage(.DATA.CSV.TAB)
  }
  setStatusBar()
}

on_csv_filechooserbutton_update_preview <- function(button)
{
  if (length(button$listFilters()) == 0)
  {
    ff <- gtkFileFilterNew()
    ff$setName("CSV Files")
    ff$addPattern("*.csv")
    button$addFilter(ff)
    
    ff <- gtkFileFilterNew()
    ff$setName("TXT Files")
    ff$addPattern("*.txt")
    button$addFilter(ff)
    
    ff <- gtkFileFilterNew()
    ff$setName("All Files")
    ff$addPattern("*")
    button$addFilter(ff)
  }
  # CAN'T GO HERE - NEED ANOTHER CALLBACK button$setCurrentFolder(crs$cwd)
}

on_rdata_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    .DATA$setCurrentPage(.DATA.RDATA.TAB)
  }
  setStatusBar()
}

on_rdata_filechooserbutton_update_preview<- function(button)
{
  #button$setCurrentFolder(crs$cwd)

  if (length(button$listFilters()) == 0)
  {
    ff <- gtkFileFilterNew()
    ff$setName("Rdata Files")
    ff$addPattern("*.R[Dd]ata")
    button$addFilter(ff)

    ff <- gtkFileFilterNew()
    ff$setName("All Files")
    ff$addPattern("*")
    button$addFilter(ff)
  }
}

## A callback for when the file name has changed. Load the
## corresponding .Rdata file.

load_rdata_set_combo <- function(button)
{
  TV <- "data_textview"
  
  ## Collect relevant data

  filename <- theWidget("rdata_filechooserbutton")$getFilename()
  setDefaultPath(filename)
  
  ## Fix filename for MS - otherwise eval/parse strip the \\.

  if (isWindows()) filename <- gsub("\\\\", "/", filename)

  ## Generate commands to read the data and then display the structure.

  load.cmd <- sprintf('load("%s")', filename)

  ## Start logging and executing the R code.

  addLogSeparator()

  addToLog("Load an Rdata file containing R objects.", load.cmd)
  set.cursor("watch")
  eval(parse(text=paste("new.objects <- ", load.cmd)), baseenv())
  set.cursor()

  ## Add new dataframes to the combo box.

  combobox <- theWidget("rdata_combobox")
  if (not.null(new.objects))
  {
    combobox$getModel()$clear()
    lapply(new.objects, combobox$appendText)
  }
  
  theWidget(TV)$setWrapMode("word")
  clearTextview(TV)
  appendTextview(TV, "Now select a data frame from those available.")
  setStatusBar()

}

on_rdataset_radiobutton_toggled <- function(button)
{
  #cat("XXX R Dataset Radio toggled XXX\n")
  if (button$getActive())
  {
    .DATA$setCurrentPage(.DATA.RDATASET.TAB)
  }
  setStatusBar()
}

on_odbc_radiobutton_toggled <- function(button)
{
  if (button$getActive()) .DATA$setCurrentPage(.DATA.ODBC.TAB)
  setStatusBar()
}

open_odbc_set_combo <- function(a, b)
{
  ## DESCRIPTION
  ## A callback for when the ODBC DNS name has changed.
  ##
  ## DETAIL Load the corresponding tables from the specified ODBC
  ## database.

  TV <- "data_textview"
  ## Close previous channel

  if (not.null(crs$odbc)) close(crs$odbc)
  
  ## Obtain name of the DNS.

  DNSname <- theWidget("odbc_dns_entry")$getText()
  
  ## Generate commands to connect to the database and retrieve the tables.

  lib.cmd <- sprintf("require(RODBC, quietly=TRUE)")
  connect.cmd <- sprintf('crs$odbc <<- odbcConnect("%s")', DNSname)
  tables.cmd  <- sprintf('sqlTables(crs$odbc)$TABLE_NAME')
  
  ## Start logging and executing the R code.

  if (! packageIsAvailable("RODBC", "connect to an ODBC database")) return()
      
  addLogSeparator()

  addToLog("Require the RODBC library", lib.cmd)
  eval(parse(text=lib.cmd))
       
  addToLog("Open the connection to the ODBC service.",
          gsub('<<-', '<-', connect.cmd))
  result <- try(eval(parse(text=connect.cmd)))
  if (inherits(result, "try-error"))
  {
    errorDialog("The attempt to open the ODBC connection failed.",
                "Please check that the DNS is correct.",
                "See the R Console for further details.")
    return()
  }
  
  addToLog("Load the names of available tables.", tables.cmd)
  set.cursor("watch")
  result <- try(eval(parse(text=paste("tables <<- ", tables.cmd))))
  set.cursor()
  if (inherits(result, "try-error"))
  {
    errorDialog("The attempt to query the ODBC connection failed.",
                "Please check that the DNS is correct.",
                "See the R Console for further details.")
    return()
  }

  ## Add list of tables to the combo box.

  combobox <- theWidget("odbc_combobox")
  if (not.null(tables))
  {
    combobox$getModel()$clear()
    lapply(tables, combobox$appendText)
  }
  
  theWidget(TV)$setWrapMode("word")
  clearTextview(TV)
  appendTextview(TV, "Now select a table from those available.")
  setStatusBar()

}

##----------------------------------------------------------------------
##
## Execution
##
executeDataTab <- function()
{
  if (theWidget("csv_radiobutton")$getActive())
    executeDataCSV()
  else if (theWidget("odbc_radiobutton")$getActive())
    executeDataODBC()
  else if (theWidget("rdata_radiobutton")$getActive())
    executeDataRdata()
  else if (theWidget("rdataset_radiobutton")$getActive())
    executeDataRdataset()
}

resetVariableRoles <- function(variables, nrows, input=NULL, target=NULL,
                               risk=NULL, ident=NULL, ignore=NULL,
                               zero=NULL,
                               boxplot=NULL,
                               hisplot=NULL, cumplot=NULL, benplot=NULL,
                               barplot=NULL, dotplot=NULL,
                               resample=TRUE)
{
  ## Update the variables treeview with the dataset variables.

  createVariablesModel(variables, input, target, risk, ident, ignore, zero,
                       boxplot, hisplot, cumplot, benplot, barplot, dotplot)

  if (resample)
  {
    ## Turn sampling on, set range bounds and generate the default 70%
    ## sample. Do the range bounds first since otherwise the value
    ## gets set back to 1. Also, need to set both the percentage and
    ## the count since if the old percentage is 70 and the new is 70,
    ## then no change in value is noticed, and thus the count is not
    ## automatically updated.

    per <- 70
    srows <- round(nrows * per / 100)
    theWidget("sample_checkbutton")$setActive(TRUE)
    theWidget("sample_count_spinbutton")$setRange(1,nrows)
    theWidget("sample_count_spinbutton")$setValue(srows)
    theWidget("sample_percentage_spinbutton")$setValue(per)

    executeTransformSample()
  }

}

executeDataCSV <- function()
{
  TV <- "data_textview"
  
  ## Collect relevant data

  filename <- theWidget("csv_filechooserbutton")$getFilename()
  setDefaultPath(filename)
  
  ## Error exit if no filename is given

  if (is.null(filename))
  {
    errorDialog("No CSV Filename has been chosen yet.",
                 "You must choose one before execution.",
                 "Change the radio button selection if you prefer to link",
                 "to a dataset already loaded into the R Console.")
    return()
  }

  ## If there is a model warn about losing it.

  if ( not.null(listBuiltModels()) )
  {
    if (is.null(questionDialog("You have chosen to load a dataset.",
                               "This will clear the old project (dataset and",
                               "models) which has not been saved.",
                               "Do you wish to continue, and lose the old",
                               "project? If you choose not to continue",
                               "you can save the project, and then load",
                               "the new dataset.")))
        
      return()
  }

  ## Fix filename for MS - otherwise eval/parse strip the \\.

  if (isWindows()) filename <- gsub("\\\\", "/", filename)

  ## Get the separator to use.

  sep = theWidget("csv_separator_entry")$getText()
  if (sep != ",")
    sep <- sprintf(', sep="%s"', sep)
  else
    sep <- ""

  ## Check whether we expect a header or not.

  if (theWidget("csv_header_checkbutton")$getActive())
    hdr <- ""
  else
    hdr <- ", header=FALSE"
  
  nastring <- ', na.strings=c(".", "NA")'
  
  ## Generate commands to read the data and then display the structure.

  read.cmd <- sprintf('crs$dataset <<- read.csv("%s"%s%s%s)',
                      filename, hdr, sep, nastring)
  str.cmd  <- "str(crs$dataset)"
  
  ## Start logging and executing the R code.

  addLogSeparator()
  theWidget(TV)$setWrapMode("none") # On for welcome msg
  clearTextview(TV)
  
  addToLog("LOAD CSV FILE", gsub('<<-', '<-', read.cmd))
  resetRattle()
  eval(parse(text=read.cmd))
  crs$dataname <<- basename(filename)
  setRattleTitle(crs$dataname)

  addToLog("Display a simple summary (structure) of the dataset.", str.cmd)
  appendTextview(TV, sprintf("Structure of %s.\n\n", filename),
                  collectOutput(str.cmd))
  
  ## Update the variables treeview and samples.

  resetVariableRoles(colnames(crs$dataset), nrow(crs$dataset)) 

  setStatusBar("The CSV data has been loaded:", crs$dataname)
}

executeDataODBC <- function()
{
  TV <- "data_textview"

  table <- theWidget("odbc_combobox")$getActiveText()

  ## Error if no table from the database has been chosen.
  
  if (is.null(table))
  {
    errorDialog("No table has been specified.",
                 "Please identify the name of the table you wish to load.",
                 "All tables in the connected database are listed",
                 "once a connection is made.")
    return()
  }
  
  ## If there is a model warn about losing it.

  if ( not.null(listBuiltModels()) )
  {
    if (is.null(questionDialog("You have chosen to load a dataset.",
                               "This will clear the old project (dataset and",
                               "models) which has not been saved.",
                               "Do you wish to continue, and lose the old",
                               "project? If you choose not to continue",
                               "you can save the project, and then load",
                               "the new dataset.")))
        
      return()
  }

  assign.cmd <- "crs$dataset <<- sqlFetch(crs$odbc, table)"
  str.cmd  <- "str(crs$dataset)"

  numRows <- sqlQuery(crs$odbc, sprintf("SELECT count(*) FROM %s", table))

  DNSname <- theWidget("odbc_dns_entry")$getText()
  if (numRows > 50000)
    if (is.null(questionDialog("You are about to extract", numRows,
                               "rows from the table", table,
                               "of the", DNSname, "ODBC connection.",
                               "That's quite a few for R to load into memory.",
                               "Do you wish to continue?")))
        
      return()
  
  ## Start logging and executing the R code.

  addLogSeparator()
  theWidget("data_textview")$setWrapMode("none") # On for welcome msg
  clearTextview(TV)
  
  addToLog("LOAD FROM DATABASE TABLE",
          gsub('<<-', '<-', assign.cmd))
  resetRattle()
  eval(parse(text=assign.cmd))
  crs$dataname <<- table
  setRattleTitle(crs$dataname)

  addToLog("Display a simple summary (structure) of the dataset.", str.cmd)
  appendTextview(TV,
                  sprintf("Structure of %s from %s.\n\n", table, DNSname),
                  collectOutput(str.cmd))
  
  ## Update the variables treeview and samples.

  resetVariableRoles(colnames(crs$dataset), nrow(crs$dataset)) 

  setStatusBar("The ODBC data has been loaded:", crs$dataname)

}

executeDataRdata <- function()
{
  TV <- "data_textview"
  
  ## Collect relevant data

  filename <- theWidget("rdata_filechooserbutton")$getFilename()
  setDefaultPath(filename)
  dataset <- theWidget("rdata_combobox")$getActiveText()

  ## Error exit if no filename is given

  if (is.null(filename))
  {
    errorDialog("No Rdata Filename has been chosen yet.",
                 "You must choose one before execution.",
                 "Change the radio button selection if you prefer to link",
                 "to a dataset already loaded into the R Console,",
                 "or to load a dataset from a CSV file.")
    return()
  }

  ## Error if no dataset from the Rdata file has been chosen.
  
  if (is.null(dataset))
  {
    errorDialog("No R dataset name has been specified.",
                 "Please identify the name of the R dataset.",
                 "Any data frames that were found in the loaded Rdata",
                 "file are available to choose from in the Data Name",
                 "combo box.")
    return()
  }

  ## If there is a model warn about losing it.

  if ( not.null(listBuiltModels()) )
  {
    if (is.null(questionDialog("You have chosen to load a dataset.",
                               "This will clear the old project (dataset and",
                               "models) which has not been saved.",
                               "Do you wish to continue, and lose the old",
                               "project? If you choose not to continue",
                               "you can save the project, and then load",
                               "the new dataset.")))
        
      return()
  }

  ## Generate commands.
  
  assign.cmd <- sprintf('crs$dataset <<- %s', dataset)
  str.cmd  <- "str(crs$dataset)"
  
  ## Start logging and executing the R code.

  addLogSeparator()
  theWidget("data_textview")$setWrapMode("none") # On for welcome msg
  clearTextview(TV)
  
  addToLog("LOAD RDATA FILE",
          gsub('<<-', '<-', assign.cmd))
  resetRattle()
  eval(parse(text=assign.cmd))
  crs$dataname <<- dataset
  setRattleTitle(crs$dataname)

  addToLog("Display a simple summary (structure) of the dataset.", str.cmd)
  appendTextview(TV,
                  sprintf("Structure of %s from %s.\n\n", dataset, filename),
                  collectOutput(str.cmd))
  
  ## Update the variables treeview and samples.

  resetVariableRoles(colnames(crs$dataset), nrow(crs$dataset)) 

  setStatusBar("The data has been loaded:", crs$dataname)
}

executeDataRdataset <- function()
{
  TV <- "data_textview"
  
  ## Collect relevant data
  dataset <- theWidget("rdataset_combobox")$getActiveText()
  
  if (is.null(dataset))
  {
    errorDialog("No R dataset name has been specified.",
                 "Please identify the name of the R dataset.",
                 "Any data frames that exist in the R Console",
                 "are available to choose from in the Data Name",
                 "combo box.")
    return()
  }

  ## Check if there is a model first and then warn about losing it.

  if ( not.null(listBuiltModels()) )
  {
    if (is.null(questionDialog("You have chosen to load a new dataset",
                               "into Rattle.",
                               "This will clear the old project (dataset and",
                               "models) which has not been saved.",
                               "Do you wish to continue, and lose the old",
                               "project? If you choose not to continue",
                               "you can save the project, and then load",
                               "the new dataset.")))
        
      return()
  }

  ## Generate commands.

  assign.cmd <- sprintf('crs$dataset <<- %s', dataset)
  str.cmd <- "str(crs$dataset)"
  
  ## Start logging and executing the R code.

  addLogSeparator()
  theWidget(TV)$setWrapMode("none") # On for welcome msg
  clearTextview(TV)
  
  addToLog("LOAD R DATA FRAME",
          gsub('<<-', '<-', assign.cmd))
  resetRattle()
  eval(parse(text=assign.cmd))
  crs$dataname <<- dataset
  setRattleTitle(crs$dataname)
  
  addToLog("Display a simple summary (structure) of the dataset.", str.cmd)
  setTextview(TV, sprintf("Structure of %s.\n\n", dataset),
               collectOutput(str.cmd), sep="")

  ## Update the variables treeview and samples.

  resetVariableRoles(colnames(crs$dataset), nrow(crs$dataset)) 

  setStatusBar("The data has been assigned into Rattle.")
}

########################################################################
##
## VARIABLES TAB
##
## The VARIABLES Execute will build the list of variable roles and
## stores these in crs$input, crs$ident, crs$ignore, crs$target, and
## crs$risk. This is then used in MODEL to limit the dataset in the
## call to rpart to just the crs$input variables.  In EVALUATE the
## crs$risk is used for the Risk Chart.
##

##------------------------------------------------------------------------
##
## Interface
##

item.toggled <- function(cell, path.str, model)
{

  ## The data passed in is the model used in the treeview.

  checkPtrType(model, "GtkTreeModel")

  ## Extract the column number of the model that has changed.

  column <- cell$getData("column")

  ## Get the current value of the corresponding flag
  
  path <- gtkTreePathNewFromString(path.str) # Current row
  iter <- model$getIter(path)$iter           # Iter for the row
  current <- model$get(iter, column)[[1]]    # Get data from specific column

  ## Only invert the current value if it is False - work like a radio button

  if (! current)
  {
    model$set(iter, column, !current)

    ## Uncheck all other Roles for this row, acting like radio buttons.
    
    columns <- .COLUMN[["input"]]:.COLUMN[["ignore"]]
    lapply(setdiff(columns, column), function(x) model$set(iter, x, FALSE))

    ## TODO Now fix up other buttons. Any in the same column, if it is
    ## Target, must be unchecked and the corresponding row
    ## made Ignore. Currently, just check this on Execute and
    ## complain. Can we use groups?


  }
}

on_variables_toggle_ignore_button_clicked <- function(action, window)
{
  ## Set the ignore flag for all selected variables, and ensure all
  ## other roles are unchecked.

  ##ptm <- proc.time()
  set.cursor("watch")
  tree.selection <- theWidget("variables_treeview")$getSelection()

  ## Under MS/Windows with Terminal Services to the host we get very
  ## slow redraws? Tried fixing it with freezeUpdates and thawUpdates
  ## but it had no impact. Changing 500 variables takes 5
  ## seconds. When connected over terminal services the elapsed time
  ## is 16 seconds, still with 5 seconds user time.
  
  ## theWidget("rattle_window")$getWindow()$freezeUpdates()

  tree.selection$selectedForeach(function(model, path, iter)
  {
    model$set(iter, .COLUMN[["ignore"]], TRUE)
    columns <- setdiff(.COLUMN[["input"]]:.COLUMN[["ignore"]], .COLUMN[["ignore"]])

    ## Timing indicates the for loop is slower on GNU/Linux but faster
    ## on MS/Windows 500! But the extra test also slows things down,
    ## so best not to conditionalise for now.

    #if (isWindows())
      for (c in columns)
        if (model$get(iter, c)[[1]]) model$set(iter, c, FALSE)
    #else
    #  lapply(columns, function(x) model$set(iter, x, FALSE))

    return(FALSE) # Keep going through all rows
  })

  ##cat("->Ig", proc.time() - ptm, "\n")
  set.cursor()

  ## theWidget("rattle_window")$getWindow()$thawUpdates()
}

on_variables_toggle_input_button_clicked <- function(action, window)
{
  ## Set the input flag for all selected variables, and ensure all
  ## other roles are unchecked.

  ##ptm <- proc.time()
  set.cursor("watch")

  treeview <- theWidget("variables_treeview")
  tree.selection <- treeview$getSelection()
  #theWidget("rattle_window")$getWindow()$freezeUpdates()

  tree.selection$selectedForeach(function(model, path, iter)
  {
    model$set(iter, .COLUMN[["input"]], TRUE)
    columns <- setdiff(.COLUMN[["input"]]:.COLUMN[["ignore"]], .COLUMN[["input"]])

    #if (isWindows())
      for (c in columns)
        if (model$get(iter, c)[[1]]) model$set(iter, c, FALSE)
    #else
    #  lapply(columns, function(x) model$set(iter, x, FALSE))

    return(FALSE) # Keep going through all rows
  })

  ##cat("->In", proc.time() - ptm, "\n")
  set.cursor()
  #theWidget("rattle_window")$getWindow()$thawUpdates()
}

##----------------------------------------------------------------------
##
## Execution
##

executeVariablesTab <- function()
{
  
  ## Can not do any preparation if there is no dataset.

  if (noDatasetLoaded()) return()

  input   <- getSelectedVariables("input")
  target  <- getSelectedVariables("target")
  risk    <- getSelectedVariables("risk")
  ident   <- getSelectedVariables("ident")
  ignore  <- getSelectedVariables("ignore")
  weights <- theWidget("weight_entry")$getText()
  if (weights == "") weights <- NULL
  
  ## Fail if there is more than one target

  if (length(target) > 1)
  {
    errorDialog("More than a single target has been identified (",
                 paste(sprintf("%s:%s",
                               getSelectedVariables("target", FALSE),
                               target), collapse=" "),
                 "). Only a single target is allowed.",
                 sep="")
    return()
  }
  
  ## Fail if the Target does not look like a taget.

##   if (not.null(target) && is.numeric(crs$dataset[[target]]) &&
##       length(levels(as.factor(crs$dataset[[target]]))) > 20)
##   {
##     errorDialog("The column selected for your target",
##                  sprintf("(%s)", crs$dataset[[target]]),
##                  "is numeric and has more than 20 distinct values.",
##                  "Please select a column with fewer classes.",
##                  "Regresion modelling is not currently supported.")
##     return()
##   }

  ## Fail if there is more than one risk

  if (length(risk) > 1)
  {
    errorDialog("More than a single risk column has been identified (",
                 paste(sprintf("%s:%s",
                               getSelectedVariables("risk", FALSE),
                               risk), collapse=" "),
                 "). Only a single risk column is allowed.",
                 sep="")
    return()
  }

  ## Fail if the Risk column is not numeric.

  if (not.null(risk) && ! is.numeric(crs$dataset[[risk]]))
  {
    errorDialog("The column selected for your risk",
                 sprintf("(%s)", crs$dataset[[risk]]),
                 "is not numeric. Please select a numeric column.")
    return()
  }

  ## Obtain a list of variables and R functions in the Weight Calculator

  if (not.null(weights) && nchar(weights) > 0)
  {
    identifiers <- unlist(strsplit(weights, "[^a-zA-Z._]"))
    identifiers <- identifiers[nchar(identifiers) > 0]
    identifiers <- union(identifiers,identifiers) # Each var/id just once
    funs <- unlist(lapply(identifiers,
                          function(x)
                          {
                            try(eval(parse(text=sprintf("class(%s)", x))),
                                silent=TRUE) == "function"}))
    vars <- ! funs

    allvars <- union(input, union(target, union(risk, union(ident, ignore))))
    for (i in 1:sum(vars))
    {
      ## Check for any missing variables

      if (identifiers[vars][i] %notin% allvars)
      {
        errorDialog("The Weight Calculator contains the variable",
                     identifiers[vars][i], "which is not known in the",
                     "dataset.")
        return()
      }

      ## Check if Weight variables are not ignored, and inform user if not

      if (identifiers[vars][i] %notin%
                        union(ident, union(target, union(ignore, risk))))
      {
        infoDialog("You have used the variable",
                    identifiers[vars][i],
                    "in the weights formula but it is an input.",
                    "This is unusual since it is both an input variable",
                    "and used to weight the outputs.",
                    "Rattle suggests you ignore the variable.")
      }
      
      ## For each Weights variable, replace with full reference to
      ## crs$dataset, since the variable is ignored.

      weights <- gsub(identifiers[vars][i],
                      sprintf("crs$dataset$%s", identifiers[vars][i]),
                      weights)
    
    }
  }
  
  ## Record appropriate information.
  
  crs$input   <<- input
  crs$target  <<- target
  crs$risk    <<- risk
  crs$ident   <<- ident
  crs$ignore  <<- ignore
  crs$weights <<- weights
  
  ## Update MODEL targets

  the.target <- sprintf("Target: %s", ifelse(is.null(crs$target),
                                             "None", crs$target))

  theWidget("explot_target_label")$setText(the.target)

  theWidget("rpart_target_label")$setText(the.target)
  theWidget("rf_target_label")$setText(the.target)
  theWidget("svm_target_label")$setText(the.target)
  theWidget("glm_target_label")$setText(the.target)
  ## theWidget("gbm_target_label")$setText(the.target)
  theWidget("ada_target_label")$setText(the.target)

  ## Update MODEL weights

  if (not.null(crs$weights))
  {
    weights.display <- gsub('crs\\$dataset\\$', '', crs$weights)
    the.weight <- sprintf("Weights: %s", weights.display)
    theWidget("rpart_weights_label")$setText(the.weight)
  }
  
  ## Update EVALUATE risk variable
  
  theWidget("evaluate_risk_label")$setText(crs$risk)

  ## Update defaults tha rely on the number of variables.
  
  .RF.MTRY.DEFAULT <<- floor(sqrt(length(crs$input)))
  theWidget("rf_mtry_spinbutton")$setValue(.RF.MTRY.DEFAULT)

  ## Finished - update the status bar.
  
  setStatusBar("Choice of variable characterics noted.",
                "There are", length(crs$input), "input variables.")
}

getSelectedVariables <- function(role, named=TRUE)
{
  ## DESCRIPTION
  ## Generate a list of variables marked with the specified role.
  ##
  ## ARGUMENTS
  ## role  = a string naming the role to query on
  ## named = if TRUE return variable names as strings, if FALSE, numbers
  ##
  ## DETAILS The variables_treeview, categorical_treeview and
  ## continuous_treeview are places where a variable can be identified
  ## as having a given role. Whilst the role of "ignore" is common
  ## across all three treeviews, only the ignore from the main
  ## variables_treeview is considered. If a role is not found, simply
  ## return NULL, rather than an error (for no particular reason).
  ##
  ## ASSUMPTIONS The variable and number columns are assumed to be the
  ## same in each of .COLUMNS, .CATEGORICAL, and .CONTINUOUS.

  variables <- NULL

  if (role %in% c("input", "target", "risk", "ident", "ignore"))
  {
    model <- theWidget("variables_treeview")$getModel()
    rcol  <- .COLUMN[[role]]
  }

  else if (role %in% c("boxplot", "hisplot", "cumplot", "benplot"))
  {
    model <- theWidget("continuous_treeview")$getModel()
    rcol  <- .CONTINUOUS[[role]]
  }

  else if (role %in% c("barplot", "dotplot"))
  {
    model <- theWidget("categorical_treeview")$getModel()
    rcol  <- .CATEGORICAL[[role]]
  }

  else if (role %in% c("zero"))
  {
    model <- theWidget("impute_treeview")$getModel()
    rcol  <- .IMPUTE[[role]]
  }
  
  else
    return(variables)

  vcol <- .COLUMN[["variable"]]
  ncol <- .COLUMN[["number"]]
  model$foreach(function(model, path, iter, data)
                {
                  flag <- model$get(iter, rcol)[[1]]
                  if (named)
                    variable <- model$get(iter, vcol)[[1]]
                  else
                    variable <- model$get(iter, ncol)[[1]]
                  if (flag) variables <<- c(variables, variable)
                  return(FALSE) # Keep going through all rows
                })
  return(variables)
}

initialiseVariableViews <- function()
{

  ## Define the models.
  model <- gtkListStoreNew("gchararray", "gchararray", "gchararray",
                           "gboolean", "gboolean", "gboolean", "gboolean",
                           "gboolean", "gchararray")

  impute <- gtkListStoreNew("gchararray", "gchararray", "gboolean",
                            "gchararray")
  
  continuous <- gtkListStoreNew("gchararray", "gchararray",
                                "gboolean", "gboolean",
                                "gboolean", "gboolean", "gchararray")
  
  
  categorical <- gtkListStoreNew("gchararray", "gchararray",
                                 "gboolean", "gboolean",
                                 "gchararray")
  
  
  ## View the model through the treeview in the VARIABLES tab
  treeview <- theWidget("variables_treeview")
  treeview$setModel(model)

  impview <- theWidget("impute_treeview")
  impview$setModel(impute)
  
  catview <- theWidget("categorical_treeview")
  catview$setModel(categorical)
  
  conview <- theWidget("continuous_treeview")
  conview$setModel(continuous)

  ## Add the NUMBER column as the row number.

  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  col.offset <-
    treeview$insertColumnWithAttributes(-1,
                                        "No.",
                                        renderer,
                                        text= .COLUMN[["number"]])
  
  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  imp.offset <-
    impview$insertColumnWithAttributes(-1,
                                       "No.",
                                       renderer,
                                       text= .IMPUTE[["number"]])
  
  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  cat.offset <-
    catview$insertColumnWithAttributes(-1,
                                       "No.",
                                       renderer,
                                       text= .CATEGORICAL[["number"]])
  
  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  con.offset <-
    conview$insertColumnWithAttributes(-1,
                                       "No.",
                                       renderer,
                                       text= .CONTINUOUS[["number"]])
  
  ## Add the VARIABLE NAME column to the views.
  
  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  col.offset <-
    treeview$insertColumnWithAttributes(-1,
                                        "Variable",
                                        renderer, 
                                        text = .COLUMN[["variable"]])

  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  imp.offset <-
    impview$insertColumnWithAttributes(-1,
                                       "Variable",
                                       renderer, 
                                       text = .IMPUTE[["variable"]])

  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  cat.offset <-
    catview$insertColumnWithAttributes(-1,
                                       "Variable",
                                       renderer, 
                                       text = .CATEGORICAL[["variable"]])

  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  con.offset <-
    conview$insertColumnWithAttributes(-1,
                                       "Variable",
                                       renderer, 
                                       text = .CONTINUOUS[["variable"]])

  ## Add the TYPE column.

  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  col.offset <-
    treeview$insertColumnWithAttributes(-1,
                                        "Data Type",
                                        renderer,
                                        text = .COLUMN[["type"]])
  
  ## Add the INPUT column.

  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(radio = TRUE)
  renderer$set(width = 60)
  renderer$setData("column", .COLUMN["input"])
  connectSignal(renderer, "toggled", item.toggled, model)
  col.offset <-
    treeview$insertColumnWithAttributes(-1,
                                        "Input",
                                        renderer,
                                        active = .COLUMN[["input"]])
  
  ## Add the TARGET column.

  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(radio = TRUE)
  renderer$set(width = 60)
  renderer$setData("column", .COLUMN["target"])
  connectSignal(renderer, "toggled", item.toggled, model)
  col.offset <-
    treeview$insertColumnWithAttributes(-1,
                                        "Target",
                                        renderer,
                                        active = .COLUMN[["target"]])
  
  ## Add the RISK column.

  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(radio = TRUE)
  renderer$set(width = 60)
  renderer$setData("column", .COLUMN["risk"])
  connectSignal(renderer, "toggled", item.toggled, model)
  col.offset <-
    treeview$insertColumnWithAttributes(-1,
                                        "Risk",
                                        renderer,
                                        active = .COLUMN[["risk"]])
  
  ## Add the IDENT column.

  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(radio = TRUE)
  renderer$set(width = 60)
  renderer$setData("column", .COLUMN["ident"])
  connectSignal(renderer, "toggled", item.toggled, model)
  col.offset <-
    treeview$insertColumnWithAttributes(-1,
                                        "Ident",
                                        renderer,
                                        active = .COLUMN[["ident"]])
  
  ## Add the IGNORE column (the Ignore check button) to the view.

  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(radio = TRUE)
  renderer$set(width = 60)
  renderer$setData("column", .COLUMN["ignore"])
  connectSignal(renderer, "toggled", item.toggled, model)
  col.offset <-
    treeview$insertColumnWithAttributes(-1,
                                        "Ignore",
                                        renderer,
                                        active = .COLUMN[["ignore"]]) 

  ## Add the ZERO column to the IMPUTE view.

  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(radio = TRUE)
  renderer$set(width = 60)
  renderer$setData("column", .IMPUTE["zero"])
  connectSignal(renderer, "toggled", imp_toggled, impute)
  imp.offset <-
    impview$insertColumnWithAttributes(-1,
                                       "Zero/Missing",
                                        renderer,
                                        active = .IMPUTE[["zero"]]) 

  ## Add the barplot and dotplot.

  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(width = 60)
  renderer$setData("column", .CATEGORICAL["barplot"])
  connectSignal(renderer, "toggled", cat_toggled, categorical)
  cat.offset <-
    catview$insertColumnWithAttributes(-1,
                                       "Bar Plot",
                                       renderer,
                                       active = .CATEGORICAL[["barplot"]])
  

  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(width = 60)
  renderer$setData("column", .CATEGORICAL["dotplot"])
  connectSignal(renderer, "toggled", cat_toggled, categorical)
  cat.offset <-
    catview$insertColumnWithAttributes(-1,
                                       "Dot Plot",
                                       renderer,
                                       active = .CATEGORICAL[["dotplot"]])
  
  ## Add the boxplot, hisplot, cumplot, benplot buttons

  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(width = 60)
  renderer$setData("column", .CONTINUOUS["boxplot"])
  connectSignal(renderer, "toggled", con_toggled, continuous)
  con.offset <-
    conview$insertColumnWithAttributes(-1,
                                       "Box Plot",
                                       renderer,
                                       active = .CONTINUOUS[["boxplot"]])
  
  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(width = 60)
  renderer$setData("column", .CONTINUOUS["hisplot"])
  connectSignal(renderer, "toggled", con_toggled, continuous)
  con.offset <-
    conview$insertColumnWithAttributes(-1,
                                       "Histogram",
                                       renderer,
                                       active = .CONTINUOUS[["hisplot"]])
  
  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(width = 60)
  renderer$setData("column", .CONTINUOUS["cumplot"])
  connectSignal(renderer, "toggled", con_toggled, continuous)
  con.offset <-
    conview$insertColumnWithAttributes(-1,
                                       "Cumulative",
                                       renderer,
                                       active = .CONTINUOUS[["cumplot"]])
  
  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(width = 60)
  renderer$setData("column", .CONTINUOUS["benplot"])
  connectSignal(renderer, "toggled", con_toggled, continuous)
  con.offset <-
    conview$insertColumnWithAttributes(-1,
                                       "Benford",
                                       renderer,
                                       active = .CONTINUOUS[["benplot"]])
  
  ## Add the COMMENT column.

  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  col.offset <-
    treeview$insertColumnWithAttributes(-1,
                                        "Comment",
                                        renderer,
                                        text = .COLUMN[["comment"]])
  
  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  imp.offset <-
    impview$insertColumnWithAttributes(-1,
                                       "Data Type and Number Missing",
                                        renderer,
                                        text = .IMPUTE[["comment"]])

  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  cat.offset <-
    catview$insertColumnWithAttributes(-1,
                                       "Levels",
                                       renderer,
                                       text = .CATEGORICAL[["comment"]])

  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  con.offset <-
    conview$insertColumnWithAttributes(-1,
                                       "Min; Median/Mean; Max",
                                       renderer,
                                       text = .CONTINUOUS[["comment"]])

  ## Allow multiple selections.
  
  treeview$getSelection()$setMode("multiple")
  impview$getSelection()$setMode("multiple")
  catview$getSelection()$setMode("multiple")
  conview$getSelection()$setMode("multiple")

}

createVariablesModel <- function(variables, input=NULL, target=NULL,
                                 risk=NULL, ident=NULL, ignore=NULL,
                                 zero=NULL,
                                 boxplot=NULL,
                                 hisplot=NULL, cumplot=NULL, benplot=NULL,
                                 barplot=NULL, dotplot=NULL)
{
  
  ## Set up initial information about variables throughout Rattle,
  ## including the Variable tab's variable model, the Explore tab's
  ## categorical and continuous models, and the Modelling tab defaults
  ## where they depend on the dataset sizes.
  ##
  ## Any values supplied for input, target, risk, ident, ignore, boxplot,
  ## hisplot, cumplot, benplot, barplot, and dotplot arguments should be lists
  ## of variable names (list of strings).

  ## Retrieve the models.
  
  model <- theWidget("variables_treeview")$getModel()
  impute <- theWidget("impute_treeview")$getModel()
  categorical <- theWidget("categorical_treeview")$getModel()
  continuous  <- theWidget("continuous_treeview")$getModel()

  ## Identify a default target - the last or first if it's a factor,
  ## or has only a few values. Then the treeview model will record
  ## this choice, and we set the appropriate labels with this, and
  ## record it in crs.

  if (is.null(target))
  {
    ## Find the last variable that is not an IMP (imputed). This is
    ## jsut a general heuristic, and works particularly for imputation
    ## performed in Rattle. Should also do this for first, and also
    ## for IGNORE variables.
    last.var <- length(variables)
    while (last.var > 1 && substr(variables[last.var], 1, 4) == "IMP_")
    {
      last.var <- last.var - 1
    }
    
    target <- -1
    if ((is.factor(crs$dataset[,last.var]) &&
         length(levels(crs$dataset[,last.var])) > 1)
        || (length(levels(as.factor(crs$dataset[,last.var]))) < 5
            && length(levels(as.factor(crs$dataset[,last.var]))) > 1))
      target <- last.var
    else if ((is.factor(crs$dataset[,1]) && length(levels(crs$dataset[,1])) > 1)
             || (length(levels(as.factor(crs$dataset[,1]))) < 5
                 && length(levels(as.factor(crs$dataset[,1]))) > 1))
      target <- 1
    else
      for (i in 2:length(variables)-1)
      {
        if ((is.factor(crs$dataset[,i]) && length(levels(crs$dataset[,i])) > 1)
            || (length(levels(as.factor(crs$dataset[,i]))) < 5
                && length(levels(as.factor(crs$dataset[,i]))) > 1))
        {
          target <- i
          break
        }
      }
    if (target != -1)
      target <- variables[target]
    else
      target <- NULL
  }

  ## Determine the list of input variables so far (i.e., not dealing
  ## with ignore and risk yet).
  
  if (is.null(input)) input <- variables
  input <- setdiff(input, target)
  
  ## Update the Model tab with the selected default target

  the.target <- sprintf("Target: %s", ifelse(is.null(target), "None", target))

  theWidget("explot_target_label")$setText(the.target)

  theWidget("glm_target_label")$setText(the.target)
  theWidget("rpart_target_label")$setText(the.target)
  ## theWidget("gbm_target_label")$setText(the.target)
  theWidget("ada_target_label")$setText(the.target)
  theWidget("rf_target_label")$setText(the.target)
  theWidget("svm_target_label")$setText(the.target)

  plots <- union(boxplot,
                 union(hisplot,
                       union(cumplot,
                             union(benplot, union(barplot, dotplot)))))
  
  ## Build the Variables treeview model with each variable's INPUT set
  ## to TRUE and all else FALSE. If the variable has only a single
  ## value then it defaults to IGNORE, and if it is a factor and has
  ## as many distinct values as there are rows, then also default to
  ## IGNORE.

  for (i in 1:length(variables))
  {
    iter <- model$append()$iter

    cl <- class(crs$dataset[[variables[i]]])

    ## First check for special variable names. Want to also do this
    ## for TARGET_, eventually
    
    if (paste("IMP_", variables[i], sep="") %in% variables)
    {
      ## This works with SAS/EM IMPutations and Rattle's imputations,
      ## which add the IMP_ at the beginning of the name of any
      ## imputed variables.
      
      ignore <- c(ignore, variables[i])
      
      ## Be sure to also remove any other role for the original
      ## variable?
    }
    else if (substr(variables[i], 1, 2) == "ID")
    {
      ident <- c(ident, variables[i])
    }
    else if (substr(variables[i], 1, 6) == "IGNORE")
    {
      ignore <- c(ignore, variables[i])
    }
    else if (substr(variables[i], 1, 4) == "RISK")
    {
      risk <- c(risk, variables[i])
    }
    else if (cl == "factor")
    {
      lv <- length(levels(crs$dataset[[variables[i]]]))
      if (lv == nrow(crs$dataset))
      {
        cl <- "ident"
        ident <- c(ident, variables[i])
      }
      else if (lv > 1)
        cl <- paste(cl, lv)
      else
      {
        cl <- "constant"
        ignore <- c(ignore, variables[i])
      }
    }
    else
    {
      lv <- length(levels(as.factor(crs$dataset[[variables[i]]])))
      if (cl == "integer" & lv == nrow(crs$dataset))
      {
        cl <- "ident"
        ident <- c(ident, variables[i])
      }
      else if (all(is.na(crs$dataset[[variables[i]]])))
      {
        cl <- "missing"
        ignore <- c(ignore, variables[i])
      }
      else if (sd(crs$dataset[[variables[i]]], na.rm=TRUE) %in% c(NA, 0))
      {
        ## sd is NA if all data items  are NA.
        cl <- "constant"
        ignore <- c(ignore, variables[i])
      }
    }
    input <- setdiff(setdiff(setdiff(input, ignore), ident), risk)

    missing.count <- sum(is.na(crs$dataset[[variables[i]]]))

    ## Every variable goes into the VARIABLES treeview.
    
    model$set(iter,
              .COLUMN["number"], i,
              .COLUMN["variable"], variables[i],
              .COLUMN["type"], cl,
              .COLUMN["input"], variables[i] %in% input,
              .COLUMN["target"], variables[i] %in% target,
              .COLUMN["risk"], variables[i] %in% risk,
              .COLUMN["ident"], variables[i] %in% ident,
              .COLUMN["ignore"], variables[i] %in% ignore,
              .COLUMN["comment"], ifelse(missing.count > 0,
                                        sprintf("%d missing values.",
                                                missing.count),
                                        ""))

    ## Selected variables go into the other treeviews.

    if (missing.count > 0) # Ignore IGNOREd variables. But crs$ignore
                           # is not yet set. Need to remove later.
    {
      impiter <- impute$append()$iter
      impute$set(impiter,
                 .IMPUTE["number"], i,
                 .IMPUTE["variable"], variables[i],
                 .IMPUTE["zero"], variables[i] %in% zero,
                 .IMPUTE["comment"], sprintf("%s with %d missing.",
                                            cl, missing.count))
    }
        
    if (strsplit(cl, " ")[[1]][1] == "factor")
    {
      ## For the IMP_ and IGNORE_ variables we don't get a chance
      ## above to add in the number of levels, so do it here.

      if (cl == "factor")
        cl <- paste(cl, length(levels(crs$dataset[[variables[i]]])))
      
      catiter <- categorical$append()$iter
      categorical$set(catiter,
                      .CATEGORICAL["number"], i,
                      .CATEGORICAL["variable"], variables[i],
                      .CATEGORICAL["barplot"], variables[i] %in% barplot,
                      .CATEGORICAL["dotplot"], variables[i] %in% dotplot,
                      .CATEGORICAL["comment"],
                      sprintf("%s", strsplit(cl, " ")[[1]][2]))
    }

    if (cl == "integer" || cl == "numeric")
    {
      coniter <- continuous$append()$iter
      continuous$set(coniter,
                     .CONTINUOUS["number"], i,
                     .CONTINUOUS["variable"], variables[i],
                     .CONTINUOUS["boxplot"], variables[i] %in% boxplot,
                     .CONTINUOUS["hisplot"], variables[i] %in% hisplot,
                     .CONTINUOUS["cumplot"], variables[i] %in% cumplot,
                     .CONTINUOUS["benplot"], variables[i] %in% benplot,
                     .CONTINUOUS["comment"],
                     sprintf("%.2f; %.2f/%.2f; %.2f",
                             min(crs$dataset[,i], na.rm=TRUE),
                             median(crs$dataset[,i], na.rm=TRUE),
                             mean(crs$dataset[,i], na.rm=TRUE),
                             max(crs$dataset[,i], na.rm=TRUE)))
    }
  }

  crs$target <<- target
  crs$input  <<- input
  crs$ident  <<- ident
  crs$ignore <<- ignore
  crs$risk   <<- risk
  
  ## Perform other setups associated with a new dataset

  .RF.MTRY.DEFAULT <<- floor(sqrt(ncol(crs$dataset)))
  theWidget("rf_mtry_spinbutton")$setValue(.RF.MTRY.DEFAULT)
  #.RF.SAMPSIZE.DEFAULT <<- nrow(crs$dataset)
  #theWidget("rf_sampsize_spinbutton")$setValue(.RF.SAMPSIZE.DEFAULT)
}

##----------------------------------------------------------------------
##
## Support
##

getIncludedVariables <- function(numonly=FALSE, listall=FALSE, risk=FALSE)
{
  ## DESCRIPTION
  ## Generate a numeric list of variables not ignored.
  ##
  ## ARGUMENTS
  ## numonly = Only include numeric variables
  ## listall = Don't simplify a full list to NULL
  ## risk =  Include any risk variable in the returned list
  ##
  ## RETURNS
  ## A string of comma separated numbers
  ##
  ## DETAILS Generates a list of input variable indicies and the
  ## target variable index and, optionally, the risk variable index.
  ## If the list contains all variables, then return NULL (as the
  ## dataset does not then need to be indexed to subset the variables).
  ##
  ## TODO This last assumption of returning NULL causes problems since we
  ## don't know whether this means all variables or no variables!

  fi <- getVariableIndicies(crs$input)
  ti <- getVariableIndicies(crs$target)
  if (risk)
    ri <- getVariableIndicies(crs$risk)
  else
    ri <- NULL
  
  if (numonly)
    fl <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.numeric))]
  else
    fl <- 1:ncol(crs$dataset)

  if (! listall && setequal(union(fi,union(ti, ri)), fl))
    return(NULL)
  else
    return(simplifyNumberList(intersect(fl, union(fi, union(ti, ri)))))
}

inputVariables <- function(numonly=FALSE)
{
  ## Return, as a comma separated list (as a string) the list of input
  ## variable indicies. If the list contains all variables except for
  ## the target variable, then return NULL (as the dataset does not then
  ## need to be indexed to subset the variables).

  fi <- getVariableIndicies(crs$input)
  ti <- getVariableIndicies(crs$target)

  if (is.null(crs$input))
  {
    errorDialog("No input variables have been selected.",
                 "This doesn't make a lot of sense.",
                 "Please choose some input variables before proceeding.")
    stop("no input variables specified")
  }
    
  if (numonly)
    fl <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.numeric))]
  else
    fl <- 1:ncol(crs$dataset)
  
  if (setequal(fi, fl))
    return(NULL)
  else
    return(simplifyNumberList(intersect(fl,fi)))
}

used.variables <- function(numonly=FALSE)
{
  ## Return, as a comma separated list (as a string) the list of all
  ## variable indicies for those that are not ignored. If the list
  ## contains all variables except for the ignored variables, then
  ## return NULL.

  ii <- union(getVariableIndicies(crs$ignore), getVariableIndicies(crs$ident))

  if (numonly)
    fl <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.numeric))]
  else
    fl <- 1:ncol(crs$dataset)
  
  if (setequal(fl, ii))
    return(NULL)
  else
    return(simplifyNumberList(setdiff(fl, ii)))
}

getCategoricalVariables <- function()
{
  ## Return a list of categorical variables from amongst those with an
  ## INPUT role.
  
  include <- NULL
  cats <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.factor))]
  if (length(cats) > 0)
  {
    indicies <- getVariableIndicies(crs$input)
    include <- simplifyNumberList(intersect(cats, indicies))
  }
  return(include)
}

getNumericVariables <- function()
{
  ## Returns a list of cumeric variables
  
  nums <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.numeric))]
  if (length(nums) > 0)
  {
    indicies <- getVariableIndicies(crs$input)
    include <- simplifyNumberList(intersect(nums, indicies))
  }
  else
    inlcude <- NULL

 return(include)
}

########################################################################
##
## TRANSFORM TAB
##

##----------------------------------------------------------------------
##
## Interface Actions
##

## When a radio button is selected, display the appropriate tab

on_sample_radiobutton_toggled <- function(button)
{
  if (button$getActive()) 
  {
    .TRANSFORM$setCurrentPage(.TRANSFORM.SAMPLE.TAB)
  }
  setStatusBar()
}

on_impute_radiobutton_toggled <- function(button)
{
  if (button$getActive()) 
  {
    .TRANSFORM$setCurrentPage(.TRANSFORM.IMPUTE.TAB)
  }
  setStatusBar()
}

imp_toggled <- function(cell, path.str, model)
{
  ## A impute variable's radio button has been toggled in the
  ## TRANSFORM's tab IMPUTE option. Handle the choice.

  ## The data passed in is the model used in the treeview.

  checkPtrType(model, "GtkTreeModel")

  ## Extract the column number of the model that has changed.

  column <- cell$getData("column")

  ## Get the current value of the corresponding flag
  
  path <- gtkTreePathNewFromString(path.str) # Current row
  iter <- model$getIter(path)$iter           # Iter for the row
  current <- model$get(iter, column)[[1]]    # Get data from specific column

  ## Always invert
  
  model$set(iter, column, !current)

}

on_sample_checkbutton_toggled <- function(button)
{
  if (button$getActive())
  {
    theWidget("sample_percentage_spinbutton")$setSensitive(TRUE)
    theWidget("sample_percentage_label")$setSensitive(TRUE)
    theWidget("sample_count_spinbutton")$setSensitive(TRUE)
    theWidget("sample_count_label")$setSensitive(TRUE)
    theWidget("sample_seed_spinbutton")$setSensitive(TRUE)
    theWidget("sample_seed_button")$setSensitive(TRUE)
    theWidget("explore_sample_checkbutton")$setSensitive(TRUE)
    crs$sample <<- NULL ## Only reset when made active to ensure Execute needed
  }
  else
  {
    theWidget("sample_percentage_spinbutton")$setSensitive(FALSE)
    theWidget("sample_percentage_label")$setSensitive(FALSE)
    theWidget("sample_count_spinbutton")$setSensitive(FALSE)
    theWidget("sample_count_label")$setSensitive(FALSE)
    theWidget("sample_seed_spinbutton")$setSensitive(FALSE)
    theWidget("sample_seed_button")$setSensitive(FALSE)
    theWidget("explore_sample_checkbutton")$setActive(FALSE)
    theWidget("explore_sample_checkbutton")$setSensitive(FALSE)
  }
    setStatusBar()
}

on_sample_percentage_spinbutton_changed <- function(action, window)
{
  if (is.null(crs$dataset)) return()
  per <- theWidget("sample_percentage_spinbutton")$getValue()
  rows <- round(nrow(crs$dataset) * per / 100)
  crows <- theWidget("sample_count_spinbutton")$getValue()
  if (rows != crows)
    theWidget("sample_count_spinbutton")$setValue(rows)
  setStatusBar()
}

on_sample_count_spinbutton_changed <- function(action, window)
{
  if (is.null(crs$dataset)) return()
  rows <- theWidget("sample_count_spinbutton")$getValue()
  per <- round(100*rows/nrow(crs$dataset))
  cper <- theWidget("sample_percentage_spinbutton")$getValue()
  if (per != cper)
    theWidget("sample_percentage_spinbutton")$setValue(per)
  setStatusBar()
}

on_sample_seed_button_clicked <- function(button)
{
  rseed <- as.integer(runif(1, 0, 1000000))
  theWidget("sample_seed_spinbutton")$setValue(rseed)
}

##----------------------------------------------------------------------
##
## Execution
##

executeTransformTab <- function()
{
  ## Can not do any transforms if there is no dataset.

  if (noDatasetLoaded()) return()

  ## DISPATCH

  if (theWidget("sample_radiobutton")$getActive())
  {
    executeTransformSample()
  }
  
  else if (theWidget("impute_radiobutton")$getActive())
  {
    if (getCurrentPageLabel(theWidget("impute_notebook")) ==
        "Perform Imputation")
      executeTransformImputePerform()
    else
      executeTransformImputeSummary()
  }
  
}

executeTransformSample <- function()
{
  ## Record that a random sample of the dataset is desired.

  if (theWidget("sample_checkbutton")$getActive())
  {
    #ssize <- theWidget("sample_percentage_spinbutton")$getValue()
    #ssize <- floor(nrow(crs$dataset)*ssize/100)
    ssize <- theWidget("sample_count_spinbutton")$getValue()

    seed <- theWidget("sample_seed_spinbutton")$getValue()
    
    sample.cmd <- paste(sprintf("set.seed(%d)\n", seed),
                        "crs$sample <<- sample(nrow(crs$dataset), ", ssize,
                        ")", sep="")

    addToLog("Build a random sample for modelling.",
            gsub("<<-", "<-", sample.cmd))
    eval(parse(text=sample.cmd))

    ## When we have sampling, assume the remainder is the test set and
    ## so enable the Testing radio button in Evaluate.
    
    theWidget("evaluate_testing_radiobutton")$setSensitive(TRUE)
    theWidget("evaluate_testing_radiobutton")$setActive(TRUE)
  }
  else
  {
    crs$sample <<- NULL

    theWidget("evaluate_testing_radiobutton")$setSensitive(FALSE)
    theWidget("evaluate_training_radiobutton")$setActive(TRUE)
  }
  
  crs$smodel <<- vector()

  ## TODO For test/train, use sample,split from caTools?

  ## Set some defaults that depend on sample size.
  
  #if (is.null(crs$sample))
  #  .RF.SAMPSIZE.DEFAULT <<- length(crs$dataset)
  #else
  #  .RF.SAMPSIZE.DEFAULT <<- length(crs$sample)
  #theWidget("rf_sampsize_spinbutton")$setValue(.RF.SAMPSIZE.DEFAULT)
  

  setStatusBar()

  if (theWidget("sample_checkbutton")$getActive())
    setStatusBar("The sample has been generated.",
                  "There are", length(crs$sample), "entities.")
  else
    setStatusBar("Sampling is inactive.")
}

executeTransformImputePerform <- function()
{
  zero <- getSelectedVariables("zero")

  if (length(zero) > 0) addLogSeparator("MISSING VALUE IMPUTATION")

  for (z in zero)
  {
    ## Take a copy of the variable to be imputed.
    
    vname <- paste("IMP_", z, sep="")
    copy.cmd <- sprintf('crs$dataset[["%s"]] <<- crs$dataset[["%s"]]', vname, z)
    addToLog(sprintf("IMPUTE %s.", z),
             sub("<<-", "<-", copy.cmd))
    eval(parse(text=copy.cmd))
             
    cl <- class(crs$dataset[[vname]])
    if (cl == "factor")
    {
      ## If Missing is not currently a category for this variable, add
      ## it in.
      
      if ("Missing" %notin% levels(crs$dataset[[vname]]))
      {
        levels.cmd <- sprintf(paste('levels(crs$dataset[["%s"]]) <<-',
                                    'c(levels(crs$dataset[["%s"]]),"Missing")'),
                              vname, vname)
        addToLog("Add a new category to the variable",
                 sub("<<-", "<-", levels.cmd))
        eval(parse(text=levels.cmd))
      }
        
      ## Change all NAs to Missing.
      
      missing.cmd <- sprintf(paste('crs$dataset[["%s"]][is.na(',
                                   'crs$dataset[["%s"]])] <<- "Missing"',
                                   sep=""),
                             vname, z)
      addToLog("Change all NAs to Missing.", sub("<<-", "<-", missing.cmd))
      eval(parse(text=missing.cmd))
    }
    else
    {
      zero.cmd <- sprintf(paste('crs$dataset[["%s"]]',
                                '[is.na(crs$dataset[["%s"]])]',
                                " <<- 0", sep=""), vname, z)
      addToLog("Change all NAs to 0.", sub("<<-", "<-", zero.cmd))
      eval(parse(text=zero.cmd))
    }
  }
  
  if (length(zero) > 0)
  {
    
    ## Reset the treeviews.

    theWidget("variables_treeview")$getModel()$clear()
    theWidget("impute_treeview")$getModel()$clear()
    theWidget("categorical_treeview")$getModel()$clear()
    theWidget("continuous_treeview")$getModel()$clear()

    ## Recreate the treeviews.

    resetVariableRoles(colnames(crs$dataset), nrow(crs$dataset), resample=FALSE)

    ## Reset the original Data textview to output of new str.

    clearTextview("data_textview")
    appendTextview("data_textview", collectOutput("str(crs$dataset)"))

    ## Update the status bar

    setStatusBar("Imputed variables added to the dataset.")
  }
  else
    setStatusBar("No variables selected to be imputed.")
}  

executeTransformImputeSummary <- function()
{
  ## Initial setup. 
  
  TV <- "impute_textview"

  ## Load the mice package into the library

  addLogSeparator("MISSING VALUE SUMMARY")
  lib.cmd <-  "require(mice, quietly=TRUE)"
  if (! packageIsAvailable("mice", "summarise missing values")) return(FALSE)
  addToLog("Summarise missing values in the dataset using the mice package.",
           lib.cmd)
  eval(parse(text=lib.cmd))

  ## Variables to be included, as a string of indicies.
  
  included <- getIncludedVariables()
  including <- not.null(included)

  ## Add radio buttons to choose: Full, Train, Test dataset to summarise.
  
  ## Build the summary command

  clearTextview(TV)
  theWidget(TV)$setWrapMode("none")
  summary.cmd <- paste("md.pattern(crs$dataset[,",
                       if (including) included,
                       "])", sep="")

  addToLog("Generate a summary of the missing values in the dataset.",
           summary.cmd)
  ow <- options(width=300)
  setTextview(TV, collectOutput(summary.cmd, TRUE))
  options(ow)

  setStatusBar("Summary has been generated.")
}

########################################################################
##
## EXPLORE TAB
##

##----------------------------------------------------------------------
##
## Interface Actions
##

## When a radio button is selected, display the appropriate tab

on_summary_radiobutton_toggled <- function(button)
{
  separator       <- theWidget("explore_vseparator")
  summary.button  <- theWidget("summary_checkbutton")
  describe.button <- theWidget("describe_checkbutton")
  basics.button   <- theWidget("basics_checkbutton")
  kurtosis.button <- theWidget("kurtosis_checkbutton")
  skewness.button <- theWidget("skewness_checkbutton")
  if (button$getActive())
  {
    .EXPLORE$setCurrentPage(.EXPLORE.SUMMARY.TAB)
    separator$show()
    summary.button$show()
    describe.button$show()
    basics.button$show()
    kurtosis.button$show()
    skewness.button$show()
  }
  else
  {
    separator$hide()
    summary.button$hide()
    describe.button$hide()
    basics.button$hide()
    kurtosis.button$hide()
    skewness.button$hide()
  }
  setStatusBar()
}

on_explot_radiobutton_toggled <- function(button)
{
  separator <- theWidget("explore_vseparator")
  barbutton <- theWidget("benford_bars_checkbutton")
  if (button$getActive()) 
  {
    .EXPLORE$setCurrentPage(.EXPLORE.PLOT.TAB)
    separator$show()
    barbutton$show()
  }
  else
  {
    separator$hide()
    barbutton$hide()
  }
  setStatusBar()
}

on_ggobi_radiobutton_toggled <- function(button)
{
  if (button$getActive()) .EXPLORE$setCurrentPage(.EXPLORE.GGOBI.TAB)
  setStatusBar()
}

on_correlation_radiobutton_toggled <- function(button)
{
  separator <- theWidget("explore_vseparator")
  nabutton  <- theWidget("correlation_na_checkbutton")
  if (button$getActive()) 
  {
    .EXPLORE$setCurrentPage(.EXPLORE.CORRELATION.TAB)
    separator$show()
    nabutton$show()
  }
  else
  {
    separator$hide()
    nabutton$hide()
  }
  setStatusBar()
}

on_hiercor_radiobutton_toggled <- function(button)
{
  if (button$getActive()) .EXPLORE$setCurrentPage(.EXPLORE.HIERCOR.TAB)
  setStatusBar()
}

on_prcomp_radiobutton_toggled <- function(button)
{
  if (button$getActive()) .EXPLORE$setCurrentPage(.EXPLORE.PRCOMP.TAB)
  setStatusBar()
}

cat_toggled <- function(cell, path.str, model)
{
  ## A categorical variable's radio button has been toggled in the
  ## Explore tab's Distribution option. Handle the choice.

  ## The data passed in is the model used in the treeview.

  checkPtrType(model, "GtkTreeModel")

  ## Extract the column number of the model that has changed.

  column <- cell$getData("column")

  ## Get the current value of the corresponding flag
  
  path <- gtkTreePathNewFromString(path.str) # Current row
  iter <- model$getIter(path)$iter           # Iter for the row
  current <- model$get(iter, column)[[1]]    # Get data from specific column

  ## Always invert
  
  model$set(iter, column, !current)

}

con_toggled <- function(cell, path.str, model)
{
  ## A continuous variable's radio button has been toggled in the
  ## Explore tab's Distribution option. Handle the choice.

  ## The data passed in is the model used in the treeview.

  checkPtrType(model, "GtkTreeModel")

  ## Extract the column number of the model that has changed.

  column <- cell$getData("column")
  
  ## Get the current value of the corresponding flag
  
  path <- gtkTreePathNewFromString(path.str) # Current row
  iter <- model$getIter(path)$iter           # Iter for the row
  current <- model$get(iter, column)[[1]]    # Get data from specific column

  model$set(iter, column, !current)

}

on_categorical_clear_button_clicked <- function(action, window)
{
  ## Ensure categorical all check boxes are unchecked.

  set.cursor("watch")

  ## Only clear selected rows.

  tree.selection <- theWidget("categorical_treeview")$getSelection()

  tree.selection$selectedForeach(function(model, path, iter)
  {
    columns <- .CATEGORICAL[["barplot"]]:.CATEGORICAL[["dotplot"]]
    for (c in columns) if (model$get(iter, c)[[1]]) model$set(iter, c, FALSE)
    return(FALSE) # Keep going through all rows
  })

  set.cursor()
}

on_continuous_clear_button_clicked <- function(action, window)
{
  ## Ensure all continuous check boxes are unchecked.

  set.cursor("watch")

  ## Only clear selected rows.

  tree.selection <- theWidget("continuous_treeview")$getSelection()

  tree.selection$selectedForeach(function(model, path, iter)
  {
    columns <- .CONTINUOUS[["boxplot"]]:.CONTINUOUS[["benplot"]]
    for (c in columns) if (model$get(iter, c)[[1]]) model$set(iter, c, FALSE)
    return(FALSE) # Keep going through all rows
  })

  set.cursor()
}

##----------------------------------------------------------------------
##
## Execution
##

executeExploreTab <- function()
{
  
  ## Can not explore the data if there is no dataset.

  if (noDatasetLoaded()) return()

  ## Ensure Sample does not require executing.

  use.sample <- theWidget("explore_sample_checkbutton")$getActive()
  sampling <- theWidget("sample_checkbutton")$getActive()
  if (use.sample && sampleNeedsExecute()) return()

  ## We generate a string representing the subset of the dataset on
  ## which the exploration is to be performed. This is then passed to
  ## the individually dispatched functions.

  vars <- getIncludedVariables(risk=TRUE)
  dataset <- sprintf("%s[%s,%s]", "crs$dataset",
                     ifelse(use.sample & sampling,"crs$sample", ""),
                     ifelse(is.null(vars),"", vars))

  ## For the distribution plot, we do list all variables in the
  ## interface, even if they are ignored. TODO 061006 We could instead
  ## grey out the ignored ones (i.e., make them not sensitive). But
  ## for now, for plots, allow all variables, even the ignored ones,
  ## and thus we need a dataset that includes all variables - the
  ## "avdataset".

  avdataset <- sprintf("%s[%s,]", "crs$dataset",
                     ifelse(use.sample & sampling,"crs$sample", ""))
  
  vars <- getIncludedVariables(numonly=TRUE)
  ## TODO 060606 The question here is whether NULL means all variables
  ## or means none found?
  
  #if (is.null(vars))
  #  ndataset <- NULL
  #else
    ndataset <- sprintf("%s[%s,%s]", "crs$dataset",
                        ifelse(use.sample & sampling,"crs$sample", ""),
                        ifelse(is.null(vars),"",vars))

  ## Numeric input variables
  vars <- inputVariables(numonly=TRUE)
  nidataset <- sprintf("%s[%s,%s]", "crs$dataset",
                       ifelse(use.sample & sampling,"crs$sample", ""),
                       ifelse(is.null(vars),"",vars))
  
  ## Dispatch
  
  if (theWidget("summary_radiobutton")$getActive())
    executeExploreSummary(dataset)
  else if (theWidget("explot_radiobutton")$getActive())
    executeExplorePlot(avdataset)
  else if (theWidget("ggobi_radiobutton")$getActive())
    executeExploreGGobi(dataset)
  else if (theWidget("correlation_radiobutton")$getActive())
  {
    if (theWidget("correlation_na_checkbutton")$getActive())
      executeExploreCorrelation(dataset)
    else
      executeExploreCorrelation(ndataset)
  }
  else if (theWidget("hiercor_radiobutton")$getActive())
    executeExploreHiercor(ndataset)
  else if (theWidget("prcomp_radiobutton")$getActive())
    executeExplorePrcomp(nidataset)
}

executeExploreSummary <- function(dataset)
{
  TV <- "summary_textview"

  ## Get the current state of the relevant buttons.
  
  use.sample  <- theWidget("explore_sample_checkbutton")$getActive()
  do.summary  <- theWidget("summary_checkbutton")$getActive()
  do.describe <- theWidget("describe_checkbutton")$getActive()
  do.basics   <- theWidget("basics_checkbutton")$getActive()
  do.kurtosis <- theWidget("kurtosis_checkbutton")$getActive()
  do.skewness <- theWidget("skewness_checkbutton")$getActive()

  ## Make sure something has been selected.
  
  if (! (do.summary || do.describe || do.basics ||
         do.kurtosis || do.skewness))
  {
    infoDialog("No summary type has been selected.",
               "Please choose at least one to get some output.")
    return()
  }
    
  ## Other useful information:
  ##   is there a sample
  ##   list of numeric variables
  
  sampling  <- not.null(crs$sample)

  numeric.cmd <- sprintf(paste("seq(1,ncol(%s))",
                               "[as.logical(sapply(%s, is.numeric))]",
                               sep=""), dataset, dataset)
  nvars <- simplifyNumberList(eval(parse(text=numeric.cmd)))

  ## Start the trace to the log.
  
  addLogSeparator()
  clearTextview(TV)

  ## Construct and execute the requested commands.

  if (do.summary)
  {
    ## Find the number of entities with any missing value for the
    ## non-ignored variables.
    
    missing.cmd <- sprintf("length((na.omit(%s))@na.action)", dataset)
    result <- try(missing <- eval(parse(text=missing.cmd)), silent=TRUE)
    if (inherits(result, "try-error")) missing <- 0
    
    ## A basic summary.

    summary.cmd <- sprintf("summary(%s)", dataset)
    addToLog("SUMMARY OF DATASET.", summary.cmd)
    appendTextview(TV,
                   paste("Summary of the ",
                         ifelse(use.sample & sampling, "** sample **", "full"),
                         " dataset.\n\n", sep=""),
                   sprintf("The data contains %d entities with missing values.",
                           missing),
                   "\n\n(Hint: 25% of values are below 1st Quartile.)\n\n",
                   collectOutput(summary.cmd, TRUE))
  }

  if (do.describe)
  {
    ## A different summary, using Hmisc's describe.
  
    if (packageIsAvailable("Hmisc", "describe the data"))
    {
      lib.cmd <- "require(Hmisc, quietly=TRUE)"
      addToLog("The describe command comes from Hmisc.", lib.cmd)
      eval(parse(text=lib.cmd))
      
      describe.cmd <- sprintf("describe(%s)", dataset)
      addToLog("Generate a description of the dataset.", describe.cmd)
      appendTextview(TV,
                     paste("Description of the",
                           ifelse(use.sample & sampling,
                                  "** sample **", "full"),
                           "dataset.\n\n"),
                     collectOutput(describe.cmd, TRUE, width=200))
    }
  }

  if (do.basics || do.kurtosis || do.skewness)
  {
    ## These all require the fBasics library, so check only once.
    
    if (packageIsAvailable("fBasics", "calculate basics, skew and kurtosis"))
    {
      lib.cmd <- "require(fBasics, quietly=TRUE)"
      addToLog("Use functionality from the fBasics package.", lib.cmd)
      eval(parse(text=lib.cmd))
      
      if (do.basics)
      {
        basics.cmd <- sprintf("lapply(%s[,%s], basicStats)", dataset,
                              ifelse(is.null(nvars), "", nvars))
        addToLog("Generate a summary of the numeric data.", basics.cmd)
        appendTextview(TV,
                       paste("Basic statistics for each numeric variable",
                             "of the",
                             ifelse(use.sample & sampling,
                                    "** sample **", "full"),
                             "dataset.\n\n"),
                       collectOutput(basics.cmd, TRUE))
      }
      
      if (do.kurtosis)
      {
        kurtosis.cmd <- sprintf("kurtosis(%s[,%s], na.rm=TRUE)", dataset,
                                ifelse(is.null(nvars), "", nvars))

        addToLog("Summarise the kurtosis of the numeric data.", kurtosis.cmd)
        appendTextview(TV,
                       paste("Kurtosis for each numeric variable ",
                             "of the ",
                             ifelse(use.sample & sampling,
                                    "** sample **", "full"),
                             " dataset.\n",
                             "Larger values mean sharper peaks and ",
                             "flatter tails.\n",
                             "Positive values indicate an acute peak around ",
                             "the mean.\n",
                             "Negative values indicate a smaller peak around ",
                             "the mean.\n\n",
                             sep=""),
                       collectOutput(kurtosis.cmd, TRUE))
      }

      if (do.skewness)
      {
        skewness.cmd <- sprintf("skewness(%s[,%s], na.rm=TRUE)", dataset,
                                ifelse(is.null(nvars), "", nvars))

        addToLog("Summarise the skewness of the numeric data.", skewness.cmd)
        appendTextview(TV,
                       paste("Skewness for each numeric variable",
                             "of the",
                             ifelse(use.sample & sampling,
                                    "** sample **", "full"),
                             "dataset.\nPositive means the right tail",
                             "is longer.\n\n"),
                       collectOutput(skewness.cmd, TRUE))
      }
    }
  }
      
  ## Report completion to the user through the Status Bar.
  
  setStatusBar("Data summary generated.")
}

getVariableIndicies <- function(variables)
{
  indicies <- NULL
  if (not.null(variables))
    indicies <- unlist(lapply(variables, match, colnames(crs$dataset)))
  return(indicies)
}

calcInitialDigitDistr <- function(l)
{
  ## DESCRIPTION
  ## From a list of numbers return vector of first digit frequencies.
  
  ds <- data.frame(digit=as.numeric(gsub("(.).*", "\\1", as.character(l))),
                   value=1)
  # Ignore any zeros
  ds <- ds[ds$digit!=0,]
  # Add in any mising digits as value=0
  missing <- setdiff(1:9, unique(ds[,1]))
  if (length(missing) >0)
    ds <- rbind(ds, data.frame(digit=missing, value=0))
  dsb <- by(ds, as.factor(ds$digit), function(x) sum(x$value))
  return(as.matrix(dsb)[,1]/sum(as.matrix(dsb)[,1]))
}

plotBenfordsLaw <- function(l)
{
  if (! packageIsAvailable("gplots", "plot Benford's law")) return()
  require(gplots, quietly=TRUE)
  
  actual <- calcInitialDigitDistr(l)
  
  x  <- 1:9
  expect <- log10(1 + 1/x)
  
  nds <- t(as.matrix(data.frame(expect=expect, actual=actual)))

  ttl <- genPlotTitleCmd("Benford's Law", vector=TRUE)
  
  barplot2(nds, beside=TRUE, main = ttl[1], sub = ttl[2],
           xlab = "Initial Digit", ylab = "Probability")
}

executeExplorePlot <- function(dataset)
{
  ## DESCRIPTION
  ## Plot the data
  ##
  ## ARGUMENTS
  ## dataset = A string that defines the dataset to use.
  ##
  ## RETURNS
  ## ignored
  ##
  ## DETAILS Information about what variables to plot and the kind of
  ## plots is obtained from the continuous_treeview and the
  ## categorical_treeview which are displayed in the Explore tab's
  ## Distribution option. The appropriate plots are displayed.
  ##

  ## Obtain the selection of variables.

  boxplots  <- getSelectedVariables("boxplot")
  nboxplots <- length(boxplots)

  hisplots <- getSelectedVariables("hisplot")
  nhisplots <- length(hisplots)

  cumplots <- getSelectedVariables("cumplot")
  benplots  <- getSelectedVariables("benplot")
  nbenplots <- length(benplots)
  
  barplots  <- getSelectedVariables("barplot")
  nbarplots <- length(barplots)
  
  dotplots  <- getSelectedVariables("dotplot")
  ndotplots <- length(dotplots)

  pmax <- theWidget("plots_per_page_spinbutton")$getValue()
  pcnt <- 0
  
  ## Iterate over all target values if a target is defined and has
  ## less than 10 values. The plots will then also display the
  ## distributions per target value.

  target <- getSelectedVariables("target")

  if (is.null(target))
    targets <- NULL
  else
    targets <- levels(as.factor(crs$dataset[[crs$target]]))

  if (length(targets) > 10)
  {
    target <- NULL
    targets <- NULL
  }
  
  ## Check for sampling.
  
  use.sample <- theWidget("explore_sample_checkbutton")$getActive()
  sampling  <- use.sample & not.null(crs$sample)

  ## Split the data, first for all values.

  bind.cmd <- sprintf('rbind(data.frame(dat=%s[,"%%s"], grp="All")', dataset)

  if (not.null(targets))
  {
    for (i in 1:length(targets))
    {
      bind.cmd <- sprintf("%s,\n            data.frame(dat=%s",
                         bind.cmd, dataset)

      bind.cmd <- sprintf('%s[crs$dataset%s$%s=="%s","%%s"], grp="%s")',
                         bind.cmd,
                         ifelse(sampling, "[crs$sample,]", ""),
                         target, targets[i], targets[i])
    }
  }
  
  ## Finish off the command to create the dataset for plotting.
  
  bind.cmd <- sprintf("%s)", bind.cmd)

  ## Build a list of generic datasets. This describes how to get the
  ## relevant rows from the dataset for All the data, then each of the
  ## levels of a target. Each contains a "%s" which is replace gor
  ## specific chosen variables at the time of using this construct to
  ## obtain the data for the plot. The form is:
  ##
  ## All = crs$dataset$%s
  ##
  ## or if sampling is enabled:
  ##
  ## All = crs$dataset[crs$sample,]$%s  
  ##
  ## For each level:
  ##
  ## '0' = crs$dataset[crs$dataset$Adjusted=="0",]$%s
  ##
  ## or if sampling is enabled:
  ##
  ## '0' = crs$dataset[crs$sample,][crs$dataset[crs$sample,]$Adjusted=="0",]$%s
  ##
  ## This is a newer alternative to identifying the dataset
  ## segments. We build this list of target and a specification of the
  ## correspending data subset. Eventually move all plotting to use
  ## this approach rather than using bind.cmd.

  genericDataSet <- data.frame(All=sprintf('%s$%%s', dataset))
  if (not.null(targets))
    for (i in 1:length(targets))
    {
      tmpDataSet <- data.frame(New=sprintf('%s[crs$dataset%s$%s=="%s",]$%%s',
                                 dataset,
                                 ifelse(sampling, "[crs$sample,]", ""),
                                 target, targets[i]))
      colnames(tmpDataSet) <-  c(targets[i])
      genericDataSet <- cbind(genericDataSet, tmpDataSet)
    }

  ## Generate a plot for each variable. If there are too many
  ## variables, ask the user if we want to continue.

  totalPlots <- nboxplots + nhisplots + length(cumplots) +
    nbenplots + nbarplots + ndotplots
  
  if (totalPlots > 10 && pmax == 1)
    if (is.null(questionDialog("Rattle is about to generate", totalPlots,
                               "individual plots. That's quite a few.",
                               "You could select fewer variables, or you",
                               "can change the number of plots per page,",
                               "but you can also proceed if you like.",
                               "Would you like to proceed?")))
      return()

  ##---------------------------------------------------------------------

  if (nboxplots > 0)
  {
    ## DESCRIPTION
    ## Box plots for numeric data.
    ##
    ## DETAIL A box plot shows the distribution of numeric data
    ## graphically. The box iteself extends from the lower to the
    ## upper quartiles with the median drawn in the box. The lines
    ## then extend to the maximum and minimum points that are no more
    ## than 1.5 times the interquartile range from the
    ## median. Outliers are then also plotted as points. The notches
    ## indicate significant differences, in that if nocthes do not
    ## overlap, then the distribution medians are significantly
    ## different.")

    plot.cmd <- paste('boxplot(dat ~ grp, ds,',
                     sprintf('col=rainbow(%d),', length(targets)+1),
                     ifelse(is.null(targets), "",
                            sprintf('xlab="%s",', target)),
                     'notch=TRUE)')

    doByLibrary <- "require(doBy, quietly=TRUE)"
    
    ## TODO: Try using "by" instead of needing another package to
    ## provide summaryBy. Also, the new version of doBy (061006) seems
    ## to be outputting extra status information that makes the R
    ## Console a little chatty unneccessarily - perhaps this will
    ## disappear again - it looks like debugging information!
    ##
    ## status:
    ## lhsvar     : dat 
    ## rhsvar     : grp 
    ## idvar      :  
    ## fun.names  : mean 
    ## varPrefix  : mean 
    ## newNames   : mean.dat 

    mean.cmd <- paste(sprintf("points(1:%d,", length(targets)+1),
                     "summaryBy(dat ~ grp, data=ds, FUN=mean)$mean.dat,",
                     "pch=8)")
    
    for (s in 1:nboxplots)
    {

      addLogSeparator()
      addToLog("BOX PLOT")

      cmd <- paste("sprintf(bind.cmd,",
                   paste(paste('"', rep(boxplots[s], length(targets)+1), '"',
                               sep=""),
                         collapse=","),
                   ")")
      cmd <- eval(parse(text=cmd))
      addToLog(paste("Generate just the data for a boxplot of ",
                    boxplots[s], ".", sep=""),
              paste("ds <-", cmd))
      ds <- eval(parse(text=cmd))

      if (pcnt %% pmax == 0) newPlot(pmax)
      pcnt <- pcnt + 1
      
      addToLog("Plot the data, grouped appropriately.", plot.cmd)
      eval(parse(text=plot.cmd))

      if (packageIsAvailable("doBy", "add means to box plots"))
      {
        addToLog("Use the doBy package to group the data for means.",
                 doByLibrary)
        eval(parse(text=doByLibrary))

        addToLog("Calculate the group means.", mean.cmd)
        eval(parse(text=mean.cmd))
      }
        
      ## Add a value for the mean to each, as in DMSurvivorP196.
      title.cmd <- genPlotTitleCmd(sprintf("Distribution of %s%s",
                                          boxplots[s],
                                          ifelse(sampling, " (sample)","")))
      addToLog("Add a title to the plot.", title.cmd)
      eval(parse(text=title.cmd))
    }
  }

  ##--------------------------------------------------------------------
  
  if (nhisplots > 0)
  {
    ## Plot a histogram for numeric data.

    plot.cmd <- paste('hs <- hist(ds[ds$grp=="All",1], main="", xlab="", ',
                      'col=rainbow(10))\n',
                      'dens <- density(ds[ds$grp=="All",1])\n',
                      'rs <- max(hs$counts)/max(dens$y)\n',
                      'lines(dens$x, dens$y*rs, type="l")',
                      sep="")
    rug.cmd <- 'rug(ds[ds$grp=="All",1])'

    for (s in 1:nhisplots)
    {
      addLogSeparator()
      addToLog("HISTOGRAM")
      
      cmd <- paste("sprintf(bind.cmd,",
                   paste(paste('"', rep(hisplots[s], length(targets)+1), '"',
                               sep=""),
                         collapse=","),
                   ")")
      cmd <- eval(parse(text=cmd))
      addToLog(paste("Generate just the data for a histogram of ",
                    hisplots[s], ".", sep=""),
              paste("ds <-", cmd))
      ds <- eval(parse(text=cmd))

      if (pcnt %% pmax == 0) newPlot(pmax)
      pcnt <- pcnt + 1
      
      addToLog("Plot the data.", plot.cmd)
      eval(parse(text=plot.cmd))
      addToLog("Add a rug to illustrate density.", rug.cmd)
      eval(parse(text=rug.cmd))
      title.cmd <- genPlotTitleCmd(sprintf("Distribution of %s%s",
                                           hisplots[s],
                                           ifelse(sampling, " (sample)","")))
      addToLog("Add a title to the plot.", title.cmd)
      eval(parse(text=title.cmd))
    }
  }

  ##---------------------------------------------------------------------
  
  if (not.null(cumplots))
  {
    ## Cumulative plot for numeric data.

    nplots <- length(cumplots)

    lib.cmd <- "require(Hmisc, quietly=TRUE)"
    
    for (s in 1:nplots)
    {
      addLogSeparator()

      col <- rainbow(length(targets)+1)
      plot.cmd <- paste('Ecdf(ds[ds$grp=="All",1],',
                       sprintf('col="%s",', col[1]),
                       'xlab="",',
                       'subtitles=FALSE)\n')
      if (not.null(targets))
      for (t in 1:length(targets))
      {
        plot.cmd <- paste(plot.cmd,
                         sprintf('Ecdf(ds[ds$grp=="%s",1], ', targets[t]),
                         sprintf('col="%s", lty=%d, ', col[t+1], t+1),
                         'xlab="", subtitles=FALSE, add=TRUE)\n',
                         sep="")
      }

      if (not.null(targets))
        legend.cmd <- sprintf(paste('legend("bottomright", c(%s), ',
                                   "col=rainbow(%d), lty=1:%d,",
                                   'title="%s", inset=c(0.05,0.05))'),
                             paste(sprintf('"%s"', c("All", targets)),
                                   collapse=","),
                             length(targets)+1, length(targets)+1,
                             target)
        
      cmd <- paste("sprintf(bind.cmd,",
                    paste(paste('"', rep(cumplots[s], length(targets)+1), '"',
                               sep=""),
                         collapse=","),
                   ")")
      cmd <- eval(parse(text=cmd))
      addToLog(paste("Generate just the data for an Ecdf plot of",
                    cumplots[s], "."),
              paste("ds <-", cmd))
       ds <- eval(parse(text=cmd))

      if (pcnt %% pmax == 0) newPlot(pmax)
      pcnt <- pcnt + 1

      if (! packageIsAvailable("Hmisc", "plot cumulative charts")) break()

      addToLog("Use Ecdf from the Hmisc package.", lib.cmd)
      eval(parse(text=lib.cmd))

      addToLog("Plot the data.", plot.cmd)
      eval(parse(text=plot.cmd))
      title.cmd <- genPlotTitleCmd(sprintf("Cumulative %s%s",
                                           cumplots[s],
                                           ifelse(sampling, " (sample)","")))

      if (not.null(targets))
      {
        addToLog("Add a legend to the plot.", legend.cmd)
        eval(parse(text=legend.cmd))
      }

      addToLog("Add a title to the plot.", title.cmd)
      eval(parse(text=title.cmd))
    }
  }

  ##---------------------------------------------------------------------

  if (nbenplots > 0)
  {
    ## Plot Benford's Law for numeric data.

    barbutton <- theWidget("benford_bars_checkbutton")$getActive()
    
    ## Using barplot2 from gplots
    
    lib.cmd <- "require(gplots, quietly=TRUE)"

    ## Calculate the expected distribution according to Benford's Law
    
    expect.cmd <- paste('unlist(lapply(1:9, function(x) log10(1 + 1/x)))')

    ## Construct the command to plot the distribution.

    if (barbutton)
    {
      plot.cmd <- paste('barplot2(ds, beside=TRUE,',
                       'xlab="Initial Digit", ylab="Probability")')
    }
    else
    {
      plot.cmd <- paste('plot(1:9, ds[1,], type="b", pch=19, col=rainbow(1), ',
                       'ylim=c(0,max(ds)), axes=FALSE, ',
                       'xlab="Initial Digit", ylab="Probability")\n',
                       'axis(1, at=1:9)\n', 'axis(2)\n',
                       sprintf('points(1:9, ds[2,], col=%s, pch=19, type="b")\n',
                               ifelse(is.null(target), "rainbow(2)[2]",
                                      sprintf("rainbow(%d)[2]",
                                              length(targets)+2))),
                       sep="")
      if (not.null(targets))
        for (i in 1:length(targets))
        {
          plot.cmd <- sprintf(paste('%s\npoints(1:9, ds[%d,],',
                                   'col=%s, pch=%d, type="b")'),
                             plot.cmd, i+2,
                             sprintf("rainbow(%d)[%d]",
                                     length(targets)+2, i+2),
                             19)
        }
    }
    if (packageIsAvailable("gplots", "plot a bar chart for Benford's Law"))
    {
      addLogSeparator()
      addToLog("BENFORD'S LAW")
      
      addToLog("Use barplot2 from gplots to plot Benford's Law.", lib.cmd)
      eval(parse(text=lib.cmd))
      
      addToLog("Generate the expected distribution for Benford's Law",
               paste("expect <-", expect.cmd))
      expect <- eval(parse(text=expect.cmd))

      if (is.null(targets) && ! barbutton)
      {
        # Plot all Benford's plots on the one line graph

        addLogSeparator()

        bc <- sub("All", "%s", substr(bind.cmd, 7, nchar(bind.cmd)-1))
        new.bind.cmd <- substr(bind.cmd, 1, 6)
        data.cmd <- 't(as.matrix(data.frame(expect=expect'
        plot.cmd <- paste('plot(1:9, ds[1,], type="b", ',
                         'pch=19, col=rainbow(1), ',
                         'ylim=c(0,max(ds)), axes=FALSE, ',
                         'xlab="Initial Digit", ylab="Probability")\n',
                         'axis(1, at=1:9)\n', 'axis(2)\n',
                         sep="")
        for (s in 1:nbenplots)
        {
          new.bind.cmd <- paste(new.bind.cmd, 
                           sprintf(bc, benplots[s], benplots[s]),
                           ",\n     ",
                           sep="")
          data.cmd <- paste(data.cmd, ",\n     ",
                           sprintf(paste('"%s"=calcInitialDigitDistr',
                                         '(ds[ds$grp=="%s", 1])', sep=""),
                                   benplots[s], benplots[s]),
                           sep="")
          plot.cmd <- paste(plot.cmd,
                           sprintf(paste('points(1:9, ds[%d,],',
                                         'col=%s, pch=19, type="b")\n'),
                                   s+1, sprintf("rainbow(%d)[%d]",
                                                nbenplots+1, s+1)),
                           sep="")
        }
        new.bind.cmd <- paste(substr(new.bind.cmd, 1, nchar(new.bind.cmd)-7), ")",
                            sep="")
        data.cmd <- paste(data.cmd, ")))", sep="")

        legend.cmd <- sprintf(paste('legend("topright", c(%s), ',
                                   'fill=rainbow(%d), title="%s")'),
                             paste(sprintf('"%s"',
                                           c("Benford", benplots)),
                                   collapse=","),
                             nbenplots+1, "Variables")

        addToLog("Generate the required data.",
                 paste("ds <-", new.bind.cmd))
        ds <- eval(parse(text=new.bind.cmd))

        addToLog("Generate specific plot data.", paste("ds <-", data.cmd))
        ds <- eval(parse(text=data.cmd))

        if (pcnt %% pmax == 0) newPlot(pmax)
        pcnt <- pcnt + 1

        par(xpd=TRUE)
        
        addToLog("Now do the actual plot.", plot.cmd)
        eval(parse(text=plot.cmd))

        addToLog("Add a legend to the plot.", legend.cmd)
        eval(parse(text=legend.cmd))
        
        if (sampling)
          title.cmd <- genPlotTitleCmd("Benford's Law (sample)")
        else
          title.cmd <- genPlotTitleCmd("Benford's Law")

        addToLog("Add a title to the plot.", title.cmd)
        eval(parse(text=title.cmd))
        

      }
      else
      {
        # Plot multiple graphs.
        
        for (s in 1:nbenplots)
        {
          addLogSeparator()

          data.cmd <- paste('t(as.matrix(data.frame(expect=expect,\n    ',
                           'All=calcInitialDigitDistr(ds[ds$grp=="All", 1])')
        
          if (not.null(targets))
            for (t in 1:length(targets))
              data.cmd <- paste(data.cmd, ",\n     ",
                               sprintf('"%s"=', targets[t]),
                               'calcInitialDigitDistr(ds[ds$grp==',
                               sprintf('"%s", ', targets[t]), '1])',
                               sep="")
          data.cmd <- paste(data.cmd, ")))", sep="")

          if (not.null(targets))
            if (barbutton)
              legend.cmd <- sprintf(paste('legend("topright", c(%s), ',
                                         'fill=heat.colors(%d), title="%s")'),
                                   paste(sprintf('"%s"',
                                                 c("Benford", "All", targets)),
                                         collapse=","),
                                   length(targets)+2, target)
            else
              legend.cmd <- sprintf(paste('legend("topright", c(%s), ',
                                         'fill=rainbow(%d), title="%s")'),
                                   paste(sprintf('"%s"',
                                                 c("Benford", "All", targets)),
                                         collapse=","),
                                   length(targets)+2, target)
          else
            if (barbutton)
              legend.cmd <- paste('legend("topright", c("Benford", "All"),',
                                 'fill=heat.colors(2))')
            else
              legend.cmd <- paste('legend("topright", c("Benford", "All"), ',
                                 'fill=rainbow(2))')
          
          cmd <- paste("sprintf(bind.cmd,",
                       paste(paste('"', rep(benplots[s], length(targets)+1),
                                   '"', sep=""), collapse=","),
                       ")")
          cmd <- eval(parse(text=cmd))
          
          addToLog(paste("Generate just the data for the plot of",
                         benplots[s], "."),
                   paste("ds <-", cmd))
          ds <- eval(parse(text=cmd))
          
          addToLog("Generate frequency of initial digit.",
                   paste("ds <-", data.cmd))
          ds <- eval(parse(text=data.cmd))

          nan.cmd <- "ds[is.nan(ds)] <- 0"
          addToLog("Ensure rows with no digits are treated as zeros.", nan.cmd)
          ds[is.nan(ds)] <- 0
          
          if (pcnt %% pmax == 0) newPlot(pmax)
          pcnt <- pcnt + 1

          par(xpd=TRUE)
          
          addToLog("Now do the actual Benford plot.", plot.cmd)
          eval(parse(text=plot.cmd))
          
          addToLog("Add a legend to the plot.", legend.cmd)
          eval(parse(text=legend.cmd))
          
          if (sampling)
            title.cmd <- genPlotTitleCmd(sprintf("Benford's Law: %s (sample)",
                                                 benplots[s]))
          else
            title.cmd <- genPlotTitleCmd(sprintf("Benford's Law: %s",
                                                 benplots[s]))
          addToLog("Add a title to the plot.", title.cmd)
          eval(parse(text=title.cmd))
        }
      }
    }
  }

  ##---------------------------------------------------------------------

  if (nbarplots > 0)
  {
    ## Plot a frequency plot for a categorical variable.

    ## Use barplot2 from gplots.
    
    lib.cmd <- "require(gplots, quietly=TRUE)"

    ## Construct a generic data command built using the genericDataSet
    ## values. To generate a barplot we use the output of the summary
    ## command on each element in the genericDataSet, and bring them
    ## together into a single structure. The resulting
    ## generic.data.cmd will have a number of "%s"s (one for the whole
    ## dataset, then one for each level) from the original
    ## genericDataSet string that will be replaced with the name of
    ## each variable as it is being plotted.

    generic.data.cmd <- paste(lapply(genericDataSet,
                                   function(x) sprintf("summary(%s)", x)),
                            collapse=",\n    ")
    generic.data.cmd <- sprintf("rbind(%s)", generic.data.cmd)

    ## If the gplots package is available then generate a plot for
    ## each chosen vairable.
    
    if (packageIsAvailable("gplots", "plot a bar chart"))
    {
      addLogSeparator()
      addToLog("Use barplot2 from gplots for the barchart.", lib.cmd)
      eval(parse(text=lib.cmd))

      for (s in 1:nbarplots)
      {

        addLogSeparator()

        ## Construct and evaluate a command string to generate the
        ## data for the plot.

        ds.cmd <- paste(sprintf("sprintf('%s',", generic.data.cmd),
                       paste(paste('"', rep(barplots[s], length(targets)+1),
                                 '"', sep=""), collapse=","), ")")
        ds.cmd <- eval(parse(text=ds.cmd))
        addToLog("Generate the summary data for plotting.",
                 paste("ds <-", ds.cmd))
        ds <- eval(parse(text=ds.cmd))
        
        ## Construct and evaluate the command to plot the
        ## distribution.  Determine maxium value so that the y axis
        ## can extend to it. We save the output from barplot2 in order
        ## to add numbers to the plot.
    
        if (pcnt %% pmax == 0) newPlot(pmax)
        pcnt <- pcnt + 1

        #if (is.null(target))
        #  ord.cmd <- 'order(ds[1,])'
        #else
          ord.cmd <- 'order(ds[1,], decreasing=TRUE)'
        addToLog("Sort the entries.", paste("ord <-", ord.cmd))
        ord <- eval(parse(text=ord.cmd))

        maxFreq <- max(ds)
        plot.cmd <- sprintf(paste('barplot2(ds[,ord], beside=TRUE,',
                                 'ylim=c(0, %d), col=rainbow(%d))'),
                           round(maxFreq+maxFreq*0.20), length(targets)+1)
        addToLog("Plot the data.", paste("bp <- ", plot.cmd))
        bp <- eval(parse(text=plot.cmd))

        ## Construct and evaluate a command to add text to the top of
        ## the bars in the bar chart. Only do this if there are not
        ## too many values for the category, otherwise the numbers
        ## look bad. I could, alternatively, scale the font?

        if (ncol(bp) <= 5)
        {
          text.cmd <- sprintf("text(bp, ds[,ord]+%d, ds[,ord])",
                             round(maxFreq*0.040))
          addToLog("Add the actual frequencies.", text.cmd)
          eval(parse(text=text.cmd))
        }

        ## Construct and evaluate a command to add a legend to the
        ## plot, but only if there is a target, optherwise it is
        ## obvious.
        
        if (not.null(targets))
        {
          legend.cmd <- sprintf(paste('legend("topright", c(%s), ',
                                     "fill=rainbow(%d), ",
                                     'title="%s")'),
                               paste(sprintf('"%s"', c("All", targets)),
                                     collapse=","),
                               length(targets)+1,
                               target)
          addToLog("Add a legend to the plot.", legend.cmd)
          eval(parse(text=legend.cmd))
        }
        
        ## Construct and evaluate a command to add the title to the
        ## plot.
        
        title.cmd <- genPlotTitleCmd(sprintf("Distribution of %s%s",
                                            barplots[s],
                                            ifelse(sampling," (sample)","")))
        addToLog("Add a title to the plot.", title.cmd)
        eval(parse(text=title.cmd))
      }
    }
  }

  ##---------------------------------------------------------------------

  if (ndotplots > 0)
  {
    
    ## Construct a generic data command built using the genericDataSet
    ## values. To generate a barplot we use the output of the summary
    ## command on each element in the genericDataSet, and bring them
    ## together into a single structure. The resulting generic.data.cmd
    ## will have a number of "%s"s (one for the whole dataset, then
    ## one for each level) from the original genericDataSet string
    ## that will be replaced with the name of each variable as it is
    ## being plotted.

    generic.data.cmd <- paste(lapply(genericDataSet,
                                   function(x) sprintf("summary(%s)", x)),
                            collapse=",\n    ")
    generic.data.cmd <- sprintf("rbind(%s)", generic.data.cmd)

      
    addToLog("Use dotplot from lattice for the plots.", lib.cmd)
    eval(parse(text=lib.cmd))

    for (s in 1:ndotplots)
    {

      addLogSeparator()

      ## Construct and evaluate a command string to generate the
      ## data for the plot.

      ds.cmd <- paste(sprintf("sprintf('%s',", generic.data.cmd),
                     paste(paste('"', rep(dotplots[s], length(targets)+1),
                                 '"', sep=""), collapse=","), ")")
      ds.cmd <- eval(parse(text=ds.cmd))
      addToLog("Generate the summary data for plotting.",
               paste("ds <-", ds.cmd))
      ds <- eval(parse(text=ds.cmd))

      ## Construct and evaluate the command to determin the order in
      ## which to print the catgories, from larges to smallest.

      if (is.null(target))
        ord.cmd <- 'order(ds[1,])'
      else
        ord.cmd <- 'order(ds[1,], decreasing=TRUE)'
      addToLog("Sort the entries.",
               paste("ord <-", ord.cmd))
      ord <- eval(parse(text=ord.cmd))
        
      ## Construct and evaluate the command to plot the
      ## distribution.
    
      if (pcnt %% pmax == 0) newPlot(pmax)
      pcnt <- pcnt + 1
      
      titles <- genPlotTitleCmd(sprintf("Distribution of %s%s",
                                        dotplots[s],
                                        ifelse(sampling," (sample)","")),
                                vector=TRUE)

      plot.cmd <- sprintf(paste('dotchart(%s, main="%s", sub="%s",',
                               'col=rainbow(%d),%s',
                               'xlab="Frequency", pch=19)'),
                         "ds[,ord]", titles[1], titles[2], length(targets)+1,
                         ifelse(is.null(target), "", ' labels="",'))
      addToLog("Plot the data.", plot.cmd)
      eval(parse(text=plot.cmd))

      if (not.null(target))
      {
        legend.cmd <- sprintf(paste('legend("bottomright", bg="white",',
                                   'c("All","0","1"), col=rainbow(%d),',
                                   'pch=19, title="%s")'),
                             length(targets)+1, target)
        addToLog("Add a legend.", legend.cmd)
        eval(parse(text=legend.cmd))
      }
    }
  }

  ## Update the status bar.
  
  if (totalPlots > 1)
    setStatusBar("All", totalPlots, "plots generated.")
  else if (totalPlots ==  1)
    setStatusBar("One plot generated.")
  else
    setStatusBar("No plots selected.")
}
  
executeExploreGGobi <- function(dataset)
{
  ## Based on code from Marco Lo
  
  ## Construct the commands.

  lib.cmd <- "require(rggobi, quietly=TRUE)"
  ggobi.cmd <- sprintf('gg <<- ggobi(%s)', dataset)
              
  ## Start logging and executing the R code.
  
  if (! packageIsAvailable("rggobi","explore the data using GGobi")) return()

  addLogSeparator()
  addToLog("GGobi is accessed using the rggobi package.", lib.cmd)
  eval(parse(text=lib.cmd))
  addToLog("Launch GGobi data visualization.", gsub("<<-", "<-", ggobi.cmd))
  eval(parse(text=ggobi.cmd))
  
  setStatusBar("GGobi executed.")
}

executeExploreCorrelation <- function(dataset)
{
  TV <- "correlation_textview"

  if (is.null(dataset))
  {
    errorDialog("Correlations are calculated only for numeric data.",
                 "No numeric data was found in the dataset.")
    return()
  }
  
  ## Construct the commands.

  ## Deal with showing the missing values plot.
  
  nas <- theWidget("correlation_na_checkbutton")$getActive()
  if (nas)
  {
    naids.cmd <- sprintf('naids <- attr(na.omit(t(%s)), "na.action")\n',
                         dataset)
    eval(parse(text=naids.cmd))
    if (is.null(naids))
    {
      errorDialog("The data contains no missing values, and so no",
                  "missing value correlation plot can be generated.")
      return()
    }
    if (length(naids) == 1)
    {
      errorDialog("The data contains only one column with missing values,",
                  "and so no missing value correlation plot can be generated.")
      return()
    }
  }

  lib.cmd <-"require(ellipse, quietly=TRUE)"
  crscor.cmd  <- sprintf("%scrscor <- cor(%s, use='pairwise')",
                         ifelse(nas, naids.cmd, ""),
                         ifelse(nas,
                                sprintf("is.na(%s[naids])", dataset),
                                dataset))
  crsord.cmd  <- paste("crsord <- order(crscor[1,])",
                       "crsxc  <- crscor[crsord, crsord]",
                       sep="\n")
  print.cmd   <- "print(crsxc)"
  if (nas)
  {
    print.cmd <- paste(print.cmd,
                       "\ncat('\nCount of missing values:\n')\n",
                       sprintf("print(apply(is.na(%s[naids]),2,sum))",
                               dataset),
                       "\ncat('\nPercent missing values:\n')\n",
                       sprintf("print(100*apply(is.na(%s[naids]),2,sum)/nrow(%s))",
                               dataset, dataset),
                       sep="")
    
  }
  plot.cmd    <- paste("plotcorr(crsxc, ",
                       'col=colorRampPalette(c("red", "white", "blue"))(11)',
                       '[5*crsxc + 6])\n',
                       genPlotTitleCmd("Correlation",
                                       ifelse(nas, "of Missing Values", ""),
                                       crs$dataname),
                       sep="")
  
  ## Start logging and executing the R code.

  if (! packageIsAvailable("ellipse","display a correlation plot")) return()
     
  addLogSeparator("Generate a correlation plot for the variables.")
  clearTextview(TV)

  addToLog("The correlation plot uses the ellipse package.", lib.cmd)
  eval(parse(text=lib.cmd))

  addToLog("Correlations work for numeric variables only.", crscor.cmd)
  addToLog("Order the correlations by their strength.", crsord.cmd)
  addToLog("Display the actual correlations.", print.cmd)
  addToLog("Graphically display the correlations.", plot.cmd)

  appendTextview(TV,
               ifelse(nas,
                      "Missing Values Correlation Summary.\n\n",
                      "Correlation Summary.\n\n"),
               "Note that only correlations between numeric variables ",
               "are reported.\n\n",
               collectOutput(paste(crscor.cmd,
                                    crsord.cmd,
                                    print.cmd,
                                    sep="\n")))

  newPlot()
  eval(parse(text=paste(crscor.cmd,
               crsord.cmd,
               plot.cmd,
               sep="\n")))
  
  ## Report completion to the user through the Status Bar.
  
  setStatusBar("Correlation plot and summary generated.")
}

executeExploreHiercor <- function(dataset)
{
  if (is.null(dataset))
  {
    errorDialog("Correlations are calculated only for numeric data.",
                 "Currently Rattle does not transform categorical data",
                 "into numeric data, but this can be done in R with",
                 "the as.integer() function.",
                 "No numeric variables were found in the dataset",
                 "from amongst those that are not ignored.")
    return()
  }

  ## Check that we have sufficient data

  ncols <- eval(parse(text=sprintf("NCOL(%s)", dataset)))
  if ( ncols < 2 )
  {
    errorDialog("The dataset contains less than two numeric features.",
                 "Correlations are calculated only for numeric data.",
                 "Currently Rattle does not transform categorical data",
                 "into numeric data, but this can be done in R with",
                 "the as.integer() function.",
                 "For now please select numeric variables.")
    return()
  }
    
  ## Construct the commands.
  
  cor.cmd    <- sprintf('cc <- cor(%s, use="pairwise")', dataset)
  hclust.cmd <- 'hc <- hclust(dist(cc), "ave")'
  dend.cmd   <- "dn <- as.dendrogram(hc)"
  plot.cmd   <- paste('plot(dn, horiz=TRUE, ',
                      'nodePar=list(col=3:2, cex=c(2.0, 0.75), pch= 21:22, ',
                      'bg= c("light blue", "pink"), ',
                      'lab.cex = 0.75, lab.col = "tomato"), ',
                      'edgePar=list(col="gray", lwd=2)',
                      ')\n',
                      genPlotTitleCmd("Variable Correlation Clusters",
                                     crs$dataname),
                      sep="")

  ## Start logging and executing the R code.

  addLogSeparator()

  addToLog("Generate the correlations (numerics only).", cor.cmd)
  eval(parse(text=cor.cmd))

  addToLog("Generate hierarchical cluster of variables.", hclust.cmd)
  eval(parse(text=hclust.cmd))

  addToLog("Generate the dendrogram.", dend.cmd)
  eval(parse(text=dend.cmd))

  addToLog("Now draw the dendrogram.", plot.cmd)
  newPlot()
  eval(parse(text=plot.cmd))

  ## Report completion to the user through the Status Bar.
  
  setStatusBar("Hierarchical cluste of correlations plotted.")

}

executeExplorePrcomp <- function(dataset)
{
  TV <- "prcomp_textview"
  
  if (is.null(dataset))
  {
    errorDialog("Principal components are only ipmlemented for numeric data.",
                 "No numeric variables were found in the dataset",
                 "from amongst those that are not ignored.")
    return()
  }

  ## Construct the commands.
  
  prcomp.cmd  <- sprintf('pc <<- prcomp(%s, scale=TRUE, center=TRUE, tol=0)',
                         dataset)
  print.cmd   <- "pc"
  summary.cmd <- "summary(pc)"
  plot.cmd    <- paste('plot(pc, main="")',
                       genPlotTitleCmd("Principal Components Importance",
                                      crs$dataname),
                       sep="\n")
  biplot.cmd  <- paste('biplot(pc, main="")',
                       genPlotTitleCmd("Principal Components",
                                      crs$dataname),
                       sep="\n")

  ## Start logging and executing the R code.

  addLogSeparator()
  clearTextview(TV)
  
  addToLog("Perform a principal components analysis (numerics only).",
          gsub("<<-", "<-", prcomp.cmd))
  eval(parse(text=prcomp.cmd))

  appendTextview(TV, "Note that principal components on only the numeric\n",
                  "variables is calculated, and so we can not use this\n",
                  "approach to remove categorical variables from ",
                  "consideration.\n\n",
                  "Any numeric variables with relatively large rotation\n",
                  "values (negative or positive) in any of the first few\n",
                  "components are generally variables that you may wish\n",
                  "to include in the modelling.")

  addToLog("Show the output of the analysis,", print.cmd)
  appendTextview(TV, collectOutput(print.cmd, TRUE))
  
  addToLog("Summarise the importance of the components found.", summary.cmd)
  appendTextview(TV, collectOutput(summary.cmd, TRUE))

  newPlot(1)
  addToLog("Display a plot showing the relative importance of the components.",
          plot.cmd)
  eval(parse(text=plot.cmd))
  
  newPlot(1)
  addToLog("Display a plot showing the two most principal components.",
          biplot.cmd)
  eval(parse(text=biplot.cmd))
  
  ## Report completion to the user through the Status Bar.
  
  setStatusBar("A principal components analysis has been completed.")

}

########################################################################
##
## EVALUATE TAB
##

##----------------------------------------------------------------------
##
## INTERFACE CALLBACKS
##

on_evaluate_csv_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    theWidget("evaluate_filechooserbutton")$setSensitive(TRUE)
  else
    theWidget("evaluate_filechooserbutton")$setSensitive(FALSE)
  setStatusBar()
}

on_evaluate_rdataset_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    theWidget("evaluate_rdataset_combobox")$setSensitive(TRUE)
  else
    theWidget("evaluate_rdataset_combobox")$setSensitive(FALSE)
  setStatusBar()
}

on_confusion_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    .EVALUATE$setCurrentPage(.EVALUATE.CONFUSION.TAB)
  setStatusBar()
}

on_risk_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    .EVALUATE$setCurrentPage(.EVALUATE.RISK.TAB)
    theWidget("evaluate_risk_variable_label")$setSensitive(TRUE)
    theWidget("evaluate_risk_label")$setSensitive(TRUE)
  }
  else
  {
    theWidget("evaluate_risk_variable_label")$setSensitive(FALSE)
    theWidget("evaluate_risk_label")$setSensitive(FALSE)
  }  
  setStatusBar()
}

on_lift_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    .EVALUATE$setCurrentPage(.EVALUATE.LIFT.TAB)
  setStatusBar()
}

on_roc_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    .EVALUATE$setCurrentPage(.EVALUATE.ROC.TAB)
  setStatusBar()
}

on_precision_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    .EVALUATE$setCurrentPage(.EVALUATE.PRECISION.TAB)
  setStatusBar()
}

on_sensitivity_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    .EVALUATE$setCurrentPage(.EVALUATE.SENSITIVITY.TAB)
  setStatusBar()
}

on_score_radiobutton_toggled <- function(button)
{
  setStatusBar()
}

on_evaluate_radiobutton_group_changed <- function(action, window)
{
  setStatusBar()
}

on_risk_comboboxentry_changed <- function(action, window)
{
  setStatusBar()
}

##----------------------------------------------------------------------
##
## Support Functions

getEvaluateModels <- function()
{
  ## Return a list of models selected for evaluation

  models <- c()
  for (m in .MODELLERS)
    if (theWidget(paste(m, "_evaluate_checkbutton", sep=""))$getActive())
      models <- c(models, m)
  return(models)
}

current.evaluate.tab <- function()
{
  cp <- .EVALUATE$getCurrentPage()
  return(.EVALUATE$getTabLabelText(.EVALUATE$getNthPage(cp)))
}


##----------------------------------------------------------------------
##
## Execution
##

executeEvaluateTab <- function()
{

  ## Ensure a dataset exists.

  if (noDatasetLoaded()) return()

  ## Obtain some background information
  
  mtypes <- getEvaluateModels() # The chosen model types in the Evaluate tab.
  
  if (is.null(mtypes))
  {
    errorDialog("No model has been specified.",
                 "Please slect one or more from the list of models available.")
    return()
  }

  ## Ensure we recognise the model type.
  
  if (length(setdiff(mtypes, .MODELLERS)) > 0)
  {
    errorDialog("E121: A model type is not recognised.",
                "We found the model types to be:", mtypes,
                "Yet, Rattle only knows about:", .MODELLERS,
                "This is a Rattle bug.",
                "Please report this to Graham.Williams@togaware.com.")
    return()
  }

  ## Ensure there is a model for each that is selected.

  if (sum(sapply(mtypes, function(x) is.null(crs[[x]]))) > 0)
  {
    errorDialog("E120: Some model has not been built?",
                "We found the model types to be:", mtypes,
                "The models not built:",
                sapply(mtypes, function(x) is.null(crs[[x]])),
                "This is a Rattle bug.",
                "Please report this to Graham.Williams@togaware.com.")
    return()
  }

  ## Ensure the appropriate package is loaded (in the case, for
  ## example, when loading a project and going straight to Evaluate,
  ## and wanting to run predict.svm on new data).

  if (.ADA %in%  mtypes &&
      ! packageIsAvailable("ada", "evaluate an adaboost model"))
    return()
  if (.KSVM %in%  mtypes &&
      ! packageIsAvailable("kernlab", "evaluate this SVM"))
    return()
  if (.RF %in%  mtypes &&
      ! packageIsAvailable("randomForest", "evaluate this rf"))
    return()
  if (.NNET %in%  mtypes &&
      ! packageIsAvailable("nnet", "evaluate a neural network model"))
    return()

  ## Identify the data on which evaluation is to be performed.

  testset0 <- "crs$dataset"
  testname <- crs$dataname
  included <- getIncludedVariables() # Need all vars, including risk.

  addLogSeparator()

  if (theWidget("evaluate_training_radiobutton")$getActive())
  {
    infoDialog("You are using the same dataset to evaluate your model as you",
                "did to build it. This will give you an optimistic estimate",
                "of the performance of your model. You may want to choose",
                "to sample the dataset and evaluate the model on the",
                "test dataset, or else",
                "load a separate test dataset from a CSV File or a",
                "pre-existing R Dataset here.")

    if (theWidget("sample_checkbutton")$getActive())
      if (is.null(included))
        testset0 <- "crs$dataset[crs$sample,]"
      else
        testset0 <- sprintf("crs$dataset[crs$sample, %s]", included)
    else
      if (is.null(included))
        testset0 <- "crs$dataset"
      else
        testset0 <- sprintf("crs$dataset[,%s]", included)

    testname <- sprintf("%s [**train**]", crs$dataname)
  }
  else if (theWidget("evaluate_testing_radiobutton")$getActive())
  {
    if (is.null(included))
      testset0 <- "crs$dataset[-crs$sample,]"
    else
      testset0 <- sprintf("crs$dataset[-crs$sample, %s]", included)
    testname <- sprintf("%s [test]", crs$dataname)
  }
  else if (theWidget("evaluate_csv_radiobutton")$getActive())
  {
    filename <- theWidget("evaluate_filechooserbutton")$getFilename()
    setDefaultPath(filename)

    if (is.null(filename))
    {
      errorDialog("You have requested that a CSV file be used",
                   "as your testing dataset, but you have not",
                   "identified which file. Please use the file",
                   "chooser button to select the CSV file you wish",
                   "to use as your testset for evaluation.")
      return()
    }
                   
    ## Only load the test dataset if it is not already loaded.
    
    if (not.null(filename) &&
        (is.null(crs$testname) || (basename(filename) != crs$testname)))
    {
      ## Fix filename for MS/Windows - otherwise eval/parse strips the \\.

      if (isWindows()) filename <- gsub("\\\\", "/", filename)

      nastring <- ', na.strings=c(".", "NA")'
      read.cmd <- sprintf('crs$testset <<- read.csv("%s"%s)',
                          filename, nastring)
      addToLog("Read a file for evaluating the model",
              gsub("<<-", "<-", read.cmd))
      eval(parse(text=read.cmd))

      ## TODO The following case for included assumes the same column
      ## orders. Should really check this to make sure.
      
      if (is.null(included))
        testset0 <- "crs$testset"
      else
        testset0 <- sprintf("crs$testset[,%s]", included)
      testname <- basename(filename)
      crs$testname <<- testname
    }
    else
    {
      testset0 <- "crs$testset"
      testname <- crs$testname
    }
  }
  else if (theWidget("evaluate_rdataset_radiobutton")$getActive())
  {
    dataset <- theWidget("evaluate_rdataset_combobox")$
               getActiveText()

    if (is.null(dataset) || nchar(dataset) == 0)
    {
      errorDialog("The R Dataset is active but",
                   "no dataset name has been specified.",
                   "Please identify the name of the R dataset",
                   "on which you would like to evaluate the model.",
                   "This dataset will be one that has been defined",
                   "in the R Console.")
      return()
    }

    testset0 <- 'crs$testset'
    testname <- dataset
    crs$testname <<- testname
    
    assign.cmd <- sprintf("crs$testset <<- %s", dataset)
    addToLog("Assign the R dataset to be used as the test set.", assign.cmd)
    eval(parse(text=assign.cmd))
  }

  ## Ensure the test dataset has the same levels for each variable of
  ## the training dataset. This can arise when we externally split a
  ## dataset into a training and testing dataset, and the smaller
  ## testing dataset may not have representatives of all of the
  ## variables. Be sure to add any new levels to the end, otherwise
  ## you'll end up renaming some of the other levels! This won't help
  ## a model that uses the variable and does not find the particular
  ## level, although it is okay if it is missing levels. TODO this
  ## might need to check for the former and error out if it is the
  ## case.

  if (not.null(crs$testname) && crs$testname != crs$dataname)
    for (c in colnames(crs$dataset))
      if (is.factor(crs$dataset[[c]]))
        levels(crs$testset[[c]]) <<- c(levels(crs$testset[[c]]),
                                       setdiff(levels(crs$dataset[[c]]),
                                               levels(crs$testset[[c]])))

  ## The default command for prediction from any model is
  ## predict(model, data). Here we tune the predict command to
  ## particular types of models where they have specific general
  ## requirements. We then modify the default predict command to
  ## generate either a prediction of the response or a probability of
  ## the class, as appropriate to the particular evaluator.
  ##
  ## PREDICT: crs$pr <<- predict(crs$model, crs$testset[crs$sample, c(...)])
  
  ## PROBABILITY: this predicts a matrix, each column a probability
  ## for that class.

  ## We want to obtain the probablity of class 1 (i.e., the second of
  ## a two level class). Start with the default predict.cmd.

  ## Now build model specific strings for each model

  testset <- list() # The string representing the test dataset
  predcmd <- list() # Command string for predictions
  respcmd <- list() # Command string for response - class of entities
  probcmd <- list() # Command string for probability
  
  if (.ADA %in%  mtypes)
  {
    testset[[.ADA]] <- testset0

    predcmd[[.ADA]] <- sprintf("crs$pr <<- predict(crs$ada, %s)",
                              testset[[.ADA]])
    respcmd[[.ADA]] <- predcmd[[.ADA]]
    probcmd[[.ADA]] <- sprintf("%s[,2]",
                              gsub(")$", ', type="prob")', predcmd[[.ADA]]))
  }

##   if ( .NNET %in%  mtypes)
##   {
##     testset[[.NNET]] <- testset0

##     predcmd[[.NNET]] <- sprintf("crs$pr <<- predict(crs$nnet, %s)",
##                               testset[[.NNET]])
##     respcmd[[.NNET]] <- predcmd[[.NNET]]
##     probcmd[[.NNET]] <- sprintf("%s[,2]",
##                               gsub(")$", ', type="class")', predcmd[[.NNET]]))
##   }

  if (.RPART %in%  mtypes)
  {
    testset[[.RPART]] <- testset0
    predcmd[[.RPART]] <- sprintf("crs$pr <<- predict(crs$rpart, %s)",
                                testset[[.RPART]])

    ## For .RPART, the default is to generate class probabilities for
    ## each output class, so ensure we instead generate the response.
  
    respcmd[[.RPART]] <- gsub(")$", ', type="class")', predcmd[[.RPART]])

    ## For RPART the default predict command generates the probabilities
    ## for each class and we assume we are interested in the final class
    ## (i.e., for binary classification we are interested in the 1's).
    
    probcmd[[.RPART]] <- sprintf("%s[,2]", predcmd[[.RPART]])
  }
    
  if (.RF %in%  mtypes)
  {
    testset[[.RF]] <- testset0
    predcmd[[.RF]] <- sprintf("crs$pr <<- predict(crs$rf, %s)",
                             testset[[.RF]])

    ## The default for .RF is to predict the class, so no
    ## modification of the predict command is required.

    respcmd[[.RF]] <- predcmd[[.RF]]

    ## For RF we request a probability with the type argument, and as
    ## with RPART we extract the column of interest (the last column).
  
    probcmd[[.RF]] <- sprintf("%s[,2]",
                             gsub(")$", ', type="prob")', predcmd[[.RF]]))

  }
    
  if (.KSVM %in%  mtypes)
  {

    ## For SVM and KSVM, we need to deal with NA's. The predict seems to
    ## need to have NA's removed from the testset, (unlike rpart and rf
    ## where perhaps the NAs don't appear in columns that are used in
    ## the model? An SVM will use all the columns. But in the way we
    ## construct the evaluate command we add extra columns in the third
    ## argument to make sure we get the risk variable in the dataset.
    ## So we need to ensure we get the same subset. It might be smaller
    ## otherwise since the extra columns may have NAs.
    ##
    ## 060527 Comment this out since I was ending up with different
    ## lengths in the 2nd and 3rd arguments in the call to evaluateRisk
    ## in the svm stuff? Using survery-2k the 2nd arg length was 600 and
    ## the 3rd 561, both using na.omit. Perhaps the 3rd should not be
    ## using na.omit, but I haven't investigated this.
    ##
    ## 060603 Put this back in!!! I was again getting the 600 in the
    ## testdata and 561 in the result from predict. Doing a
    ## na.omit(testset) resulted in 561, from the original 600., which
    ## matches the number output from the predict. Try out
    ## survey-training with 10% training to see that that also works! I
    ## suspect that if it does not work, then the issue is missing
    ## levels.
    ## 060622 Seems like the problem is that the na.omit is working on
    ## different subsets of the columns:
    ##   na.omit(crs$dataset[-crs$sample, c(2:22,25)]) versus
    ##   na.omit(crs$dataset[-crs$sample,])
    ## because in the second one we want to retrieve the Risk variable,
    ## which is
    ## not in the first! Instead, let's always extract the list of omitted
    ## rows, and use that here.
    ##
    ##  romit <- attr(na.omit(testset), "na.action")[]
    ## Then use testset[-romit,]
    ## Note that predict automatically removes NAs.
    ## I.e.
    ## crs$eval <- evaluateRisk(crs$pr, na.omit(crs$dataset[-crs$sample,
    ## c(2:22,25)])$Target1,crs$dataset[-crs$sample,][-romit,]$NETADJ_AS_LBLTY)
    ##
    ## 060623 For now in the risk chart function we add the risk
    ## variable back into the testset to ensure it can be accessed,
    ## whereas previously we added in all columns to ensure the risk
    ## variable was included, and this latter resulted in much more
    ## potential for a row to include an NA, and hence to be omitted,
    ## leading to different sized vetors being passed to evaluateRisk. I
    ## hope this now solves the problem and we don't need the top
    ## solution for now.

    testset[[.KSVM]] <- sprintf("na.omit(%s)", testset0)

    predcmd[[.KSVM]] <- sprintf("crs$pr <<- predict(crs$ksvm, %s)",
                               testset[[.KSVM]])

    ## The default for KSVM is to predict the class, so no
    ## modification of the predict command is required.

    respcmd[[.KSVM]] <- predcmd[[.KSVM]]

    ## For KSVM we request a probability with the type argument set to
    ## probability (but need prob.model=TRUE in model building). For SVM
    ## we request probabilities by setting probablity=TRUE and don't
    ## need the second column stuff (and in building the model we needed
    ## probability=TRUE).

    probcmd[[.KSVM]] <- sprintf("%s[,2]",
                               gsub(")$",
                                    ', type="probabilities")',
                                    predcmd[[.KSVM]]))
    ## For SVM: 
    ## probability.cmd <- sprintf("%s",
    ##                             gsub(")$",
    ##                                  ', probability=TRUE)',
    ##                                  probability.cmd))
  }
    
  if (.GLM %in%  mtypes)
  {
    ## GLM's predict removes rows with missing values, so we also need
    ## to ensure we remove rows with missing values here.
    
    testset[[.GLM]] <- sprintf("na.omit(%s)", testset0)

    predcmd[[.GLM]] <- sprintf("crs$pr <<- predict(crs$glm, %s)",
                              testset[[.GLM]])

    ## For GLM, a response is a figure close to the class, either close
    ## to 1 or close to 0, so threshold it to be either 1 or 0. TODO
    ## Simplify this like?
    ##    response.cmd <- gsub("predict", "(predict",
    ##                         gsub(")$", ")>0.5)*1", response.cmd))

    respcmd[[.GLM]] <- gsub("predict", "as.factor(as.vector(ifelse(predict",
                           gsub(")$", ', type="response") > 0.5, 1, 0)))',
                                predcmd[[.GLM]]))

    ## For GLM, the response is a probability of the class.
  
    probcmd[[.GLM]] <- gsub(")$", ', type="response")', predcmd[[.GLM]])
  
  }
    
##   if (GBM %in%  mtypes)
##   {
##     testset[[GBM]] <- testset0

##     ## For GBM the default needs to know the number of trees to include.

##     predcmd[[GBM]] <- sprintf(paste("crs$pr <<- predict(crs$gbm, %s,",
##                                     "n.trees=length(crs$gbm$trees))"),
##                               testset[[GBM]])
##     respcmd[[GBM]] <- predcmd[[GBM]]
##     probcmd[[GBM]] <- predcmd[[GBM]]
##   }

  ## Currently (and perhaps permanently) the ROCR package deals only
  ## with binary classification, as does my own Risk Chart.
  
  if (!theWidget("confusion_radiobutton")$getActive()
      && is.factor(crs$dataset[[crs$target]])
      && length(levels(crs$dataset[[crs$target]])) > 2)
  {
    errorDialog("The number of levels in the target is > 2.",
                 "Currently, Rattle's Risk chart, and the ROCR package",
                 "(which implements the Lift, ROC, Precision, and Specificity",
                 "charts) apply only for binary classification.",
                 "Either restructure the data for binary classificaiton,",
                 "or else suggest an alternative to the author of Rattle",
                 "at Graham.Williams@togaware.com!")
    return()
  }

  ## DISPATCH
  
  if (theWidget("confusion_radiobutton")$getActive())
    msg <- executeEvaluateConfusion(respcmd, testset, testname)
  else if (theWidget("risk_radiobutton")$getActive())
    msg <- executeEvaluateRisk(probcmd, testset, testname)
  else if (theWidget("roc_radiobutton")$getActive())
    msg <- executeEvaluateROC(probcmd, testset, testname)
  else if (theWidget("lift_radiobutton")$getActive())
    msg <- executeEvaluateLift(probcmd, testset, testname)
  else if (theWidget("precision_radiobutton")$getActive())
    msg <- executeEvaluatePrecision(probcmd, testset, testname)
  else if (theWidget("sensitivity_radiobutton")$getActive())
    msg <- executeEvaluateSensitivity(probcmd, testset, testname)
  else if (theWidget("score_radiobutton")$getActive())
    msg <- executeEvaluateScore(probcmd, testset, testname)

  if (not.null(msg)) setStatusBar(msg)
}

##----------------------------------------------------------------------
##
## EVALUATE CONFUSION TABLE
##
  
executeEvaluateConfusion <- function(respcmd, testset, testname)
{
  clearTextview("confusion_textview")

  for (mtype in getEvaluateModels())
  {
    
    ## Generate the command to show the confusion matrix.
    
    confuse.cmd <- paste(sprintf("table(crs$pr, %s$%s, ",
                                 testset[[mtype]], crs$target),
                         'dnn=c("Predicted", "Actual"))')
  
    percentage.cmd <- paste("round(100*table(crs$pr, ",
                            sprintf("%s$%s, ", testset[[mtype]], crs$target),
                            'dnn=c("Predicted", "Actual"))',
                            "/length(crs$pr))",
                            sep="")

    ## Log the R commands and execute them.

    addToLog(sprintf("%sGenerate a confusion table for the %s model.",
                     .START.LOG.COMMENT, mtype), no.start=TRUE)
    addToLog(sprintf("Obtain the response from the %s model.", mtype),
             gsub("<<-", "<-", respcmd[[mtype]]))
  
    result <- try(eval(parse(text=respcmd[[mtype]])), TRUE)

    ## Check for errors - in particular, new levels in the test dataset.

    if (inherits(result, "try-error"))
    {
      if (any(grep("has new level", result)) || any(grep("New levels",result)))
        infoDialog("It seems that the dataset on which the predictions",
                   "from the", mtype, "model are required has a categorical",
                   "variable with levels not found in the training",
                   "dataset. The predictions can not be made in",
                   "this situation. You may need to either ensure",
                   "the training dataset has representatives of all levels",
                   "or else remove them from the testing dataset.",
                   "Alternatively, do not include that variable in the",
                   "modelling. \n\n The actual error message was:\n\n",
                   paste(result, "\n"))
      else
        errorDialog("Some error occured with", respcmd, "Best to let",
                    "Graham.Williams@togaware.com know.\n\n",
                    "The error was:\n\n", result)
      next()
    }
    
    addToLog("Now generate the confusion matrix.", confuse.cmd)

    confuse.output <- collectOutput(confuse.cmd, TRUE)
  
    addToLog("Generate confusion matrix showing percentages.", percentage.cmd)

    percentage.output <- collectOutput(percentage.cmd, TRUE)
  
    appendTextview("confusion_textview",
                   sprintf("Confusion matrix %s model on %s (counts):\n\n",
                           mtype, testname),
                   confuse.output,
                   "\n\n",
                   sprintf("Confusion matrix %s model on %s (%%):\n\n",
                           mtype, testname),
                   percentage.output)

  }
  
  return(sprintf("Generated Confusion Tables.", mtype, testname))
}

##----------------------------------------------------------------------
##
## EVALUATE RISK CHART
##

executeEvaluateRisk <- function(probcmd, testset, testname)
{
  ## Initial setup. 

  TV <- "risk_textview"
  clearTextview(TV)
  
  ## Ensure a risk variable has been specified.
  
  risk <- crs$risk
  if (is.null(risk))
  {
    errorDialog("No risk variable has been specified.",
                 "From the Variables tab please identify one variable as",
                 "a risk variable and rerun the modelling (if the variable",
                 "was previously an input variable).",
                 "The risk variable is a measure of the size of the risk.",
                 "For example, it might be the dollar amount of fraud",
                 "that has been recovered for each case.",
                 "TODO: The Risk Variable is not actually required,",
                 "and the requirement will be removed sometime soon,",
                 "essentially giving a ROC type of curve, but with",
                 "coverage on the x axis rather than false positives.")
    return()
  }

  ## Put 1 or 2 charts onto their own plots. Otherwise, put the
  ## multiple charts onto one plot, keeping them all the same size
  ## (thus if numplots is odd, leave a cell of the plot empty.
  
  numplots <- length(getEvaluateModels())
  if (numplots == 1)
    newPlot(1)
  else if (numplots == 2)
    newPlot(1)
  else if (numplots %% 2 == 0)
    newPlot(numplots)
  else
    newPlot(numplots + 1)

  if (numplots <= 2 )
    cex <- 1.0
  else if (numplots <= 4)
    cex <- 0.5
  else
    cex <- 0.5

  opar <- par(cex=cex)

  model.list <- getEvaluateModels()
  
  for (mtype in model.list)
  {

    ## We need the base testset name here to get the risk variable, which
    ## is not usually in the list of included columns.
  
    ## testbase <- gsub(", ?c\\(.*\\]", ",]", testset)

    ## Instead, obtain the column list, and if it exists, add the risk
    ## variable to it, to avoid listing all columns, since this can
    ## affect the na.omit function which will omit more rows if these
    ## extra columns have NAs.

    testcols <- gsub("])$", "", gsub(".*, ", "", testset[[mtype]]))
    if (testcols != "")
    {
      newcols <- gsub(")", sprintf(",%d)",
                                   getVariableIndicies(crs$risk)), testcols)
      testsetr <- gsub(testcols, newcols, testset[[mtype]], fixed=TRUE)
    }
  
    evaluate.cmd <- paste("crs$eval <<- evaluateRisk(crs$pr,",
                          sprintf("%s$%s,", testset[[mtype]], crs$target),
                          sprintf("%s$%s)", testsetr, risk))

    plot.cmd <- paste("plotRisk(crs$eval$Caseload, ",
                      "crs$eval$Precision, crs$eval$Recall, crs$eval$Risk)",
                      "\n",
                      genPlotTitleCmd("Risk Chart", mtype, testname,
                                      risk),
                      sep="")

    addToLog("Generate a Risk Chart",
             "## Rattle provides evaluateRisk and plotRisk.\n\n",
             gsub("<<-", "<-", probcmd[[mtype]]), "\n",
             gsub("<<-", "<-", evaluate.cmd), "\n",
             plot.cmd, sep="")

    result <- try(eval(parse(text=probcmd[[mtype]])), TRUE)

    ## Check for errors - in particular, new levels in the test dataset.
    
    if (inherits(result, "try-error"))
    {
      if (any(grep("has new level", result)) || any(grep("New levels",result)))
        infoDialog("It seems that the dataset on which the probabilities",
                   "from the", mtype, "model are required has a categorical",
                   "variable with levels not found in the training",
                   "dataset. The probabilities can not be determined in",
                   "this situation. You may need to either ensure",
                   "the training dataset has representatives of all levels",
                   "or else remove them from the testing dataset.",
                   "Alternatively, do not include that variable in the",
                   "modelling. \n\n The actual error message was:\n\n",
                   paste(result, "\n"))
      else
        errorDialog("Some error occured with", probcmd, "Best to let",
                    "Graham.Williams@togaware.com know.\n\n",
                    "The error was:\n\n", result)
      next()
    }

    ## Check for all results the same.
    
    if (length(levels(as.factor(crs$pr))) == 1)
    {
      errorDialog("The model predicts the same result for all records,",
                  "so there is nothing to plot!")
      return()
    }
    ## Now generate a summary of the performance at various probability
    ## cutoffs, with the result being stored in crs$eval.
  
    eval(parse(text=evaluate.cmd))

    ## We want to display the numeric results of the evaluation. But if
    ## there are too many rows, as produced by KSVM for example, it will
    ## be too much, so limit it to 100 row, which need to be selected
    ## every Nth.

    ne <- nrow(crs$eval)
    maxev <- 100
    if (ne > maxev)
    {
      id <- round(seq(1, ne, length=maxev))
      msg <- sprintf("The sequence has been truncated to just %d from %d.\n\n",
                     maxev, ne)
    }
    else
    {
      id <- 1:ne
      msg <- ""
    }
    id <- sprintf("c(%s)", paste(id, collapse=","))
    msg <- paste("Summary ", mtype, " model on ",
                 testname,
                 " by probability cutoffs.\n\n", msg, sep="")
    appendTextview(TV, msg, collectOutput(sprintf("crs$eval[%s,]", id), TRUE))

    ## Display the AUC measures.

    #auc <- calculateRiskAUC(crs$eval)
    #print(auc)
    aucRisk <- calculateAUC(crs$eval$Caseload, crs$eval$Risk)
    aucRecall <- calculateAUC(crs$eval$Caseload, crs$eval$Recall)
    appendTextview(TV, paste("The area under the Risk and Recall curves for ",
                             mtype, " model\n\n",
                             "Area under the Risk   (red)   curve: ",
                             sprintf("%d%% (%0.3f)\n",
                                     round(100*aucRisk), aucRisk),
                             "Area under the Recall (green) curve: ",
                             sprintf("%d%% (%0.3f)\n",
                                     round(100*aucRecall), aucRecall),
                             sep=""))
    
    ## Display the Risk Chart itself now.

    ## For 2 plots, so as not to overwrite the first plot, if we are
    ## about to plot the second plot, initiate a new plot.
    
    if (numplots == 2 && mtype == model.list[length(model.list)]) newPlot(1)

    eval(parse(text=plot.cmd))

  }
  
  ## Restore par
  
  par(opar)

  return(sprintf("Generated %d risk chart%s.",
                 numplots, ifelse(numplots>1, "s", "")))

}

grid.plot <- function (colour="gray", tops=100)
{
  opar = par(lwd=1)
  abline(v=seq(0,tops,tops/10), col=colour, lty="dotted")
  abline(h=seq(0,tops,tops/10), col=colour, lty="dotted")
  par(opar)
}

plotOptimalLine <- function(x, y1, y2, pr=NULL, colour="plum", label=NULL)
{
  lines(c(x, x), c(-13, max(y1, y2)), lty=6, col=colour)
  lines(c(-13, x), c(y1, y1), lty=6, col=colour)
  lines(c(-13, x), c(y2, y2), lty=6, col=colour)
  if (not.null(label))
  {
    text(x, 0, label, pos=4)
    text(x, 0, sprintf("%2.0f%%", x), pos=2)
    text(0, y2, sprintf("%2.0f%%", y2), pos=3, offset=0.2)
    text(0, y1, sprintf("%2.0f%%", y1), pos=3, offset=0.2)
    if (not.null(pr))
      text(x, pr+4, sprintf("%2.0f%%", pr), pos=2)
  }
}

evaluateRisk <- function(predicted, actual, risks)
{
  if (is.factor(actual))
    actual <- as.integer(actual)-1
      
  if (min(actual) != 0 | max(actual) !=1 )
    stop("actual must be binary (0,1) but found (",
         min(actual), ",", max(actual), ").")

  ## For KSVMs, and perhaps other modellers, the predictied values are
  ## probabilites, which may be a very high level of precision (e.g.,
  ## 0.999999999999996 or 2.58015830922886e-13), and thus, when
  ## converted to a factor, we have almost a one-to-one match from an
  ## entity to a probability. When converted to a data frame the
  ## resulting row names (these probablities of being a 1) have
  ## caseloads of 1, 2, or 3, thus there are very many, and sometimes,
  ## the probablities are the same! We then get duplicate row names
  ## and the assigning of new names to the columns below gives an
  ## error about duplicat row names! We should aggregate up to three
  ## significant figures in the probabilities to make everything much
  ## easier. BUT this then lumsp all ot eh 0.9999999.... together, and
  ## leaves a very large jump at the right end of the plot! We really
  ## might want to instead aggregate on caseload! But rounding it to
  ## 13 digits seems okay! We gete a good plot, and no duplicates in
  ## survey-training (10% train/90%test).

  predicted <- as.factor(round(predicted, 13))
  
  ds.actual <- data.frame(Actual=actual,
                            Risk=risks,
                            Predict=as.factor(predicted))
  #Predict=as.factor(ds.predict[,2]))

  ds.evaluation <- as.data.frame(t(rbind(tapply(ds.actual$Actual,
                                                ds.actual$Predict, sum),
                                         tapply(ds.actual$Risk,
                                                ds.actual$Predict,
                                                sum, na.rm=TRUE),
                                         tapply(ds.actual$Actual,
                                              ds.actual$Predict, length))))

  colnames(ds.evaluation) <- c("Recall", "Risk", "Caseload")

  last <- nrow(ds.evaluation)
  ds.evaluation$Precision[last] <- ds.evaluation$Recall[last]/
    ds.evaluation$Caseload[last]

  for (i in (nrow(ds.evaluation)-1):1)
    {
      ds.evaluation$Recall[i] <- ds.evaluation$Recall[i+1] +
        ds.evaluation$Recall[i]
      ds.evaluation$Risk[i] <- ds.evaluation$Risk[i+1] +
        ds.evaluation$Risk[i]
      ds.evaluation$Caseload[i] <- ds.evaluation$Caseload[i+1] +
        ds.evaluation$Caseload[i]
      ds.evaluation$Precision[i] <- ds.evaluation$Recall[i] /
        ds.evaluation$Caseload[i]
    }
  ds.evaluation$Recall <- ds.evaluation$Recall/ds.evaluation$Recall[1]
  ds.evaluation$Risk <- ds.evaluation$Risk/ds.evaluation$Risk[1]
  ds.evaluation$Caseload <- ds.evaluation$Caseload/ds.evaluation$Caseload[1]
  ## This is Michael's measure of performance.
  ds.evaluation$Measure <- abs(ds.evaluation$Recall - ds.evaluation$Caseload) +
    abs(ds.evaluation$Risk - ds.evaluation$Caseload)
  return(ds.evaluation)
}

## REPLACED BY calculateAUC
##
## calculateRiskAUC <- function(ev)
## {
##   len <- nrow(ev)
##   ria <- ev$Caseload[len] * ev$Risk[len] / 2
##   rea <- ev$Caseload[len] * ev$Recall[len] / 2

##   for (i in (len-1):1)
##   {
##     ria <- ria +
##       (ev$Caseload[i] - ev$Caseload[i+1]) * ev$Risk[i+1] +
##       (ev$Caseload[i] - ev$Caseload[i+1]) * (ev$Risk[i] - ev$Risk[i+1]) / 2
##     rea <- rea + 
##       (ev$Caseload[i] - ev$Caseload[i+1]) * ev$Recall[i+1] +
##       (ev$Caseload[i] - ev$Caseload[i+1]) * (ev$Recall[i] - ev$Recall[i+1]) / 2
##   }
##   return(c(ria, rea))
## }

calculateAUC <- function(x, y)
{
  len <- length(x)
  ria <- x[len] * y[len] / 2

  for (i in (len-1):1)
  {
    ria <- ria +
      (x[i] - x[i+1]) * y[i+1] + (x[i] - x[i+1]) * (y[i] - y[i+1]) / 2
  }
  return(ria)
}

openMyDevice <- function(dev, filename)
{
  if (dev == "" & filename != "")
  {
    fn <- unlist(strsplit(filename, "\\."))
    dev=fn[length(fn)]
  }
    
  if (dev == "wmf")
    win.metafile(filename)
  else if (dev == "png")
    png(filename)
  else if (dev == "pdf")
    pdf(filename)

  return(dev)
    
}

plotRisk <- function (cl, pr, re, ri=NULL,
                      title=NULL,
                      show.legend=TRUE,
                      xleg=60, yleg=55,
                      optimal=NULL, optimal.label="",
                      chosen=NULL, chosen.label="",
                      include.baseline=TRUE,
                      dev="", filename="",
                      show.knots=NULL,
                      risk.name="Revenue", #"Risk",
                      recall.name="Adjustments", #"Recall",
                      precision.name="Strike Rate") #"Precision")
{
  openMyDevice(dev, filename)

  ## If proportions, convert to percentages

  if (all(cl <= 1)) cl <- cl * 100
  if (all(re <= 1)) re <- re * 100
  if (not.null(ri) & all(ri <= 1.5)) ri <- ri * 100 # Can sometimes be just >1
  if (all(pr <= 1)) pr <- pr * 100
  #
  # If list is in min to max order then reverse
  #
  if (cl[1] < cl[length(cl)])
  {
    cl <- rev(cl)
    pr <- rev(pr)
    re <- rev(re)
    ri <- rev(ri)
  }
  #
  # Add a zero point for the display
  #
  cl <- c(cl, 0)
  re <- c(re, 0)
  if (not.null(ri)) ri <- c(ri, 0)
  pr <- c(pr, NA)
  #
  # Also add the 100 point just in case?
  #
  if (cl[1] != 100)
  {
    cl <- c(100, cl)
    re <- c(100, re)
    if (not.null(ri)) ri <- c(100, ri)
    pr <- c(min( pr[!is.na(pr)]), pr)
  }
  #
  # Now plot
  #
  opar <- par(lwd=2)
  plot(c(0,100), c(0,100), type='l', col=1,
       xlab="Caseload (%)", ylab="Performance (%)",
       ylim=c(0,100), xlim=c(0,100))
  grid.plot()
  if (not.null(title))
    title(main=title, sub=paste("Rattle", Sys.time(), Sys.info()["user"]))
  points(re ~ cl, type='l', col=3, lty=5)
  points(pr ~ cl, type='l', col=4, lty=4)
  if (not.null(ri)) points(ri ~ cl, type='l', col=2, lty=1)
  if (include.baseline) text(100, pr[1]+4, sprintf("%0.0f%%", pr[1]))
  # Optimal
  if (not.null(optimal))
  {
    optimal.index <- which(abs(cl-optimal) == min(abs(cl-optimal)))
    if (length(optimal.index) > 1) optimal.index <- optimal.index[1]
    plotOptimalLine(optimal, ri[optimal.index], re[optimal.index],
                      pr[optimal.index], label=optimal.label)
  }
  # Chosen
  if (not.null(chosen))
  {
    chosen.index <- which(abs(cl-chosen) == min(abs(cl-chosen)))
    if (length(chosen.index) > 1) chosen.index <- chosen.index[1]
    plotOptimalLine(chosen, ri[chosen.index], re[chosen.index],
                      label=chosen.label, col="grey")
  }

  legend <- c()
  lty <- c()
  col <- c()
  if (not.null(ri))
  {
    auc <- calculateAUC(cl/100, ri/100)
    legend <- c(legend, sprintf("%s (%d%%)", risk.name, round(100*auc)))
    lty <- c(lty, 1)
    col <- c(col, 2)
  }  
  auc <- calculateAUC(cl/100, re/100)
  legend <- c(legend, sprintf("%s (%d%%)", recall.name, round(100*auc)))
  legend <- c(legend, precision.name)
  lty <- c(lty,5,4)
  col <- c(col,3,4)
  if (not.null(optimal))
  {
    legend <- c(legend, "Optimal")
    lty <- c(lty,6)
    col <- c(col,"plum")
  }
  if (not.null(chosen))
  {
    legend <- c(legend, "Chosen")
    lty <- c(lty,6)
    col <- c(col,"grey")
  }
  if (show.legend)
    legend(xleg, yleg, legend, lty=lty, lwd=2, col=col)
  #
  #
  # Add in knot labels
  #
  if (not.null(show.knots))
  {
    len <- length(cl)
    text(cl[c(-1,-len)]-2, ri[c(-1,-len)]+3, rev(show.knots)[-1])
  }
  if (dev != "") dev.off()
  par(opar)
}

##----------------------------------------------------------------------
##
## EVALUATE LIFT CHART
##

executeEvaluateLift <- function(predcmd, testset, testname)
{
  lib.cmd <- "require(ROCR, quietly=TRUE)"
  newPlot()
  addplot <- "FALSE"

  nummodels <- length(predcmd)
  mcolors <- rainbow(nummodels, 1, .8)
  mcount <- 0  
  
  for (mtype in getEvaluateModels())
  {
    mcount <- mcount + 1
    plot.cmd <- paste("plot(performance(prediction(crs$pr, ",
                      sprintf("%s$%s),", testset[[mtype]], crs$target),
                      '"lift", "rpp"), ',
                      sprintf('col="%s", lty=%d, ', mcolors[mcount], mcount),
                      sprintf("add=%s)\n", addplot),
                      sep="")
    addplot <- "TRUE"
    
    addToLog("Display Lift Chart using the ROCR package.", lib.cmd)
    eval(parse(text=lib.cmd))
    
    addToLog(sprintf("Generate a Lift Chart for the %s model on %s.",
                     mtype, testname),
             gsub("<<-", "<-", predcmd[[mtype]]), "\n", plot.cmd)

    result <- try(eval(parse(text=predcmd[[mtype]])), silent=TRUE)

    ## Check for errors - in particular, new levels in the test dataset.
    
    if (inherits(result, "try-error"))
    {
      if (any(grep("has new level", result)) || any(grep("New levels",result)))
        infoDialog("It seems that the dataset on which the probabilities",
                   "from the", mtype, "model are required has a categorical",
                   "variable with levels not found in the training",
                   "dataset. The probabilities can not be determined in",
                   "this situation. You may need to either ensure",
                   "the training dataset has representatives of all levels",
                   "or else remove them from the testing dataset.",
                   "Alternatively, do not include that variable in the",
                   "modelling. \n\n The actual error message was:\n\n",
                   paste(result, "\n"))
      else
        errorDialog("Some error occured with", probcmd, "Best to let",
                    "Graham.Williams@togaware.com know.\n\n",
                    "The error was:\n\n", result)
      next()
    }

    eval(parse(text=plot.cmd))
    
  }

  ## If just one model, and we are plotting the test dataset, then
  ## also plot the training dataset.

  if (nummodels==1 && length(grep("\\[test\\]", testname))>0)
  {
    mcount <- mcount + 1
    plot.cmd <- paste("plot(performance(prediction(crs$pr, ",
                      sprintf("%s$%s),",
                              sub("-crs\\$sample", "crs$sample",
                                  testset[[mtype]]), crs$target),
                      '"lift", "rpp"), ',
                      'col="#00CCCCFF", lty=2, ',
                      sprintf("add=%s)\n", addplot),
                      sep="")
    addToLog(sprintf("Generate a Lift Chart for the %s model on %s.",
                     mtype, sub('\\[test\\]', '[train]', testname)),
             gsub("<<-", "<-", sub("-crs\\$sample", "crs$sample",
                                   predcmd[[mtype]])), "\n", plot.cmd)

    result <- try(eval(parse(text=sub("-crs\\$sample",
                               "crs$sample", predcmd[[mtype]]))), silent=TRUE)
    eval(parse(text=plot.cmd))
    models <- c("Test", "Train")
    nummodels <- 2
    legtitle <- getEvaluateModels()
    title <- sub('\\[test\\]', '', testname)
  }
  else
  {
    models <- getEvaluateModels()
    legtitle <- "Models"
    title <- testname
  }

  legendcmd <- paste('legend("topright",',
                     sprintf("c(%s),",
                             paste('"', models, '"',
                                   sep="", collapse=",")),
                     sprintf('col=rainbow(%d, 1, .8), lty=1:%d,',
                             nummodels, nummodels),
                     sprintf('title="%s", inset=c(0.05, 0.05))', legtitle))
  addToLog("Add a legend to the plot.", legendcmd)
  eval(parse(text=legendcmd))
  
  decorcmd <- paste(genPlotTitleCmd("Lift Chart", "", title),
                    '\ngrid()', sep="")
  addToLog("Add decorations to the plot.", decorcmd)
  eval(parse(text=decorcmd))
  
  return("Generated Lift Charts.")
}

##----------------------------------------------------------------------
##
## EVALUATE ROC CHART
##

executeEvaluateROC <- function(predcmd, testset, testname)
{
  TV <- "roc_textview"
  clearTextview(TV)
  lib.cmd <- "require(ROCR, quietly=TRUE)"
  newPlot()
  addplot <- "FALSE"

  nummodels <- length(predcmd)
  mcolors <- rainbow(nummodels, 1, .8)
  mcount <- 0  
  
  for (mtype in getEvaluateModels())
  {
    mcount <- mcount + 1
    plot.cmd <- paste("plot(performance(prediction(crs$pr, ",
                      sprintf("%s$%s),", testset[[mtype]], crs$target),
                      '"tpr", "fpr"), ',
                      sprintf('col="%s", lty=%d, ', mcolors[mcount], mcount),
                      sprintf("add=%s)\n", addplot),
                      sep="")
    addplot <- "TRUE"

    addToLog("Plot an ROC curve using the ROCR package.", lib.cmd)
    eval(parse(text=lib.cmd))
  
    addToLog(sprintf("Generate an ROC Curve for the %s model on %s.",
                     mtype, testname),
             gsub("<<-", "<-", predcmd[[mtype]]), "\n", plot.cmd)

    result <- try(eval(parse(text=predcmd[[mtype]])), silent=TRUE)

    ## Check for errors - in particular, new levels in the test dataset.
    
    if (inherits(result, "try-error"))
    {
      if (any(grep("has new level", result)) || any(grep("New levels",result)))
        infoDialog("It seems that the dataset on which the probabilities",
                   "from the", mtype, "model are required has a categorical",
                   "variable with levels not found in the training",
                   "dataset. The probabilities can not be determined in",
                   "this situation. You may need to either ensure",
                   "the training dataset has representatives of all levels",
                   "or else remove them from the testing dataset.",
                   "Alternatively, do not include that variable in the",
                   "modelling. \n\n The actual error message was:\n\n",
                   paste(result, "\n"))
      else
        errorDialog("Some error occured with", probcmd, "Best to let",
                    "Graham.Williams@togaware.com know.\n\n",
                    "The error was:\n\n", result)
      next()
    }

    eval(parse(text=plot.cmd))

    ## Report the area under the curve.
  
    auc.cmd <- paste("performance(prediction(crs$pr, ",
                    sprintf("%s$%s),", testset[[mtype]], crs$target),
                    '"auc")', sep="")
    addToLog("Calculate the area under the curve for the plot.", auc.cmd)
    auc <- eval(parse(text=auc.cmd))
    appendTextview(TV, paste("Area under the ROC curve for the",
                             sprintf("%s model on %s is %0.4f",
                                     mtype, testname,
                                     attr(auc, "y.values"))))
  }
  lines(c(0,1), c(0,1)) # Baseline

  ## If just one model, and we are plotting the test dataset, then
  ## also plot the training dataset.

  if (nummodels==1 && length(grep("\\[test\\]", testname))>0)
  {
    mcount <- mcount + 1
    plot.cmd <- paste("plot(performance(prediction(crs$pr, ",
                      sprintf("%s$%s),",
                              sub("-crs\\$sample", "crs$sample",
                                  testset[[mtype]]), crs$target),
                      '"tpr", "fpr"), ',
                      'col="#00CCCCFF", lty=2, ',
                      sprintf("add=%s)\n", addplot),
                      sep="")
    addToLog(sprintf("Generate an ROC Curve for the %s model on %s.",
                     mtype, sub('\\[test\\]', '[train]', testname)),
             gsub("<<-", "<-", sub("-crs\\$sample", "crs$sample",
                                   predcmd[[mtype]])), "\n", plot.cmd)

    result <- try(eval(parse(text=sub("-crs\\$sample",
                               "crs$sample", predcmd[[mtype]]))), silent=TRUE)
    eval(parse(text=plot.cmd))
    models <- c("Test", "Train")
    nummodels <- 2
    legtitle <- getEvaluateModels()
    title <- sub('\\[test\\]', '', testname)
  }
  else
  {
    models <- getEvaluateModels()
    legtitle <- "Models"
    title <- testname
  }

  legendcmd <- paste('legend("bottomright",',
                     sprintf("c(%s),",
                             paste('"', models, '"',
                                   sep="", collapse=",")),
                     sprintf('col=rainbow(%d, 1, .8), lty=1:%d,',
                             nummodels, nummodels),
                     sprintf('title="%s", inset=c(0.05, 0.05))', legtitle))
  addToLog("Add a legend to the plot.", legendcmd)
  eval(parse(text=legendcmd))
  
  decor.cmd <- paste(genPlotTitleCmd("ROC Curve", "", title),
                    '\ngrid()', sep="")
  addToLog("Add decorations to the plot.", decor.cmd)
  eval(parse(text=decor.cmd))

  return(sprintf("Generated ROC Curves on %s.", testname))
}
  
##----------------------------------------------------------------------
##
## EVALUATE PRECISION PLOT
##

executeEvaluatePrecision <- function(predcmd, testset, testname)
{
  lib.cmd <- "require(ROCR, quietly=TRUE)"
  newPlot()
  addplot <- "FALSE"

  nummodels <- length(predcmd)
  mcolors <- rainbow(nummodels, 1, .8)
  mcount <- 0  
  
  for (mtype in getEvaluateModels())
  {
    mcount <- mcount + 1

    plot.cmd <- paste("plot(performance(prediction(crs$pr, ",
                      sprintf("%s$%s),", testset[[mtype]], crs$target),
                      '"prec", "rec"), ',
                      sprintf('col="%s", lty=%d, ', mcolors[mcount], mcount),
                      sprintf("add=%s)\n", addplot),
                      sep="")
    addplot <- "TRUE"
  
    addToLog("Precision/Recall Plot using the ROCR package", lib.cmd)
    eval(parse(text=lib.cmd))

    addToLog(sprintf("Generate a Precision/Recall Plot for the %s model on %s.",
                     mtype, testname),
             gsub("<<-", "<-", predcmd[[mtype]]), "\n", plot.cmd)

    result <- try(eval(parse(text=predcmd[[mtype]])), silent=TRUE)

    ## Check for errors - in particular, new levels in the test dataset.
    
    if (inherits(result, "try-error"))
    {
      if (any(grep("has new level", result)) || any(grep("New levels",result)))
        infoDialog("It seems that the dataset on which the probabilities",
                   "from the", mtype, "model are required has a categorical",
                   "variable with levels not found in the training",
                   "dataset. The probabilities can not be determined in",
                   "this situation. You may need to either ensure",
                   "the training dataset has representatives of all levels",
                   "or else remove them from the testing dataset.",
                   "Alternatively, do not include that variable in the",
                   "modelling. \n\n The actual error message was:\n\n",
                   paste(result, "\n"))
      else
        errorDialog("Some error occured with", probcmd, "Best to let",
                    "Graham.Williams@togaware.com know.\n\n",
                    "The error was:\n\n", result)
      next()
    }

    eval(parse(text=plot.cmd))
  }

  ## If just one model, and we are plotting the test dataset, then
  ## also plot the training dataset.

  if (nummodels==1 && length(grep("\\[test\\]", testname))>0)
  {
    mcount <- mcount + 1
    plot.cmd <- paste("plot(performance(prediction(crs$pr, ",
                      sprintf("%s$%s),",
                              sub("-crs\\$sample", "crs$sample",
                                  testset[[mtype]]), crs$target),
                      '"prec", "rec"), ',
                      'col="#00CCCCFF", lty=2, ',
                      sprintf("add=%s)\n", addplot),
                      sep="")
    addToLog(sprintf("Generate a Precision/Recall Plot for the %s model on %s.",
                     mtype, sub('\\[test\\]', '[train]', testname)),
             gsub("<<-", "<-", sub("-crs\\$sample", "crs$sample",
                                   predcmd[[mtype]])), "\n", plot.cmd)

    result <- try(eval(parse(text=sub("-crs\\$sample",
                               "crs$sample", predcmd[[mtype]]))), silent=TRUE)
    eval(parse(text=plot.cmd))
    models <- c("Test", "Train")
    nummodels <- 2
    legtitle <- getEvaluateModels()
    title <- sub('\\[test\\]', '', testname)
  }
  else
  {
    models <- getEvaluateModels()
    legtitle <- "Models"
    title <- testname
  }

  legendcmd <- paste('legend("bottomleft",',
                     sprintf("c(%s),",
                             paste('"', models, '"',
                                   sep="", collapse=",")),
                     sprintf('col=rainbow(%d, 1, .8), lty=1:%d,',
                             nummodels, nummodels),
                     sprintf('title="%s", inset=c(0.05, 0.05))', legtitle))
  addToLog("Add a legend to the plot.", legendcmd)
  eval(parse(text=legendcmd))
  
  decor.cmd <- paste(genPlotTitleCmd("Precision/Recall Chart", "", title),
                    '\ngrid()', sep="")
  addToLog("Add decorations to the plot.", decor.cmd)
  eval(parse(text=decor.cmd))
  
  return(sprintf("Generated Precision/Recall Plot on %s.", title))
}

##----------------------------------------------------------------------
##
## EVALUATE SENSITIVITY PLOT
##

executeEvaluateSensitivity <- function(predcmd, testset, testname)
{
  lib.cmd <- "require(ROCR, quietly=TRUE)"
  newPlot()
  addplot <- "FALSE"

  nummodels <- length(predcmd)
  mcolors <- rainbow(nummodels, 1, .8)
  mcount <- 0  
  
  for (mtype in getEvaluateModels())
  {
    mcount <- mcount + 1
    plot.cmd <- paste("plot(performance(prediction(crs$pr, ",
                      sprintf("%s$%s),", testset[[mtype]], crs$target),
                      '"sens", "spec"), ',
                      sprintf('col="%s", lty=%d, ', mcolors[mcount], mcount),
                      sprintf("add=%s)\n", addplot),
                      sep="")
     addplot <- "TRUE"
 
    addToLog("Display a Sensitivity/Specificity Plot using the ROCR package",
             lib.cmd)
    eval(parse(text=lib.cmd))

    addToLog(sprintf("Generate Sensitivity/Specificity Plot for %s model on %s.",
                     mtype, testname),
             gsub("<<-", "<-", predcmd[[mtype]]), "\n", plot.cmd)

    result <- try(eval(parse(text=predcmd[[mtype]])), silent=TRUE)

    ## Check for errors - in particular, new levels in the test dataset.
    
    if (inherits(result, "try-error"))
    {
      if (any(grep("has new level", result)) || any(grep("New levels",result)))
        infoDialog("It seems that the dataset on which the probabilities",
                   "from the", mtype, "model are required has a categorical",
                   "variable with levels not found in the training",
                   "dataset. The probabilities can not be determined in",
                   "this situation. You may need to either ensure",
                   "the training dataset has representatives of all levels",
                   "or else remove them from the testing dataset.",
                   "Alternatively, do not include that variable in the",
                   "modelling. \n\n The actual error message was:\n\n",
                   paste(result, "\n"))
      else
        errorDialog("Some error occured with", probcmd, "Best to let",
                    "Graham.Williams@togaware.com know.\n\n",
                    "The error was:\n\n", result)
      next()
    }

    eval(parse(text=plot.cmd))
  }
  ## If just one model, and we are plotting the test dataset, then
  ## also plot the training dataset.

  if (nummodels==1 && length(grep("\\[test\\]", testname))>0)
  {
    mcount <- mcount + 1
    plot.cmd <- paste("plot(performance(prediction(crs$pr, ",
                      sprintf("%s$%s),",
                              sub("-crs\\$sample", "crs$sample",
                                  testset[[mtype]]), crs$target),
                      '"sens", "spec"), ',
                      'col="#00CCCCFF", lty=2, ',
                      sprintf("add=%s)\n", addplot),
                      sep="")
    addToLog(sprintf("Generate a Lift Chart for the %s model on %s.",
                     mtype, sub('\\[test\\]', '[train]', testname)),
             gsub("<<-", "<-", sub("-crs\\$sample", "crs$sample",
                                   predcmd[[mtype]])), "\n", plot.cmd)

    result <- try(eval(parse(text=sub("-crs\\$sample",
                               "crs$sample", predcmd[[mtype]]))), silent=TRUE)
    eval(parse(text=plot.cmd))
    models <- c("Test", "Train")
    nummodels <- 2
    legtitle <- getEvaluateModels()
    title <- sub('\\[test\\]', '', testname)
  }
  else
  {
    models <- getEvaluateModels()
    legtitle <- "Models"
    title <- testname
  }

  legendcmd <- paste('legend("bottomleft",',
                     sprintf("c(%s),",
                             paste('"', models, '"',
                                   sep="", collapse=",")),
                     sprintf('col=rainbow(%d, 1, .8), lty=1:%d,',
                             nummodels, nummodels),
                     sprintf('title="%s", inset=c(0.05, 0.05))', legtitle))
  addToLog("Add a legend to the plot.", legendcmd)
  eval(parse(text=legendcmd))
  
  decor.cmd <- paste(genPlotTitleCmd("Sensitivity/Specificity (tpr/tnr)", "",
                                    title),
                    '\ngrid()', sep="")
  addToLog("Add decorations to the plot.", decor.cmd)
  eval(parse(text=decor.cmd))

  return(sprintf("Generated Sensitivity/Specificity Plot on %s.", testname))
}

##----------------------------------------------------------------------
##
## SCORE - Save the probability scores for each selected model to a file
## Would be best into one file but testset may be different for each.
##
## TODO: Wouldn't this be better as the Export functionality for the
## Evaluate tab?
##

executeEvaluateScore <- function(predcmd, testset, testname)
{

  ## Obtain filename to write the scores to. TODO Wait until we get
  ## all scores into a single file, then this will be the filename we
  ## obtain here (since currently need to add the mtyp to each file
  ## name.
  
##   dialog <- gtkFileChooserDialog("Score Files", NULL, "save",
##                                  "gtk-cancel", GtkResponseType["cancel"],
##                                  "gtk-save", GtkResponseType["accept"])

##   if(not.null(testname))
##     dialog$setCurrentName(paste(get.stem(testname), "_", mtype, "_score"))

  ##  dialog$setCurrentFolder(crs$cwd)

  ##   ff <- gtkFileFilterNew()
##   ff$setName("CSV Files")
##   ff$addPattern("*.csv")
##   dialog$addFilter(ff)

##   ff <- gtkFileFilterNew()
##   ff$setName("All Files")
##   ff$addPattern("*")
##   dialog$addFilter(ff)
  
##   if (dialog$run() == GtkResponseType["accept"])
##   {
##     save.name <- dialog$getFilename()
##     dialog$destroy()
##   }
##   else
##   {
##     dialog$destroy()
##     return()
##   }

  ## Now process each model separately, at least for now. TODO,
  ## collect the outputs and then wrtie them all at once.
  
  for (mtype in getEvaluateModels())
  {

    addToLog(sprintf("%s: Save probability scores to file for %s model on %s.",
                     toupper(mtype), mtype, testname),
             gsub("<<-", "<-", predcmd[[mtype]]))

    result <- try(eval(parse(text=predcmd[[mtype]])), silent=TRUE)

    ## Check for errors - in particular, new levels in the test dataset.
    
    if (inherits(result, "try-error"))
    {
      if (any(grep("has new level", result)) || any(grep("New levels",result)))
        infoDialog("It seems that the dataset on which the probabilities",
                   "from the", mtype, "model are required has a categorical",
                   "variable with levels not found in the training",
                   "dataset. The probabilities can not be determined in",
                   "this situation. You may need to either ensure",
                   "the training dataset has representatives of all levels",
                   "or else remove them from the testing dataset.",
                   "Alternatively, do not include that variable in the",
                   "modelling. \n\n The actual error message was:\n\n",
                   paste(result, "\n"))
      else
        errorDialog("Some error occured with", probcmd, "Best to let",
                    "Graham.Williams@togaware.com know.\n\n",
                    "The error was:\n\n", result)
      next()
    }

    ## Determine an appropriate filename (TODO fixed for now but should ask)
    
    score.file <- sprintf("%s_%s_score.csv",
                          gsub(" ", "_",
                               gsub("\\.[[:alnum:]]*", "",
                                    gsub("(\\[|\\])", "", testname))),
                          mtype)

    ## Obtain a list of the identity vartiables
    
    idents <- getSelectedVariables("ident")
    
    ## Transform the dataset expression into what we need to extract
    ## the relevant columns.
    ##
    ## Various formats include:
    ##
    ##    train	crs$dataset[crs$sample, c(3:12,14)]
    ##    test	crs$dataset[-crs$sample, c(3:12,14)]
    ##    csv	crs$testset[,c(3:12,14)]
    ##    df	crs$testset
    ##
    ## Want
    ##    subset(crs$dataset[-crs$sample,], select=Idents) + crs$pr
    ##

    scoreset <- testset[[mtype]]

    ## If no comma in scoreset, leave as is, else find first comma,
    ## remove everything after, and replace with "]". PROBLEM TODO If
    ## the testset[[.MODEL]] includes na.omit, we need to do something
    ## different because after the following step of replacing the
    ## column list with nothing, it is very likely that new columns
    ## are included that have NAs, and hence the na.omit will remove
    ## even more rows for the subset command than it does for the
    ## predict command. Yet we still want to ensure we have all the
    ## appropriate columns available. So use
    ## na.omit(crs$dataset[-crs$sample, c(2:4,6:10,13)])@na.action to
    ## remove the rows from crs$dataset[-crs$sample,] that have
    ## missing values with regard the columns c(2:4,6:10,13). Thus if
    ## we have scoreset as:
    ##
    ##  na.omit(crs$dataset[-crs$sample, c(2:4,6:10,13)])
    ##
    ## we want to:
    ##
    ##  omitted <- na.omit(crs$dataset[-crs$sample, c(2:4,6:10,13)])@na.action
    ##
    ## and then scoreset should become:
    ##
    ##  crs$dataset[-crs$sample,][-omitted,]

    ## First deal with the na.omit case, to capture the list of rows
    ## omitted.
    
    if (substr(scoreset, 1, 7) == "na.omit")
    {
      omit.cmd <- paste("omitted <- ", scoreset, "@na.action", sep="")
      addToLog("Record rows omitted from predict command.", omit.cmd)
      eval(parse(text=omit.cmd))
    }
    else
      omitted <- NULL

    ## Now clean out the column subsets.
    
    if (length(grep(",", scoreset)) > 0)
      scoreset = gsub(",.*]", ",]", scoreset)

    ## And finally, remove the na.omit if there is one, replacing it
    ## with specifically removing just the rows that were removed in
    ## the predict command.

    if (not.null(omitted))
      scoreset = sub(")", "[-omitted,]", sub("na.omit\\(", "", scoreset))
    
    scoreset <- sprintf('subset(%s, select=c(%s))',
                        scoreset,
                        ifelse(is.null(idents), "", 
                               sprintf('"%s"', paste(idents, collapse='", "'))))
    addToLog("Extract the corresponding identifier fields from the dataset.",
             sprintf("scores <- %s", scoreset))
    
    scores <- eval(parse(text=scoreset))
    
    addToLog("Write the scores to file.",
             paste('write.csv(cbind(scores, predict=crs$pr), file="',
                   score.file, '", row.names=FALSE)', sep=""))
    
    write.csv(cbind(scores, predict=crs$pr), file=score.file, row.names=FALSE)

    infoDialog("The scores for", mtype, "have been saved into the file",
               score.file, "in the folder", getwd())

  }
  return(sprintf("Scores saved.", getwd(), score.file))
}

########################################################################

## General Menu Callbacks

on_rattle_menu_activate <- function(action, window)
{
  browseURL("http://rattle.togaware.com")
}

on_save_menu_activate <- function(action, window) {saveProject()}
on_delete_menu_activate <- notImplemented

## Map the unchanged glade defaults

on_cut1_activate <- notImplemented

on_about_menu_activate <-  function(action, window)
{
  result <- try(etc <- file.path(.path.package(package="rattle")[1], "etc"),
                silent=TRUE)
  if (inherits(result, "try-error"))
    about <- gladeXMLNew("rattle.glade", root="aboutdialog")
  else
    about <- gladeXMLNew(file.path(etc, "rattle.glade"), root="aboutdialog")

  about$getWidget("aboutdialog")$setVersion(VERSION)
  about$getWidget("aboutdialog")$
    setCopyright(COPYRIGHT)
}

on_paste1_activate <- notImplemented
on_copy1_activate <- notImplemented

on_tooltips_activate <- function(action, window)
{
  infoDialog("Currently this functionality is not implemented.",
              "It is awaiting some insight into how to get hold of",
              "the glade GtkTooltips group, which can then be",
              "disabled or enabled as requested.")
}
  
##----------------------------------------------------------------------

## Miscellaneous callbacks

on_notebook_switch_page <- function(notebook, window, page)
{
  ## notebook is the GtkNotebook object.
  ## window is ??.
  ## page is the index of the page switched to.

  #ct <- current_(page)

  switchToPage(page)
}

on_tools_data_activate <- function(action, window)
{
  .NOTEBOOK$setCurrentPage(getNotebookPage(.NOTEBOOK, .NOTEBOOK.DATA.NAME))
  switchToPage(.NOTEBOOK.DATA.NAME)
}

on_tools_variables_activate <- function(action, window)
{
  .NOTEBOOK$setCurrentPage(getNotebookPage(.NOTEBOOK, .NOTEBOOK.VARIABLES.NAME))
  switchToPage(.NOTEBOOK.VARIABLES.NAME)
}

on_tools_transform_activate <- function(action, window)
{
  .NOTEBOOK$setCurrentPage(getNotebookPage(.NOTEBOOK, .NOTEBOOK.TRANSFORM.NAME))
  switchToPage(.NOTEBOOK.TRANSFORM.NAME)
}

on_tools_explore_activate <- function(action, window)
{
  .NOTEBOOK$setCurrentPage(getNotebookPage(.NOTEBOOK, .NOTEBOOK.EXPLORE.NAME))
  switchToPage(.NOTEBOOK.EXPLORE.NAME)
}

on_tools_cluster_activate <- function(action, window)
{
  .NOTEBOOK$setCurrentPage(getNotebookPage(.NOTEBOOK, .NOTEBOOK.CLUSTER.NAME))
  switchToPage(.NOTEBOOK.CLUSTER.NAME)
}

on_tools_model_activate <- function(action, window)
{
  .NOTEBOOK$setCurrentPage(getNotebookPage(.NOTEBOOK, .NOTEBOOK.MODEL.NAME))
  switchToPage(.NOTEBOOK.MODEL.NAME)
}

on_tools_evaluate_activate <- function(action, window)
{
  .NOTEBOOK$setCurrentPage(getNotebookPage(.NOTEBOOK, .NOTEBOOK.EVALUATE.NAME))
  switchToPage(.NOTEBOOK.EVALUATE.NAME)
}

on_tools_log_activate <- function(action, window)
{
  .NOTEBOOK$setCurrentPage(getNotebookPage(.NOTEBOOK, .NOTEBOOK.LOG.NAME))
  switchToPage(.NOTEBOOK.LOG.NAME)
}

switchToPage <- function(page)
{

  ## Blank the status bar whenever we change pages
  
  setStatusBar()

  ## This function used to accept numeric pages, so check for that and
  ## convert to the page name rather than the now changing page number
  ## (page numbers used to be fixed).
  
  if (is.numeric(page))
    page <- .NOTEBOOK$getTabLabelText(.NOTEBOOK$getNthPage(page))
  
  if (page == .NOTEBOOK.EVALUATE.NAME)
  {
    
    ## On moving to the EVALUATE page, ensure each built model's
    ## checkbox is active, and check the active model's checkbox, but
    ## leave all the other as they are.

    mtypes <- listBuiltModels()
    
    if (not.null(mtypes) )
    {
      ## We have some models, so make sure their checkboxes are
      ## sensitive.

      lapply(mtypes,
             function(x) theWidget(paste(x, "_evaluate_checkbutton",
                                            sep=""))$setSensitive(TRUE))
      
      if (is.null(crs$page) || crs$page == .NOTEBOOK.MODEL.NAME)
      {
        ## By default check the current model's check button if we
        ## have just come from the MODEL page. This makes it easy when
        ## swaping from the Model page to this page to evaluate the
        ## just built model (usually). The NULL test on crs$page
        ## simply covers the loading of a project that did not save
        ## the crs$page, as was the case for old project files.
        theWidget(paste(currentModelTab(), "_evaluate_checkbutton",
                           sep=""))$setActive(TRUE)
      }
    }
  }


  ## When changing to the LOG page desensitise the Execute button. Not
  ## sure why anyone would push the execute button anyhow, so maybe
  ## this is just better to result in an errorDialog rather than extra
  ## logic here to greyt out the button?
  
  if (page == .NOTEBOOK.LOG.NAME)
  {
    theWidget("execute_button")$setSensitive(FALSE)
    theWidget("execute_menu")$setSensitive(FALSE)
  }
  else
  {
    theWidget("execute_button")$setSensitive(TRUE)
    theWidget("execute_menu")$setSensitive(TRUE)
  }
    
  ## Record the current page so when we change we know which was last.

  crs$page <<- page

}

########################################################################

## HELP

popupTextviewHelpWindow <- function(topic)
{
  collectOutput(sprintf("help(%s, htmlhelp=TRUE)", topic), TRUE)
}

showHelpPlus <- function(msg)
{
  if (is.null(questionDialog(paste(gsub(" <<>> ", "\n\n",
                                         gsub("\n", " ", msg)),
                                    "Would you like to view the R help?",
                                    sep="\n\n"))))
    return(FALSE)
  else
    return(TRUE)
}

showHelp <- function(msg)
{
  infoDialog(paste(gsub(" <<>> ", "\n\n", gsub("\n", " ", msg))))
}

on_help_general_activate <- function(action, window)
{
  showHelp("Rattle is an Open Source project
written in GNOME and R, and licensed under the GNU General Public License.
<<>>
Interaction with Rattle logically proceeds by progressing through the Tabs:
first load in some Data, select Variables for exploring and mining,
possibly Sample the data, Explore the data, build your Models,
and Evaluate them. For any tab, the modus operandi is to configure
the options available and then click the Execute button (or F5) to perform
the appropriate tasks. Note that the tasks are NOT performed until
the Execute button (or F5 or the Execute menu item under Tools) is clicked.
<<>>
The Status Bar indicates when the action
is completed. Messages from R (e.g., error messages. although I do attempt
to catch them first) will appear in the R console
from where you started Rattle. The corresponding R Code will
appear in the Log tab.
This allows you to review the R commands
that perform the corresponding data mining tasks. Even better though,
you can copy the text from here and paste it into the same R Console
from which Rattle is running, and execute the commands directly.
This allows you to use Rattle to do the basics, and then where you
need more sophistication, go into R directly. Rattle uses a variable called
crs to store its current state, and you can modify this directly.
<<>>
Rattle is being extensively tested
on binary classification problems (with 0/1 or a two level variable
as the outcomes for the Target variable). It is less well tested on
general classification and regression tasks.
<<>>
The most I can guarantee about this
code is that there are bugs! When you find one, or a misfeature or
something else you would like Rattle to do, please do email me at
Graham.Williams@togaware.com.
<<>>
Enjoy.")
}

on_help_nomenclature_data_activate <- function(action, window)
{
  showHelp("There are many
different nomenclatures being used in data mining, deriving from the many
different contributory fields. Here, we attempt to stay with a single,
consistent nomenclature.
<<>>
A dataset consists of entities described using variables,
which might consist of a mixture of input variables and output variables,
either of which may be categorical or numeric.
<<>>
dataset = A collection of data.
<<>>
entity = An object of interest, descibed by variables.
Also called a record or object.
<<>>
variable = The data items used to describe an enitity.
Also called an attribute or feature.
<<>>
input variable = A measured or preset data item.
Also called predictor, independent variable, observed variable,
or descriptive variable.
<<>>
output variable = A variable possibly influenced by the input variables.
Also called response or dependent variable.
<<>>
categorical variable = A variable that takes on a value from a fixed
set of values. In R these are called factors and the set of possible values
is refered to as the levels of the factor.
<<>>
numeric variable = A variable that has values that are integers or real
numbers.")
}

on_help_csv_activate <- function(action, window)
{
  if (showHelpPlus("Rattle can load data from
a comma separated value (CSV) file, as might be generated
by spreadsheets and databases,
including Excel, Gnumeric, SAS/EM, QueryMan, and many other applications.
This is a good option for importing your data into Rattle.
<<>>
The CSV file is assumed to begin with a header row, listing the names
of the variables. 
The remainder of the file is expected to consist of rows of data that record
information about the entities, with fields generally separated by commas
recording the values of the variables for this entity.
<<>>
Use the Separator box to choose a separator other than the default comma.
A common alternative is a tab (\\t), or simply leave it blank to have
any white space act as a separator.
<<>>
The corresponding R code uses the simple read.csv() function."))
    popupTextviewHelpWindow("read.csv") }

on_help_rdata_file_activate <- function(action, window)
{
  showHelp("Choose this if you have data stored in an R dataset
(usually with a filename extension of .Rdata).
The named file will be loaded and any data frames found in there will
be listed for selection.")
}

on_help_rdataset_activate <- function(action, window)
{
  showHelp("Rattle can use a dataset that is already loaded
into R (although it will take a copy of it, with memory implications).
Only data frames are currently supported, and Rattle will list
for you the names of all of the available data frames.
<<>>
The data frames need to be constructed in the same R session
that is running Rattle (i.e., the same R Console in which you
sourced the Rattle package). This provides much more flexibility in
loading data into Rattle, than is provided directly through the actual Rattle
interface. For example, you may want to use the SQLLite package to load
data from a database directly.")
}

on_help_odbc_activate <- function(action, window)
{
  if(showHelpPlus("Rattle can establish a connection to a database
through the RODBC package. Tables avilable in the database will then be
listed for selection."))
  {
    require(RODBC, quietly=TRUE)
    popupTextviewHelpWindow("RODBC")
  }
}

on_help_roles_activate <- function(action, window)
{
  showHelp("The Variables tab allows you to select roles for the
variables.
<<>>
By default, all variables have an Input role, except for any variables
that have constant value, or categoricals with as many values as there
are rows (identifier variables). These will be marked as Ignore.
<<>>
One variable may also be identified as the Target (the first or last
categorical by default).
<<>>
    Modify the roles as appropriate for each variable.
<<>>
    Input: Used for modelling.
<<>>
    Target: Output for modelling.
<<>>
    Risk: A variable used in the Risk Chart
<<>>
    Ident: An identify for unique entities in the data set.
<<>>
    Ignore: Do not use this variable
<<>>
The Input and Ignore buttons can be used to operate on a selection.
A Shift-Click in the variable list will select all variables from the
last click to current variable. A Ctrl-Click will add the current
variable to those selected.
<<>>
The Weights Calculator option allows a weights formula to be specified
to assign a weight to each entity. See the separate help for details.")
}

on_help_weight_calculator_activate <- function(action, window)
{
  showHelp("Weights are used by variable modellers to identify
some entities as more important than others. The Weights Calculator
can be used to specify a formula in terms of the variables in the dataset.
You can list just a variable name, or
any formula can be used, as long as it is a valid R formula - R will be
asked to evaluate the formula.
<<>>
An example might be 'abs(Rev)/max(Rev)*10+1' which takes the absolute
value of a variable called Rev, divides it by the maximum value of Rev in the
dataset, times 10, adding 1 to it to give numbers from 1 up.")
}

on_help_sample_activate <- function(action, window)
{
  showHelp("Sampling is activated by default, randomly choosing 70%% of the
data for a training dataset and 30%% for a test dataset. The training dataset
is used to build models whilst the test dataset is used to evaluate the
models on otherwise unseen data.
<<>>
A new random sample is extracted each time the tab is executed. However,
you will get the same random sample each time, for a given seed. Changing the
seed allows different random samples to be extracted. This could be useful in
testing the sensitivity of modelling with different training sets.")
}


on_help_impute_activate <- function(action, window)
{
  showHelp("Imputation is used to fill in the missing values in the data.
The Zero/Missing imputation is a very simple method. Any missing numeric data
is simply assigned 0 and any missing categorical data is put into a new
category, Missing.")
}

on_help_summary_activate <- function(action, window)
{
  if (showHelpPlus("A summary of the dataset includes various pieces of
information about each of the variables of the dataset.
<<>>
For numeric data, this
can include the minimum, maximum, median (the value of the variable at the
midpoint of the dataset), mean (the average value of the variable),
and the first and third quartiles (25 percent of the data has values
below the first
quartile, and another 25 percent of the data has values above the third quartile).
<<>>
For categorical data the frequency distribution across the values is listed.
If there are too many possible values, then only the top few are listed, with
the remainder counted as Other.
<<>>
The R function summary() is used for the summary.
<<>>
Additional or differently presented summary information is provided
through additional options. Describe produces a similar summary presented
differently. For numeric variables, the Basic statistics can be obtained,
including kurtosis and skewness.
<<>>
The kurtosis is a measure of the nature of the peaks
in the distribution of the data. A high kurtosis indicates a sharper peak
and fatter tails while a lower kurtosis indicates a more rounded peak
with wider shoulders.
<<>>
The skewness indicates the assymetry of the distribution. A positive skew
indicates that the tail to the right is longer, and a negative skew that the
tail to the left is longer.
<<>>
The fBasics package is used for the Basic summary and
the kurtosis and skewness."))
    {
      popupTextviewHelpWindow("summary")
      if (packageIsAvailable("Hmisc"))
      {
        require(Hmisc, quietly=TRUE)
        popupTextviewHelpWindow("describe")
      }
      if (packageIsAvailable("fBasics"))
      {
        require(fBasics, quietly=TRUE)
        popupTextviewHelpWindow("basicStats")
      }
    }
}

on_help_correlation_activate <- function(action, window)
{
  if (showHelpPlus( "A pairwise correlation between each numeric variable
is calculated and displayed numerically in the text window whilst
a graphic plot is also generated. The plot uses circles and colour to
indicate the strength of any correlation.
<<>>
The R function cor() is used to produce the correlation data."))
    popupTextviewHelpWindow("cor")
}

on_help_hierarchical_correlation_activate <- function(action, window)
{
  if (showHelpPlus( "A hierarchical cluster
of the correlations between the variables of the dataset is generated, and
presented pictorially as a dendrogram.  From the dendrogram you can
see groups of variables that are highly correlated. The code uses the
cor() function to gnerate the correlations between the variables, the
hclust() function to perform the hierarchical clustering, and converts
the result to a dendrogram, using as.dendrogram(), for plotting."))
  {
    popupTextviewHelpWindow("cor")
    popupTextviewHelpWindow("hclust")
    popupTextviewHelpWindow("dendrogram")
  }
 
}

on_help_principal_components_activate <- function(action, window)
{
  if (showHelpPlus("Principal components analysis identifies
a collection of derived variables (expressed as a linear combination
of the other variables) that account for the variance
in the data. Often, the first few components account for the majority
of the variation. The plot is called a scree plot.
<<>>
There will be as many components as there are (numeric) variables in
the dataset, but by discarding those components contributing very
little, you may end up with fewer variables for modelling.
<<>>
Interpretability may reduce through using the derived variables rather
than the original variables, so you may like to instead identify those
variables that contribute most to the first few principal components.
<<>>
The prcomp() function is used to generate the principal components
which are then displayed in the textview and the relative importance
of the components is plotted.
<<>>
Note that only numeric data is included in the analysis."))
    popupTextviewHelpWindow("prcomp")
}

on_help_glm_activate <- function(action, window)
{
  if (showHelpPlus("A tradition approach to model building is
regression. Logistic regression (using the binomial family) is used
to model binary outcomes. Linear regression (using the gaussian family)
is used to model a linear numeric outcome. For predicting where the
outcome is a count, the poisson family is used. Further families are
available, but for now require you to run the glm command directly.
Please see the additional documentation.
<<>>
The R function glm() is used for regression."))
  {
    popupTextviewHelpWindow("glm")
    popupTextviewHelpWindow("family")
  }
}

on_help_support_vector_machine_activate <- function(action, window)
{
  if (showHelpPlus("SVM (Support Vector Machine) is a modern approach
to modelling where the data is mapped to a higher dimensional space so
that it is more likely that we can find vectors separating the classes.
Rattle deploys ksvm from the kernlab package."))
  {
    #if (packageIsAvailable("e1071", "view documentation for e1071"))
    #{
    #  require(e1071, quietly=TRUE)
    #  popupTextviewHelpWindow("svm")
    #}
    if (packageIsAvailable("kernlab", "view documentation for kernlab"))
    {
      require(kernlab, quietly=TRUE)
      popupTextviewHelpWindow("ksvm")
    }
  }
}

on_help_confusion_table_activate <- function(action, window)
{
  if (showHelpPlus("A confusion table concisely reports the performance
of a model against a testing dataset. Generally, the number of entities
predicted by the model into each of the classes is presented against the
actual class to which that entity belongs. Rattle reports two confusion tables.
The first is the raw entity counts whilst the second reports the
percentages."))
  {
    popupTextviewHelpWindow("table")
  }
}

on_help_sensitivity_activate <- function(action, window)
{
  if (showHelpPlus("The Sensitivity versus Specificity chart
is simply an alternative ROC curve, with Sensitivity being the
true positive rate (the count of true positives divided by the
count of positives) and Specificity being the true negative rate
(the count of true negatives divided by the count of negatives).
An ROC curve has the false positive rate instead of Specificity, which
is simply the count of false positives divided by the number of negatives
(1-fnr)."))
  {
    require(ROCR, quietly=TRUE)
    popupTextviewHelpWindow("performance")
  }
}

