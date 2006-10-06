## Gnome R Data Miner: GNOME interface to R for Data Mining

## Time-stamp: <2006-10-06 22:17:39 Graham Williams>

## TODO: The different varieties of Rattle paradigms can be chosen as
## radio buttons above the tabs, and different choices result in
## different collections of tabs being exposed.
##
## Two Class -> data variables sample explore model evaluate log
## Unsupervised -> data variables sample explore cluster log
## Text Miner -> text ... variables sample explore model evaluate log
## Multi Class -> data variables sample explore model evaluate log
## Regression ->  data variables sample explore model evaluate log

MAJOR <- "2"
MINOR <- "1"
REVISION <- unlist(strsplit("$Revision$", split=" "))[2]
VERSION <- paste(MAJOR, MINOR, REVISION, sep=".")

## Copyright (c) 2006 Graham Williams, Togaware.com

.onLoad <- function(libname, pkgname)
{
  ## TODO: How to not do this if quietly=TRUE? Otherwise it will be
  ## annoying, just like fBasics. randomForest seems to do it
  ## correctly?
  
  cat("Rattle, (c) 2006, Graham Williams, togaware.com, GPL")
  cat("\nGraphical interface for data mining using R\n")
  cat(sprintf("Version %s\n", VERSION))
}

## Acknowledgements: Frank Lu has provided much feedback and has
## extensively tested the application.

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
##    Use dot spearated words for variables: current.rattle.list.of.frames
##    RGtk2 uses the capitalised word convention.
##    Use same names in R code as for the Glade objects.

## BUGS
##
##   Tooltips are not working on GNU/Linux. Just fine on MS/Windows.
##
##   The RGtk2 author, Michael Lawrence, notes that most of the GUI
##   functionality in Gnome (i.e., libgnome and libgnomeui) will soon
##   be merged into GTK. At that time, that functionality will be part
##   of RGtk2.

rattleTODO <- function()
{
  
  todo <- 'Suggestion, Proposed, Proposer, Comment
Add SOMs (SOM_PAK3.1),2006-09-03,Stuart Hamilton,Suitable for RattleEX
Add MART (R-MART),2006-09-03,Stuart Hamilton,Useful? Perhaps for rattleRG
'
  todo <- read.csv(textConnection(todo))

  return(todo)
}


########################################################################

## INITIALISATIONS

rattle <- function()
{

  require(RGtk2, quietly=TRUE) # From http://www.ggobi.org/rgtk2/
      
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
  
  ## VARIABLES Treeview Columns

  COLUMN <<- c(number = 0, variable = 1, type = 2, input = 3,
               target = 4, risk = 5, ident = 6, ignore = 7, comment = 8)

  CATEGORICAL <<- c(number = 0, variable = 1, barplot = 2,
                    dotplot = 3, comment = 4)

  CONTINUOUS <<-  c(number = 0, variable = 1, boxplot = 2,
                    hisplot = 3, cumplot = 4, benplot = 5, comment = 6)
  
  ## MODELLERS

  GLM   <<- "glm"
  RPART <<- "rpart"
  GBM   <<- "gbm"
  RF    <<- "rf"
  SVM   <<- "svm"
  KSVM  <<- "ksvm"

  MODELLERS <<- c(RPART, RF, KSVM, GLM, GBM)
  
  ## RPART
  
  RPART.CP.DEFAULT        <<- 0.010
  RPART.MINSPLIT.DEFAULT  <<- 20
  RPART.MINBUCKET.DEFAULT <<- 7
  RPART.MAXDEPTH.DEFAULT  <<- 30
  
  RF.NTREE.DEFAULT    <<- 500
  RF.MTRY.DEFAULT     <<- 10
  RF.SAMPSIZE.DEFAULT <<- 100
  
  ## MISC
  
  START.LOG.COMMENT <<- "\n\n## "	# Assume paste with sep=""
  LOG.COMMENT       <<- "\n## "	# Assume paste with sep=""
  END.LOG.COMMENT   <<- "\n\n"	# Assume paste with sep=""
  
########################################################################
  
  ## PACKAGE STATE VARIABLE
  
  ## Global variables are generally a bad idea, but until a better idea
  ## comes to mind.
  
  crs <<- list(dataset=NULL,
               dataname=NULL,
               input=NULL,
               target=NULL,
               weights=NULL,
               risk=NULL,
               ident=NULL,
               ignore=NULL,
               sample=NULL,
               seed=NULL,
               kmeans=NULL,
               hclust=NULL,
               smodel=NULL, # Record whether the sample has been modelled
               glm=NULL,
               rpart=NULL,
               gbm=NULL,
               rf=NULL,
               svm=NULL,
               ksvm=NULL,
               perf=NULL,
               eval=NULL,
               testset=NULL,
               testname=NULL)
  
  NOTEBOOK               <<- rattleWidget("notebook")
  NOTEBOOK.DATA.TAB      <<- getNotebookPage(NOTEBOOK, "Data")
  NOTEBOOK.EXPLORE.TAB   <<- getNotebookPage(NOTEBOOK, "Explore")
  NOTEBOOK.VARIABLES.TAB <<- getNotebookPage(NOTEBOOK, "Variables")
  NOTEBOOK.SAMPLE.TAB    <<- getNotebookPage(NOTEBOOK, "Sample")
  NOTEBOOK.CLUSTER.TAB   <<- getNotebookPage(NOTEBOOK, "Cluster")
  NOTEBOOK.MODEL.TAB     <<- getNotebookPage(NOTEBOOK, "Model")
  NOTEBOOK.EVALUATE.TAB  <<- getNotebookPage(NOTEBOOK, "Evaluate")
  NOTEBOOK.LOG.TAB       <<- getNotebookPage(NOTEBOOK, "Log")
  
  DATA              <<- rattleWidget("data_notebook")
  DATA.CSV.TAB      <<- getNotebookPage(DATA, "csv")
  DATA.RDATA.TAB    <<- getNotebookPage(DATA, "rdata")
  DATA.RDATASET.TAB <<- getNotebookPage(DATA, "rdataset")
  DATA.ODBC.TAB     <<- getNotebookPage(DATA, "odbc")
  
  EXPLORE                 <<- rattleWidget("explore_notebook")
  EXPLORE.SUMMARY.TAB     <<- getNotebookPage(EXPLORE, "summary")
  EXPLORE.PLOT.TAB        <<- getNotebookPage(EXPLORE, "explot")
  EXPLORE.GGOBI.TAB       <<- getNotebookPage(EXPLORE, "ggobi")
  EXPLORE.CORRELATION.TAB <<- getNotebookPage(EXPLORE, "correlation")
  EXPLORE.HIERCOR.TAB     <<- getNotebookPage(EXPLORE, "hiercor")
  EXPLORE.PRCOMP.TAB      <<- getNotebookPage(EXPLORE, "prcomp")
  
  CLUSTER            <<- rattleWidget("cluster_notebook")
  CLUSTER.KMEANS.TAB <<- getNotebookPage(CLUSTER, "kmeans")
  CLUSTER.HCLUST.TAB <<- getNotebookPage(CLUSTER, "hclust")
  
  MODEL           <<- rattleWidget("model_notebook")
  MODEL.RPART.TAB <<- getNotebookPage(MODEL, RPART)
  MODEL.GLM.TAB   <<- getNotebookPage(MODEL, GLM)
  MODEL.GBM.TAB   <<- getNotebookPage(MODEL, GBM)
  MODEL.RF.TAB    <<- getNotebookPage(MODEL, RF)
  MODEL.SVM.TAB   <<- getNotebookPage(MODEL, SVM)

  SVMNB           <<- rattleWidget("svm_notebook")
  SVMNB.ESVM.TAB  <<- getNotebookPage(SVMNB, "esvm")
  SVMNB.KSVM.TAB  <<- getNotebookPage(SVMNB, "ksvm")
  
  EVALUATE                 <<- rattleWidget("evaluate_notebook")
  EVALUATE.CONFUSION.TAB   <<- getNotebookPage(EVALUATE, "confusion")
  EVALUATE.RISK.TAB        <<- getNotebookPage(EVALUATE, "risk")
  EVALUATE.LIFT.TAB        <<- getNotebookPage(EVALUATE, "lift")
  EVALUATE.ROC.TAB         <<- getNotebookPage(EVALUATE, "roc")
  EVALUATE.PRECISION.TAB   <<- getNotebookPage(EVALUATE, "precision")
  EVALUATE.SENSITIVITY.TAB <<- getNotebookPage(EVALUATE, "sensitivity")
  
########################################################################
  
  ## Now connect the callbacks
  
  gladeXMLSignalAutoconnect(rattleGUI)
  
  ## A friendly startup message in the status bar
  
  ##setStatusBar("Select a CSV filename to load into Rattle to get started.")
  
  ## Some initialisations
  
  initialiseVariableViews()
  
  ## Turn off the sub-notebook tabs.
  
  DATA$setShowTabs(FALSE)
  EXPLORE$setShowTabs(FALSE)
  CLUSTER$setShowTabs(FALSE)
  MODEL$setShowTabs(FALSE)
  EVALUATE$setShowTabs(FALSE)
  
  ## Set glm_family_comboboxentry to default value.
  
  rattleWidget("glm_family_comboboxentry")$setActive(0)
  
  ## Tell MS/Windows to use 2GB (TODO - What's needed under Win64?)
  
  if (is.windows()) memory.limit(2073)
  
  ##
  
  addToLog(sprintf("Started %s by %s", Sys.time(), Sys.info()["user"]),
          "## You can save all this to file by using the right mouse button to
## Select All of the text, and then the right mouse button again to Copy.
## Then paste the text into a text editor and save to file and/or print.
## The point of doing this would be to save a log of what you have done,
## potentially to repeat by sending the same commands directly to R.
## You can also save and load projects, which also retains this log.

library(rattle)

crs <- NULL")
  
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
  crs$seed     <<- NULL
  crs$kmeans   <<- NULL
  crs$hclust   <<- NULL
  crs$page     <<- NULL
  crs$smodel   <<- NULL
  crs$glm      <<- NULL
  crs$rpart    <<- NULL
  crs$gbm      <<- NULL
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
  setTextview("correlation_textview")
  setTextview("prcomp_textview")
  setTextview("kmeans_textview")
  setTextview("hclust_textview")
  setTextview("rpart_textview")
  setTextview("glm_textview")
  setTextview("gbm_textview")
  setTextview("rf_textview")
  setTextview("esvm_textview")
  setTextview("ksvm_textview")
  setTextview("confusion_textview")
  setTextview("roc_textview")

  ## Set all sub tabs back to the default tab page and reflect this in
  ## the appropriate radio button.

  EXPLORE$setCurrentPage(EXPLORE.SUMMARY.TAB)
  rattleWidget("summary_radiobutton")$setActive(TRUE)

  CLUSTER$setCurrentPage(CLUSTER.KMEANS.TAB)
  rattleWidget("kmeans_radiobutton")$setActive(TRUE)

  MODEL$setCurrentPage(MODEL.RPART.TAB)
  rattleWidget("rpart_radiobutton")$setActive(TRUE)

  EVALUATE$setCurrentPage(EVALUATE.CONFUSION.TAB)
  rattleWidget("confusion_radiobutton")$setActive(TRUE)

  ## Reset the VARIABLES tab.
  
  rattleWidget("variables_treeview")$getModel()$clear()
  rattleWidget("categorical_treeview")$getModel()$clear()
  rattleWidget("continuous_treeview")$getModel()$clear()

  rattleWidget("weight_entry")$setText("")
  rattleWidget("rpart_weights_label")$setText("Weights:")
  
  ## Reset RPART options.
  
  rattleWidget("rpart_priors_entry")$setText("")
  rattleWidget("rpart_loss_entry")$setText("")
  rattleWidget("rpart_minsplit_spinbutton")$setValue(RPART.MINSPLIT.DEFAULT)
  rattleWidget("rpart_maxdepth_spinbutton")$setValue(RPART.MAXDEPTH.DEFAULT)
  rattleWidget("rpart_cp_spinbutton")$setValue(RPART.CP.DEFAULT)
  rattleWidget("rpart_minbucket_spinbutton")$setValue(RPART.MINBUCKET.DEFAULT)
  
  ## Update EXPLORE, MODEL and EVALUATE targets

  rattleWidget("explot_target_label")$setText("No target selected")

  rattleWidget("glm_target_label")$setText("No target selected")
  rattleWidget("rpart_target_label")$setText("No target selected")
  rattleWidget("gbm_target_label")$setText("No target selected")
  rattleWidget("rf_target_label")$setText("No target selected")
  rattleWidget("svm_target_label")$setText("No target selected")
  rattleWidget("evaluate_risk_label")$setText("No risk variable selected")
  
  rattleWidget("evaluate_training_radiobutton")$setActive(TRUE)
  rattleWidget("evaluate_filechooserbutton")$setFilename("")
  rattleWidget("evaluate_rdataset_combobox")$setActive(-1)
  
}

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

  infoDialog(sprintf(paste("The function you activated (via %s)",
                            "of type %s is not yet implemented."),
                      action$getName(), action$typeName()))
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
  if (! is.element(pkg, (rownames(installed.packages()))))
  {
    if (!is.null(msg))
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
  
  if (rattleWidget("sample_checkbutton")$getActive()
      && is.null(crs$sample))
  {
    errorDialog("Sampling is active but has not been Executed.",
                    "Either ensure you Execute the sampling by clicking",
                    "the Execute button on the Sample tab,",
                    "or else de-activate Sampling on the Sample tab.")
    return(TRUE)
  }

  ## If sampling is inactive, make sure there is no sample.

  if (! rattleWidget("sample_checkbutton")$getActive()
      && ! is.null(crs$sample))
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
## Simplify updates to status bar and textviews
##

setRattleTitle <- function(title)
{
  standard <- "Rattle: Effective Data Mining with R"
  if (is.null(title))
    rattleWidget("rattle_window")$setTitle(standard)
  else
    rattleWidget("rattle_window")$setTitle(sprintf("%s: %s", standard, title))
}
                                           

setStatusBar <- function(..., sep=" ")
{
  msg <- paste(sep=sep, ...)
  if (length(msg) == 0) msg <-""
  rattleWidget("statusbar")$push(1, msg)
  invisible(NULL)
}

setTextview <- function(tv, ..., sep="")
{
  msg <- paste(sep=sep, ...)
  if (length(msg) == 0) msg <-""
  rattleWidget(tv)$getBuffer()$setText(msg)
}

textviewSeparator <- function()
{
  return(paste(sprintf("\n\nGenerated by Rattle %s %s\n", Sys.time(),
                       Sys.info()["user"]),
               paste(rep("=", 70), collapse=""), "\n", sep=""))
}

clearTextview <- function(tv)
{
  rattleWidget(tv)$modifyFont(pangoFontDescriptionFromString("monospace 10"))
  rattleWidget(tv)$getBuffer()$setText("")
}

appendTextview <- function(tv, ..., sep="")
{
  msg <- paste(sep=sep, ...)
  if (length(msg) == 0)
    msg <-""
  else
    msg <- paste(msg, sprintf("\n\nGenerated by Rattle %s %s\n",
                              Sys.time(), Sys.info()["user"]),
                 paste(rep("=", 70), collapse=""), "\n", sep="")
  tv.buf <- rattleWidget(tv)$getBuffer()
  location <- tv.buf$getEndIter()$iter
  tv.buf$insert(location, msg)
}

addLogSeparator <- function(msg=NULL)
{
  ## Output a suitable separator to the log textview, and if there is
  ## an optional MSG, display that message, as an introduction to this
  ## section.
  
  addToLog(paste("\n\n##", paste(rep("=", 60), collapse=""),
                "\n## Timestamp: ", Sys.time(), sep=""),
          no.start=TRUE)
  if (!is.null(msg))
    addToLog(paste(sep="", START.LOG.COMMENT, msg), no.start=TRUE)
}

addToLog <- function(start, ..., sep=" ", no.start=FALSE)
{
  if (no.start)
    msg <- paste(sep=sep, start, ...)
  else
    msg <- paste(sep="", START.LOG.COMMENT, start, END.LOG.COMMENT, ...)
  if (length(msg) == 0) msg <-""

  ## Always place text at the end, irrespective of where the cursor is.

  log.buf <- rattleWidget("log_textview")$getBuffer()
  location <- log.buf$getEndIter()$iter

  log.buf$insert(location, msg)
}

collect.output <- function(command, use.print=FALSE)
{
  ## TODO Should this use cat or print? Cat translates the \n to a
  ## newline and doesn't precede the output by [1].  For pretty output
  ## with sprintf() you probably want cat(), but if you have a vector
  ## of formatted text and you want to look at it (as data), print()
  ## would be better.

  if (use.print)
    command <- paste("print(", command, ")", sep="")
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
  return(paste(commandsink, collapse="\n"))
}

########################################################################
##
## Miscellaneous Support
##

rattleWidget <- function(widget)
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

is.windows <- function()
{
  return(.Platform$OS.type == "windows")
}

listBuiltModels <- function()
{
#  return(! (is.null(crs$glm) && is.null(crs$rpart) &&
#            is.null(crs$gbm) && is.null(crs$rf) &&
#            is.null(crs$svm)))
  models <- c()
  for (m in MODELLERS)
    if (! is.null(eval(parse(text=sprintf("crs$%s", m)))))
      models <- c(models, m)
  return(models)
}

setDefaultPath <- function(filename)
{
  if (! is.null(filename)) setwd(dirname(filename))
}

newPlot <- function(pcnt=1)
{
  if (.Platform$GUI == "X11")
    x11()
  else if (is.windows())
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
  rattleWidget("rattle_window")$getWindow()$
  setCursor(gdkCursorNew(cursor))
}

simplify.number.list <- function(nums)
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
  current <- rattleWidget("rdataset_combobox")$getActiveText()
  
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
  if (! is.null(dl))
  {
    action$getModel()$clear()
    lapply(dl, action$appendText)
    ## Set the selection to that which was already selected, if possible.
    if (! is.null(current) && is.element(current, dl))
      action$setActive(which(sapply(dl, function(x) x==current))[1]-1)
  }
}

quit_rattle <- function(action, window)
{
  for (i in dev.list()) dev.off(i)
  rattleWidget("rattle_window")$destroy()
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
  rattleWidget("data_textview")$setWrapMode("word")
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
    DATA$setCurrentPage(DATA.CSV.TAB)
  }
  setStatusBar()
}

on_csv_filechooserbutton_update_preview<- function(button)
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
}

on_rdata_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    DATA$setCurrentPage(DATA.RDATA.TAB)
  }
  setStatusBar()
}

on_rdata_filechooserbutton_update_preview<- function(button)
{
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

  filename <- rattleWidget("rdata_filechooserbutton")$getFilename()
  setDefaultPath(filename)
  
  ## Fix filename for MS - otherwise eval/parse strip the \\.

  if (is.windows()) filename <- gsub("\\\\", "/", filename)

  ## Generate commands to read the data and then display the structure.

  load.cmd <- sprintf('load("%s")', filename)

  ## Start logging and executing the R code.

  addLogSeparator()

  addToLog("Load an Rdata file containing R objects.", load.cmd)
  set.cursor("watch")
  eval(parse(text=paste("new.objects <- ", load.cmd)), baseenv())
  set.cursor()

  ## Add new dataframes to the combo box.

  combobox <- rattleWidget("rdata_combobox")
  if (! is.null(new.objects))
  {
    combobox$getModel()$clear()
    lapply(new.objects, combobox$appendText)
  }
  
  rattleWidget(TV)$setWrapMode("word")
  clearTextview(TV)
  appendTextview(TV, "Now select a data frame from those available.")
  setStatusBar()

}

on_rdataset_radiobutton_toggled <- function(button)
{
  #cat("XXX R Dataset Radio toggled XXX\n")
  if (button$getActive())
  {
    DATA$setCurrentPage(DATA.RDATASET.TAB)
  }
  setStatusBar()
}

on_odbc_radiobutton_toggled <- function(button)
{
  if (button$getActive()) DATA$setCurrentPage(DATA.ODBC.TAB)
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

  if (! is.null(crs$odbc)) close(crs$odbc)
  
  ## Obtain name of the DNS.

  DNSname <- rattleWidget("odbc_dns_entry")$getText()
  
  ## Generate commands to connect to the database and retrieve the tables.

  libraryCmd <- sprintf("require(RODBC, quietly=TRUE)")
  connectCmd <- sprintf('crs$odbc <<- odbcConnect("%s")', DNSname)
  tablesCmd  <- sprintf('sqlTables(crs$odbc)$TABLE_NAME')
  
  ## Start logging and executing the R code.

  if (! packageIsAvailable("RODBC", "connect to an ODBC database")) return()
      
  addLogSeparator()

  addToLog("Require the RODBC library", libraryCmd)
  eval(parse(text=libraryCmd))
       
  addToLog("Open the connection to the ODBC service.",
          gsub('<<-', '<-', connectCmd))
  result <- try(eval(parse(text=connectCmd)))
  if (inherits(result, "try-error"))
  {
    errorDialog("The attempt to open the ODBC connection failed.",
                "Please check that the DNS is correct.",
                "See the R Console for further details.")
    return()
  }
  
  addToLog("Load the names of available tables.", tablesCmd)
  set.cursor("watch")
  result <- try(eval(parse(text=paste("tables <<- ", tablesCmd))))
  set.cursor()
  if (inherits(result, "try-error"))
  {
    errorDialog("The attempt to query the ODBC connection failed.",
                "Please check that the DNS is correct.",
                "See the R Console for further details.")
    return()
  }

  ## Add list of tables to the combo box.

  combobox <- rattleWidget("odbc_combobox")
  if (! is.null(tables))
  {
    combobox$getModel()$clear()
    lapply(tables, combobox$appendText)
  }
  
  rattleWidget(TV)$setWrapMode("word")
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
  if (rattleWidget("csv_radiobutton")$getActive())
    execute.data.csv()
  else if (rattleWidget("odbc_radiobutton")$getActive())
    executeDataODBC()
  else if (rattleWidget("rdata_radiobutton")$getActive())
    executeDataRdata()
  else if (rattleWidget("rdataset_radiobutton")$getActive())
    executeDataRdataset()
}

resetVariableRoles <- function(variables, nrows, input=NULL, target=NULL,
                               risk=NULL, ident=NULL, ignore=NULL,
                               boxplot=NULL,
                               hisplot=NULL, cumplot=NULL, benplot=NULL,
                               barplot=NULL, dotplot=NULL)
{
  ## Update the variables treeview with the dataset variables.

  createVariablesModel(variables, input, target, risk, ident, ignore,
                       boxplot, hisplot, cumplot, benplot, barplot, dotplot)

  ## Turn sampling on, set range bounds and generate the default 70%
  ## sample. Do the range bounds first since otherwise the value gets
  ## set back to 1. Also, need to set both the percentage and the
  ## count since if the old percentage is 70 and the new is 70, then
  ## no change in value is noticed, and thus the count is not
  ## automatically updated.

  per <- 70
  srows <- round(nrows * per / 100)
  rattleWidget("sample_checkbutton")$setActive(TRUE)
  rattleWidget("sample_count_spinbutton")$setRange(1,nrows)
  rattleWidget("sample_count_spinbutton")$setValue(srows)
  rattleWidget("sample_percentage_spinbutton")$setValue(per)

  executeSampleTab()

}

execute.data.csv <- function()
{
  TV <- "data_textview"
  
  ## Collect relevant data

  filename <- rattleWidget("csv_filechooserbutton")$getFilename()
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

  if ( ! is.null(listBuiltModels()) )
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

  if (is.windows()) filename <- gsub("\\\\", "/", filename)

  ## Get the separator to use.

  sep = rattleWidget("csv_separator_entry")$getText()
  if (sep != ",")
    sep <- sprintf(', sep="%s"', sep)
  else
    sep <- ""

  nastring <- ', na.strings=c(".", "NA")'
  
  ## Generate commands to read the data and then display the structure.

  read.cmd <- sprintf('crs$dataset <<- read.csv("%s"%s%s)',
                      filename, sep, nastring)
  str.cmd  <- "str(crs$dataset)"
  
  ## Start logging and executing the R code.

  addLogSeparator()
  rattleWidget(TV)$setWrapMode("none") # On for welcome msg
  clearTextview(TV)
  
  addToLog("Load a dataset from a CSV file.", gsub('<<-', '<-', read.cmd))
  resetRattle()
  eval(parse(text=read.cmd))
  crs$dataname <<- basename(filename)
  setRattleTitle(crs$dataname)

  addToLog("Display a simple summary (structure) of the dataset.", str.cmd)
  appendTextview(TV, sprintf("Structure of %s.\n\n", filename),
                  collect.output(str.cmd))
  
  ## Update the variables treeview and samples.

  resetVariableRoles(colnames(crs$dataset), nrow(crs$dataset)) 

  setStatusBar("The data has been loaded:", crs$dataname)
}

executeDataODBC <- function()
{
  TV <- "data_textview"

  table <- rattleWidget("odbc_combobox")$getActiveText()

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

  if ( ! is.null(listBuiltModels()) )
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

  assignCmd <- "crs$dataset <<- sqlFetch(crs$odbc, table)"
  strCmd  <- "str(crs$dataset)"

  numRows <- sqlQuery(crs$odbc, sprintf("SELECT count(*) FROM %s", table))

  DNSname <- rattleWidget("odbc_dns_entry")$getText()
  if (numRows > 50000)
    if (is.null(questionDialog("You are about to extract", numRows,
                               "rows from the table", table,
                               "of the", DNSname, "ODBC connection.",
                               "That's quite a few for R to load into memory.",
                               "Do you wish to continue?")))
        
      return()
  
  ## Start logging and executing the R code.

  addLogSeparator()
  rattleWidget("data_textview")$setWrapMode("none") # On for welcome msg
  clearTextview(TV)
  
  addToLog("Load a dataset from a database table.",
          gsub('<<-', '<-', assignCmd))
  resetRattle()
  eval(parse(text=assignCmd))
  crs$dataname <<- table
  setRattleTitle(crs$dataname)

  addToLog("Display a simple summary (structure) of the dataset.", strCmd)
  appendTextview(TV,
                  sprintf("Structure of %s from %s.\n\n", table, DNSname),
                  collect.output(strCmd))
  
  ## Update the variables treeview and samples.

  resetVariableRoles(colnames(crs$dataset), nrow(crs$dataset)) 

  setStatusBar("The data has been loaded:", crs$dataname)

}

executeDataRdata <- function()
{
  TV <- "data_textview"
  
  ## Collect relevant data

  filename <- rattleWidget("rdata_filechooserbutton")$getFilename()
  setDefaultPath(filename)
  dataset <- rattleWidget("rdata_combobox")$getActiveText()

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

  if ( ! is.null(listBuiltModels()) )
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
  rattleWidget("data_textview")$setWrapMode("none") # On for welcome msg
  clearTextview(TV)
  
  addToLog("Assign an R dataset from the Rdata file.",
          gsub('<<-', '<-', assign.cmd))
  resetRattle()
  eval(parse(text=assign.cmd))
  crs$dataname <<- dataset
  setRattleTitle(crs$dataname)

  addToLog("Display a simple summary (structure) of the dataset.", str.cmd)
  appendTextview(TV,
                  sprintf("Structure of %s from %s.\n\n", dataset, filename),
                  collect.output(str.cmd))
  
  ## Update the variables treeview and samples.

  resetVariableRoles(colnames(crs$dataset), nrow(crs$dataset)) 

  setStatusBar("The data has been loaded:", crs$dataname)
}

executeDataRdataset <- function()
{
  TV <- "data_textview"
  
  ## Collect relevant data
  dataset <- rattleWidget("rdataset_combobox")$getActiveText()
  
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

  if ( ! is.null(listBuiltModels()) )
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
  rattleWidget(TV)$setWrapMode("none") # On for welcome msg
  clearTextview(TV)
  
  addToLog("Assign an R dataset into the crs object.",
          gsub('<<-', '<-', assign.cmd))
  resetRattle()
  eval(parse(text=assign.cmd))
  crs$dataname <<- dataset
  setRattleTitle(crs$dataname)
  
  addToLog("Display a simple summary (structure) of the dataset.", str.cmd)
  setTextview(TV, sprintf("Structure of %s.\n\n", dataset),
               collect.output(str.cmd), sep="")

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
    
    columns <- COLUMN[["input"]]:COLUMN[["ignore"]]
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
  tree.selection <- rattleWidget("variables_treeview")$getSelection()

  ## Under MS/Windows with Terminal Services to the host we get very
  ## slow redraws? Tried fixing it with freezeUpdates and thawUpdates
  ## but it had no impact. Changing 500 variables takes 5
  ## seconds. When connected over terminal services the elapsed time
  ## is 16 seconds, still with 5 seconds user time.
  
  ## rattleWidget("rattle_window")$getWindow()$freezeUpdates()

  tree.selection$selectedForeach(function(model, path, iter)
  {
    model$set(iter, COLUMN[["ignore"]], TRUE)
    columns <- setdiff(COLUMN[["input"]]:COLUMN[["ignore"]], COLUMN[["ignore"]])

    ## Timing indicates the for loop is slower on GNU/Linux but faster
    ## on MS/Windows 500! But the extra test also slows things down,
    ## so best not to conditionalise for now.

    #if (is.windows())
      for (c in columns)
        if (model$get(iter, c)[[1]]) model$set(iter, c, FALSE)
    #else
    #  lapply(columns, function(x) model$set(iter, x, FALSE))

    return(FALSE) # Keep going through all rows
  })

  ##cat("->Ig", proc.time() - ptm, "\n")
  set.cursor()

  ## rattleWidget("rattle_window")$getWindow()$thawUpdates()
}

on_variables_toggle_input_button_clicked <- function(action, window)
{
  ## Set the input flag for all selected variables, and ensure all
  ## other roles are unchecked.

  ##ptm <- proc.time()
  set.cursor("watch")

  treeview <- rattleWidget("variables_treeview")
  tree.selection <- treeview$getSelection()
  #rattleWidget("rattle_window")$getWindow()$freezeUpdates()

  tree.selection$selectedForeach(function(model, path, iter)
  {
    model$set(iter, COLUMN[["input"]], TRUE)
    columns <- setdiff(COLUMN[["input"]]:COLUMN[["ignore"]], COLUMN[["input"]])

    #if (is.windows())
      for (c in columns)
        if (model$get(iter, c)[[1]]) model$set(iter, c, FALSE)
    #else
    #  lapply(columns, function(x) model$set(iter, x, FALSE))

    return(FALSE) # Keep going through all rows
  })

  ##cat("->In", proc.time() - ptm, "\n")
  set.cursor()
  #rattleWidget("rattle_window")$getWindow()$thawUpdates()
}

##----------------------------------------------------------------------
##
## Execution
##

execute.variables.tab <- function()
{
  
  ## Can not do any preparation if there is no dataset.

  if (noDatasetLoaded()) return()

  input   <- getSelectedVariables("input")
  target  <- getSelectedVariables("target")
  risk    <- getSelectedVariables("risk")
  ident   <- getSelectedVariables("ident")
  ignore  <- getSelectedVariables("ignore")
  weights <- rattleWidget("weight_entry")$getText()
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

##   if (! is.null(target) && is.numeric(crs$dataset[[target]]) &&
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

  if (! is.null(risk) && ! is.numeric(crs$dataset[[risk]]))
  {
    errorDialog("The column selected for your risk",
                 sprintf("(%s)", crs$dataset[[risk]]),
                 "is not numeric. Please select a numeric column.")
    return()
  }

  ## Obtain a list of variables and R functions in the Weight Calculator

  if (! is.null(weights) && nchar(weights) > 0)
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

      if ( ! is.element(identifiers[vars][i], allvars))
      {
        errorDialog("The Weight Calculator contains the variable",
                     identifiers[vars][i], "which is not known in the",
                     "dataset.")
        return()
      }

      ## Check if Weight variables are not ignored, and inform user if not

      if ( ! is.element(identifiers[vars][i],
                        union(ident, union(target, union(ignore, risk)))))
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

  rattleWidget("explot_target_label")$setText(the.target)

  rattleWidget("rpart_target_label")$setText(the.target)
  rattleWidget("rf_target_label")$setText(the.target)
  rattleWidget("svm_target_label")$setText(the.target)
  rattleWidget("glm_target_label")$setText(the.target)
  rattleWidget("gbm_target_label")$setText(the.target)

  ## Update MODEL weights

  if (! is.null(crs$weights))
  {
    weights.display <- gsub('crs\\$dataset\\$', '', crs$weights)
    the.weight <- sprintf("Weights: %s", weights.display)
    rattleWidget("rpart_weights_label")$setText(the.weight)
  }
  
  ## Update EVALUATE risk variable
  
  rattleWidget("evaluate_risk_label")$setText(crs$risk)

  ## Update defaults tha rely on the number of variables.
  
  RF.MTRY.DEFAULT <<- floor(sqrt(length(crs$input)))
  rattleWidget("rf_mtry_spinbutton")$setValue(RF.MTRY.DEFAULT)

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
  ## same in each of COLUMNS, CATEGORICAL, and CONTINUOUS.

  variables <- NULL
  if (is.element(role, c("input", "target", "risk", "ident", "ignore")))
  {
    model <- rattleWidget("variables_treeview")$getModel()
    rcol  <- COLUMN[[role]]
  }
  else if (is.element(role, c("boxplot", "hisplot", "cumplot", "benplot")))
  {
    model <- rattleWidget("continuous_treeview")$getModel()
    rcol  <- CONTINUOUS[[role]]
  }
  else if (is.element(role, c("barplot", "dotplot")))
  {
    model <- rattleWidget("categorical_treeview")$getModel()
    rcol  <- CATEGORICAL[[role]]
  }
  else
    return(variables)

  vcol <- COLUMN[["variable"]]
  ncol <- COLUMN[["number"]]
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

  continuous <- gtkListStoreNew("gchararray", "gchararray",
                                "gboolean", "gboolean",
                                "gboolean", "gboolean", "gchararray")
  
  
  categorical <- gtkListStoreNew("gchararray", "gchararray",
                                 "gboolean", "gboolean",
                                 "gchararray")
  
  
  ## View the model through the treeview in the VARIABLES tab
  treeview <- rattleWidget("variables_treeview")
  treeview$setModel(model)

  catview <- rattleWidget("categorical_treeview")
  catview$setModel(categorical)
  
  conview <- rattleWidget("continuous_treeview")
  conview$setModel(continuous)

  ## Add the NUMBER column as the row number.

  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  col.offset <-
    treeview$insertColumnWithAttributes(-1,
                                        "No.",
                                        renderer,
                                        text= COLUMN[["number"]])
  
  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  cat.offset <-
    catview$insertColumnWithAttributes(-1,
                                       "No.",
                                       renderer,
                                       text= CATEGORICAL[["number"]])
  
  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  con.offset <-
    conview$insertColumnWithAttributes(-1,
                                       "No.",
                                       renderer,
                                       text= CONTINUOUS[["number"]])
  
  ## Add the VARIABLE NAME column to the views.
  
  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  col.offset <-
    treeview$insertColumnWithAttributes(-1,
                                        "Variable",
                                        renderer, 
                                        text = COLUMN[["variable"]])

  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  cat.offset <-
    catview$insertColumnWithAttributes(-1,
                                       "Variable",
                                       renderer, 
                                       text = CATEGORICAL[["variable"]])

  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  con.offset <-
    conview$insertColumnWithAttributes(-1,
                                       "Variable",
                                       renderer, 
                                       text = CONTINUOUS[["variable"]])

  ## Add the TYPE column.

  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  col.offset <-
    treeview$insertColumnWithAttributes(-1,
                                        "Data Type",
                                        renderer,
                                        text = COLUMN[["type"]])
  
  ## Add the INPUT column.

  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(radio = TRUE)
  renderer$set(width = 60)
  renderer$setData("column", COLUMN["input"])
  connectSignal(renderer, "toggled", item.toggled, model)
  col.offset <-
    treeview$insertColumnWithAttributes(-1,
                                        "Input",
                                        renderer,
                                        active = COLUMN[["input"]])
  
  ## Add the TARGET column.

  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(radio = TRUE)
  renderer$set(width = 60)
  renderer$setData("column", COLUMN["target"])
  connectSignal(renderer, "toggled", item.toggled, model)
  col.offset <-
    treeview$insertColumnWithAttributes(-1,
                                        "Target",
                                        renderer,
                                        active = COLUMN[["target"]])
  
  ## Add the RISK column.

  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(radio = TRUE)
  renderer$set(width = 60)
  renderer$setData("column", COLUMN["risk"])
  connectSignal(renderer, "toggled", item.toggled, model)
  col.offset <-
    treeview$insertColumnWithAttributes(-1,
                                        "Risk",
                                        renderer,
                                        active = COLUMN[["risk"]])
  
  ## Add the IDENT column.

  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(radio = TRUE)
  renderer$set(width = 60)
  renderer$setData("column", COLUMN["ident"])
  connectSignal(renderer, "toggled", item.toggled, model)
  col.offset <-
    treeview$insertColumnWithAttributes(-1,
                                        "Ident",
                                        renderer,
                                        active = COLUMN[["ident"]])
  
  ## Add the IGNORE column (the Ignore check button) to the view.

  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(radio = TRUE)
  renderer$set(width = 60)
  renderer$setData("column", COLUMN["ignore"])
  connectSignal(renderer, "toggled", item.toggled, model)
  col.offset <-
    treeview$insertColumnWithAttributes(-1,
                                        "Ignore",
                                        renderer,
                                        active = COLUMN[["ignore"]]) 

  ## Add the barplot and dotplot.

  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(width = 60)
  renderer$setData("column", CATEGORICAL["barplot"])
  connectSignal(renderer, "toggled", cat_toggled, categorical)
  cat.offset <-
    catview$insertColumnWithAttributes(-1,
                                       "Bar Plot",
                                       renderer,
                                       active = CATEGORICAL[["barplot"]])
  

  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(width = 60)
  renderer$setData("column", CATEGORICAL["dotplot"])
  connectSignal(renderer, "toggled", cat_toggled, categorical)
  cat.offset <-
    catview$insertColumnWithAttributes(-1,
                                       "Dot Plot",
                                       renderer,
                                       active = CATEGORICAL[["dotplot"]])
  
  ## Add the boxplot, hisplot, cumplot, benplot buttons

  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(width = 60)
  renderer$setData("column", CONTINUOUS["boxplot"])
  connectSignal(renderer, "toggled", con_toggled, continuous)
  con.offset <-
    conview$insertColumnWithAttributes(-1,
                                       "Box Plot",
                                       renderer,
                                       active = CONTINUOUS[["boxplot"]])
  
  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(width = 60)
  renderer$setData("column", CONTINUOUS["hisplot"])
  connectSignal(renderer, "toggled", con_toggled, continuous)
  con.offset <-
    conview$insertColumnWithAttributes(-1,
                                       "Histogram",
                                       renderer,
                                       active = CONTINUOUS[["hisplot"]])
  
  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(width = 60)
  renderer$setData("column", CONTINUOUS["cumplot"])
  connectSignal(renderer, "toggled", con_toggled, continuous)
  con.offset <-
    conview$insertColumnWithAttributes(-1,
                                       "Cumulative",
                                       renderer,
                                       active = CONTINUOUS[["cumplot"]])
  
  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(width = 60)
  renderer$setData("column", CONTINUOUS["benplot"])
  connectSignal(renderer, "toggled", con_toggled, continuous)
  con.offset <-
    conview$insertColumnWithAttributes(-1,
                                       "Benford",
                                       renderer,
                                       active = CONTINUOUS[["benplot"]])
  
  ## Add the COMMENT column.

  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  col.offset <-
    treeview$insertColumnWithAttributes(-1,
                                        "Comment",
                                        renderer,
                                        text = COLUMN[["comment"]])
  
  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  cat.offset <-
    catview$insertColumnWithAttributes(-1,
                                       "Levels",
                                       renderer,
                                       text = CATEGORICAL[["comment"]])

  renderer <- gtkCellRendererTextNew()
  renderer$set(xalign = 0.0)
  con.offset <-
    conview$insertColumnWithAttributes(-1,
                                       "Min; Median/Mean; Max",
                                       renderer,
                                       text = CONTINUOUS[["comment"]])

  ## Allow multiple selections.
  
  treeview$getSelection()$setMode("multiple")
  catview$getSelection()$setMode("multiple")
  conview$getSelection()$setMode("multiple")

}

createVariablesModel <- function(variables, input=NULL, target=NULL,
                                 risk=NULL, ident=NULL, ignore=NULL,
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
  
  model <- rattleWidget("variables_treeview")$getModel()
  categorical <- rattleWidget("categorical_treeview")$getModel()
  continuous  <- rattleWidget("continuous_treeview")$getModel()

  ## Identify a default target - the last or first if it's a factor,
  ## or has only a few values. Then the treeview model will record
  ## this choice, and we set the appropriate labels with this, and
  ## record it in crs.

  if (is.null(target))
  {
    target <- -1
    if ((is.factor(crs$dataset[,length(variables)]) &&
         length(levels(crs$dataset[,length(variables)])) > 1)
        || (length(levels(as.factor(crs$dataset[,length(variables)]))) < 5
            && length(levels(as.factor(crs$dataset[,length(variables)]))) > 1))
      target <- length(variables)
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

  rattleWidget("explot_target_label")$setText(the.target)

  rattleWidget("glm_target_label")$setText(the.target)
  rattleWidget("rpart_target_label")$setText(the.target)
  rattleWidget("gbm_target_label")$setText(the.target)
  rattleWidget("rf_target_label")$setText(the.target)
  rattleWidget("svm_target_label")$setText(the.target)

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
    if (cl == "factor")
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
      else if (is.element(sd(crs$dataset[[variables[i]]], na.rm=TRUE), c(NA, 0)))
      {
        ## sd is NA if all data items  are NA.
        cl <- "constant"
        ignore <- c(ignore, variables[i])
      }
    }
    input <- setdiff(setdiff(input, ignore), ident)
    
    model$set(iter,
              COLUMN["number"], i,
              COLUMN["variable"], variables[i],
              COLUMN["type"], cl,
              COLUMN["input"], is.element(variables[i], input),
              COLUMN["target"], is.element(variables[i], target),
              COLUMN["risk"], is.element(variables[i], risk),
              COLUMN["ident"], is.element(variables[i], ident),
              COLUMN["ignore"], is.element(variables[i], ignore))

    if (strsplit(cl, " ")[[1]][1] == "factor")
    {
      catiter <- categorical$append()$iter
      categorical$set(catiter,
                      CATEGORICAL["number"], i,
                      CATEGORICAL["variable"], variables[i],
                      CATEGORICAL["barplot"],is.element(variables[i],barplot),
                      CATEGORICAL["dotplot"],is.element(variables[i],dotplot),
                      CATEGORICAL["comment"],
                      sprintf("%s", strsplit(cl, " ")[[1]][2]))
    }
    if (cl == "integer" || cl == "numeric")
    {
      coniter <- continuous$append()$iter
      continuous$set(coniter,
                     CONTINUOUS["number"], i,
                     CONTINUOUS["variable"], variables[i],
                     CONTINUOUS["boxplot"],is.element(variables[i],boxplot),
                     CONTINUOUS["hisplot"],is.element(variables[i],hisplot),
                     CONTINUOUS["cumplot"],is.element(variables[i],cumplot),
                     CONTINUOUS["benplot"],is.element(variables[i],benplot),
                     CONTINUOUS["comment"],
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

  RF.MTRY.DEFAULT <<- floor(sqrt(ncol(crs$dataset)))
  rattleWidget("rf_mtry_spinbutton")$setValue(RF.MTRY.DEFAULT)
  RF.SAMPSIZE.DEFAULT <<- nrow(crs$dataset)
  rattleWidget("rf_sampsize_spinbutton")$setValue(RF.SAMPSIZE.DEFAULT)
}

########################################################################
##
## SAMPLE TAB
##

##----------------------------------------------------------------------
##
## Interface Actions
##
on_sample_checkbutton_toggled <- function(button)
{
  if (button$getActive())
  {
    rattleWidget("sample_percentage_spinbutton")$setSensitive(TRUE)
    rattleWidget("sample_percentage_label")$setSensitive(TRUE)
    rattleWidget("sample_count_spinbutton")$setSensitive(TRUE)
    rattleWidget("sample_count_label")$setSensitive(TRUE)
    rattleWidget("sample_seed_spinbutton")$setSensitive(TRUE)
    rattleWidget("sample_seed_label")$setSensitive(TRUE)
    rattleWidget("explore_sample_checkbutton")$setSensitive(TRUE)
    crs$sample <<- NULL ## Only reset when made active to ensure Execute needed
  }
  else
  {
    rattleWidget("sample_percentage_spinbutton")$setSensitive(FALSE)
    rattleWidget("sample_percentage_label")$setSensitive(FALSE)
    rattleWidget("sample_count_spinbutton")$setSensitive(FALSE)
    rattleWidget("sample_count_label")$setSensitive(FALSE)
    rattleWidget("sample_seed_spinbutton")$setSensitive(FALSE)
    rattleWidget("sample_seed_label")$setSensitive(FALSE)
    rattleWidget("explore_sample_checkbutton")$setActive(FALSE)
    rattleWidget("explore_sample_checkbutton")$setSensitive(FALSE)
  }
    setStatusBar()
}

on_sample_percentage_spinbutton_changed <- function(action, window)
{
  if (is.null(crs$dataset)) return()
  per <- rattleWidget("sample_percentage_spinbutton")$getValue()
  rows <- round(nrow(crs$dataset) * per / 100)
  crows <- rattleWidget("sample_count_spinbutton")$getValue()
  if (rows != crows)
    rattleWidget("sample_count_spinbutton")$setValue(rows)
  setStatusBar()
}

on_sample_count_spinbutton_changed <- function(action, window)
{
  if (is.null(crs$dataset)) return()
  rows <- rattleWidget("sample_count_spinbutton")$getValue()
  per <- round(100*rows/nrow(crs$dataset))
  cper <- rattleWidget("sample_percentage_spinbutton")$getValue()
  if (per != cper)
    rattleWidget("sample_percentage_spinbutton")$setValue(per)
  setStatusBar()
}

##----------------------------------------------------------------------
##
## Execution
##
executeSampleTab <- function()
{
  ## Can not do any sampling if there is no dataset.

  if (noDatasetLoaded()) return()

  ## Record that a random sample of the dataset is desired.

  if (rattleWidget("sample_checkbutton")$getActive())
  {
    #ssize <- rattleWidget("sample_percentage_spinbutton")$getValue()
    #ssize <- floor(nrow(crs$dataset)*ssize/100)
    ssize <- rattleWidget("sample_count_spinbutton")$getValue()

    seed <- rattleWidget("sample_seed_spinbutton")$getValue()
    
    sample.cmd <- paste(sprintf("set.seed(%d)\n", seed),
                        "crs$sample <<- sample(nrow(crs$dataset), ", ssize,
                        ")", sep="")

    addToLog("Build random sample for modelling.",
            gsub("<<-", "<-", sample.cmd))
    eval(parse(text=sample.cmd))

    ## When we have sampling, assume the remainder is the test set and
    ## so enable the Testing radio button in Evaluate.
    
    rattleWidget("evaluate_testing_radiobutton")$setSensitive(TRUE)
    rattleWidget("evaluate_testing_radiobutton")$setActive(TRUE)
  }
  else
  {
    crs$sample <<- NULL

    rattleWidget("evaluate_testing_radiobutton")$setSensitive(FALSE)
    rattleWidget("evaluate_training_radiobutton")$setActive(TRUE)
  }
  
  crs$smodel <<- vector()

  ## TODO For test/train, use sample,split from caTools?

  ## Set some defaults that depend on sample size.
  
  if (is.null(crs$sample))
    RF.SAMPSIZE.DEFAULT <<- length(crs$dataset)
  else
    RF.SAMPSIZE.DEFAULT <<- length(crs$sample)
  rattleWidget("rf_sampsize_spinbutton")$setValue(RF.SAMPSIZE.DEFAULT)
  

  setStatusBar()

  if (rattleWidget("sample_checkbutton")$getActive())
    setStatusBar("The sample has been generated.",
                  "There are", length(crs$sample), "entities.")
  else
    setStatusBar("Sampling is inactive.")
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
  if (button$getActive()) EXPLORE$setCurrentPage(EXPLORE.SUMMARY.TAB)
  setStatusBar()
}

on_explot_radiobutton_toggled <- function(button)
{
  barbutton <- rattleWidget("benford_bars_checkbutton")
  if (button$getActive()) 
  {
    EXPLORE$setCurrentPage(EXPLORE.PLOT.TAB)
    barbutton$show()
  }
  else
  {
    barbutton$hide()
  }
  setStatusBar()
}

on_ggobi_radiobutton_toggled <- function(button)
{
  if (button$getActive()) EXPLORE$setCurrentPage(EXPLORE.GGOBI.TAB)
  setStatusBar()
}

on_correlation_radiobutton_toggled <- function(button)
{
  nabutton <- rattleWidget("correlation_na_checkbutton")
  if (button$getActive()) 
  {
    EXPLORE$setCurrentPage(EXPLORE.CORRELATION.TAB)
    nabutton$show()
  }
  else
  {
    nabutton$hide()
  }
  setStatusBar()
}

on_hiercor_radiobutton_toggled <- function(button)
{
  if (button$getActive()) EXPLORE$setCurrentPage(EXPLORE.HIERCOR.TAB)
  setStatusBar()
}

on_prcomp_radiobutton_toggled <- function(button)
{
  if (button$getActive()) EXPLORE$setCurrentPage(EXPLORE.PRCOMP.TAB)
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

  tree.selection <- rattleWidget("categorical_treeview")$getSelection()

  tree.selection$selectedForeach(function(model, path, iter)
  {
    columns <- CATEGORICAL[["barplot"]]:CATEGORICAL[["dotplot"]]
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

  tree.selection <- rattleWidget("continuous_treeview")$getSelection()

  tree.selection$selectedForeach(function(model, path, iter)
  {
    columns <- CONTINUOUS[["boxplot"]]:CONTINUOUS[["benplot"]]
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

  use.sample <- rattleWidget("explore_sample_checkbutton")$getActive()
  sampling <- rattleWidget("sample_checkbutton")$getActive()
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
  vars <- input.variables(numonly=TRUE)
  nidataset <- sprintf("%s[%s,%s]", "crs$dataset",
                       ifelse(use.sample & sampling,"crs$sample", ""),
                       ifelse(is.null(vars),"",vars))
  
  ## Dispatch
  
  if (rattleWidget("summary_radiobutton")$getActive())
    execute.explore.summary(dataset)
  else if (rattleWidget("explot_radiobutton")$getActive())
    executeExplorePlot(avdataset)
  else if (rattleWidget("ggobi_radiobutton")$getActive())
    executeExploreGGobi(dataset)
  else if (rattleWidget("correlation_radiobutton")$getActive())
  {
    if (rattleWidget("correlation_na_checkbutton")$getActive())
      executeExploreCorrelation(dataset)
    else
      executeExploreCorrelation(ndataset)
  }
  else if (rattleWidget("hiercor_radiobutton")$getActive())
    execute.explore.hiercor(ndataset)
  else if (rattleWidget("prcomp_radiobutton")$getActive())
    executeExplorePrcomp(nidataset)
}

execute.explore.summary <- function(dataset)
{
  TV <- "summary_textview"
  
  ## Construct the command.

  ## First, a basic summary command
  
  summary.cmd <- sprintf("summary(%s)", dataset)

  ## Now some additional information: kurtosis, skewness, and sum.
  ## Use the functions from fBasics. Different functions available in
  ## the packages e1071, fBasics, moments.

  ## We check for just those colums of the dataset which are numeric.

  nvars <- simplify.number.list(eval(parse(text=sprintf(paste("seq(1,ncol(%s))",
                                         "[as.logical(sapply(%s, is.numeric))]",
                                             sep=""),
                                             dataset, dataset))))

  library.cmd <- "require(fBasics, quietly=TRUE)"
  kurtosis.cmd <- sprintf("kurtosis(%s[,%s], na.rm=TRUE)", dataset,
                          ifelse(is.null(nvars), "", nvars))
  skewness.cmd <- sprintf("skewness(%s[,%s], na.rm=TRUE)", dataset,
                          ifelse(is.null(nvars), "", nvars))
  
  basicstats.cmd <- sprintf("lapply(%s[,%s], basicStats)", dataset,
                          ifelse(is.null(nvars), "", nvars))
                            
  sum.cmd <- sprintf("unlist(lapply(%s[,%s],function(x){sum(as.numeric(x), na.rm=TRUE)}))",
                     dataset, ifelse(is.null(nvars), "", nvars))
  
  ## Start logging and executing the R code.
  
  addLogSeparator()
  clearTextview(TV)

  addToLog("Generate a summary of the dataset.", summary.cmd)
  use.sample <- rattleWidget("explore_sample_checkbutton")$getActive()
  sampling  <- ! is.null(crs$sample)
  appendTextview(TV, paste("Summary of the",
                            ifelse(use.sample & sampling, "** sample **", "full"),
                            "dataset.\n\n",
                            "(Hint: 25% of values are below 1st Quartile.)",
                            "\n\n"),
                  collect.output(summary.cmd, TRUE))

  addToLog("Additional information using fBasics package.", library.cmd)
  addToLog("Kurtosis: Only suitable for numeric data.", kurtosis.cmd)
  addToLog("Skewness: Only suitable for numeric data.", skewness.cmd)
  addToLog("Sum: Only suitable for numeric data.", sum.cmd)
  addToLog("Basic Statustics: Only suitable for numeric data.", basicstats.cmd)
                            
  if (! packageIsAvailable("fBasics", "calculate measures of skew and shape"))
    return()

  eval(parse(text=library.cmd))
  appendTextview(TV, paste("Kurtosis for numeric data:",
                           "Larger means sharper peak, flatter tails.\n\n"),
                 collect.output(kurtosis.cmd, TRUE),
                 paste("\n\nSkewness for numeric data:",
                       "Positive means the right tail is longer.\n\n"),
                 collect.output(skewness.cmd, TRUE),
                 paste("\n\nSum of each numeric column\n\n"),
                 collect.output(sum.cmd, TRUE),
                 paste("\n\nBasic stats for each numeric column\n\n"),
                 collect.output(basicstats.cmd, TRUE))

  ## Report completion to the user through the Status Bar.
  
  setStatusBar("Data summary generated.")
}

getVariableIndicies <- function(variables)
{
  indicies <- NULL
  if (! is.null(variables))
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

  pmax <- rattleWidget("plots_per_page_spinbutton")$getValue()
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
  
  use.sample <- rattleWidget("explore_sample_checkbutton")$getActive()
  sampling  <- use.sample & ! is.null(crs$sample)

  ## Split the data, first for all values.

  bindCmd <- sprintf('rbind(data.frame(dat=%s[,"%%s"], grp="All")', dataset)

  if (! is.null(targets))
  {
    for (i in 1:length(targets))
    {
      bindCmd <- sprintf("%s,\n            data.frame(dat=%s",
                         bindCmd, dataset)

      bindCmd <- sprintf('%s[crs$dataset%s$%s=="%s","%%s"], grp="%s")',
                         bindCmd,
                         ifelse(sampling, "[crs$sample,]", ""),
                         target, targets[i], targets[i])
    }
  }
  
  ## Finish off the command to create the dataset for plotting.
  
  bindCmd <- sprintf("%s)", bindCmd)

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
  ## this approach rather than using bindCmd.

  genericDataSet <- data.frame(All=sprintf('%s$%%s', dataset))
  if (! is.null(targets))
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

    plotCmd <- paste('boxplot(dat ~ grp, ds,',
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

    meanCmd <- paste(sprintf("points(1:%d,", length(targets)+1),
                     "summaryBy(dat ~ grp, data=ds, FUN=mean)$mean.dat,",
                     "pch=8)")
    
    for (s in 1:nboxplots)
    {

      addLogSeparator()

      cmd <- paste("sprintf(bindCmd,",
                   paste(paste('"', rep(boxplots[s], length(targets)+1), '"',
                               sep=""),
                         collapse=","),
                   ")")
      cmd <- eval(parse(text=cmd))
      addToLog(paste("Generate just the data for a boxplot of",
                    boxplots[s], "."),
              paste("ds <-", cmd))
      ds <- eval(parse(text=cmd))

      if (pcnt %% pmax == 0) newPlot(pmax)
      pcnt <- pcnt + 1
      
      addToLog("Plot the data, grouped appropriately.", plotCmd)
      eval(parse(text=plotCmd))

      if (packageIsAvailable("doBy", "add means to box plots"))
      {
        addToLog("Use the doBy package to group the data for means.",
                 doByLibrary)
        eval(parse(text=doByLibrary))

        addToLog("Calculate the group means.", meanCmd)
        eval(parse(text=meanCmd))
      }
        
      ## Add a value for the mean to each, as in DMSurvivorP196.
      titleCmd <- genPlotTitleCmd(sprintf("Distribution of %s%s",
                                          boxplots[s],
                                          ifelse(sampling, " (sample)","")))
      addToLog("Add a title to the plot.", titleCmd)
      eval(parse(text=titleCmd))
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
    rugCmd <- 'rug(ds[ds$grp=="All",1])'

    for (s in 1:nhisplots)
    {
      addLogSeparator()

      cmd <- paste("sprintf(bindCmd,",
                   paste(paste('"', rep(hisplots[s], length(targets)+1), '"',
                               sep=""),
                         collapse=","),
                   ")")
      cmd <- eval(parse(text=cmd))
      addToLog(paste("Generate just the data for a histogram of",
                    hisplots[s], "."),
              paste("ds <-", cmd))
      ds <- eval(parse(text=cmd))

      if (pcnt %% pmax == 0) newPlot(pmax)
      pcnt <- pcnt + 1
      
      addToLog("Plot the data.", plot.cmd)
      eval(parse(text=plot.cmd))
      addToLog("Add a rug to illustrate density.", rugCmd)
      eval(parse(text=rugCmd))
      title.cmd <- genPlotTitleCmd(sprintf("Distribution of %s%s",
                                           hisplots[s],
                                           ifelse(sampling, " (sample)","")))
      addToLog("Add a title to the plot.", title.cmd)
      eval(parse(text=title.cmd))
    }
  }

  ##---------------------------------------------------------------------
  
  if (! is.null(cumplots))
  {
    ## Cumulative plot for numeric data.

    nplots <- length(cumplots)

    libraryCmd <- "require(Hmisc, quietly=TRUE)"
    
    for (s in 1:nplots)
    {
      addLogSeparator()

      col <- rainbow(length(targets)+1)
      plotCmd <- paste('ecdf(ds[ds$grp=="All",1],',
                       sprintf('col="%s",', col[1]),
                       'xlab="",',
                       'subtitles=FALSE)\n')
      if (! is.null(targets))
      for (t in 1:length(targets))
      {
        plotCmd <- paste(plotCmd,
                         sprintf('ecdf(ds[ds$grp=="%s",1], ', targets[t]),
                         sprintf('col="%s", lty=%d, ', col[t+1], t+1),
                         'xlab="", subtitles=FALSE, add=TRUE)\n',
                         sep="")
      }

      if (! is.null(targets))
        legendCmd <- sprintf(paste('legend("bottomright", c(%s), ',
                                   "col=rainbow(%d), lty=1:%d,",
                                   'title="%s", inset=c(0.05,0.05))'),
                             paste(sprintf('"%s"', c("All", targets)),
                                   collapse=","),
                             length(targets)+1, length(targets)+1,
                             target)
        
      cmd <- paste("sprintf(bindCmd,",
                    paste(paste('"', rep(cumplots[s], length(targets)+1), '"',
                               sep=""),
                         collapse=","),
                   ")")
      cmd <- eval(parse(text=cmd))
      addToLog(paste("Generate just the data for an ecdf plot of",
                    cumplots[s], "."),
              paste("ds <-", cmd))
       ds <- eval(parse(text=cmd))

      if (pcnt %% pmax == 0) newPlot(pmax)
      pcnt <- pcnt + 1

      if (! packageIsAvailable("Hmisc", "plot cumulative charts")) break()

      addToLog("Used ecdf from the Hmisc package.", libraryCmd)
      eval(parse(text=libraryCmd))

      addToLog("Plot the data.", plotCmd)
      eval(parse(text=plotCmd))
      title.cmd <- genPlotTitleCmd(sprintf("Cumulative %s%s",
                                           cumplots[s],
                                           ifelse(sampling, " (sample)","")))

      if (! is.null(targets))
      {
        addToLog("Add a legend to the plot.", legendCmd)
        eval(parse(text=legendCmd))
      }

      addToLog("Add a title to the plot.", title.cmd)
      eval(parse(text=title.cmd))
    }
  }

  ##---------------------------------------------------------------------

  if (nbenplots > 0)
  {
    ## Plot Benford's Law for numeric data.

    barbutton <- rattleWidget("benford_bars_checkbutton")$getActive()
    
    ## Using barplot2 from gplots
    
    libraryCmd <- "require(gplots, quietly=TRUE)"

    ## Calculate the expected distribution according to Benford's Law
    
    expectCmd <- paste('unlist(lapply(1:9, function(x) log10(1 + 1/x)))')

    ## Construct the command to plot the distribution.

    if (barbutton)
    {
      plotCmd <- paste('barplot2(ds, beside=TRUE,',
                       'xlab="Initial Digit", ylab="Probability")')
    }
    else
    {
      plotCmd <- paste('plot(1:9, ds[1,], type="b", pch=19, col=rainbow(1), ',
                       'ylim=c(0,max(ds)), axes=FALSE, ',
                       'xlab="Initial Digit", ylab="Probability")\n',
                       'axis(1, at=1:9)\n', 'axis(2)\n',
                       sprintf('points(1:9, ds[2,], col=%s, pch=19, type="b")\n',
                               ifelse(is.null(target), "rainbow(2)[2]",
                                      sprintf("rainbow(%d)[2]",
                                              length(targets)+2))),
                       sep="")
      if (! is.null(targets))
        for (i in 1:length(targets))
        {
          plotCmd <- sprintf(paste('%s\npoints(1:9, ds[%d,],',
                                   'col=%s, pch=%d, type="b")'),
                             plotCmd, i+2,
                             sprintf("rainbow(%d)[%d]",
                                     length(targets)+2, i+2),
                             19)
        }
    }
    if (packageIsAvailable("gplots", "plot a bar chart for Benford's Law"))
    {
      addLogSeparator()

      addToLog("Use barplot2 from gplots to plot Benford's Law.", libraryCmd)
      eval(parse(text=libraryCmd))
      
      addToLog("Generate the expected distribution for Benford's Law",
               paste("expect <-", expectCmd))
      expect <- eval(parse(text=expectCmd))

      if (is.null(targets) && ! barbutton)
      {
        # Plot all Benford's plots on the one line graph

        addLogSeparator()

        bc <- sub("All", "%s", substr(bindCmd, 7, nchar(bindCmd)-1))
        newBindCmd <- substr(bindCmd, 1, 6)
        dataCmd <- 't(as.matrix(data.frame(expect=expect'
        plotCmd <- paste('plot(1:9, ds[1,], type="b", ',
                         'pch=19, col=rainbow(1), ',
                         'ylim=c(0,max(ds)), axes=FALSE, ',
                         'xlab="Initial Digit", ylab="Probability")\n',
                         'axis(1, at=1:9)\n', 'axis(2)\n',
                         sep="")
        for (s in 1:nbenplots)
        {
          newBindCmd <- paste(newBindCmd, 
                           sprintf(bc, benplots[s], benplots[s]),
                           ",\n     ",
                           sep="")
          dataCmd <- paste(dataCmd, ",\n     ",
                           sprintf(paste('"%s"=calcInitialDigitDistr',
                                         '(ds[ds$grp=="%s", 1])', sep=""),
                                   benplots[s], benplots[s]),
                           sep="")
          plotCmd <- paste(plotCmd,
                           sprintf(paste('points(1:9, ds[%d,],',
                                         'col=%s, pch=19, type="b")\n'),
                                   s+1, sprintf("rainbow(%d)[%d]",
                                                nbenplots+1, s+1)),
                           sep="")
        }
        newBindCmd <- paste(substr(newBindCmd, 1, nchar(newBindCmd)-7), ")",
                            sep="")
        dataCmd <- paste(dataCmd, ")))", sep="")

        legendCmd <- sprintf(paste('legend("topright", c(%s), ',
                                   'fill=rainbow(%d), title="%s")'),
                             paste(sprintf('"%s"',
                                           c("Benford", benplots)),
                                   collapse=","),
                             nbenplots+1, "Variables")

        addToLog("Generate the required data.",
                 paste("ds <-", newBindCmd))
        ds <- eval(parse(text=newBindCmd))

        addToLog("Generate specific plot data.", paste("ds <-", dataCmd))
        ds <- eval(parse(text=dataCmd))

        if (pcnt %% pmax == 0) newPlot(pmax)
        pcnt <- pcnt + 1

        par(xpd=TRUE)
        
        addToLog("Now do the actual plot.", plotCmd)
        eval(parse(text=plotCmd))

        addToLog("Add a legend to the plot.", legendCmd)
        eval(parse(text=legendCmd))
        
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

          dataCmd <- paste('t(as.matrix(data.frame(expect=expect,\n    ',
                           'All=calcInitialDigitDistr(ds[ds$grp=="All", 1])')
        
          if (! is.null(targets))
            for (t in 1:length(targets))
              dataCmd <- paste(dataCmd, ",\n     ",
                               sprintf('"%s"=', targets[t]),
                               'calcInitialDigitDistr(ds[ds$grp==',
                               sprintf('"%s", ', targets[t]), '1])',
                               sep="")
          dataCmd <- paste(dataCmd, ")))", sep="")

          if (! is.null(targets))
            if (barbutton)
              legendCmd <- sprintf(paste('legend("topright", c(%s), ',
                                         'fill=heat.colors(%d), title="%s")'),
                                   paste(sprintf('"%s"',
                                                 c("Benford", "All", targets)),
                                         collapse=","),
                                   length(targets)+2, target)
            else
              legendCmd <- sprintf(paste('legend("topright", c(%s), ',
                                         'fill=rainbow(%d), title="%s")'),
                                   paste(sprintf('"%s"',
                                                 c("Benford", "All", targets)),
                                         collapse=","),
                                   length(targets)+2, target)
          else
            if (barbutton)
              legendCmd <- paste('legend("topright", c("Benford", "All"),',
                                 'fill=heat.colors(2))')
            else
              legendCmd <- paste('legend("topright", c("Benford", "All"), ',
                                 'fill=rainbow(2))')
          
          cmd <- paste("sprintf(bindCmd,",
                       paste(paste('"', rep(benplots[s], length(targets)+1),
                                   '"', sep=""), collapse=","),
                       ")")
          cmd <- eval(parse(text=cmd))
          
          addToLog(paste("Generate just the data for the plot of",
                         benplots[s], "."),
                   paste("ds <-", cmd))
          ds <- eval(parse(text=cmd))
          
          addToLog("Generate specific plot data.", paste("ds <-", dataCmd))
          ds <- eval(parse(text=dataCmd))
          
          if (pcnt %% pmax == 0) newPlot(pmax)
          pcnt <- pcnt + 1

          par(xpd=TRUE)
          
          addToLog("Now do the actual plot.", plotCmd)
          eval(parse(text=plotCmd))
          
          addToLog("Add a legend to the plot.", legendCmd)
          eval(parse(text=legendCmd))
          
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
    
    libraryCmd <- "require(gplots, quietly=TRUE)"

    ## Construct a generic data command built using the genericDataSet
    ## values. To generate a barplot we use the output of the summary
    ## command on each element in the genericDataSet, and bring them
    ## together into a single structure. The resulting genericDataCmd
    ## will have a number of "%s"s (one for the whole dataset, then
    ## one for each level) from the original genericDataSet string
    ## that will be replaced with the name of each variable as it is
    ## being plotted.

    genericDataCmd <- paste(lapply(genericDataSet,
                                   function(x) sprintf("summary(%s)", x)),
                            collapse=",\n    ")
    genericDataCmd <- sprintf("rbind(%s)", genericDataCmd)

    ## If the gplots package is available then generate a plot for
    ## each chosen vairable.
    
    if (packageIsAvailable("gplots", "plot a bar chart"))
    {
      addLogSeparator()
      addToLog("Use barplot2 from gplots for the barchart.", libraryCmd)
      eval(parse(text=libraryCmd))

      for (s in 1:nbarplots)
      {

        addLogSeparator()

        ## Construct and evaluate a command string to generate the
        ## data for the plot.

        dsCmd <- paste(sprintf("sprintf('%s',", genericDataCmd),
                       paste(paste('"', rep(barplots[s], length(targets)+1),
                                 '"', sep=""), collapse=","), ")")
        dsCmd <- eval(parse(text=dsCmd))
        addToLog("Generate the summary data for plotting.",
                 paste("ds <-", dsCmd))
        ds <- eval(parse(text=dsCmd))
        
        ## Construct and evaluate the command to plot the
        ## distribution.  Determine maxium value so that the y axis
        ## can extend to it. We save the output from barplot2 in order
        ## to add numbers to the plot.
    
        if (pcnt %% pmax == 0) newPlot(pmax)
        pcnt <- pcnt + 1

        #if (is.null(target))
        #  ordCmd <- 'order(ds[1,])'
        #else
          ordCmd <- 'order(ds[1,], decreasing=TRUE)'
        addToLog("Sort the entries.", paste("ord <-", ordCmd))
        ord <- eval(parse(text=ordCmd))

        maxFreq <- max(ds)
        plotCmd <- sprintf(paste('barplot2(ds[,ord], beside=TRUE,',
                                 'ylim=c(0, %d), col=rainbow(%d))'),
                           round(maxFreq+maxFreq*0.20), length(targets)+1)
        addToLog("Plot the data.", paste("bp <- ", plotCmd))
        bp <- eval(parse(text=plotCmd))

        ## Construct and evaluate a command to add text to the top of
        ## the bars in the bar chart. Only do this if there are not
        ## too many values for the category, otherwise the numbers
        ## look bad. I could, alternatively, scale the font?

        if (ncol(bp) <= 5)
        {
          textCmd <- sprintf("text(bp, ds[,ord]+%d, ds[,ord])",
                             round(maxFreq*0.040))
          addToLog("Add the actual frequencies.", textCmd)
          eval(parse(text=textCmd))
        }

        ## Construct and evaluate a command to add a legend to the
        ## plot, but only if there is a target, optherwise it is
        ## obvious.
        
        if (! is.null(targets))
        {
          legendCmd <- sprintf(paste('legend("topright", c(%s), ',
                                     "fill=rainbow(%d), ",
                                     'title="%s")'),
                               paste(sprintf('"%s"', c("All", targets)),
                                     collapse=","),
                               length(targets)+1,
                               target)
          addToLog("Add a legend to the plot.", legendCmd)
          eval(parse(text=legendCmd))
        }
        
        ## Construct and evaluate a command to add the title to the
        ## plot.
        
        titleCmd <- genPlotTitleCmd(sprintf("Distribution of %s%s",
                                            barplots[s],
                                            ifelse(sampling," (sample)","")))
        addToLog("Add a title to the plot.", titleCmd)
        eval(parse(text=titleCmd))
      }
    }
  }

  ##---------------------------------------------------------------------

  if (ndotplots > 0)
  {
    
    ## Construct a generic data command built using the genericDataSet
    ## values. To generate a barplot we use the output of the summary
    ## command on each element in the genericDataSet, and bring them
    ## together into a single structure. The resulting genericDataCmd
    ## will have a number of "%s"s (one for the whole dataset, then
    ## one for each level) from the original genericDataSet string
    ## that will be replaced with the name of each variable as it is
    ## being plotted.

    genericDataCmd <- paste(lapply(genericDataSet,
                                   function(x) sprintf("summary(%s)", x)),
                            collapse=",\n    ")
    genericDataCmd <- sprintf("rbind(%s)", genericDataCmd)

      
    addToLog("Use dotplot from lattice for the plots.", libraryCmd)
    eval(parse(text=libraryCmd))

    for (s in 1:ndotplots)
    {

      addLogSeparator()

      ## Construct and evaluate a command string to generate the
      ## data for the plot.

      dsCmd <- paste(sprintf("sprintf('%s',", genericDataCmd),
                     paste(paste('"', rep(dotplots[s], length(targets)+1),
                                 '"', sep=""), collapse=","), ")")
      dsCmd <- eval(parse(text=dsCmd))
      addToLog("Generate the summary data for plotting.",
               paste("ds <-", dsCmd))
      ds <- eval(parse(text=dsCmd))

      ## Construct and evaluate the command to determin the order in
      ## which to print the catgories, from larges to smallest.

      if (is.null(target))
        ordCmd <- 'order(ds[1,])'
      else
        ordCmd <- 'order(ds[1,], decreasing=TRUE)'
      addToLog("Sort the entries.",
               paste("ord <-", ordCmd))
      ord <- eval(parse(text=ordCmd))
        
      ## Construct and evaluate the command to plot the
      ## distribution.
    
      if (pcnt %% pmax == 0) newPlot(pmax)
      pcnt <- pcnt + 1
      
      titles <- genPlotTitleCmd(sprintf("Distribution of %s%s",
                                        dotplots[s],
                                        ifelse(sampling," (sample)","")),
                                vector=TRUE)

      plotCmd <- sprintf(paste('dotchart(%s, main="%s", sub="%s",',
                               'col=rainbow(%d),%s',
                               'xlab="Frequency", pch=19)'),
                         "ds[,ord]", titles[1], titles[2], length(targets)+1,
                         ifelse(is.null(target), "", ' labels="",'))
      addToLog("Plot the data.", plotCmd)
      eval(parse(text=plotCmd))

      if (! is.null(target))
      {
        legendCmd <- sprintf(paste('legend("bottomright", bg="white",',
                                   'c("All","0","1"), col=rainbow(%d),',
                                   'pch=19, title="%s")'),
                             length(targets)+1, target)
        addToLog("Add a legend.", legendCmd)
        eval(parse(text=legendCmd))
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

  library.cmd <- "require(rggobi, quietly=TRUE)"
  ggobi.cmd <- sprintf('gg <<- ggobi(%s)', dataset)
              
  ## Start logging and executing the R code.
  
  if (! packageIsAvailable("rggobi","explore the data using GGobi")) return()

  addLogSeparator()
  addToLog("GGobi is accessed using the rggobi package.", library.cmd)
  eval(parse(text=library.cmd))
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
  
  nas <- rattleWidget("correlation_na_checkbutton")$getActive()
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

  library.cmd <-"require(ellipse, quietly=TRUE)"
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

  addToLog("The correlation plot uses the ellipse package.", library.cmd)
  eval(parse(text=library.cmd))

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
               collect.output(paste(crscor.cmd,
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

execute.explore.hiercor <- function(dataset)
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
  appendTextview(TV, collect.output(print.cmd, TRUE))
  
  addToLog("Summarise the importance of the components found.", summary.cmd)
  appendTextview(TV, collect.output(summary.cmd, TRUE))

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
## CLUSTER TAB
##

##----------------------------------------------------------------------
##
## Interface Actions
##

## When radio button is selected, display appropriate tab page

on_kmeans_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    CLUSTER$setCurrentPage(CLUSTER.KMEANS.TAB)
  setStatusBar()
}

on_hclust_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    CLUSTER$setCurrentPage(CLUSTER.HCLUST.TAB)
  setStatusBar()
}

##----------------------------------------------------------------------
##
## Execution
##
execute.cluster.tab <- function()
{
  ## Can not cluster without a dataset.

  if (noDatasetLoaded()) return()

  ## If it looks like the VARIABLES page has not been executed, complain..

  if (length(crs$input) != length(getSelectedVariables("input")) ||
      length(crs$ident) != length(getSelectedVariables("ident")) ||
      length(crs$ignore) != length(getSelectedVariables("ignore")))
  {
    errorDialog("You seem to have changed some selections in the",
                 "Variables tab, but have not Executed the Variables tab.",
                 "Please do so before clustering.")
    return()
  }

  ## Check if sampling needs executing.

  if (sampleNeedsExecute()) return()
    
  ## Kmeans only works for numeric data, so identify variables to
  ## include.  Only work with the INPUT/TARGET/RISK
  ## variables. That is, only exclude the IGNORE variables.

  nums <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.numeric))]
  if (length(nums) > 0)
  {
    indicies <- getVariableIndicies(crs$input)
    include <- simplify.number.list(intersect(nums, indicies))
  }
  
  if (length(nums) == 0 || length(indicies) == 0)
  {
    errorDialog("Clusters are currently calculated only for numeric data.",
                 "No numeric variables were found in the dataset",
                 "from amongst those having an input role.")
    return()
  }

  ## Dispatch

  if (rattleWidget("kmeans_radiobutton")$getActive())
    execute.cluster.kmeans(include, length(intersect(nums, indicies))>1)
  else if (rattleWidget("hclust_radiobutton")$getActive())
    execute.cluster.hclust(include)
}

##----------------------------------------------------------------------
##
## KMEANS
##

execute.cluster.kmeans <- function(include, doPlot=TRUE)
{

  sampling  <- ! is.null(crs$sample)

  nclust <- rattleWidget("kmeans_clusters_spinbutton")$getValue()
  
  kmeans.cmd <- sprintf('crs$kmeans <<- kmeans(crs$dataset[%s,%s], %d)',
                        ifelse(sampling, "crs$sample", ""),
                        include, nclust)

  if (packageIsAvailable("fpc","plot discriminant coordinates charts"))
  {
    plot.cmd <- paste("require(fpc, quietly=TRUE)\n",
                      sprintf("plotcluster(crs$dataset[%s,%s], ",
                              ifelse(sampling, "crs$sample", ""),
                              include),
                      "crs$kmeans$cluster)\n",
                      genPlotTitleCmd("Discriminant Coordinates",
                                     crs$dataname),
                      sep="")
  }
  ## Log the R commands and execute them.

  addLogSeparator()
  addToLog("Generate a kmeans cluster of size 10.",
          gsub("<<-", "<-", kmeans.cmd))
  eval(parse(text=kmeans.cmd))

  ## Show the resulting model.

  clearTextview("kmeans_textview")
  appendTextview("kmeans_textview",
                 "Cluster Sizes\n\n",
                 collect.output("paste(crs$kmeans$size, collapse=' ')", TRUE),
                 "\n\n",
                 "Cluster centroids.\n\n",
                 collect.output("crs$kmeans$centers", TRUE),
                 textviewSeparator())

  if (doPlot && packageIsAvailable("fpc"))
  {
    addToLog("Generate a discriminate coordinates plot using the fpc package.",
            plot.cmd)
    newPlot()
    eval(parse(text=plot.cmd))
  }

  setStatusBar("K Means cluster has been generated.")
  
}

##----------------------------------------------------------------------
##
## HCLUST
##

execute.cluster.hclust <- function(include)
{

  TV <- "hclust_textview"
  
  ## TODO : If data is larg put up a question about wanting to continue?
  
  library.cmd <- "require(cba, quietly=TRUE)"

  sampling  <- ! is.null(crs$sample)

  hclust.cmd <- paste("crs$hclust <<- ",
                      sprintf('hclust(dist(crs$dataset[%s,%s]), "%s")',
                              ifelse(sampling, "crs$sample", ""),
                              include, "ave"),
                      sep="")

  plot.cmd <- paste("plot(as.dendrogram(crs$hclust))\n",
                    genPlotTitleCmd("Discriminant Coordinates",
                                     crs$dataname),
                      sep="")

##   seriation.cmd <- paste("d <- dist(as.matrix(crs$dataset",
##                          sprintf("[%s,%s]",
##                                  ifelse(sampling, "crs$sample", ""),
##                                  include),
##                          "))\n",
##                          "l <- pam(d, 10, cluster.only = TRUE)\n",
##                          "res <- cluproxplot(d, l, method = ",
##                          'c("Optimal", "Optimal"), plot = FALSE)\n',
##                          'plot(res, plotOptions = list(main = "PAM + ',
##                          'Seriation (Optimal Leaf ordering)", ',
##                          'col = terrain.colors(64)))', sep="")

  ## Log the R command

  addLogSeparator()
  addToLog("Generate a hierarchical cluster of the data.",
          gsub("<<-", "<-", hclust.cmd))
  
  ## Perform the commands.

  result <- try(eval(parse(text=hclust.cmd)), silent=TRUE)
  if (inherits(result, "try-error"))
  {
    if (any(grep("cannot allocate vector", result)))
    {
      errorDialog("The call to hclust appears to have failed.",
                   "This is often due, as in this case,",
                   "to running out of memory",
                   "as hclust is rather memory hungry.",
                   "A quick solution is to sample the dataset, through the",
                   "Sample tab. On 32 bit machines you may be limited to",
                   "less than 2000 entities.")
      setTextview(TV)
    }
    else
      errorDialog("The call to hclust appears to have failed.",
                   "The error message was:", result,
                   "I am not familiar with this error, and you may",
                   "want to report it to the Rattle author",
                   "at Graham.Williams@togaware.com")
    return()
  }

##  addToLog("Plot the Hierarchical Dedogram.", plot.cmd)
##   if (packageIsAvailable("cba", "plot seriation charts"))
##   {
##     addToLog("We use the cba package for seriation.", library.cmd)
##     addToLog("Plot the Seriation (Experimental).", seriation.cmd)
##   }

  clearTextview(TV)
  appendTextview(TV,
                  "Hiearchical Cluster\n\n",
                  collect.output("crs$hclust", TRUE),
                  eval(parse(text=plot.cmd)))

## VERY SLOW - PERHAPS NEED A CHECKBOX TO TURN IT ON
##
##   if (packageIsAvailable("cba"))
##   {
##     eval(parse(text=library.cmd))
##     newPlot()
##     appendTextview(TV,
##                     "\n\nNote that seriation is still experimental",
##                     eval(parse(text=seriation.cmd)))
##                                         #"\n\n",
##                #"Cluster centroids.\n\n",
##                #collect.output("crs$kmeans$centers", TRUE),
##   }
  
  setStatusBar("Hierarchical cluster has been generated.")
  
}

########################################################################
##
## MODEL TAB
##

##----------------------------------------------------------------------
##
## Interface Actions
##

## When radio button is selected, display appropriate tab page

on_regression_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    MODEL$setCurrentPage(MODEL.GLM.TAB)
    setTextview("confusion_textview")
  }
  setStatusBar()
}

on_dtree_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    MODEL$setCurrentPage(MODEL.RPART.TAB)
    setTextview("confusion_textview")
  }
  setStatusBar()
}

on_boost_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    MODEL$setCurrentPage(MODEL.GBM.TAB)
    setTextview("confusion_textview")
  }
  setStatusBar()
}

on_rf_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    MODEL$setCurrentPage(MODEL.RF.TAB)
    setTextview("confusion_textview")
  }
  setStatusBar()
}

on_svm_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    MODEL$setCurrentPage(MODEL.SVM.TAB)
    setTextview("confusion_textview")
  }
  setStatusBar()
}

on_e1071_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    SVMNB$setCurrentPage(SVMNB.ESVM.TAB)
  }
  setStatusBar()
}

on_kernlab_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    SVMNB$setCurrentPage(SVMNB.KSVM.TAB)
  }
  setStatusBar()
}

on_priors_entry_changed <- function(action, window)
{
  setStatusBar()
}

## When the rpart loss combobox is entered, retrieve the list of
## matrix objects in R and populate the choices. This is the simplest
## approach to handling the loss matrix at present and may be
## sufficient.

on_rpart_loss_comboboxentry_set_focus_child <- function(action, window)
{
  ## Generate a list of suitable (matrix) R objects

  ml <- unlist(sapply(ls(sys.frame(0)),
                      function(x)
                      {
                        cmd <- sprintf("is.matrix(%s)",x)
                        var <- try(ifelse(eval(parse(text=cmd), sys.frame(0)),
                                          x, NULL), silent=TRUE)
                        if (inherits(var, "try-error"))
                          var <- NULL
                        return(var)
                      }))
  if (! is.null(ml))
  {
    action$getModel()$clear()
    lapply(ml, action$appendText)
  }
}

##----------------------------------------------------------------------
##
## Support
##

currentModelTab <- function()
{
  cp <- MODEL$getCurrentPage()
  lb <- MODEL$getTabLabelText(MODEL$getNthPage(cp))
  if (lb == SVM && rattleWidget("kernlab_radiobutton")$getActive())
    lb <- KSVM
  return(lb)
}

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
    return(simplify.number.list(intersect(fl, union(fi, union(ti, ri)))))
}

input.variables <- function(numonly=FALSE)
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
    return(simplify.number.list(intersect(fl,fi)))
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
    return(simplify.number.list(setdiff(fl, ii)))
}

##----------------------------------------------------------------------

##
## Execution
##

execute.model.tab <- function()
{
  ## Can not build a model without a dataset.

  if (noDatasetLoaded()) return()

  ## If VARIABLES has some ignores but crs$ignore is NULL, complain.

  if (variablesHaveChanged("building a model")) return()

  ## If WeightCalculator has changed but not same as crs$weight,
  ## complain. This doesn't work any more since we add crs$dataset to
  ## the variable names in the Weights Calculator, so they are
  ## different! But, let's remove the crs$dataset and compare.

  weights.display <- gsub('crs\\$dataset\\$', '', crs$weights)

  if (! is.null(crs$weights)
      && weights.display != rattleWidget("weight_entry")$getText())
  {
    errorDialog("You appear to have changed the formula for calculating the",
                 "weights on the Variables tab, but have not executed the tab.",
                 "The previous formula",
                 sprintf('was "%s" and is now "%s".', crs$weights,
                         rattleWidget("weight_entry")$getText()),
                 "Please be sure to execute the tab before continuing.")
    return()
  }
    
  ## Retrieve the target and make sure there is one.

  if (length(crs$target) == 0)
  {
    errorDialog("No target has been specified.",
                 "Please identify the target using the Variables tab.",
                 "Be sure to Execute the tab once the target has",
                 "been identified.")
    return()
  }

  ## Check if sampling needs executing.

  if (sampleNeedsExecute()) return()
    
  ## If the target has more than 2 levels, disable the ROCR and Risk
  ## plots, and place a message on the first textview of the Evaluate
  ## tab. We make this word wrap here and then turn that off once the
  ## tab is Executed.
  
  if (length(levels(as.factor(crs$dataset[[crs$target]]))) > 2)
  {
    deactivate.rocr.plots()
    rattleWidget("confusion_textview")$setWrapMode("word")
    clearTextview("confusion_textview")
    appendTextview("confusion_textview",
                   "Note that the target you have chosen has more than",
                   "2 classes. Some functionality on the Evaluate tab",
                   "will not be available. In particular, the ROCR",
                   "package (Lift, ROC, Precision, and Sensitivity",
                   "charts) and the Risk Chart only handle binary",
                   "classification.")
  }
  else
  {
    activate.rocr.plots()
    setTextview("confusion_textview") # Clear any confusion table
  }

  ## DISPATCH

  if (currentModelTab() == GLM)
    execute.model.glm()
  else if (currentModelTab() == RPART)
    execute.model.rpart()
  else if (currentModelTab() == GBM)
    execute.model.gbm()
  else if (currentModelTab() == RF)
    execute.model.rf()
  else if (is.element(currentModelTab(), c(SVM, KSVM)))
    executeModelSVM()
}

execute.model.glm <- function()
{

  ## Currently only handling binary classification.
  
##   num.classes <- length(levels(as.factor(crs$dataset[[crs$target]])))
##   if (num.classes > 2)
##   {
##     errorDialog("Currently Rattle only handles logistic regression for",
##                  "binary classification.",
##                  sprintf("The %s dataset has %d classes.",
##                          crs$dataname, num.classes))
##     return()
##   }

  ## Obtain the family

  family <- rattleWidget("glm_family_comboboxentry")$getActiveText()
  
  ## Build the formula for the model.

  frml <- paste(crs$target, "~ .")

  ## List, as a string, the variables to be included. 
  
  included <- getIncludedVariables()
  
  ## Some convenience booleans.

  sampling  <- ! is.null(crs$sample)
  including <- ! is.null(included)
  subsetting <- sampling || including
  
  ## Assume logistic regression for binary classification for now.
  
  glm.cmd <- paste("crs$glm <<- glm(", frml, ", data=crs$dataset",
                       if (subsetting) "[",
                       if (sampling) "crs$sample",
                       if (subsetting) ",",
                       if (including) included,
                       if (subsetting) "]",
                       ", family=", family,
                       ")", sep="")

  summary.cmd <- "summary(crs$glm)"
  
  ## Build the model.

  addLogSeparator()
  addToLog("Build a logistic regression model using glm.",
          gsub("<<-", "<-", glm.cmd), sep="")

  eval(parse(text=glm.cmd))
  
  ## Summarise the model.

  addToLog("Summary of the resulting GLM model", summary.cmd)
          
  clearTextview("glm_textview")
  setTextview("glm_textview",
              "Summary of the model built using glm.\n",
              collect.output(summary.cmd, TRUE),
              textviewSeparator())

  if (sampling) crs$smodel <<- union(crs$smodel, GLM)
  
  setStatusBar("GLM model has been generated.")
}

##----------------------------------------------------------------------
##
## MODEL RPART
##

execute.model.rpart <- function()
{
  num.classes <- length(levels(as.factor(crs$dataset[[crs$target]])))
  control <- NULL
  parms <- NULL
  
  ## Retrieve the Priors, and check there is the right number and that
  ## they add up to 1.
  
  priors <- rattleWidget("rpart_priors_entry")$getText()
  if (nchar(priors) > 0)
  {
    pr <- as.numeric(unlist(strsplit(priors, ",")))
    if (length(pr) != num.classes)
      {
        errorDialog(sprintf("The supplied priors (%s)", priors),
                     "need to correspond to the number of classes",
                     sprintf("found in the selected variable '%s'.",crs$target),
                     sprintf("Please supply exactly %d priors.", num.classes))
        return()
      }
    if (sum(pr) != 1)
      {
        errorDialog(sprintf("The supplied priors (%s)", priors),
                     sprintf("add up to %0.2f whereas", sum(pr)),
                     "they need to add up 1.00")
        return()
      }
    if (is.null(parms))
      parms <- sprintf(", parms=list(prior=c(%s))", priors)
    else
      parms <- gsub(")$", sprintf(", prior=c(%s)", priors), parms)
  }

  ## Retrieve the Min Split and check if it is different from the
  ## default, and if so then use it.

  minsplit <- rattleWidget("rpart_minsplit_spinbutton")$getValue()
  if (minsplit != RPART.MINSPLIT.DEFAULT)
  {
    if (is.null(control))
      control <- sprintf(", control=rpart.control(minsplit=%d)", minsplit)
    else
      control <- gsub(")$", sprintf(", minsplit=%d)", minsplit), control)
  }

  ## Retrieve the Min Bucket and check if it is different from the
  ## default, and if so then use it.

  minbucket <- rattleWidget("rpart_minbucket_spinbutton")$getValue()
  if (minbucket != RPART.MINBUCKET.DEFAULT)
  {
    if (is.null(control))
      control <- sprintf(", control=rpart.control(minbucket=%d)", minbucket)
    else
      control <- gsub(")$", sprintf(", minbucket=%d)", minbucket), control)
  }

  ## Retrieve the Max Depth and check if it is different from the
  ## default, and if so then use it.

  maxdepth <- rattleWidget("rpart_maxdepth_spinbutton")$getValue()
  if (maxdepth != RPART.MAXDEPTH.DEFAULT)
  {
    if (is.null(control))
      control <- sprintf(", control=rpart.control(maxdepth=%d)", maxdepth)
    else
      control <- gsub(")$", sprintf(", maxdepth=%d)", maxdepth), control)
  }

  ## Retrieve the Complexity and check if it is different from the
  ## default, and if so then use it.

  cp <- rattleWidget("rpart_cp_spinbutton")$getValue()

  if (abs(cp-RPART.CP.DEFAULT) > 0.00001) ## Diff when same is 2.2352e-10!!!
  {
    if (is.null(control))
      control <- sprintf(", control=rpart.control(cp=%f)", cp)
    else
      control <- gsub(")$", sprintf(", cp=%f)", cp), control)
  }

  ## Retrieve the Cross Validation value and if different from
  ## default, use it. No longer. Common wisdom is that 10 is right, so
  ## in Rattle just go with that.
  
  ## xval <- rattleWidget("rpart_xval_spinbutton")$getValue()
  ## if (xval != RPART.XVAL.DEFAULT)
  ## {
  ##  if (is.null(control))
  ##    control <- sprintf(", control=rpart.control(xval=%d)", xval)
  ##  else
  ##    control <- gsub(")$", sprintf(", xval=%d)", xval), control)
  ## }

  ## Retrieve the loss matrix and ensure it matches the shape of the
  ## data.

  loss <- rattleWidget("rpart_loss_entry")$getText()
  if (nchar(loss) > 0)
  {
    lo <- as.numeric(unlist(strsplit(loss, ",")))
    if (length(lo) != num.classes * num.classes)
    {
      errorDialog(sprintf("The supplied loss matrix values (%s)", loss),
                   sprintf("need to have %d values.", num.classes*num.classes),
                   "Please enter that many values, comma separated.")
      return()
    }
      
    ## TODO: Perform other checks on the matrix here.  The loss matrix
    ## must have zeros on the diagonal and positive numbers
    ## elsewhere. It must be the same dimensions as the number of
    ## classes.

    lo <- sprintf("matrix(c(%s), byrow=TRUE, nrow=%d)", loss, num.classes) 
    
    if (is.null(parms))
      parms <- sprintf(", parms=list(loss=%s)", lo)
    else
      parms <- gsub(")$", sprintf(", loss=%s)", lo), parms)
  }

  ## Build the formula for the model, noting that rpart has only a
  ## formula interface.

  frml <- paste(crs$target, "~ .")

  ## List, as a string of indicies, the variables to be included. 
  
  included <- getIncludedVariables()
  
  ## Some convenience booleans

  sampling  <- ! is.null(crs$sample)
  including <- ! is.null(included)
  subsetting <- sampling || including

  ## Commands.
  
  library.cmd <- "require(rpart, quietly=TRUE)"
  if (! packageIsAvailable("rpart", "build decision trees")) return()
    
  rpart.cmd <- paste("crs$rpart <<- rpart(", frml, ", data=crs$dataset",
                     if (subsetting) "[",
                     if (sampling) "crs$sample",
                     if (subsetting) ",",
                     if (including) included,
                     if (subsetting) "]",
                     ifelse(is.null(crs$weights), "",
                            sprintf(", weights=(%s)%s",
                                    crs$weights,
                                    ifelse(sampling, "[crs$sample]", ""))),
                     ', method="class"',
                     ifelse(is.null(parms), "", parms),
                     ifelse(is.null(control), "", control),
                     ")", sep="")

  plot.cmd <- paste("drawTreeNodes(crs$rpart)\n",
                    genPlotTitleCmd("Decision Tree",
                                   crs$dataname, "$", crs$target),
                    sep="")
  
##   plotcp.cmd <- paste("\n\n## Plot the cross validation results.\n\n",
##                           "plotcp(crs$rpart)\n",
##                           genPlotTitleCmd("Cross Validated Error",
##                                              crs$dataname, "$", crs$target))

  print.cmd <- paste("print(crs$rpart)", "printcp(crs$rpart)", sep="\n")
  listrules.cmd <- "list.rules.rpart(crs$rpart)"
                             
  ## Load the required library.

  addLogSeparator()
  addToLog("Build a decision tree using the rpart package.", library.cmd)
  eval(parse(text=library.cmd))

  ## Build the model.

  addToLog("Build an rpart model.", gsub("<<-", "<-", rpart.cmd))
  result <- try(eval(parse(text=rpart.cmd)), silent=TRUE)
  if (inherits(result, "try-error"))
  {
    if (any(grep("syntax error.*weights", result)))
      errorDialog("The call to rpart has a syntax error. This may be due",
                   "to a syntax error in the weights formula if you have",
                   "specified one. The error message was:", result)
    else
      errorDialog("An error occured in the call to rpart.",
                   "the error was:", result)
    return()
  }

  ## If more than a root node then plot the model.

  if (nrow(crs$rpart$frame) > 1)
  {
    newPlot()
    addToLog(paste("Plot the resulting rpart tree using Rattle",
                  "and maptools support functions."),
            plot.cmd)
    eval(parse(text=plot.cmd))

    ## newPlot()
    ## addToLog(plotcp.command)
    ## eval(parse(text=plotcp.command))
  }
  
  ## Display the resulting model.

  addToLog("Generate textual output of the rpart model.", print.cmd)
  addToLog("List the rules from the tree using a Rattle support function.",
          listrules.cmd)
          
  clearTextview("rpart_textview")
  setTextview("rpart_textview",
              "Summary of the rpart model:\n\n",
              collect.output(print.cmd),
              textviewSeparator(),
              "Tree as rules:\n\n",
              collect.output(listrules.cmd, TRUE),
              textviewSeparator())

  if (sampling) crs$smodel <<- union(crs$smodel, RPART)
  
  setStatusBar("An RPart model has been generated.")
}

##----------------------------------------------------------------------
#
# Print out RPart Rules
#
list.rules.rpart <- function(model, compact=FALSE)
{
  if (!inherits(model, "rpart")) stop("Not a legitimate rpart tree")
  # if (model$method != "class")) stop("Model method needs to be class")
  #
  # Get some information.
  #
  frm <- model$frame
  names <- row.names(frm)
  ylevels <- attr(model, "ylevels")
  ds.size <-  model$frame[1,]$n
  #
  # Print each leaf node as a rule.
  #
  ordered <- rev(sort(frm$yval2[,5], index=TRUE)$ix)
  for (i in ordered)
  {
    if (frm[i,1] == "<leaf>")
    {
      # The following [,5] is hardwired and works on one example....
      yval <- ylevels[frm[i,]$yval]
      cover <- frm[i,]$n
      pcover <- round(100*cover/ds.size)
      prob <- frm[i,]$yval2[,5]
      cat("\n")
      pth <- path.rpart(model, nodes=as.numeric(names[i]), print.it=FALSE)
      pth <- unlist(pth)[-1]
      if (length(pth) == 0) pth <- "True"
      if (compact)
      {
        cat(sprintf("R%03s ", names[i]))
        cat(sprintf("[%2.0f%%,%0.2f]",
                    pcover, prob))
        cat(sprintf(" %s", pth), sep="")
      }
      else
      {
        cat(sprintf(" Rule number: %s ", names[i]))
        cat(sprintf("[yval=%s cover=%d (%.0f%%) prob=%0.2f]\n",
                    yval, cover, pcover, prob))
        cat(sprintf("   %s\n", pth), sep="")
      }
    }
  }
  cat("\n")
  invisible(ordered)
}

list.rule.nodes.rpart <- function(model)
{
  # The information we need is in the rpart frame
  frm <- model$frame
  # Obtain the probabilities
  nodes <- frm$yval2[,5]
  # Get the probability ordered index of leaf nodes
  ordered <- sort(frm$yval2[,5][frm[,1] == "<leaf>"],
                  decr=TRUE, index=TRUE)
  # Return the list of node numbers
  return(row.names(frm)[which(frm[,1] == "<leaf>")][ordered$ix])
}

#
# Modify draw.tree from maptree to draw rule nodes rather than sequential
# numbers to label the leaves.
#
drawTreeNodes <- function (tree, cex = par("cex"), pch = par("pch"),
                           size = 2.5 * cex, col = NULL, nodeinfo = FALSE,
                           units = "", cases = "cases", 
                           digits = getOption("digits"),
                           decimals = 2,
                           print.levels = TRUE, new = TRUE) 
{
  #if (!packageIsAvailable("maptree", "draw a decision tree"))
  #  return
  #require(maptree)
  if (new) 
    plot.new()
  rtree <- length(attr(tree, "ylevels")) == 0
  tframe <- tree$frame
  rptree <- length(tframe$complexity) > 0
  node <- as.numeric(row.names(tframe))
  leafnode <- node[tframe$var == "<leaf>"]
  proportions <- sprintf("%0.2f", tframe$yval2[,5])
  depth <- floor(log(node, base = 2) + 1e-07)
  depth <- as.vector(depth - min(depth))
  maxdepth <- max(depth)
  x <- -depth
  y <- x
  leaves <- tframe$var == "<leaf>"
  x[leaves] <- seq(sum(leaves))
  depth <- split(seq(node)[!leaves], depth[!leaves])
  parent <- match(node%/%2, node)
  left.child <- match(node * 2, node)
  right.child <- match(node * 2 + 1, node)
  for (i in rev(depth)) x[i] <- 0.5 * (x[left.child[i]] + x[right.child[i]])
  nleaves <- sum(leaves)
  nnodes <- length(node)
  if (rtree) {
    dev <- tframe$dev
    pcor <- rep(0, nnodes)
    for (i in 1:nnodes) if (!leaves[i]) {
      l <- dev[node == (node[i] * 2)]
      r <- dev[node == (node[i] * 2 + 1)]
      pcor[i] <- dev[i] - l - r
    }
    pcor <- round(pcor/dev[1], 3) * 100
  }
  else {
    crate <- rep(0, nnodes)
    trate <- 0
    if (!rptree) {
      for (i in 1:nnodes) {
        yval <- tframe$yval[i]
        string <- paste("tframe$yprob[,\"", as.character(yval), 
                        "\"]", sep = "")
        crate[i] <- eval(parse(text = string))[i]
        if (leaves[i]) 
          trate <- trate + tframe$n[i] * crate[i]
      }
    }
    else {
      for (i in 1:nnodes) {
        yval <- tframe$yval[i]
        nlv <- floor(ncol(tframe$yval2)/2)
        index <- rev(order(tframe$yval2[i, 2:(nlv + 1)]))[1]
        crate[i] <- tframe$yval2[i, (nlv + 1 + index)]
        if (leaves[i]) 
          trate <- trate + tframe$n[i] * crate[i]
      }
    }
    crate <- round(crate, 3) * 100
    trate <- round(trate/tframe$n[1], 3) * 100
  }
  if (is.null(col)) 
    kol <- rainbow(nleaves)
  else if (col == "gray" | col == "grey") 
    kol <- gray(seq(0.8, 0.2, length = nleaves))
  else kol <- col
  xmax <- max(x)
  xmin <- min(x)
  ymax <- max(y)
  ymin <- min(y)
  pinx <- par("pin")[1]
  piny <- par("pin")[2]
  xscale <- (xmax - xmin)/pinx
  box <- size * par("cin")[1]
  if (box == 0) 
    xbh <- xscale * 0.2
  else xbh <- xscale * box/2
  chr <- cex * par("cin")[2]
  tail <- box + chr
  yscale <- (ymax - ymin)/(piny - tail)
  ytail <- yscale * tail
  if (box == 0) 
    ybx <- yscale * 0.2
  else ybx <- yscale * box
  ychr <- yscale * chr
  ymin <- ymin - ytail
  xf <- 0.1 * (xmax - xmin)
  yf <- 0.1 * (ymax - ymin)
  x1 <- xmin - xf
  x2 <- xmax + xf
  y1 <- ymin - yf
  y2 <- ymax + yf
  par(usr = c(x1, x2, y1, y2))
    v <- as.character(tframe$var[1])
    if (rptree) {
        sp <- tree$splits[1, ]
        val <- sp["index"]
        if (sp["ncat"] > 1) {
            r <- sp["index"]
            string <- "attributes(tree)$xlevels$"
            string <- paste(string, v, sep = "")
            xl <- eval(parse(text = string))
            lf <- rf <- ""
            for (k in 1:sp["ncat"]) if (tree$csplit[r, k] == 
                1) 
                lf <- paste(lf, xl[k], sep = ",")
            else rf <- paste(rf, xl[k], sep = ",")
            if (!print.levels) 
                string <- v
            else if (nchar(lf) + nchar(rf) > 30) # Avoid too long
              string <- v
            else
              string <- paste(lf, "=", v, "=", rf)
            
        }
        else {
            if (sp["ncat"] < 0) 
                op <- "< - >"
            else op <- "> - <"
            string <- paste(v, op, round(val, decimals))
        }
    }
    else {
        val <- substring(as.character(tframe$splits[1, 1]), 2)
        string <- paste(as.character(v), "< - >", round(val, decimals))
    }
    text.default(x[1], y[1], string, cex = cex)
    if (nodeinfo) {
        n <- tframe$n[1]
        if (rtree) {
            z <- round(tframe$yval[1], digits)
            r <- pcor[1]
            string <- paste(z, " ", units, "; ", n, " ", cases, 
                "; ", r, "%", sep = "")
        }
        else {
            z <- attr(tree, "ylevels")[tframe$yval[1]]
            r <- crate[1]
            string <- paste(z, "; ", n, " ", cases, "; ", r, 
                "%", sep = "")
        }
        text.default(x[1], y[1] - ychr, string, cex = cex)
    }
    for (i in 2:nnodes) {
        ytop <- ychr * (as.integer(nodeinfo) + 1)
        if (y[i] < y[i - 1]) {
            lines(c(x[i - 1], x[i]), c(y[i - 1] - ytop, y[i - 
                1] - ytop))
            lines(c(x[i], x[i]), c(y[i - 1] - ytop, y[i] + ychr))
        }
        else {
            lines(c(x[parent[i]], x[i]), c(y[parent[i]] - ytop, 
                y[parent[i]] - ytop))
            lines(c(x[i], x[i]), c(y[parent[i]] - ytop, y[i] + 
                ychr))
        }
        if (!leaves[i]) {
            v <- as.character(tframe$var[i])
            if (rptree) {
                k <- 1
                for (j in 1:(i - 1)) {
                  m <- tframe$ncompete[j]
                  if (m > 0) 
                    k <- k + m + 1
                  m <- tframe$nsurrogate[j]
                  if (m > 0) 
                    k <- k + m
                }
                sp <- tree$splits[k, ]
                val <- sp["index"]
                if (sp["ncat"] > 1) {
                  r <- sp["index"]
                  string <- "attributes(tree)$xlevels$"
                  string <- paste(string, v, sep = "")
                  xl <- eval(parse(text = string))
                  lf <- rf <- ""
                  for (k in 1:sp["ncat"]) if (tree$csplit[r, 
                    k] == 1) 
                    lf <- paste(lf, xl[k], sep = ",")
                  else rf <- paste(rf, xl[k], sep = ",")
                  if (!print.levels) 
                    string <- v
                  else if (nchar(lf) + nchar(rf) > 10) # Avoid too long
                    string <- v
                  else string <- paste(lf, "=", v, "=", rf)
                }
                else {
                  if (sp["ncat"] < 0) 
                    op <- "< - >"
                  else op <- "> - <"
                  string <- paste(v, op, round(val, decimals))
                }
            }
            else {
                val <- substring(as.character(tframe$splits[i, 
                  1]), 2)
                string <- paste(as.character(v), "< - >", round(val, decimals))
            }
            text.default(x[i], y[i], string, cex = cex)
            if (nodeinfo) {
                n <- tframe$n[i]
                if (rtree) {
                  z <- round(tframe$yval[i], digits)
                  r <- pcor[i]
                  string <- paste(z, " ", units, "; ", n, " ", 
                    cases, "; ", r, "%", sep = "")
                }
                else {
                  z <- attr(tree, "ylevels")[tframe$yval[i]]
                  r <- crate[i]
                  string <- paste(z, "; ", n, " ", cases, "; ", 
                    r, "%", sep = "")
                }
                text.default(x[i], y[i] - ychr, string, cex = cex)
            }
        }
        else {
            if (box == 0) {
                lines(c(x[i], x[i]), c(y[i], y[i] + ychr))
                lines(c(x[i] - xbh, x[i] + xbh), c(y[i], y[i]))
            }
            else {
                # points(x[i], y[i], pch = pch, cex = size, col = kol[x[i]])
            }
            if (rtree) {
                z <- round(tframe$yval[i], digits)
                text.default(x[i], y[i] - ybx, paste(z, units, 
                  sep = " "), cex = cex)
            }
            else {
                z <- attr(tree, "ylevels")[tframe$yval[i]]
                text.default(x[i], y[i] - ybx, z, cex = cex)
            }
            n <- tframe$n[i]
            text.default(x[i], y[i] - ybx - ychr, paste(n, cases,
                sep = " "), cex = cex)
            text.default(x[i], y[i] - 1.6*ybx - ychr,
                         paste(crate[i], "%", sep = ""), cex = cex)
                         #paste(crate[i], "%/", proportions[i], sep = ""), cex = cex)
            if (box != 0)
            {
                # ORIG text.default(x[i], y[i], as.character(x[i]), 
                text.default(x[i], y[i], as.character(leafnode[x[i]]),
                  cex = cex, col=kol[x[i]], font=2)
            }    
        }
    }
    if (nodeinfo) {
        if (rtree) 
            string <- paste("Total deviance explained =", sum(pcor), 
                "%")
        else string <- paste("Total classified correct =", trate, 
            "%")
        if (box == 0) 
            text.default(mean(x), ymin - 3.5 * ychr, string, cex = 1.2 * 
                cex)
        else text.default(mean(x), ymin - 2.2 * ybx, string, 
            cex = 1.2 * cex)
    }
}

##------------------------------------------------------------------------
##
## GBM - BOOSTING
##

execute.model.gbm <- function()
{
  num.classes <- length(levels(as.factor(crs$dataset[[crs$target]])))

  ## Check for ignored variables

  indicies <- NULL
  if (! is.null(crs$input))
    indicies <- getVariableIndicies(crs$input)

  ## Build the formula for the model - why can't GBM use "."?

  target.index <- which(colnames(crs$dataset)==crs$target)
##   frml <- paste(target, "~",
##                 paste(colnames(crs$dataset)[-c(indicies, target.index)],
##                       collapse="+"))

  ## Some convenience booleans

  sampling <- ! is.null(crs$sample)
  subsetting <- sampling

  ## Greg Ridgway's advice is to use bernoulli, not adaboost or gaussian

  if (is.factor(crs$dataset[[crs$target]]) || num.classes > 2)
    distribution <- "gaussian"
  else
    #distribution <- "adaboost"
    distribution <- "bernoulli"
  
  ## Required library

  if (! packageIsAvailable("gbm", "build an AdaBoost model"))
    return()
  
  library.cmd <- paste(sprintf("\n\n## Build a GBN (%s) model.",
                                   distribution),
                           "\n\require(gbm, quietly=TRUE)")

  ## Boost command

  ## Use gbm.fit rather than gbm for more efficiency.

  included <- simplify.number.list(indicies)
  
  boost.cmd <- paste("crs$gbm <<- gbm.fit(crs$dataset[",
                         if (sampling) "crs$sample",
                         ",", included, "], ",
                         "crs$dataset$", crs$target,
                         if (sampling) "[crs$sample]",
                         ", ",
                         'distribution="', distribution, '"',
                         ")", sep="")

  ## Summary command

  summary.cmd <- "summary(crs$gbm, cBars=5)"
  show.cmd <- "gbmShowRules(crs$gbm)"
 
  ## Log

  addToLog(library.cmd, "\n",
          gsub("<<-", "<-", boost.cmd), "\n",
          summary.cmd, "\n",
          show.cmd, sep="")

  ## Run model and show results.
  eval(parse(text=library.cmd))
  clearTextview("gbm_textview")
  setTextview("gbm_textview",
               "Output from GBM model builder:\n\n",
               collect.output(boost.cmd),
               "\n\nSummary of relative influence of each variable:\n\n",
               collect.output(paste("print(",summary.cmd, ")")),
               "\n\nRules making up the model:\n\n",
               collect.output(show.cmd),
               sep="")

  if (sampling) crs$smodel <<- union(crs$smodel, GBM)
  
  setStatusBar("Boosted model has been generated.")
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


##------------------------------------------------------------------------
##
## MODEL RF - RANDOM FOREST
##

execute.model.rf <- function()
{
  num.classes <- length(levels(as.factor(crs$dataset[[crs$target]])))
  parms <- ""

  ## Make sure there are no included variables which have more than 32
  ## levels, which can not be handled by randomForest (perhaps a limit
  ## only on 32 bit machines)
  
  categoricals <- crs$input[unlist(lapply(crs$input,
                            function(x) is.factor(crs$dataset[,x])))]

  for (i in categoricals)
    if (length(levels(crs$dataset[,i])) > 32)
    {
      errorDialog("This implementation of randomForest does not handle",
                   "categorical variables with more than 32 levels.",
                   "The variable", i, "has", length(levels(crs$dataset[,i])),
                   "levels. Please choose to ignore it in the",
                   "Variables tab if you wish to build a randomForest model.")
      return()
    }

  ## Retrieve options and set up parms.

  ntree <- rattleWidget("rf_ntree_spinbutton")$getValue()
  if (ntree != RF.NTREE.DEFAULT)
    parms <- sprintf("%s, ntree=%d", parms, ntree)
  
  mtry <- rattleWidget("rf_mtry_spinbutton")$getValue()
  if (mtry != RF.MTRY.DEFAULT)
    parms <- sprintf("%s, mtry=%d", parms, mtry)

  sampsize <- rattleWidget("rf_sampsize_spinbutton")$getValue()
  if (sampsize != RF.SAMPSIZE.DEFAULT)
    parms <- sprintf("%s, sampsize=%d", parms, sampsize)

  if (rattleWidget("rf_importance_checkbutton")$getActive())
    parms <- sprintf("%s, importance=TRUE", parms)
  
  if (rattleWidget("rf_proximity_checkbutton")$getActive())
    parms <- sprintf("%s, proximity=TRUE", parms)
  
  ## Build the formula for the model. TODO We assume we will always do
  ## classification rather than regression, at least for now.

  frml <- paste(ifelse(is.factor(crs$dataset[[crs$target]]),
                       crs$target,
                       sprintf("as.factor(%s)", crs$target)),
                "~ .")

  ## List, as a string of indicies, the variables to be included. 

  included <- getIncludedVariables()
  
  ## Some convenience booleans

  sampling <- ! is.null(crs$sample)
  including <- ! is.null(included)
  subsetting <- sampling || including

  ## Commands

  library.cmd <- "require(randomForest, quietly=TRUE)"

  rf.cmd <- paste("crs$rf <<- randomForest(", frml, ", data=crs$dataset",
                  if (subsetting) "[",
                  if (sampling) "crs$sample",
                  if (subsetting) ",",
                  if (including) included,
                  if (subsetting) "]",
                  parms,
                  ", na.action=na.omit",
                  ")", sep="")

  summary.cmd <- "crs$rf"
  
  importance.cmd <- "round(importance(crs$rf), 2)"
 
  ## Load the required library.

  addLogSeparator()
  addToLog("The randomForest package supplies the randomForest function.",
          library.cmd)
  eval(parse(text=library.cmd))

  ## Build the model.

  addToLog("Build a randomForest model.", gsub("<<-", "<-", rf.cmd))
  result <- try(eval(parse(text=rf.cmd)), silent=TRUE)
  if (inherits(result, "try-error"))
  {
    if (any(grep("cannot allocate vector", result)))
    {
      errorDialog("The call to randomForest appears to have failed.",
                   "This is often due, as in this case,",
                   "to running out of memory",
                   "as randomForest is rather memory hungry.",
                   "A quick solution is to sample the dataset, through the",
                   "Sample tab. On 32 bit machines you may be limited to",
                   "less than 2000 entities.")
      setTextview("rf_textview")
    }
    else
      errorDialog("The call to randomForest appears to have failed.",
                   "The error message was:", result,
                   "I am not familiar with this error, and you may",
                   "want to report it to the Rattle author",
                   "at Graham.Williams@togaware.com")
    return()
  }

  ## Display the resulting model.

  addToLog("Generate textual output of randomForest model.", summary.cmd)
  addToLog("List the importance of the variables.", importance.cmd)
  
  clearTextview("rf_textview")
  setTextview("rf_textview",
              "Summary of the randomForest model:\n\n",
              collect.output(summary.cmd, TRUE),
              textviewSeparator(),
              "Variable importance:\n\n",
              collect.output(importance.cmd, TRUE),
              textviewSeparator(),
              "To view model 5, for example, run printRandomForests(crs$rf, 5)\n",
              "in the R conosle. Generating all 500 models takes quite some time.",
              textviewSeparator())

  if (sampling) crs$smodel <<- union(crs$smodel, RF)

  setStatusBar("A randomForest model has been generated.")
}

printRandomForests <- function(model, models=NULL)
{
  if (! packageIsAvailable("randomForest", "print the rule sets"))
    return()

  require(randomForest, quietly=TRUE)

  if (is.null(models)) models <- 1:model$ntree

  strings <- c()
  
  for (i in models)
    printRandomForest(model, i)
  
  return(strings)
}

printRandomForest <- function(model, n)
{
  if (! packageIsAvailable("randomForest", "generate the rule sets"))
    return()

  require(randomForest, quietly=TRUE)

  tr <- getTree(model, n)
  tr.paths <- getRFPathNodes(tr)
  tr.vars <- attr(model$terms, "dataClasses")[-1]
  
  ## Initialise the output

  cat("===================================================================\n")
  cat(sprintf("Random Forest Model %d\n\n", n))

  ## Generate rpart form for each rule.

  cat("-----------------------------------------------------------------\n")
  
  for (i in 1:length(tr.paths))
  {
    cat(sprintf("Rule Number %d of Forest %d\n\n", i, n))
    
    tr.path <- tr.paths[[i]]

    ## Indicies of variables in the path
    
    var.index <- tr[,3][abs(tr.path)] # 3rd col is "split var"
    var.names <- names(tr.vars)[var.index]
    var.values <- tr[,4][abs(tr.path)] # 4th col is "split point"
    
    for (j in 1:length(tr.path))
    {
      var.class <- tr.vars[var.index[j]]
      if (var.class == "character" | var.class == "factor")
      {
        node.op <- "="

        ## Convert the binary to a 0/1 list for the levels.
        
        var.levels <- levels(eval(model$call$data)[[var.names[j]]])
        bins <- sdecimal2binary(var.values[j])
        bins <- c(bins, rep(0, length(var.levels)-length(bins)))
        if (tr.path[j] > 0)
          node.value <- var.levels[bins==1]
        else
          node.value <- var.levels[bins==0]
        node.value <- paste(node.value, collapse=",")
      }
      else if (var.class == "integer" | var.class == "numeric")
      {
        ## Assume spliting to the left means "<", and right ">="
        if (tr.path[j]>0)
          node.op <- "<"
        else
          node.op <- ">="
        node.value <- var.values[j]
      }
      else
        stop(sprintf("Rattle: getRFRuleSet: class %s not supported.",
                     var.class))

      cat(sprintf("%s %s %s\n", var.names[j], node.op, node.value))
    }
    cat("-----------------------------------------------------------------\n")
  }
}

randomForest2Rules <- function(model, models=NULL)
{
  if (! packageIsAvailable("randomForest", "generate the rule sets"))
    return()

  require(randomForest, quietly=TRUE)

  if (is.null(models)) models <- 1:model$ntree

  ## Obtain information we need about the data
  
  vars <- attr(model$terms, "dataClasses")[-1]

  ruleset <- list()
  
  for (i in models)
  {
    ruleset[[i]] <- list(ruleset=getRFRuleSet(model, i))
  }
  return(ruleset)
}

## Generate a list of rules (conditions and outcomes) for RF MODEL
## number N.

getRFRuleSet <- function(model, n)
{
  if (! packageIsAvailable("randomForest", "generate the rule sets"))
    return()

  require(randomForest, quietly=TRUE)

  tr <- getTree(model, n)
  tr.paths <- getRFPathNodes(tr)
  tr.vars <- attr(model$terms, "dataClasses")[-1]
  
  ## Initialise the output
  
  rules <- list()

  ## Generate rpart form for each rule.

  for (i in 1:length(tr.paths))
  {
    tr.path <- tr.paths[[i]]

    ## Indicies of variables in the path
    
    var.index <- tr[,3][abs(tr.path)] # 3rd col is "split var"
    var.names <- names(tr.vars)[var.index]
    var.values <- tr[,4][abs(tr.path)] # 4th col is "split point"
    
    tr.rule <- c("root")

    for (j in 1:length(tr.path))
    {
      var.class <- tr.vars[var.index[j]]
      if (var.class == "character" | var.class == "factor")
      {
        node.op <- "="

        ## Convert the binary to a 0/1 list for the levels.
        
        var.levels <- levels(eval(model$call$data)[[var.names[j]]])
        bins <- sdecimal2binary(var.values[j])
        bins <- c(bins, rep(0, length(var.levels)-length(bins)))
        if (tr.path[j] > 0)
          node.value <- var.levels[bins==1]
        else
          node.value <- var.levels[bins==0]
        node.value <- paste(node.value, collapse=",")
      }
      else if (var.class == "integer" | var.class == "numeric")
      {
        ## Assume spliting to the left means "<", and right ">="
        if (tr.path[j]>0)
          node.op <- "<"
        else
          node.op <- ">="
        node.value <- var.values[j]
      }
      else
        stop(sprintf("Rattle: getRFRuleSet: class %s not supported.",
                     var.class))

      tr.rule <- c(tr.rule, paste(var.names[j], node.op, node.value))
    }
    
    ## TODO Walk through tr.rule and remove all but the last "VAR<"
    ## and "VAR>=" conditions.
    
    rules[[i]] <- list(rule=tr.rule)
  }
  return(rules)
}

getRFPathNodes <- function(treeMat)
{
  ## The columns in the RF tree matrix are:
  ##   1. left daughter;
  ##   2. right daughter;
  ##   3. split var;
  ##   4. split point;
  ##   5. status;
  ##   6. prediction

  ## Number of nodes in the tree
  
  nnodes <- dim(treeMat)[1] 

  ## Leaf node indices

  leafIndex <- 1:nnodes * as.integer(treeMat[,5]== -1) # Non-leaf index is 0
  leafIndex <- leafIndex[leafIndex!=0] # Remove non-zeros (non-leafs)
	  
  paths <- list() # A list, each element a vector of the indices of a path

  ## Process each leaf's path
  
  for (i in 1:length(leafIndex))
  {
    ## Initialise the node to the leaf index
    
    node <- leafIndex[i] # e.g. i=1, node=3
    pathI <- c()
    repeat
    {
      leftV <- 1:nnodes * as.integer(treeMat[,1]==abs(node))
      leftNode <- leftV[leftV!=0]
      if (length(leftNode)!= 0)
      {
        node <- leftNode
      }
      else # else must not be in the next line
      {
        rightV <- 1:nnodes * as.integer(treeMat[,2]==abs(node))
        node <- -rightV[rightV!=0] # rhs node identified with negative index
      }

      pathI <-c(node, pathI)

      ## If the node is the root node (first row in the matrix), then
      ## the path is complete.
      
      if (abs(node)==1) break
    }
    paths[[i]] <- pathI
  }

  ## Each path is named after its leaf index number
  
  names(paths) <- as.character(leafIndex)

  return(paths)
}

sdecimal2binary <- function(x)
{
  return(rev(sdecimal2binary.smallEndian(x)))
}
	
sdecimal2binary.smallEndian <- function(x)
{
  if (x==0) return(0)
  if (x<0) stop("Sorry, the input must be positive")
  dec <- x
	 
  n <- floor(log(x)/log(2))
  bin <- c(1)
  dec <- dec - 2 ^ n
	  
  while(n > 0)
  {
    if (dec >= 2 ^ (n-1)) {bin <- c(bin,1); dec <- dec - 2 ^ (n-1)}
    else bin <- c(bin,0)
    n <- n - 1
  }
  return(bin)
}
	
##------------------------------------------------------------------------
##
## MODEL SVM - SUPPORT VECTOR MACHINE
##

executeModelSVM <- function()
{
  ## DESCRIPTION
  ## Build a support vector machine predictor.
  ##
  ## RETURNS
  ## Ignored.
  ##
  ## DETAILS There are two model builders for SVMs: The e1071 version
  ## is older and is supported by tune, and the kernlab version is
  ## much more extensive. I did move back to e1071 because I thought
  ## issues around the handling of NAs in kernlab a problem, but
  ## essentially I think it is an issue with svm using all variables,
  ## so I had to clean up my handling of NAs.
  
  useKernlab <- rattleWidget("kernlab_radiobutton")$getActive()

  TV <- ifelse(useKernlab, "ksvm_textview", "esvm_textview")
  
  addLogSeparator()

  ## Library.

  if (useKernlab)
  {
    if (packageIsAvailable("kernlab", "build an SVM model using ksvm"))
    {
      libCmd <- "require(kernlab, quietly=TRUE)"
      addToLog("The kernlab package supplies the ksvm function.", libCmd)
    }
    else
      return()
  }
  else
  {
    if (packageIsAvailable("e1071", "build an SVM model using svm"))
    {
      libCmd <- "require(e1071, quietly=TRUE)"
      addToLog("The e1071 package supplies the svm function.", libCmd)
    }
    else
      return()
   }
  eval(parse(text=libCmd))

  ## Formula. TODO For kernlab we assume we will always do
  ## classification rather than regression, at least for now.

  if (useKernlab)
    frml <- paste(ifelse(is.factor(crs$dataset[[crs$target]]),
                         crs$target,
                         sprintf("as.factor(%s)", crs$target)),
                  "~ .")
  else
    frml <- paste(crs$target, "~ .")

  ## Included variables.

  included <- getIncludedVariables()
  
  ## Convenience booleans.

  sampling   <- ! is.null(crs$sample)
  including  <- ! is.null(included)
  subsetting <- sampling || including

  ## Parameters.

  parms <- ""
  
  ## Build the model.

  if (useKernlab)
    svmCmd <- paste("crs$ksvm <<- ksvm(", frml, ", data=crs$dataset", sep="")
  else
    svmCmd <- paste("crs$svm <<- svm(", frml, ", data=crs$dataset", sep="")
  svmCmd <- paste(svmCmd,
                   if (subsetting) "[",
                   if (sampling) "crs$sample",
                   if (subsetting) ",",
                   if (including) included,
                   if (subsetting) "]",
                   parms, sep="")
  if (useKernlab)
    svmCmd <- paste(svmCmd, ", prob.model=TRUE", sep="")  # Probabilities
  else
    svmCmd <- paste(svmCmd, ", probability=TRUE", sep="")  # Probabilities
  svmCmd <- paste(svmCmd, ")", sep="")
  addToLog("Build a support vector machine model.", gsub("<<-", "<-", svmCmd))
  result <- try(eval(parse(text=svmCmd)), silent=TRUE)
  if (inherits(result, "try-error"))
  {
    errorDialog("The call to svm appears to have failed.",
                 "The error message was:", result,
                 "I am not familiar with this error, and you may",
                 "want to report it to the Rattle author",
                 "at Graham.Williams@togaware.com")
    return()
  }

  ## Display the resulting model.

  if (useKernlab)
    summaryCmd <- "crs$ksvm"
  else
    summaryCmd <- "crs$svm"
  addToLog("Generate textual output of the svm model.", summaryCmd)
  clearTextview(TV)
  setTextview(TV,
              "Summary of the svm model:\n\n",
              collect.output(summaryCmd, TRUE),
              textviewSeparator())

  if (sampling)
    if (useKernlab)
      crs$smodel <<- union(crs$smodel, KSVM)
    else
      crs$smodel <<- union(crs$smodel, SVM)

  setStatusBar(sprintf("A %s model has been generated.",
                       ifelse(useKernlab, KSVM, SVM)))
}

##----------------------------------------------------------------------
##
## MARS
##
## y <- pchresp[, c(1)]
## x <- pchresp[, -c(1)]
##
## m1 <- mars(x, y)
##
## showcuts <- function(obj)
## {
##   tmp <- obj$cuts[obj$sel, ]
##   dimnames(tmp) <- list(NULL, dimnames(x)[[2]])
##   tmp
## }
##
## m2 <- mars(x, y, degree=2)

##----------------------------------------------------------------------
##
## SVM
##
## > >
## > > I am using the "svm" command in the e1071 package.
## > >
## > > Does it have an automatic way of setting the "cost" parameter?
## >
## > See ?best.svm in that package.
## >
## > > I changed a few values for the "cost" parameter but I hope there is a
## > > systematic way of obtaining the best "cost" value.
## > >
## > > I noticed that there is a "cross" (Cross validation)
## > > parameter in the "svm"
## > > function.
## > >
## > > But I did not see how it can be used to optimize the "cost" parameter.
## > >
## > > By the way, what does a 0 training error and a high testing
## > > error mean?
## > > Varying "cross=5", or "cross=10", etc. does not change the
## > > training error
## > > and testing error at all. How to improve?
## >
## > Overfitting, which varying different validation method will not solve.


## You might find http://www.csie.ntu.edu.tw/~cjlin/papers/guide/guide.pdf
## <http://www.csie.ntu.edu.tw/~cjlin/papers/guide/guide.pdf>  helpful.

## Parameter tuning is essential for avoiding overfitting.


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
    rattleWidget("evaluate_filechooserbutton")$setSensitive(TRUE)
  else
    rattleWidget("evaluate_filechooserbutton")$setSensitive(FALSE)
  setStatusBar()
}

on_evaluate_rdataset_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    rattleWidget("evaluate_rdataset_combobox")$setSensitive(TRUE)
  else
    rattleWidget("evaluate_rdataset_combobox")$setSensitive(FALSE)
  setStatusBar()
}

on_confusion_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    EVALUATE$setCurrentPage(EVALUATE.CONFUSION.TAB)
  setStatusBar()
}

on_risk_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    EVALUATE$setCurrentPage(EVALUATE.RISK.TAB)
    rattleWidget("evaluate_risk_variable_label")$setSensitive(TRUE)
    rattleWidget("evaluate_risk_label")$setSensitive(TRUE)
  }
  else
  {
    rattleWidget("evaluate_risk_variable_label")$setSensitive(FALSE)
    rattleWidget("evaluate_risk_label")$setSensitive(FALSE)
  }  
  setStatusBar()
}

on_lift_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    EVALUATE$setCurrentPage(EVALUATE.LIFT.TAB)
  setStatusBar()
}

on_roc_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    EVALUATE$setCurrentPage(EVALUATE.ROC.TAB)
  setStatusBar()
}

on_precision_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    EVALUATE$setCurrentPage(EVALUATE.PRECISION.TAB)
  setStatusBar()
}

on_sensitivity_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    EVALUATE$setCurrentPage(EVALUATE.SENSITIVITY.TAB)
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

getActiveModel <- function()
{
  ## THIS NO LONGER EXISTS - FOR NOW JUST RETURN FIRST MODEL SELECTED!
  ## AND THEN MIGRATE TO WORKING ON ALL MODELS!
  ## mtype <- rattleWidget("model_evaluate_comboboxentry")$getActiveText()
  mtype <- getEvaluateModels()
  if (!is.null(mtype)) mtype <- mtype[1]
  return(mtype)
}

getEvaluateModels <- function()
{
  ## Return a list of models selected for evaluation

  models <- c()
  for (m in MODELLERS)
    if (rattleWidget(paste(m, "_evaluate_checkbutton", sep=""))$getActive())
      models <- c(models, m)
  return(models)
}

current.evaluate.tab <- function()
{
  cp <- EVALUATE$getCurrentPage()
  return(EVALUATE$getTabLabelText(EVALUATE$getNthPage(cp)))
}


deactivate.rocr.plots <- function()
{
  rattleWidget("lift_radiobutton")$setSensitive(FALSE)
  rattleWidget("roc_radiobutton")$setSensitive(FALSE)
  rattleWidget("precision_radiobutton")$setSensitive(FALSE)
  rattleWidget("sensitivity_radiobutton")$setSensitive(FALSE)
  rattleWidget("risk_radiobutton")$setSensitive(FALSE)
}

activate.rocr.plots <- function()
{
  rattleWidget("lift_radiobutton")$setSensitive(TRUE)
  rattleWidget("roc_radiobutton")$setSensitive(TRUE)
  rattleWidget("precision_radiobutton")$setSensitive(TRUE)
  rattleWidget("sensitivity_radiobutton")$setSensitive(TRUE)
  rattleWidget("risk_radiobutton")$setSensitive(TRUE)
}

##----------------------------------------------------------------------
##
## Execution
##

executeEvaluateTab <- function()
{

  ## Work toward allowing mtype = "All" and then the ROCR plots are:
  ## rp.pr <- predict(crs$rpart, crs$dataset[-crs$sample, c(2:10,13)])[,2]
  ## rf.pr <- predict(crs$rf, crs$dataset[-crs$sample, c(2:10,13)], type="prob")[,2]
  ## tg <- crs$dataset[-crs$sample, c(2:10,13)]$Adjusted
  ## plot(performance(prediction(cbind(rp.pr, rf.pr), cbind(tg, tg)), "tpr", "fpr"))
  ## OR this to get colours, then add a legend
  ## plot(performance(prediction(rp.pr, tg), "tpr", "fpr"), col="red")
  ## plot(performance(prediction(rf.pr, tg), "tpr", "fpr"), col="blue", add=TRUE)
  ## THEN we might be able to simply use the code already here, but conditionally include the "add=TRUE"
  
  ## Ensure a dataset exists.

  if (noDatasetLoaded()) return()

  ## Obtain some background information
  
  #mtype <- getActiveModel()  # The chosen model type in the Evaluate tab.
  mtypes <- getEvaluateModels() 
  
  if (is.null(mtypes))
  {
    errorDialog("No model has been specified.",
                 "Please slect one or more from the list of models available.")
    return()
  }

  ## Ensure we recognise the model type.
  
  if (length(setdiff(mtypes, MODELLERS)) > 0)
  {
    errorDialog("This is a traditional 'You should not be here'",
                "error message! A model type is not recognised.",
                "We found the model types to be:", mtypes,
                "Please report this to Graham.Williams@togaware.com.")
    return()
  }

  ## Ensure there is a model for each that is selected.
  
  if (sum(sapply(mtypes, function(x) is.null(crs[[x]]))) > 0)
  {
    errorDialog("We should not be here. Some model has not been built?",
                mtypes)
    return()
  }

  ## Ensure the appropriate package is loaded (in the case, for
  ## example, when loading a project and going straight to Evaluate,
  ## and wanting to run predict.svm on new data).

  if (is.element(RF, mtypes) &&
      ! packageIsAvailable("randomForest", "evaluate this rf"))
    return()
  if (is.element(KSVM,mtypes) &&
      ! packageIsAvailable("kernlab", "evaluate this SVM"))
    return()
  
  ## Identify the data on which evaluation is to be performed.

  testset0 <- "crs$dataset"
  testname <- crs$dataname
  included <- getIncludedVariables() # Need all vars, including risk.

  addLogSeparator()

  if (rattleWidget("evaluate_training_radiobutton")$getActive())
  {
    infoDialog("You are using the same dataset to evaluate your model as you",
                "did to build it. This will give you an optimistic estimate",
                "of the performance of your model. You may want to choose",
                "to sample the dataset and evaluate the model on the",
                "test dataset, or else",
                "load a separate test dataset from a CSV File or a",
                "pre-existing R Dataset here.")

    if (rattleWidget("sample_checkbutton")$getActive())
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
  else if (rattleWidget("evaluate_testing_radiobutton")$getActive())
  {
    if (is.null(included))
      testset0 <- "crs$dataset[-crs$sample,]"
    else
      testset0 <- sprintf("crs$dataset[-crs$sample, %s]", included)
    testname <- sprintf("%s [test]", crs$dataname)
  }
  else if (rattleWidget("evaluate_csv_radiobutton")$getActive())
  {
    filename <- rattleWidget("evaluate_filechooserbutton")$getFilename()
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
    
    if (! is.null(filename) &&
        (is.null(crs$testname) || (basename(filename) != crs$testname)))
    {
      ## Fix filename for MS/Windows - otherwise eval/parse strips the \\.

      if (is.windows()) filename <- gsub("\\\\", "/", filename)

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
  else if (rattleWidget("evaluate_rdataset_radiobutton")$getActive())
  {
    dataset <- rattleWidget("evaluate_rdataset_combobox")$
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

  if (! is.null(crs$testname) && crs$testname != crs$dataname)
    for (c in colnames(crs$dataset))
      if (is.factor(crs$dataset[[c]]))
        levels(crs$testset[[c]]) <<- c(levels(crs$testset[[c]]),
                                       setdiff(levels(crs$dataset[[c]]),
                                               levels(crs$testset[[c]])))

  ## Default command for prediction from any model is predict(model,
  ## data). Here we tune the predict command to particular types of
  ## models where they have specific general requirements. We then
  ## modify the default predict command to generate either a
  ## prediction of the response or a probability of the class, as
  ## appropriate to the particular evaluator.
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
  
  if (is.element(RPART, mtypes))
  {
    testset[[RPART]] <- testset0
    predcmd[[RPART]] <- sprintf("crs$pr <<- predict(crs$rpart, %s)",
                                testset[[RPART]])

    ## For RPART, the default is to generate class probabilities for
    ## each output class, so ensure we instead generate the response.
  
    respcmd[[RPART]] <- gsub(")$", ', type="class")', predcmd[[RPART]])

    ## For RPART the default predict command generates the probabilities
    ## for each class and we assume we are interested in the final class
    ## (i.e., for binary classification we are interested in the 1's).
    
    probcmd[[RPART]] <- sprintf("%s[,2]", predcmd[[RPART]])
  }
    
  if (is.element(RF, mtypes))
  {
    testset[[RF]] <- testset0
    predcmd[[RF]] <- sprintf("crs$pr <<- predict(crs$rf, %s)",
                             testset[[RF]])

    ## The default for RF is to predict the class, so no
    ## modification of the predict command is required.

    respcmd[[RF]] <- predcmd[[RF]]

    ## For RF we request a probability with the type argument, and as
    ## with RPART we extract the column of interest (the last column).
  
    probcmd[[RF]] <- sprintf("%s[,2]",
                             gsub(")$", ', type="prob")', predcmd[[RF]]))

  }
    
  if (is.element(KSVM, mtypes))
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

    testset[[KSVM]] <- sprintf("na.omit(%s)", testset0)

    predcmd[[KSVM]] <- sprintf("crs$pr <<- predict(crs$ksvm, %s)",
                               testset[[KSVM]])

    ## The default for KSVM is to predict the class, so no
    ## modification of the predict command is required.

    respcmd[[KSVM]] <- predcmd[[KSVM]]

    ## For KSVM we request a probability with the type argument set to
    ## probability (but need prob.model=TRUE in model building). For SVM
    ## we request probabilities by setting probablity=TRUE and don't
    ## need the second column stuff (and in building the model we needed
    ## probability=TRUE).

    probcmd[[KSVM]] <- sprintf("%s[,2]",
                               gsub(")$",
                                    ', type="probabilities")',
                                    predcmd[[KSVM]]))
    ## For SVM: 
    ## probability.cmd <- sprintf("%s",
    ##                             gsub(")$",
    ##                                  ', probability=TRUE)',
    ##                                  probability.cmd))
  }
    
  if (is.element(GLM, mtypes))
  {
    ## GLM's predict removes rows with missing values, so we also need
    ## to ensure we remove rows with missing values here.
    
    testset[[GLM]] <- sprintf("na.omit(%s)", testset0)

    predcmd[[GLM]] <- sprintf("crs$pr <<- predict(crs$glm, %s)",
                              testset[[GLM]])

    ## For GLM, a response is a figure close to the class, either close
    ## to 1 or close to 0, so threshold it to be either 1 or 0. TODO
    ## Simplify this like?
    ##    response.cmd <- gsub("predict", "(predict",
    ##                         gsub(")$", ")>0.5)*1", response.cmd))

    respcmd[[GLM]] <- gsub("predict", "as.factor(as.vector(ifelse(predict",
                           gsub(")$", ', type="response") > 0.5, 1, 0)))',
                                predcmd[[GLM]]))

    ## For GLM, the response is a probability of the class.
  
    probcmd[[GLM]] <- gsub(")$", ', type="response")', predcmd[[GLM]])
  
  }
    
  if (is.element(GBM, mtypes))
  {
    testset[[GBM]] <- testset0

    ## For GBM the default needs to know the number of trees to include.

    predcmd[[GBM]] <- sprintf(paste("crs$pr <<- predict(crs$gbm, %s,",
                                    "n.trees=length(crs$gbm$trees))"),
                              testset[[GBM]])
    respcmd[[GBM]] <- predcmd[[GBM]]
    probcmd[[GBM]] <- predcmd[[GBM]]
  }

  ## Currently (and perhaps permanently) the ROCR package deals only
  ## with binary classification, as does my own Risk Chart.
  
  if (!rattleWidget("confusion_radiobutton")$getActive()
      && is.factor(crs$dataset[[crs$target]])
      && length(levels(crs$dataset[[crs$target]])) > 2)
  {
    errorDialog("The number of levels in the target is > 2.",
                 "Currently, Rattle's Risk chart, and the ROCR package",
                 "(which implements the Lift, ROC, Precision, and Specificity",
                 "charts) apply only for binary classification.",
                 "Either restructure your data for binary classificaiton,",
                 "or else suggest an alternative to the author of Rattle",
                 "at Graham.Williams@togaware.com!")
    return()
  }

  ## DISPATCH
  
  if (rattleWidget("confusion_radiobutton")$getActive())
    msg <- executeEvaluateConfusion(respcmd, testset, testname)
  else if (rattleWidget("risk_radiobutton")$getActive())
    msg <- executeEvaluateRisk(probcmd, testset, testname)
  else if (rattleWidget("roc_radiobutton")$getActive())
    msg <- executeEvaluateROC(probcmd, testset, testname)
  else if (rattleWidget("lift_radiobutton")$getActive())
    msg <- executeEvaluateLift(probcmd, testset, testname)
  else if (rattleWidget("precision_radiobutton")$getActive())
    msg <- executeEvaluatePrecision(probcmd, testset, testname)
  else if (rattleWidget("sensitivity_radiobutton")$getActive())
    msg <- executeEvaluateSensitivity(probcmd, testset, testname)
  else if (rattleWidget("score_radiobutton")$getActive())
    msg <- executeEvaluateScore(probcmd, testset, testname)

  if (! is.null(msg)) setStatusBar(msg)
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
                     START.LOG.COMMENT, mtype), no.start=TRUE)
    addToLog(sprintf("Obtain the response from the %s model.", mtype),
             gsub("<<-", "<-", respcmd[[mtype]]))
  
    result <- try(eval(parse(text=respcmd[[mtype]])), TRUE)
    if (inherits(result, "try-error"))
    {
      if (any(grep("has new level", result)) || any(grep("New levels",result)))
        errorDialog("It seems that the dataset on which the predictions",
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
      return()
    }
    
    addToLog("Now generate the confusion matrix.", confuse.cmd)

    confuse.output <- collect.output(confuse.cmd, TRUE)
  
    addToLog("Generate confusion matrix showing percentages.", percentage.cmd)

    percentage.output <- collect.output(percentage.cmd, TRUE)
  
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

  for (mtype in getEvaluateModels())
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

    eval(parse(text=probcmd[[mtype]]))
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
    appendTextview(TV, msg, collect.output(sprintf("crs$eval[%s,]", id), TRUE))

    ## Display the Risk Chart itself now.

    newPlot()
    eval(parse(text=plot.cmd))

    ## Display the AUC measures.

    auc <- calculateRiskAUC(crs$eval)
    appendTextview(TV, paste("The area under the Risk and Recall curves for ",
                             mtype, " model\n\n",
                             "Area under the Risk   (red)   curve: ",
                             sprintf("%d%% (%0.3f)\n",
                                     round(100*auc[1]), auc[1]),
                             "Area under the Recall (green) curve: ",
                             sprintf("%d%% (%0.3f)\n",
                                     round(100*auc[2]), auc[2]),
                             sep=""))
    
  }
  
  return("Generated Risk Charts.")

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
  if (!is.null(label))
  {
    text(x, 0, label, pos=4)
    text(x, 0, sprintf("%2.0f%%", x), pos=2)
    text(0, y2, sprintf("%2.0f%%", y2), pos=3, offset=0.2)
    text(0, y1, sprintf("%2.0f%%", y1), pos=3, offset=0.2)
    if (!is.null(pr))
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

calculateRiskAUC <- function(ev)
{
  len <- nrow(ev)
  ria <- ev$Caseload[len] * ev$Risk[len] / 2
  rea <- ev$Caseload[len] * ev$Recall[len] / 2

  for (i in (len-1):1)
  {
    ria <- ria +
      (ev$Caseload[i] - ev$Caseload[i+1]) * ev$Risk[i+1] +
      (ev$Caseload[i] - ev$Caseload[i+1]) * (ev$Risk[i] - ev$Risk[i+1]) / 2
    rea <- rea + 
      (ev$Caseload[i] - ev$Caseload[i+1]) * ev$Recall[i+1] +
      (ev$Caseload[i] - ev$Caseload[i+1]) * (ev$Recall[i] - ev$Recall[i+1]) / 2
  }
  return(c(ria, rea))
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
                      xleg=70, yleg=60,
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

  #
  # If proportions, convert to percentages
  #
  if (all(cl <= 1)) cl <- cl * 100
  if (all(re <= 1)) re <- re * 100
  if (!is.null(ri) & all(ri <= 1.5)) ri <- ri * 100 # Can sometimes be just >1
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
  if (!is.null(ri)) ri <- c(ri, 0)
  pr <- c(pr, NA)
  #
  # Also add the 100 point just in case?
  #
  if (cl[1] != 100)
  {
    cl <- c(100, cl)
    re <- c(100, re)
    if (!is.null(ri)) ri <- c(100, ri)
    pr <- c(min( pr[!is.na(pr)]), pr)
  }
  #
  # Now plot
  #
  par(lwd=2)
  plot(c(0,100), c(0,100), type='l', col=1,
       xlab="Caseload (%)", ylab="Performance (%)",
       ylim=c(0,100), xlim=c(0,100))
  grid.plot()
  if (!is.null(title))
    title(main=title, sub=paste("Rattle", Sys.time(), Sys.info()["user"]))
  points(re ~ cl, type='l', col=3, lty=5)
  points(pr ~ cl, type='l', col=4, lty=4)
  if (!is.null(ri)) points(ri ~ cl, type='l', col=2, lty=1)
  if (include.baseline) text(100, pr[1]+4, sprintf("%0.0f%%", pr[1]))
  # Optimal
  if (!is.null(optimal))
  {
    optimal.index <- which(abs(cl-optimal) == min(abs(cl-optimal)))
    if (length(optimal.index) > 1) optimal.index <- optimal.index[1]
    plotOptimalLine(optimal, ri[optimal.index], re[optimal.index],
                      pr[optimal.index], label=optimal.label)
  }
  # Chosen
  if (!is.null(chosen))
  {
    chosen.index <- which(abs(cl-chosen) == min(abs(cl-chosen)))
    if (length(chosen.index) > 1) chosen.index <- chosen.index[1]
    plotOptimalLine(chosen, ri[chosen.index], re[chosen.index],
                      label=chosen.label, col="grey")
  }

  legend <- c()
  lty <- c()
  col <- c()
  if (!is.null(ri))
  {
    legend <- c(legend, risk.name)
    lty <- c(lty, 1)
    col <- c(col, 2)
  }  
  legend <- c(legend, recall.name, precision.name)
  lty <- c(lty,5,4)
  col <- c(col,3,4)
  if (!is.null(optimal))
  {
    legend <- c(legend, "Optimal")
    lty <- c(lty,6)
    col <- c(col,"plum")
  }
  if (!is.null(chosen))
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
  if (!is.null(show.knots))
  {
    len <- length(cl)
    text(cl[c(-1,-len)]-2, ri[c(-1,-len)]+3, rev(show.knots)[-1])
  }
  if (dev != "") dev.off()
}

##----------------------------------------------------------------------
##
## EVALUATE LIFT CHART
##

executeEvaluateLift <- function(predcmd, testset, testname)
{
  library.cmd <- "require(ROCR, quietly=TRUE)"
  newPlot()
  addplot <- "FALSE"

  nummodels <- length(predcmd)
  mcolors <- rainbow(nummodels)
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
    
    addToLog("Display Lift Chart using the ROCR package.", library.cmd)
    eval(parse(text=library.cmd))
    
    addToLog(sprintf("Generate a Lift Chart for the %s model on %s.",
                     mtype, testname),
             gsub("<<-", "<-", predcmd[[mtype]]), "\n", plot.cmd)
    eval(parse(text=predcmd[[mtype]]))
    eval(parse(text=plot.cmd))
    
  }
  legendcmd <- paste('legend("topright",',
                     sprintf("c(%s),",
                             paste('"', getEvaluateModels(), '"',
                                   sep="", collapse=",")),
                     sprintf('col=rainbow(%d), lty=1:%d,',
                             nummodels, nummodels),
                     'title="Models", inset=c(0.05, 0.05))')
  addToLog("Add a legend to the plot.", legendcmd)
  eval(parse(text=legendcmd))
  
  decorcmd <- paste(genPlotTitleCmd("Lift Chart", "", testname),
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
  library.cmd <- "require(ROCR, quietly=TRUE)"
  newPlot()
  addplot <- "FALSE"

  nummodels <- length(predcmd)
  mcolors <- rainbow(nummodels)
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

    addToLog("Plot an ROC curve using the ROCR package.", library.cmd)
    eval(parse(text=library.cmd))
  
    addToLog(sprintf("Generate an ROC Curve for the %s model on %s.",
                     mtype, testname),
             gsub("<<-", "<-", predcmd[[mtype]]), "\n", plot.cmd)
    eval(parse(text=predcmd[[mtype]]))
    eval(parse(text=plot.cmd))

    ## Report the area under the curve.
  
    aucCmd <- paste("performance(prediction(crs$pr, ",
                    sprintf("%s$%s),", testset[[mtype]], crs$target),
                    '"auc")', sep="")
    addToLog("Calculate the area under the curve for the plot.", aucCmd)
    auc <- eval(parse(text=aucCmd))
    appendTextview(TV, paste("Area under the ROC curve for the",
                             sprintf("%s model on %s is %0.4f",
                                     mtype, testname,
                                     attr(auc, "y.values"))))
  }
  lines(c(0,1), c(0,1)) # Baseline

  legendcmd <- paste('legend("bottomright",',
                     sprintf("c(%s),",
                             paste('"', getEvaluateModels(), '"',
                                   sep="", collapse=",")),
                     sprintf('col=rainbow(%d), lty=1:%d,',
                             nummodels, nummodels),
                     'title="Models", inset=c(0.05, 0.05))')
  addToLog("Add a legend to the plot.", legendcmd)
  eval(parse(text=legendcmd))
  
  decorcmd <- paste(genPlotTitleCmd("ROC Curve", "", testname),
                    '\ngrid()', sep="")
  addToLog("Add decorations to the plot.", decorcmd)
  eval(parse(text=decorcmd))

  return(sprintf("Generated ROC Curves on %s.", testname))
}
  
##----------------------------------------------------------------------
##
## EVALUATE PRECISION PLOT
##

executeEvaluatePrecision <- function(predcmd, testset, testname)
{
  library.cmd <- "require(ROCR, quietly=TRUE)"
  newPlot()
  addplot <- "FALSE"

  nummodels <- length(predcmd)
  mcolors <- rainbow(nummodels)
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
  
    addToLog("Precision/Recall Plot using the ROCR package", library.cmd)
    eval(parse(text=library.cmd))

    addToLog(sprintf("Generate a Precision/Recall Plot for the %s model on %s.",
                     mtype, testname),
             gsub("<<-", "<-", predcmd[[mtype]]), "\n", plot.cmd)
    eval(parse(text=predcmd[[mtype]]))
    eval(parse(text=plot.cmd))
  }

  legendcmd <- paste('legend("bottomleft",',
                     sprintf("c(%s),",
                             paste('"', getEvaluateModels(), '"',
                                   sep="", collapse=",")),
                     sprintf('col=rainbow(%d), lty=1:%d,',
                             nummodels, nummodels),
                     'title="Models", inset=c(0.05, 0.05))')
  addToLog("Add a legend to the plot.", legendcmd)
  eval(parse(text=legendcmd))
  
  decorcmd <- paste(genPlotTitleCmd("Precision/Recall Chart", "", testname),
                    '\ngrid()', sep="")
  addToLog("Add decorations to the plot.", decorcmd)
  eval(parse(text=decorcmd))
  
  return(sprintf("Generated Precision/Recall Plot on %s.", testname))
}

##----------------------------------------------------------------------
##
## EVALUATE SENSITIVITY PLOT
##

executeEvaluateSensitivity <- function(predcmd, testset, testname)
{
  library.cmd <- "require(ROCR, quietly=TRUE)"
  newPlot()
  addplot <- "FALSE"

  nummodels <- length(predcmd)
  mcolors <- rainbow(nummodels)
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
             library.cmd)
    eval(parse(text=library.cmd))

    addToLog(sprintf("Generate Sensitivity/Specificity Plot for %s model on %s.",
                     mtype, testname),
             gsub("<<-", "<-", predcmd[[mtype]]), "\n", plot.cmd)

    eval(parse(text=predcmd[[mtype]]))
    eval(parse(text=plot.cmd))
  }
  legendcmd <- paste('legend("bottomleft",',
                     sprintf("c(%s),",
                             paste('"', getEvaluateModels(), '"',
                                   sep="", collapse=",")),
                     sprintf('col=rainbow(%d), lty=1:%d,',
                             nummodels, nummodels),
                     'title="Models", inset=c(0.05, 0.05))')
  addToLog("Add a legend to the plot.", legendcmd)
  eval(parse(text=legendcmd))
  
  decorcmd <- paste(genPlotTitleCmd("Sensitivity/Specificity (tpr/tnr)", "",
                                    testname),
                    '\ngrid()', sep="")
  addToLog("Add decorations to the plot.", decorcmd)
  eval(parse(text=decorcmd))

  return(sprintf("Generated Sensitivity/Specificity Plot on %s.", testname))
}

##----------------------------------------------------------------------
##
## SCORE - Save the probability scores for each selected model to a file
## Would be best into one file but testset may be different for each.
##

executeEvaluateScore <- function(predcmd, testset, testname)
{
  for (mtype in getEvaluateModels())
  {

    addToLog(sprintf("Save the probability scores to file for the %s model on %s.",
                     mtype, testname),
             gsub("<<-", "<-", predcmd[[mtype]]))

    eval(parse(text=predcmd[[mtype]]))

    ## Determine an appropriate filename (fixed for now)
    
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
    ## remove everything after, and replace with "]"

    if (length(grep(",", scoreset)) > 0)
    {
      scoreset = gsub(",.*", ",]", scoreset)
    }
    scoreset <- sprintf('subset(%s, select=getSelectedVariables("ident"))',
                        scoreset)

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
##
## PMML Generation
##
on_export_pmml_activate <- function(action, window)
{
  require(XML, quietly=TRUE)

  if (noDatasetLoaded()) return()

  ct <- NOTEBOOK$getCurrentPage()
      
  if (ct == NOTEBOOK.CLUSTER.TAB)
    
    if (rattleWidget("kmeans_radiobutton")$getActive())
    {
      if (is.null(crs$kmeans))
      {
        errorDialog("No KMeans cluster is available. Be sure to build",
                     "a cluster before trying to export it! You will need",
                     "to press the Execute button (F5) in order to build the",
                     "KMeans cluster.")
        return()
      }
      else
      {
        write(collect.output("pmml.kmeans(crs$kmeans)", TRUE),
              file=sprintf("%s-kmeans.pmml", gsub(".csv", "", crs$dataname)))
        infoDialog("The PMML file",
                    sprintf('"%s-kmeans.pmml"', gsub(".csv", "", crs$dataname)),
                    "has been written.")
      }
    }
    else if (rattleWidget("hclust_radiobutton")$getActive())
    {
      errorDialog("PMML export for hierarchical clustering is not yet",
                   "implemented.")
      return()
    }

  if (ct == NOTEBOOK.MODEL.TAB)
  {
    if (rattleWidget("rpart_radiobutton")$getActive())
    {
      if (is.null(crs$rpart))
      {
        errorDialog("No decision tree model is available. Be sure to build",
                     "the model before trying to export it! You will need",
                     "to press the Execute button (F5) in order to build the",
                     "model.")
        return()
      }
      else
      {
        write(collect.output("pmml.rpart(crs$rpart)", TRUE),
              file=sprintf("%s-rpart.pmml", gsub(".csv", "", crs$dataname)))
        infoDialog("The PMML file",
                    sprintf('"%s-rpart.pmml"', gsub(".csv", "", crs$dataname)),
                    "has been written.")
      }
    }
    else
    {
      errorDialog("PMML export for this model is not yet implemented.")
      return()
    }
  }
}

pmml.kmeans <- function(cl)
{
  
  ## First collect the required information

  number.of.fields <- ncol(cl$centers)
  field.names <-  colnames(cl$centers)
  number.of.clusters <- length(cl$size)
  cluster.names <- rownames(cl$centers)

  ## Root node

  pmml <- xmlNode("PMML")

  ## DataDictionary child node

  data.dictionary <- xmlNode("DataDictionary",
                             attrs=c(numderOfFields=number.of.fields))
  data.fields <- list()
  for (i in 1:number.of.fields)
  {
    data.fields[[i]] <- xmlNode("DataField",
                                attrs=c(name=field.names[i]))
  }
  data.dictionary$children <- data.fields
  pmml$children[[1]] <- data.dictionary
  #
  # ClusteringModel root node
  #
  clustering.model <- xmlNode("ClusteringModel",
                              attrs=c(algorithmName="KMeans",
                                numberOfClusters=number.of.clusters))
  clusters <- list()
  for (i in 1:number.of.clusters)
  {
    clusters[[i]] <- xmlNode("Cluster",
                             attrs=c(name=cluster.names[i],
                               size=cl$size[i]),
                             xmlNode("Array",
                                     attrs=c(n=number.of.fields),
                                     paste(cl$centers[i,], collapse=" ")))
  }
  clustering.model$children <- clusters
  pmml$children[[2]] <- clustering.model
  #
  # All done
  #
  return(pmml)
}

pmml.rpart <- function(rp)
{
  if (!inherits(rp, "rpart")) stop("Not a legitimate rpart tree")

  ## Collect the required information

  field.names <- as.character(rp$frame$var) # GET UNIQUE LIST.....
  field.names <- setdiff(union(field.names, field.names), "<leaf>")
  number.of.fields <- length(field.names)
  tree.nodes <- rownames(rp$frame)
  rule.paths <- path.rpart(rp, node=c(tree.nodes), print.it=FALSE)
  
  ## Root node
  
  pmml <- xmlNode("PMML", attrs=c(version="3.0"))

  ## Header

  header <- xmlNode("Header",
                    attrs=c(copyright=(paste("Copyright (c) Togaware, 2006.",
                      "All Rights Reserved."))))
  header[[1]] <- xmlNode("Application",
                         attrs=c(name="Rattle",
                           version=REVISION))

  pmml$children[[1]] <- header
  
  ## DataDictionary child node
  
  data.dictionary <- xmlNode("DataDictionary",
                             attrs=c(numderOfFields=number.of.fields))
  data.fields <- list()
  for (i in 1:number.of.fields)
  {
    data.fields[[i]] <- xmlNode("DataField",
                                attrs=c(name=field.names[i]))
  }
  data.dictionary$children <- data.fields
  pmml$children[[2]] <- data.dictionary

  ## Tree Node: Generate a rule set for now - simpler that a decision
  ## tree.
  
##   tree.model <- xmlNode("TreeModel",
##                         attrs=c(modelName=crs$dataname,
##                           functionName="classification",
##                           splitCharacteristic="binary",
##                           algorithmName="rpart"))

  tree.model <- xmlNode("RuleSetModel",
                        attrs=c(modelName=crs$dataname,
                          functionName="classification",
                          splitCharacteristic="binary",
                          algorithmName="rpart"))

  ## Mining Schema
  
  mining.fields <- list()
  for (i in 1:number.of.fields)
  {
    mining.fields[[i]] <- xmlNode("MiningField",
                                  attrs=c(name=field.names[i],
                                    usageType="active"))
  }
  target <- attr(rp$terms,"variables")[[2]]
  mining.fields[[i+1]] <- xmlNode("MiningField",
                                  attrs=c(name=target,
                                    usageType="predicted"))

  mining.schema <- xmlNode("MiningSchema")
  mining.schema$children <- mining.fields
  tree.model[[1]] <- mining.schema

  ## Add in actual tree nodes.

  rule.set <- xmlNode("RuleSet")
  rule.set$children[[1]] <- xmlNode("RuleSelectionMethod",
                                    attrs=c(criterion="firstHit"))
  
  ## Visit each leaf node to generate a rule.

  ordered <- rev(sort(rp$frame$yval2[,5], index=TRUE)$ix)
  names <- row.names(rp$frame)
  next.child <- 2
  for (i in ordered)
  {
    if (rp$frame[i,1] == "<leaf>")
    {
      simple.rule <- xmlNode("SimpleRule",
                             attrs=c(id=sprintf("R%03d", as.integer(names[i])),
                               recordCount=rp$frame[i,]$n))
      pth <- path.rpart(rp, nodes=as.numeric(names[i]), print.it=FALSE)
      pth <- unlist(pth)[-1]
      if (length(pth) != 0)
      {
        predicate <- xmlNode("CompoundPredicate",
                             attrs=c(booleanOperator="and"))
        for (p in (1:length(pth)))
        {
          f <- unlist(strsplit(pth[p], "<|>=|="))[[1]]
          o <- ifelse(length(grep("<", pth[p]))>0, "lessThen",
               ifelse(length(grep(">=", pth[p]))>0, "greaterOrEqual",
               ifelse(length(grep("=", pth[p]))>0, "equal", "DONTKNOW")))
          v <- unlist(strsplit(pth[p], "<|>=|="))[[2]]
          predicate$children[[p]] <- xmlNode("SimplePredicate",
                                             attrs=c(field=f,
                                               operator=o,
                                               value=v))
        }
      }
      simple.rule$children[[1]] <- predicate
      rule.set$children[[next.child]] <- simple.rule
      next.child <- next.child + 1
    }
  }

  tree.model[[2]] <- rule.set
  
  ## Add to the top level structure.
  
  pmml$children[[3]] <- tree.model
  
  return(pmml)
}

########################################################################

## General Menu Callbacks

on_save_menu_activate <- function(action, window) {saveProject()}
on_delete_menu_activate <- notImplemented

## Map the unchanged glade defaults

on_new1_activate <- notImplemented

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
    setCopyright("Copyright (C) 2006 Graham.Williams@togaware.com")
}

on_open_activate <- function(action, window) {loadProject()}
on_paste1_activate <- notImplemented
on_save_as_activate <- function(action, window) {saveProject()}
on_copy1_activate <- notImplemented

on_tooltips_activate <- function(action, window)
{
  infoDialog("Currently this functionality is not implemented.",
              "It is awaiting some insight into how to get hold of",
              "the glade GtkTooltips group, which can then be",
              "disabled or enabled as requested.")
}
  
##----------------------------------------------------------------------

## Button Callbacks

on_new_button_clicked <- notImplemented
on_open_button_clicked <- function(action, window) { loadProject() }
on_save_button_clicked <- function(action, window) { saveProject() }

getTextContents <- function(TV)
{
  log.buf <- rattleWidget(TV)$getBuffer()
  start <- log.buf$getStartIter()$iter
  end <- log.buf$getEndIter()$iter
  return(log.buf$getText(start, end))
}

saveProject <- function()
{

  ## Pre-conditions
  
  if (noDatasetLoaded()) return()
  if (variablesHaveChanged("saving the project")) return()

  ## Obtain filename to save to
  
  dialog <- gtkFileChooserDialog("Save Project", NULL, "save",
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-save", GtkResponseType["accept"])

  dialog$setCurrentName(get.stem(crs$dataname))

  ff <- gtkFileFilterNew()
  ff$setName("Rattle Files")
  ff$addPattern("*.rattle")
  dialog$addFilter(ff)

  ff <- gtkFileFilterNew()
  ff$setName("RData Files")
  ff$addPattern("*.Rdata")
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

  if (get.extension(save.name) != "rattle")
    save.name <- sprintf("%s.rattle", save.name)
    
  if (file.exists(save.name))
    if (is.null(questionDialog("The rattle project file", save.name,
                                "already exists. Do you want to overwrite",
                                "this file?")))
      return()

  ## Save all of the text views to be restored on a load.
  ## Put the following into a function and call for each textview.

  crs$text$data <<- getTextContents("data_textview")
  crs$text$summary <<- getTextContents("summary_textview")
  crs$text$correlation <<- getTextContents("correlation_textview")
  crs$text$prcomp <<- getTextContents("prcomp_textview")
  crs$text$kmeans <<- getTextContents("kmeans_textview")
  crs$text$rpart <<-  getTextContents("rpart_textview")
  crs$text$rf <<-  getTextContents("rf_textview")
  crs$text$esvm <<-  getTextContents("esvm_textview")
  crs$text$ksvm <<-  getTextContents("ksvm_textview")
  crs$text$glm <<-  getTextContents("glm_textview")
  crs$text$gbm <<-  getTextContents("gbm_textview")
  crs$text$risk <<- getTextContents("risk_textview")
  crs$text$roc <<- getTextContents("roc_textview")
  crs$text$log <<- getTextContents("log_textview")

  ## Save Distribution variable selections

  crs$boxplots <<- getSelectedVariables("boxplot")
  crs$hisplots <<- getSelectedVariables("hisplot")
  crs$cumplots <<- getSelectedVariables("cumplot")
  crs$benplots <<- getSelectedVariables("benplot")
  crs$barplots <<- getSelectedVariables("barplot")
  crs$dotplots <<- getSelectedVariables("dotplot")

  ## Save Sample information

  crs$seed <<- rattleWidget("sample_seed_spinbutton")$getValue()
  
  ## Save Model options

  crs$rpart$priors <<- rattleWidget("rpart_priors_entry")$getText()
  crs$rpart$loss   <<- rattleWidget("rpart_loss_entry")$getText()
  crs$rpart$split  <<- rattleWidget("rpart_minsplit_spinbutton")$getValue()
  crs$rpart$depth  <<- rattleWidget("rpart_maxdepth_spinbutton")$getValue()
  crs$rpart$cp     <<- rattleWidget("rpart_cp_spinbutton")$getValue()
  crs$rpart$bucket <<- rattleWidget("rpart_minbucket_spinbutton")$getValue()

  crs$rf$trees     <<- rattleWidget("rf_ntree_spinbutton")$getValue()
  crs$rf$vars      <<- rattleWidget("rf_mtry_spinbutton")$getValue()
  crs$rf$sample    <<- rattleWidget("rf_sampsize_spinbutton")$getValue()
  crs$rf$import    <<- rattleWidget("rf_importance_checkbutton")$getActive()
  crs$rf$proximity <<- rattleWidget("rf_proximity_checkbutton")$getActive()

  crs$glm$family   <<- rattleWidget("glm_family_comboboxentry")$getActive()
  
  save(crs, file=save.name, compress=TRUE)

  setStatusBar("The current project has been saved to", save.name)
}

restoreTextContents <- function(TV, text)
{
  clearTextview(TV)
  rattleWidget(TV)$setWrapMode("none")
  if (is.null(text))
    rattleWidget(TV)$getBuffer()$setText("")
  else
    rattleWidget(TV)$getBuffer()$setText(text)
}

loadProject <- function()
{
  ## Check if crs exists and if so warn about losing the current project.

  if ( ! is.null(listBuiltModels()) )
  {
    if (is.null(questionDialog("You have chosen to load a project.",
                                "This will clear the old project (dataset and",
                                "models) which may not have been saved.",
                                "Do you wish to continue, and lose the old",
                                "project? If you choose not to continue",
                                "you can save the project, and then load",
                                "the new project.")))
        
      return()
  }

  ## Request the rattle filename to be loaded

  dialog <- gtkFileChooserDialog("Open Project", NULL, "load",
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-open", GtkResponseType["accept"])

  ff <- gtkFileFilterNew()
  ff$setName("Rattle Files")
  ff$addPattern("*.rattle")
  dialog$addFilter(ff)

  ff <- gtkFileFilterNew()
  ff$setName("RData Files")
  ff$addPattern("*.Rdata")
  dialog$addFilter(ff)

  ff <- gtkFileFilterNew()
  ff$setName("All Files")
  ff$addPattern("*")
  dialog$addFilter(ff)
  
  if (dialog$run() == GtkResponseType["accept"])
  {
    load.name <- dialog$getFilename()
    dialog$destroy()
  }
  else
  {
    dialog$destroy()
    return()
  }

  if (!file.exists(load.name))
    if (is.null(questionDialog("The rattle project file", load.name,
                                "does not exist?")))
      return()
  
  ## Load the file

  set.cursor("watch")

  NOTEBOOK$setCurrentPage(0)
  load(load.name)
  
  ## Now update all appropriate textviews and associated data.

  resetRattle()  # Seems appropriate to clear out the crs
  setRattleTitle(basename(load.name))

  ## DATA

  rattleWidget("csv_filechooserbutton")$setFilename("")
  
  restoreTextContents("data_textview", crs$text$data)
  
  crs$dataname <<- crs$dataname
  crs$dataset <<- crs$dataset

  resetVariableRoles(colnames(crs$dataset), nrow(crs$dataset),
                     crs$input, crs$target, crs$risk, crs$ident, crs$ignore,
                     crs$boxplot, crs$hisplot, crs$cumplot, crs$benplot,
                     crs$barplot, crs$dotplot)

  if (!is.null(crs$risk))
    rattleWidget("evaluate_risk_label")$setText(crs$risk)
  
  ## VARIABLES

  if (! is.null(crs$weights))
  {
    weights.display <- gsub('crs\\$dataset\\$', '', crs$weights)
    rattleWidget("weight_entry")$setText(weights.display)
    the.weight <- sprintf("Weights: %s",weights.display)
    rattleWidget("rpart_weights_label")$setText(the.weight)
    crs$weights <<- crs$weights
  }

  ## SAMPLE

  crs$sample   <<- crs$sample
  crs$seed     <<- crs$seed

  if (!is.null(crs$sample))
  {
    nrows <- nrow(crs$dataset)
    srows <- length(crs$sample)
    per <- 100*srows/nrows
    rattleWidget("sample_checkbutton")$setActive(TRUE)
    rattleWidget("sample_count_spinbutton")$setRange(1,nrows)
    rattleWidget("sample_count_spinbutton")$setValue(srows)
    if (! is.null(crs$seed))
      rattleWidget("sample_seed_spinbutton")$setValue(crs$seed)
    else
      rattleWidget("sample_seed_spinbutton")$setValue(123)
    rattleWidget("sample_percentage_spinbutton")$setValue(per)
  }
  
  ## EXPLORE
  
  restoreTextContents("summary_textview", crs$text$summary)
  restoreTextContents("correlation_textview", crs$text$correlation)
  restoreTextContents("prcomp_textview", crs$text$prcomp)

  ## CLUSTER
  
  crs$kmeans   <<- crs$kmeans
  restoreTextContents("kmeans_textview", crs$text$kmeans)
  crs$hclust   <<- crs$hclust

  ## MODELS

  crs$page     <<- crs$page
  crs$smodel   <<- crs$smodel
  crs$rpart    <<- crs$rpart
  restoreTextContents("rpart_textview", crs$text$rpart)
  crs$rf       <<- crs$rf
  restoreTextContents("rf_textview", crs$text$rf)
  crs$svm      <<- crs$svm
  restoreTextContents("esvm_textview", crs$text$esvm)
  crs$ksvm     <<- crs$ksvm
  restoreTextContents("ksvm_textview", crs$text$ksvm)
  crs$glm      <<- crs$glm
  restoreTextContents("glm_textview", crs$text$glm)
  crs$gbm      <<- crs$gbm
  restoreTextContents("gbm_textview", crs$text$gbm)

  if (! is.null(crs$rpart$priors))
    rattleWidget("rpart_priors_entry")$setText(crs$rpart$priors)
  if (! is.null(crs$rpart$loss))
    rattleWidget("rpart_loss_entry")$setText(crs$rpart$loss)
  if (! is.null(crs$rpart$split))
    rattleWidget("rpart_minsplit_spinbutton")$setValue(crs$rpart$split)
  if (! is.null(crs$rpart$depth))
    rattleWidget("rpart_maxdepth_spinbutton")$setValue(crs$rpart$depth)
  if (! is.null(crs$rpart$cp))
    rattleWidget("rpart_cp_spinbutton")$setValue(crs$rpart$cp)
  if (! is.null(crs$rpart$bucket))
    rattleWidget("rpart_minbucket_spinbutton")$setValue(crs$rpart$bucket)

  if (! is.null(crs$rf$trees))
    rattleWidget("rf_ntree_spinbutton")$setValue(crs$rf$trees)
  if (! is.null(crs$rf$vars))
    rattleWidget("rf_mtry_spinbutton")$setValue(crs$rf$vars)
  if (! is.null(crs$rf$sample))
    rattleWidget("rf_sampsize_spinbutton")$setValue(crs$rf$sample)
  if (! is.null(crs$rf$import))
    rattleWidget("rf_importance_checkbutton")$setActive(crs$rf$import)
  if (! is.null(crs$rf$proximity))
    rattleWidget("rf_proximity_checkbutton")$setActive(crs$rf$proximity)

  if (! is.null(crs$svm))
    rattleWidget("e1071_radiobutton")$setActive(TRUE)

  if (! is.null(crs$glm$family))
    rattleWidget("glm_family_comboboxentry")$setActive(crs$glm$family)

  ## EVALUATE

  ## 060930 Only restore the checkbuttons that have a model
  ## Currently, bleow is still the old version.
  
  combo <- rattleWidget("model_evaluate_comboboxentry")
  combo$getModel()$clear()
#  combo$appendText("All")
  lapply(listBuiltModels(), combo$appendText)
  combo$setActive(0)
  restoreTextContents("risk_textview", crs$text$risk)

  restoreTextContents("roc_textview", crs$text$roc )

  
  crs$perf     <<- crs$perf
  crs$eval     <<- crs$eval
  crs$testset  <<- crs$testset
  crs$testname <<- crs$testname
  
  ## LOG
  
  restoreTextContents("log_textview", crs$text$log)
  addLogSeparator(paste("Reloaded the project from", load.name))

  set.cursor()
  
  setStatusBar("Project loaded from", load.name)

}

on_execute_button_clicked <- function(action, window)
{
  ## Wrap up the call with a try so that the watch cursor turns off
  ## even on error.
  
  setStatusBar()
  set.cursor("watch")
  try(dispatchExecuteButton())
  set.cursor()
}

dispatchExecuteButton <- function()
{
  
  ## Check which tab of notebook and dispatch to appropriate execute action

  ct <- NOTEBOOK$getCurrentPage()
  
  if (ct == NOTEBOOK.DATA.TAB) 
  {
    executeDataTab()
  }
  else if (ct == NOTEBOOK.EXPLORE.TAB)
  {
    executeExploreTab()
  }
  else if (ct == NOTEBOOK.VARIABLES.TAB)
  {
    execute.variables.tab()
  }
  else if (ct == NOTEBOOK.SAMPLE.TAB)
  {
    executeSampleTab()
  }
  else if (ct == NOTEBOOK.CLUSTER.TAB)
  {
    execute.cluster.tab()
  }
  else if (ct == NOTEBOOK.MODEL.TAB)
  {
    execute.model.tab()
  }
  else if (ct == NOTEBOOK.EVALUATE.TAB)
  {
    
    ## The wrap mode of the confusion_textview may have been set to
    ## word wrap when a model was Executed if it had more than 2
    ## classes, since a message is printed about ROCR etc not handling
    ## any more than 2 classes.
    
    rattleWidget("confusion_textview")$setWrapMode("none")
    executeEvaluateTab()
  }
}

##----------------------------------------------------------------------

## Miscellaneous callbacks

on_notebook_switch_page <- function(notebook, window, page)
{
  ## notebook is the GtkNotebook object.
  ## window is ??.
  ## page is the index of the page switched to.

  #ct <- current_(page)

  ## Blank the status bar whenever we change pages
  
  setStatusBar()

  if (page == NOTEBOOK.EVALUATE.TAB)
  {
    ## On moving to the EVALUATE page, ensure each built model's
    ## checkbox is active, and check the active model's checkbox, but
    ## leave all the other as they are.

    mtypes <- listBuiltModels()
    
    if (! is.null(mtypes) )
    {
      ## We have some models, so make sure their checkboxes are
      ## sensitive.

      lapply(mtypes,
             function(x) rattleWidget(paste(x, "_evaluate_checkbutton",
                                            sep=""))$setSensitive(TRUE))
      
      if (is.null(crs$page) || crs$page == NOTEBOOK.MODEL.TAB)
      {
        ## By default check the current model's check button if we
        ## have just come from the MODEL page. This makes it easy when
        ## swaping from the Model page to this page to evaluate the
        ## just built model (usually). The NULL test on crs$page
        ## simply covers the loading of a project that did not save
        ## the crs$page, as was the case for old project files.
        rattleWidget(paste(currentModelTab(), "_evaluate_checkbutton",
                           sep=""))$setActive(TRUE)
      }
    }
  }
  
  ## When changing to the LOG page desensitise the Execute button.
  ## TODO: The execute button could be used to save the log file.
  
  if (page == NOTEBOOK.LOG.TAB)
    rattleWidget("execute_button")$setSensitive(FALSE)
  else
    rattleWidget("execute_button")$setSensitive(TRUE)

  ## Record the current page so when we change we know which was last.

  crs$page <<- page

}

########################################################################

## HELP

popup.textview.help.window <- function(topic)
{
  collect.output(sprintf("help(%s, htmlhelp=TRUE)", topic), TRUE)
}

further.help <- function(msg)
{
  if (is.null(questionDialog(paste(gsub(" <<>> ", "\n\n",
                                         gsub("\n", " ", msg)),
                                    "Would you like to view the R help?",
                                    sep="\n\n"))))
    return(FALSE)
  else
    return(TRUE)
}

no.further.help <- function(msg)
{
  infoDialog(paste(gsub(" <<>> ", "\n\n", gsub("\n", " ", msg))))
}

on_help_general_activate <- function(action, window)
{
  no.further.help("Rattle is an Open Source project
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
  no.further.help("There are many
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
  if (further.help("Rattle can load data from
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
    popup.textview.help.window("read.csv") }

on_help_rdata_file_activate <- function(action, window)
{
  no.further.help("Choose this if you have data stored in an R dataset
(usually with a filename extension of .Rdata).
The named file will be loaded and any data frames found in there will
be listed for selection.")
}

on_help_rdataset_activate <- function(action, window)
{
  no.further.help("Rattle can use a dataset that is already loaded
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
  if(further.help("Rattle can establish a connection to a database
through the RODBC package. Tables avilable in the database will then be
listed for selection."))
  {
    require(RODBC, quietly=TRUE)
    popup.textview.help.window("RODBC")
  }
}

on_help_roles_activate <- function(action, window)
{
  no.further.help("The Variables tab allows you to select roles for the
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
  no.further.help("Weights are used by variable modellers to identify
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

on_help_summary_activate <- function(action, window)
{
  if (further.help("A summary of the dataset includes various pieces of
information about each of the variables of the dataset.
<<>>
For numeric data, this
will include the minimum, maximum, median (the value of the variable at the
midpoint of the dataset), mean (the average value of the variable),
and the first and third quartiles (25% of the data has values below the first
quartile, and another 25% of the data has values above the third quartile).
<<>>
For categorical data the frequency distribution across the values is listed.
If there are too many possible values, then only the top few are listed, with
the remainder counted as Other.
<<>>
The R function summary() is used for the basic summary.
<<>>
Additional information is provided for numeric variables, including the
kurtosis and skewness. The kurtosis is a measure of the nature of the peaks
in the distribution of the data. A high kurtosis indicates a sharper peak
and fatter tails while a lower kurtosis indicates a more rounded peak
with wider shoulders.
<<>>
The skewness indicates the assymetry of the distribution. A positive skew
indicates that the tail to the right is longer, and a negative skew that the
tail to the left is longer.
<<>>
The fBasics package is used to obtain the kurtosis and skewness."))
    {
      popup.textview.help.window("summary")
      if (packageIsAvailable("fBasics"))
      {
        require(fBasics, quietly=TRUE)
        popup.textview.help.window("kurtosis")
        popup.textview.help.window("skewness")
      }
    }
}

on_help_correlation_activate <- function(action, window)
{
  if (further.help( "A pairwise correlation between each numeric variable
is calculated and displayed numerically in the text window whilst
a graphic plot is also generated. The plot uses circles and colour to
indicate the strength of any correlation.
<<>>
The R function cor() is used to produce the correlation data."))
    popup.textview.help.window("cor")
}

on_help_hierarchical_correlation_activate <- function(action, window)
{
  if (further.help( "A hierarchical cluster
of the correlations between the variables of the dataset is generated, and
presented pictorially as a dendrogram.  From the dendrogram you can
see groups of variables that are highly correlated. The code uses the
cor() function to gnerate the correlations between the variables, the
hclust() function to perform the hierarchical clustering, and converts
the result to a dendrogram, using as.dendrogram(), for plotting."))
  {
    popup.textview.help.window("cor")
    popup.textview.help.window("hclust")
    popup.textview.help.window("dendrogram")
  }
 
}

on_help_principal_components_activate <- function(action, window)
{
  if (further.help("Principal components analysis identifies
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
    popup.textview.help.window("prcomp")
}

on_help_kmeans_activate <- function(action, window)
{
  if (further.help("KMeans is a traditional approach to clustering.
In addition to building a cluster, a discriminate coordinates plot
is generated, using tha package fpc, as a display of the clusters."))
  {
    popup.textview.help.window("kmeans")
    popup.textview.help.window("plotcluster")
  }
}

on_help_rpart_activate <- function(action, window)
{
  if (further.help("A decision tree is quite the typical data mining tool,
used widely for its ease of interpretation. It consists of a root node
split by a single variable into two partitions. In turn, these two new
nodes may then each be further split on a single (and usually
different) variable. This divide and conquering continues until no
further splitting would improve the performance of the model.
<<>>
While a choice of measures are available to select a variable to split
the dataset on, the Gini measure is used, and generally is no
different to the information measure for binary classification. To
explore the alternatives, copy the relevant code from the Log and
paste it into the R Console and change any of the options.
<<>>
Common options that a user may change from their default values are
available. Tooltips with each of them provide further details. Other
options exist, but are not usually required. For example, 10-fold
cross validation, used in deciding how to prune to the best deicision
tree, is generally regarded as the right number. Transfering the
commands from the Log tab into the R Console does give you full access
to all options.
<<>>
Decision trees work with both numeric and categorical data.
<<>>
The rpart package is used to build the decision tree."))
  {
    require(rpart, quietly=TRUE)
    popup.textview.help.window("rpart")
  }
}

on_help_glm_activate <- function(action, window)
{
  if (further.help("A tradition approach to model building is
regression. Logistic regression (using the binomial family) is used
to model binary outcomes. Linear regression (using the gaussian family)
is used to model a linear numeric outcome. For predicting where the
outcome is a count, the poisson family is used. Further families are
available, but for now require you to run the glm command directly.
Please see the additional documentation.
<<>>
The R function glm() is used for regression."))
  {
    popup.textview.help.window("glm")
    popup.textview.help.window("family")
  }
}

on_help_randomForest_activate <- function(action, window)
{
  if (further.help("The randomForest algorithm builds multiple
decision trees from different samples of the dataset, and while
building each tree, random subsets of the available variables are
considered for splitting the data at each node of the tree. A simple
majority vote is then used for prediction in the case of
classificaiton (and average for
regression). RandomForest's are generally robust against overfitting.
<<>>
The default is to build 500 trees and to select the square root of the
number of variables as the subset to choose from at each node. The
resulting model is generally not very sensitive to the choice of these
parameters.
<<>>
Any entity with missing values will be ignored, which may lead to some
suprises, like many fewer entities to model when many missing values
exist. It can also lead to losing all examples of a particular class!
<<>>
An estimate of the error rate is provided as the out-of-bag (OOB)
estimate. This applies each tree to the data that was not used in
building the tree to give a quite accurate estimate of the error
rate.
<<>>
The R package is called randomForest."))
    {
      require(randomForest, quietly=TRUE)
      popup.textview.help.window("randomForest")
    }
}

on_help_support_vector_machine_activate <- function(action, window)
{
  if (further.help("SVM (Support Vector Machine) is a modern approach
to modelling where the data is mapped to a higher dimensional space so
that it is more likely that we can find vectors separating the classes.
Rattle supports both svm from the e1071 package and
ksvm from the kernlab package."))
  {
    if (packageIsAvailable("e1071", "view documentation for e1071"))
    {
      require(e1071, quietly=TRUE)
      popup.textview.help.window("svm")
    }
    if (packageIsAvailable("kernlab", "view documentation for kernlab"))
    {
      require(kernlab, quietly=TRUE)
      popup.textview.help.window("ksvm")
    }
  }
}

on_help_gbm_activate <- function(action, window)
{
  if (further.help("Boosting builds multiple, but generally simple, models.
The models might be decision trees that have just one split - these
are often called decision stumps. After building each model any
training entities that the model misclassifies are boosted - they are
given more weight or more importance in the next model building
step. The resulting model is then the weighted sum of the ensemble of
models built.
<<>>
The gbm package is used to build the boosted model."))
    {
      require(gbm, quietly=TRUE)
      popup.textview.help.window("gbm")
    }
}

on_help_confusion_table_activate <- function(action, window)
{
  if (further.help("A confusion table concisely reports the performance
of a model against a testing dataset. Generally, the number of entities
predicted by the model into each of the classes is presented against the
actual class to which that entity belongs. Rattle reports two confusion tables.
The first is the raw entity counts whilst the second reports the
percentages."))
  {
    popup.textview.help.window("table")
  }
}

on_help_sensitivity_activate <- function(action, window)
{
  if (further.help("The Sensitivity versus Specificity chart
is simply an alternative ROC curve, with Sensitivity being the
true positive rate (the count of true positives divided by the
count of positives) and Specificity being the true negative rate
(the count of true negatives divided by the count of negatives).
An ROC curve has the false positive rate instead of Specificity, which
is simply the count of false positives divided by the number of negatives
(1-fnr)."))
  {
    require(ROCR, quietly=TRUE)
    popup.textview.help.window("performance")
  }
}

