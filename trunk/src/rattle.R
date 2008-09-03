# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2008-09-03 19:16:38 Graham Williams>
#
# Copyright (c) 2008 Togaware Pty Ltd
#
# The Rattle package is made of the following R source files:
#
# cluster.R	KMeans and Hierachical Clustering.
# data.R	Handle Data management tasks.
# execute.R	The Execute functionality.
#

MAJOR <- "2"
MINOR <- "3"
REVISION <- unlist(strsplit("$Revision$", split=" "))[2]
VERSION <- paste(MAJOR, MINOR, REVISION, sep=".")
VERSION.DATE <- "Released 03 Sep 2008"
COPYRIGHT <- "Copyright (C) 2008 Togaware Pty Ltd"

# Acknowledgements: Frank Lu has provided much feedback and has
# extensively tested the application. Many colleagues at the
# Australian Taxation Office have used Rattle and made many and
# varied suggestions. These include Anthony Nolan, Stuart Hamilton,
# Liyin Zue, Weiqiang Lin, Robert Williams, Shawn Wicks, Ray Lindsay.

# LICENSE
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

# STYLE GUIDE
#
#    Use the "_" convention only for Glade variables and functions.
#    Use capitalised verbs for own functions: displayPlotAgain
#    Use dot separated words for variables: list.of.frames, lib.cmd
#    RGtk2 uses the capitalised word convention.
#    Use same names in R code as for the Glade objects.
#    Hide global variables, all capitalised, by including in crv$

# INTERFACE STYLE
#
#    080427 For options like a button to show a model once it has been
#    built or which model builders are available given the nature of
#    the data, we generally toggle the Sensistivity of the widgets
#    appropraitely.
#
#    If the functionality is not yet implemented, full stop, then have
#    the interface item(s) not present. This is better than having
#    them greyed out as the expectation is that perhaps there is some
#    way within the interface of getting it not to be greyed out! But
#    displaying future functionality also encourages those with an
#    interest in the greyed out bits to either complain (i.e., I get
#    to know what is wanted) or else help implement them!
#
#    If the functionality is not appropriate in a particular
#    circumstance then don't provide the button. Simply check, in the
#    one place in the code (e.g., when the button is pushed) and pop
#    up an error dialogue.
#
#    This doesn't always work, as in the case of sample where you do
#    want greyed out functionality, but you don't want it to mean not
#    yet implemented.

# BUGS
#
#   Tooltips are not working on GNU/Linux. Just fine on MS/Windows.
#
#   The RGtk2 author, Michael Lawrence, notes that most of the GUI
#   functionality in Gnome (i.e., libgnome and libgnomeui) will soon
#   be merged into GTK. At that time, that functionality will be part
#   of RGtk2.

# GLOBALS
#
#   Original design placed state variables into crs and global
#   constants into . variables that then moved into crv instead, after
#   R CMD check started complaining about possibly unbound
#   variables. The real solution is probably environments, but I
#   haven't explored these yet.
#
#   Be aware that the trick of doing
#
# 	crs <- crs
#
#   within functions only works if we <<- assign to crs and don't make
#   use of the value in crs after it might change within the function
#   (or a sub function)! Probably not a good thing to do.

########################################################################
#
# INITIALISATIONS

RStat <- function(csvname=NULL, ...) 
{
  rattle(csvname, appname="RStat", ...)
}

rattle <- function(csvname=NULL, appname="Rattle", tooltiphack=FALSE)
{
  # If "tooltiphack" is TRUE then gtkMain is called on focus, blocking
  # the R console, but at least tooltips work, and on losing focus
  # gtkMainQuit is called, and thus the console is no longer blocked!
  # A bit ugly, but seems to work. This was suggested by Felix Andrew,
  # 080705. I notice that to load the supplied audit dataset I need to
  # change focus out of Rattle.

  # [080319 gjw] Create GLOBAL to avoid many "no visible binding" from
  # "R CMD check" by adding all hidden variables to it. Previously
  # they all began with "." as in crv$ADA used to be .ADA. "R CMD
  # check" complained a lot, once for each of these, so putting them
  # all into crv means only one complaint each time!

  crv <<- list()
  crv$appname <<- appname
  crv$tooltiphack <<- tooltiphack # Record the value globally
  crv$.gtkMain <<- FALSE # Initially gtkMain is not running.
  
  # Some global constants

  crv$max.vars.correlation <<- 40
  
  if (! packageIsAvailable("RGtk2", "display the Rattle GUI"))
    stop("RGtk2 package is not available but is required for the GUI.")

  require(RGtk2, quietly=TRUE) # From http://www.ggobi.org/rgtk2/

  # Check to make sure libglade is available.

  if (! exists("gladeXMLNew"))
    stop("The RGtk2 package did not find libglade installed. ",
         "Please install it.")

  on_aboutdialog_response <<- gtkWidgetDestroy
  
  # Keep the loading of Hmisc quiet.

  options(Hverbose=FALSE)

 # Try firstly to load the glade file from the installed rattle
  # package, if it exists. Otherwise, look locally.
  
  result <- try(etc <- file.path(.path.package(package="rattle")[1], "etc"),
                silent=TRUE)
  if (inherits(result, "try-error"))
    rattleGUI <<- gladeXMLNew("rattle.glade",
                              root="rattle_window")
  else
    rattleGUI <<- gladeXMLNew(file.path(etc,"rattle.glade"),
                              root="rattle_window")

  # Tune the interface to suit RStat

  setRattleTitle()
  
  if (crv$appname == "RStat")
    tuneRStat()
  else
    tuneRattle()

  # 080511 Record the current options and set the scientific penalty
  # to be 5 so we generally get numerics pinted using fixed rather
  # than exponential notation. We reset all options to what they were
  # at the startup of Rattle on closing Rattle. Not necessarily a good
  # idea since the knowing user may actually also change options
  # whilst Rattle is running.
  
  crv$options <<- options(scipen=5)
  
  # Load data from the file identified by the csvname supplied in the
  # call to Rattle, or from the environment variable RATTLE_DATA if
  # defined, or from the variable .RATTLE.DATA (as might be defined in
  # a .Rattle file), or else don't load any data by default.

  # First, always execute any .Rattle file in the current working
  # directory.
  
  # When reading the .Rattle file and identifying a dataset to load,
  # for some reason the stats package will not have been loaded at
  # this stage. The symptom is that median is not defined. So make
  # sure it is always available.

  require(stats, quietly=TRUE)
  
  if (file.exists(".Rattle")) source(".Rattle")

  if (is.null(csvname))
  {
    # Use the .Rattle settings first, but these might be overriden if
    # the environment variable is defined.
    
    if (exists(".RATTLE.DATA")) csvname <- .RATTLE.DATA

    # Obtain the value of the RATTLE_DATA environment variable and if
    # it is defined then use that at the csvname.
    
    if ((.rattle.data <- Sys.getenv("RATTLE_DATA")) != "")
      csvname <- .rattle.data
  }

  # Tidy up the csvname. TODO Is there an R command to do this, or
  # else put this into a function as I want to do it in a couple of
  # places (like further below in using .RATTLE.SCORE.IN).

  if (not.null(csvname) && substr(csvname, 1, 4) == "http")
  {
    errorDialog("URLs for the CSV filename are not currently supported.",
                sprintf("\n\nWe found %s.", csvname),
                "\n\nWe will continue but you will need to choose a",
                "data file to load using the Filename button.")
    csvname <- NULL
  }
  
  if (not.null(csvname))
  {
    csvname <- path.expand(csvname)

    # If it does not look like an absolute path then add in the
    # current location to make it absolute.
    
    if (substr(csvname, 1, 1) %notin% c("\\", "/")
        && substr(csvname, 2, 2) != ":")
      csvname <- file.path(getwd(), csvname)
    if (! file.exists(csvname))
    {
      infoDialog('The supplied CSV file "', csvname, '" does not exist.')
      csvname <- NULL
    }
  }
    
  # Keep the loading of Hmisc quiet.

  options(Hverbose=FALSE)

  # Load the Rattle GUI specification. The three commands here
  # represent an attempt to be independent of where R is running and
  # where rattle.R is located by finding out from the system calls the
  # actual call to source rattle.R, and then point to this same
  # location for finding rattle.glade. Assumes the call to source is
  # something like: source("abc/def/rattle.R"). The better alternative
  # might be to tell people to use the chdir=TRUE option in source.
  
  ##s <- as.character(sys.calls())
  ##n <- grep("source", s)
  ##p <- gsub("\.R..$", ".glade", gsub("source..", "", s[n]))

  # Constants: I would like these available within this package, but
  # not outside? Do I use assign in some way? That is, how to keep
  # these constants within the package only.
  
  # TODO Put these constants into the top level of this file, defined
  # as NULL. Then keep these double arrow assignments here. I think
  # then that they will stay with the package, but not be in
  # .GlobalEnv because the package scope will be found before the top
  # level.
  
  ########################################################################
  # PACKAGE GLOBAL CONSTANTS
  #
  # These are double arrow assigned here to place them in
  # .GlobalEnv. I couldn't figure out an easy way to keep them scoped
  # locally. TODO Needs cleaning up.
  #
  # Various Treeview Columns
  
  .COLUMN <<- c(number = 0, variable = 1, type = 2, input = 3,
              target = 4, risk = 5, ident = 6, ignore = 7, comment = 8)
  
  .IMPUTE <<- c(number=0, variable=1, comment=2)
  
  .CATEGORICAL <<- c(number = 0, variable = 1, barplot = 2,
                     dotplot = 3, mosplot = 4, comment = 5)
  
  .CONTINUOUS <<-  c(number = 0, variable = 1, boxplot = 2,
                     hisplot = 3, cumplot = 4, benplot = 5, comment = 6)
  
  # Create constants naming the MODELLERS (i.e., the model
  # builders). Note that these are migrating into the crv variable,
  # but not all are done yet.
  
  crv$GLM   <<- "glm"
  .RPART <<- "rpart"
  #GBM <<- "gbm"
  .ADA   <<- "ada"
  .RF    <<- "rf"
  .SVM   <<- "svm"
  .KSVM  <<- "ksvm"
  crv$NNET  <<- "nnet"

  crv$MODELLERS <<- c(.RPART, .ADA, .RF, .KSVM, crv$GLM, crv$NNET)
  
  # RPART
  
  .RPART.CP.DEFAULT        <<- 0.010
  .RPART.MINSPLIT.DEFAULT  <<- 20
  .RPART.MINBUCKET.DEFAULT <<- 7
  .RPART.MAXDEPTH.DEFAULT  <<- 30

  .ADA.NTREE.DEFAULT   <<- 50
  
  .RF.NTREE.DEFAULT    <<- 500
  .RF.MTRY.DEFAULT     <<- 10
  .RF.SAMPSIZE.DEFAULT <<- ""
  
  # MISC
  
  .START.LOG.COMMENT <<- "\n\n# "	# Assume paste with sep=""
  .LOG.COMMENT       <<- "\n## "	# Assume paste with sep=""
  .END.LOG.COMMENT   <<- "\n\n"	# Assume paste with sep=""
  
  # PACKAGE STATE VARIABLE
  
  # Global variables are generally a bad idea, but until a better idea
  # comes to mind.

  crs <<- list(dataset=NULL,
               dataname=NULL,
               dwd=NULL, 	# Data Working Directory
               mtime=NULL,	# Modification time of file
               pwd=NULL,	# Project Working Directory
               input=NULL,
               target=NULL,
               weights=NULL,
               risk=NULL,
               ident=NULL,
               ignore=NULL,
               nontargets=NULL, # 080426 Started but not yet implemented
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
               alog=NULL,	# Record of interaction - useful?
               transforms=NULL  # Record of variable transforms for inclusion in PMML
               )

  # Main notebook related constants and widgets.  Track the widgets
  # that are needed for removing and inserting tabs in the notebook,
  # depending on the selected paradigm. TODO Paradigms have gone as of
  # 080519 so we may not need all this machinery now!
  
  crv$NOTEBOOK <<- theWidget("notebook")

  crv$NOTEBOOK.DATA.NAME <<- "Data"

  crv$NOTEBOOK.SELECT.NAME <<- "Select"

  crv$NOTEBOOK.EXPLORE.NAME <<- "Explore"

  crv$NOTEBOOK.TRANSFORM.NAME <<- "Transform"

  crv$NOTEBOOK.CLUSTER.NAME    <<- "Cluster"
  crv$NOTEBOOK.CLUSTER.WIDGET <<- theWidget("cluster_tab_widget")
  crv$NOTEBOOK.CLUSTER.LABEL  <<- theWidget("cluster_tab_label")

  crv$NOTEBOOK.ASSOCIATE.NAME    <<- "Associate"
  crv$NOTEBOOK.ASSOCIATE.WIDGET <<- theWidget("associate_tab_widget")
  crv$NOTEBOOK.ASSOCIATE.LABEL  <<- theWidget("associate_tab_label")

  crv$NOTEBOOK.MODEL.NAME     <<- "Model"
  crv$NOTEBOOK.MODEL.WIDGET  <<- theWidget("model_tab_widget")
  crv$NOTEBOOK.MODEL.LABEL   <<- theWidget("model_tab_label")

  crv$NOTEBOOK.EVALUATE.NAME    <<- "Evaluate"
  crv$NOTEBOOK.EVALUATE.WIDGET <<- theWidget("evaluate_tab_widget")
  crv$NOTEBOOK.EVALUATE.LABEL  <<- theWidget("evaluate_tab_label")

  crv$NOTEBOOK.LOG.NAME       <<- "Log"

  # Define the TRANSFORM tab pages
  
  crv$TRANSFORM               <<- theWidget("transform_notebook")
  # TODO 080423 Change to RESCALE
  crv$TRANSFORM.NORMALISE.TAB <<- getNotebookPage(crv$TRANSFORM, "normalise")
  crv$TRANSFORM.IMPUTE.TAB    <<- getNotebookPage(crv$TRANSFORM, "impute")
  crv$TRANSFORM.REMAP.TAB     <<- getNotebookPage(crv$TRANSFORM, "remap")
  crv$TRANSFORM.OUTLIER.TAB   <<- getNotebookPage(crv$TRANSFORM, "outlier")
  crv$TRANSFORM.CLEANUP.TAB   <<- getNotebookPage(crv$TRANSFORM, "cleanup")

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
  
  crv$MODEL           <<- theWidget("model_notebook")
  crv$MODEL.RPART.TAB <<- getNotebookPage(crv$MODEL, .RPART)
  crv$MODEL.GLM.TAB   <<- getNotebookPage(crv$MODEL, crv$GLM)
  crv$MODEL.ADA.TAB   <<- getNotebookPage(crv$MODEL, .ADA)
  ## crv$MODEL.GBM.TAB   <<- getNotebookPage(crv$MODEL, .GBM)
  crv$MODEL.RF.TAB    <<- getNotebookPage(crv$MODEL, .RF)
  crv$MODEL.SVM.TAB   <<- getNotebookPage(crv$MODEL, .SVM)
  crv$MODEL.NNET.TAB   <<- getNotebookPage(crv$MODEL, crv$NNET)

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
  # Connect the callbacks.
  
  gladeXMLSignalAutoconnect(rattleGUI)

  # Manually for the tooltiphack

  if (tooltiphack)
  {
    myWin <- theWidget("rattle_window")
    myWin$addEvents(GdkEventMask["focus-change-mask"])
    gSignalConnect(myWin, "focus-in-event", gtkmain_handler)
    gSignalConnect(myWin, "focus-out-event", gtkmainquit_handler)
    gSignalConnect(myWin, "delete-event", gtkmainquit_handler)
  }

  ########################################################################
  # User interface initialisations.
  
  initialiseVariableViews()
  
  # Turn off the sub-notebook tabs.
  
#  crv$DATA$setShowTabs(FALSE)
  .EXPLORE$setShowTabs(FALSE)
  crv$TRANSFORM$setShowTabs(FALSE)
  .CLUSTER$setShowTabs(FALSE)
  crv$MODEL$setShowTabs(FALSE)
  .EVALUATE$setShowTabs(FALSE)

  # Ensure the filechooserbutton by default will filter CSVs.

  updateFilenameFilters("data_filechooserbutton", "CSV")
  
  # Do not enable ARFF option for versions before 2.5.0 where it was
  # not included in the foreign package.

  if (R.version$minor < "4.0")
    theWidget("arff_radiobutton")$hide()
  
  theWidget("model_tree_include_missing_checkbutton")$setActive(FALSE)
  #theWidget("glm_family_comboboxentry")$setActive(0)
  theWidget("svm_kernel_comboboxentry")$setActive(0)

  ## Check if some external applications are available and if not
  ## de-sensitise their functionality.

  ## How to test if ggobi is actually available?

  # If the cairoDevice package is not available then turn off the
  # option in the settings menu and make it insensitive.
  
  if (! packageIsAvailable("cairoDevice", "enable the cairo device option"))
  {
    theWidget("use_cairo_graphics_device")$setActive(FALSE)
    theWidget("use_cairo_graphics_device")$hide()
  }
  
  # Tell MS/Windows to use 2GB (TODO - What's needed under Win64?)
  #
  # Brian D. Ripley 15 Jul 2007 07:57:49 +0100 requested the memory mod
  # be removed:
  #
  # First, because you should not be setting the limit high if the
  # user has only limited memory: the defaults are chosen with a lot
  # of care.  Second, because the default can be as high as 2.5Gb on a
  # machine with 4Gb RAM and the /3GB switch set (the case here).
  #
  # The correct way to refer to things in packages on which you have
  # not declared a dependence is utils::memory.limit.

  # if (isWindows()) utils::memory.limit(2073)

  ## By default the CLUSTER page is not showing.

  ## Don't turn this off until we move away from using the numeric tab
  ## variables above, since a Execute on the Model tab runs the
  ## Cluster tab :-)

##  crv$NOTEBOOK$removePage(getNotebookPage(crv$NOTEBOOK, crv$NOTEBOOK.CLUSTER.NAME))
##  crv$NOTEBOOK$removePage(getNotebookPage(crv$NOTEBOOK, crv$NOTEBOOK.ASSOCIATE.NAME))

##  while (gtkEventsPending()) gtkMainIteration() # Make sure window is displayed

   # Tooltips work when gtkMain is called, but the console is blocked
   # and need gtkMainQuit.
  
  # if (tooltiphack) gtkMain()

  # TODO Add a console into Rattle to interact with R.

  # 080510 Display a relevant welcome message in the textview.

## PUT THE MAIN TEXT HERE INTO THE ABOUT.
##
##   if (crv$appname == "Rattle")
##   {
##     resetTextview("data_textview", "Welcome to Rattle.\n\n", tvsep=FALSE)
##     resetTextview("log_textview", "# Rattle Log File.\n\n", tvsep=FALSE)
##   }
##   else if (crv$appname == "RStat")
##   {
##     resetTextview("data_textview",
##                   paste("Welcome to RStat, the WebFOCUS Data Miner,",
##                         "built on Rattle and R.\n\n"),
##                   tvsep=FALSE)
##     resetTextview("log_textview",
##                   paste("# RStat Log File.\n",
##                         "\n# RStat is built on Rattle and R.",
##                         "\n# This file is an R script.\n\n"),
##                   tvsep=FALSE)
##   }
  
##   appendTextview("data_textview",
##                  paste("Rattle is a free graphical user",
##                        "interface for Data Mining, developed using R.",
##                        "R is a free software environment",
##                        "for statistical computing and graphics.",
##                        "Together they provide one of the most sophisticated",
##                        "and complete environments for performing data mining,",
##                        "statistical analyses, and data visualisation.",
##                        "\n\nSee the Help menu for extensive support in",
##                        "using Rattle.",
##                        "\n\nTogaware's Desktop Data Mining Survival Guide",
##                        "(under development) includes extensive documentation",
##                        "on using Rattle. It is available from\n\n",
##                        "    http://datamining.togaware.com",
##                        "\n\nRattle is licensed under the",
##                        "GNU General Public License, Version 2.",
##                        "\nRattle comes with ABSOLUTELY NO WARRANTY.",
##                        "\nSee Help -> About for details.",
##                        "\n\nRattle version", VERSION,
##                        "\nCopyright (C) 2008 Togaware Pty Ltd"),
##                  tvsep=FALSE)
  appendTextview("log_textview",
                 paste("# Rattle is Copyright (C) 2008",
                       "Togaware Pty Ltd"),
                 tvsep=FALSE)

  initiateLog()
  
  # Make sure the text is shown on startup.
  
  while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
  
  # Now deal with any arguments to rattle.

  if (not.null(csvname))
  {
    theWidget("data_filechooserbutton")$setFilename(csvname)
    # Make sure GUI updates
    while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
    executeDataTab(csvname)
  }

###   # Tune the interface to suit RStat

###   if (crv$appname == "RStat")
###     tuneRStat()
###   else
###     tuneOthers()
  
  ## theWidget("csv_filechooserbutton")$setFilename("audi.csv")
  
  invisible()
}

tuneRStat <- function()
{

  ## Toolbar
  
  theWidget("report_toolbutton")$hide()
  theWidget("rattle_menu")$hide()

  ## Data
  
  # Data -> R Dataset

  theWidget("data_rdataset_radiobutton")$hide()

  # Data -> Edit
  
  theWidget("data_edit_button")$hide()

  # Data -> Weight
  
  theWidget("weight_label")$hide()
  theWidget("weight_entry")$hide()

  ## Explore
  
  # Explore -> Summary -> Find
  
  theWidget("summary_find_label")$hide()
  theWidget("summary_find_entry")$hide()
  theWidget("summary_find_button")$hide()
  theWidget("summary_next_button")$hide()

  ## Model

  # Model -> Tree

  theWidget("model_tree_rpart_radiobutton")$hide()
  theWidget("model_tree_ctree_radiobutton")$hide()
 
  # Model -> Regression

  # 080815 I've moved to using the "Linear" label for the lm/glm
  # family. Regression is perhaps a more general term. I've not
  # approval for this from IBI so reating Regression there for now.
  theWidget("model_linear_radiobutton")$setLabel("Regression")
  theWidget("model_linear_probit_radiobutton")$hide()

  # Model -> All

  theWidget("all_models_radiobutton")$hide()

}

tuneRattle <- function()
{
  ## Toolbar

  theWidget("report_toolbutton")$show()
  
  id.string <- paste('<span foreground="blue">',
                     '<i>', crv$appname, '</i> ',
                     '<i>Version ', VERSION, '</i> ',
                     '<i><span underline="single">togaware.com</span></i>',
                     '</span>', sep="")

  rattle.menu <- theWidget("rattle_menu")
  rattle.menu$SetRightJustified(TRUE)
  rattle.menu$getChild()$setMarkup(id.string)

}



#-----------------------------------------------------------------------
# MAINLOOP ITERATION
#
# Attempt to get tooltips working forGNU/Linux by starting up gtkMain
# on the window etting focus, and stopping it when it loses
# focus. Based on idea from Felix Andrews.

gtkmain_handler <- function(widget, event)
{
#  if (! crv$tooltiphack)
#    return(gtkmainquit_handler(widget, event))
  
  # Switch to GTK event loop while the window is in focus (for tooltips)
  
  if (! crv$.gtkMain)
  {
    crv$.gtkMain <<- TRUE
    gtkMain()
  }
  return(FALSE)
}

gtkmainquit_handler <- function(widget, event)
{
  if (crv$.gtkMain)
  {
    crv$.gtkMain <<- FALSE
    gtkMainQuit()
  }
  return(FALSE)
}

#-----------------------------------------------------------------------
# RESET RATTLE

resetRattle <- function(new.dataset=TRUE)
{
  # Cleanup various bits of Rattle, as when a new dataset is loaded or
  # a project is loaded. Might also be useful for the New button. If
  # new.dataset is FALSE then just reset various textviews and default
  # options.

  if (new.dataset)
  {
    # Initialise CRS

    crs$dataset  <<- NULL
    crs$dataname <<- NULL
    # crs$dwd      <<- NULL
    crs$mtime    <<- NULL
    crs$input    <<- NULL
    crs$target   <<- NULL
    crs$weights  <<- NULL
    crs$risk     <<- NULL
    crs$ident    <<- NULL
    crs$ignore   <<- NULL
    crs$nontargets <<- NULL # 080426 Started but not yet implemented.
    crs$sample   <<- NULL
    crs$sample.seed <<- NULL
    crs$testset  <<- NULL
    crs$testname <<- NULL
    crs$transforms <<- NULL
  }

  # Clear out all current models.
  
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
  crs$nnet     <<- NULL
  crs$perf     <<- NULL
  crs$eval     <<- NULL

  # Clear all now outdated text views

  setTextview("summary_textview")
  setTextview("correlation_textview")
  setTextview("prcomp_textview")
  setTextview("kmeans_textview")
  setTextview("hclust_textview")
  setTextview("associate_textview")
  setTextview("rpart_textview")
  setTextview("glm_textview")
  setTextview("ada_textview")
  setTextview("rf_textview")
  setTextview("esvm_textview")
  setTextview("ksvm_textview")
  setTextview("nnet_textview")
  setTextview("confusion_textview")
  setTextview("risk_textview")
  setTextview("roc_textview")

  # Reset some textviews back to standard text.

  # Set all sub tabs back to the default tab page and reflect this in
  # the appropriate radio button.

  # TODO 080423 Change name to RESCALE
  crv$TRANSFORM$setCurrentPage(crv$TRANSFORM.NORMALISE.TAB)
  theWidget("normalise_radiobutton")$setActive(TRUE)
  theWidget("impute_zero_radiobutton")$setActive(TRUE)
  theWidget("impute_constant_entry")$setText("")
  theWidget("remap_quantiles_radiobutton")$setActive(TRUE)
  theWidget("delete_ignored_radiobutton")$setActive(TRUE)
  
  .EXPLORE$setCurrentPage(.EXPLORE.SUMMARY.TAB)
  theWidget("summary_radiobutton")$setActive(TRUE)

  .CLUSTER$setCurrentPage(.CLUSTER.KMEANS.TAB)
  theWidget("kmeans_radiobutton")$setActive(TRUE)
  
  crv$MODEL$setCurrentPage(crv$MODEL.RPART.TAB)
  theWidget("rpart_radiobutton")$setActive(TRUE)
  #theWidget("all_models_radiobutton")$setActive(TRUE)

  .EVALUATE$setCurrentPage(.EVALUATE.CONFUSION.TAB)
  theWidget("confusion_radiobutton")$setActive(TRUE)

  # Reset the DATA tab. But we don't want to do this because
  # resetRattle is called on loading a database table, and this ends
  # up clearing all the widgets!

  if (new.dataset)
  {
    theWidget("sample_count_spinbutton")$setValue(0)
    theWidget("sample_checkbutton")$setActive(FALSE)
    theWidget("target_type_radiobutton")$setActive(TRUE)
  }
  
  ## 080520 Don't turn these off - it makes sesne to allow the user to
  ## set these options even before the dataset is loaded.
  
  ##theWidget("target_type_radiobutton")$setSensitive(FALSE)
  ##theWidget("target_categoric_radiobutton")$setSensitive(FALSE)
  ##theWidget("target_numeric_radiobutton")$setSensitive(FALSE)
  
##   theWidget("odbc_dsn_entry")$setText("")
##   theWidget("odbc_combobox")$setActive(-1)
##   theWidget("odbc_limit_spinbutton")$setValue(0)
##   theWidget("odbc_believeNRows_checkbutton")$setActive(FALSE)
  
  if (new.dataset)
  {
    # Clear the treeviews.

    theWidget("select_treeview")$getModel()$clear()
    theWidget("impute_treeview")$getModel()$clear()
    theWidget("categorical_treeview")$getModel()$clear()
    theWidget("continuous_treeview")$getModel()$clear()

    theWidget("weight_entry")$setText("")
    theWidget("model_tree_rpart_weights_label")$
    setText("")
  
    # Reset Model -> Tree -> RPart
  
    theWidget("model_tree_priors_entry")$setText("")
    theWidget("model_tree_loss_entry")$setText("")
    theWidget("rpart_minsplit_spinbutton")$setValue(.RPART.MINSPLIT.DEFAULT)
    theWidget("rpart_maxdepth_spinbutton")$setValue(.RPART.MAXDEPTH.DEFAULT)
    theWidget("model_tree_cp_spinbutton")$setValue(.RPART.CP.DEFAULT)
    theWidget("rpart_minbucket_spinbutton")$setValue(.RPART.MINBUCKET.DEFAULT)
    theWidget("model_tree_include_missing_checkbutton")$setActive(FALSE)
    theWidget("model_tree_rpart_radiobutton")$setActive(TRUE)
    showModelRPartExists()

    # Reset MODEL:ADA
  
    showModelAdaExists()
    setGuiDefaultsAda()
  
    # Reset MODEL:RF
  
    showModelRFExists()

    # Reset MODEL:SVM

    setGuiDefaultsSVM()

    # Update EXPLORE, MODEL and EVALUATE targets

    theWidget("explot_target_label")$setText("No target selected")
    theWidget("explot_annotate_checkbutton")$setActive(FALSE)
    theWidget("summary_find_entry")$setText("")
    theWidget("benford_bars_checkbutton")$setActive(FALSE)
    theWidget("benford_abs_radiobutton")$setActive(TRUE)
    theWidget("benford_digits_spinbutton")$setValue(1)
    theWidget("explore_correlation_method_combobox")$setActive(0)

    theWidget("glm_target_label")$setText("No target selected")
    theWidget("rpart_target_label")$setText("No target selected")
    ##theWidget("gbm_target_label")$setText("No target selected")
    theWidget("ada_target_label")$setText("No target selected")
    theWidget("rf_target_label")$setText("No target selected")
    theWidget("svm_target_label")$setText("No target selected")
    theWidget("nnet_target_label")$setText("No target selected")
    theWidget("evaluate_risk_label")$setText("No risk variable selected")
  
    theWidget("evaluate_training_radiobutton")$setActive(TRUE)
    theWidget("evaluate_filechooserbutton")$setFilename("")
    theWidget("evaluate_rdataset_combobox")$setActive(-1)

    # If there is a .RATTLE.SCORE.IN defined, as might be from a .Rattle
    # file, then use that for the filename of the CSV evaluate option.
  
    if (exists(".RATTLE.SCORE.IN"))
    {
      scorename <- .RATTLE.SCORE.IN
      if (not.null(scorename))
      {
        scorename <- path.expand(scorename)
      
        # If it does not look like an absolute path then add in the
        # current location to make it absolute.
      
        if (substr(scorename, 1, 1) %notin% c("\\", "/")
            && substr(scorename, 2, 2) != ":")
          scorename <- file.path(getwd(), scorename)
        if (! file.exists(scorename))
        {
          errorDialog("The specified SCORE file", sprintf('"%s"', scorename),
                      "(sourced from the .Rattle file through the",
                      ".RATTLE.SCORE.IN variable)",
                      "does not exist. We will continue",
                      "as if it had not been speficied.")
        
          # Remove the variable (from the global environment where the
          # source command will have plade the bindings) so the rest of
          # the code continues to work on the assumption that it has not
          # been supplied.

          rm(.RATTLE.SCORE.IN, pos=globalenv())
        }
        else
        {
          theWidget("evaluate_filechooserbutton")$setFilename(scorename)
          theWidget("evaluate_csv_radiobutton")$setActive(TRUE)
        }
      }
    }
  }
  
  resetEvaluateCheckbuttons("all_inactive")
  resetEvaluateCheckbuttons("all_insensitive")

  #theWidget("rpart_evaluate_checkbutton")$hide()
  #theWidget("rf_evaluate_checkbutton")$hide()
  #theWidget("ksvm_evaluate_checkbutton")$hide()
  #theWidget("glm_evaluate_checkbutton")$hide()
  #theWidget("ada_evaluate_checkbutton")$hide()

  ## Update CLUSTER tab

  theWidget("kmeans_hclust_centers_checkbutton")$setActive(FALSE)
  theWidget("hclust_distance_combobox")$setActive(FALSE)
  theWidget("hclust_link_combobox")$setActive(FALSE)
  theWidget("hclust_dendrogram_button")$setSensitive(FALSE)
  theWidget("hclust_clusters_label")$setSensitive(FALSE)
  theWidget("hclust_clusters_spinbutton")$setSensitive(FALSE)
  theWidget("hclust_stats_button")$setSensitive(FALSE)
  theWidget("hclust_data_plot_button")$setSensitive(FALSE)
  theWidget("hclust_discriminant_plot_button")$setSensitive(FALSE)
  
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
  # If the RGtk2 package's functions are not available, then just
  # issue a warning instad of a popup.
  
  if (exists("gtkMessageDialogNew"))
  {
    dialog <- gtkMessageDialogNew(NULL, "destroy-with-parent", "info", "close",
                                  ...)
    connectSignal(dialog, "response", gtkWidgetDestroy)
  }
  else
    # 080706 This fails the MS/Windows check with "crv" not defined????? 
    if (! isWindows()) warning(...)
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
                                ...,
                                sprintf("\n\n[%s Version %s]", crv$appname, VERSION))
  connectSignal(dialog, "response", gtkWidgetDestroy)
}

errorReport <- function(cmd, result)
{
  # A standard command error report that is not being captured by
  # Rattle. Eventually, all of these should be identified by Rattle
  # and a sugggestion given as to how to avoid the error.
  
  errorDialog("An error occured with", cmd,
              "Please report this to support@togaware.com\n\n",
              "The error was:\n\n", result)
}

questionDialog <- function(...)
{
  dialog <- gtkMessageDialogNew(NULL, "destroy-with-parent", "question",
                                "yes-no",
                                ...)
  result <- dialog$run()
  dialog$destroy()
  if (result == GtkResponseType["yes"])
    return(TRUE)
  else
    return(FALSE)
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
                "\n\nAt a minimum, please load a dataset from the Data tab",
                "before attempting any other operation.",
                "\n\nBe sure to Execute the Data tab once the",
                "data source has been specified.")
    return(TRUE)
  }
  else
    return(FALSE)
}

variablesHaveChanged <- function(action)
{
  # PARAMETERS
  #
  # action: a string that is displayed in the error dialogue.

  if (length(crs$ignore) != length(getSelectedVariables("ignore")) ||
      length(crs$ident) != length(getSelectedVariables("ident")) ||
      length(crs$input) != length(getSelectedVariables("input")))
  {
    errorDialog("It appears that there have been some changes made",
                "to the variables in the",
                "Data tab that have not been Executed.",
                "\n\nPlease click Execute on the Data tab before",
                paste(action, ".", sep=""))
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
                 sprintf('install.packages("%s"),', pkg),
                 "to use the full",
                 "functionality of", crv$appname)
    return(FALSE)
  }
  else
    return(TRUE)
}

sampleNeedsExecute <- function()
{
  # Popup an error dialog if sampling needs to be executed and return
  # TRUE.

  # If sampling is active, make sure there is a sample.
  
  if (theWidget("sample_checkbutton")$getActive()
      && is.null(crs$sample))
  {
    errorDialog("Sampling is active but has not been Executed.",
                "Either ensure you Execute the sampling by clicking",
                "the Execute button on the Transform tab,",
                "or else de-activate Sampling on the Transform tab.")
    return(TRUE)
  }

  # If sampling is inactive, make sure there is no sample. 080601 Why
  # would I need this test?

###   if (! theWidget("sample_checkbutton")$getActive()
###       && not.null(crs$sample))
###   {
###     errorDialog("Sampling is inactive but has not been Executed",
###                  "since being made inactive.",
###                  "Please ensure you Execute the Transform tab",
###                  "after de-activating the Sampling on the Transform tab.")
###         return(TRUE)
###   }

  return(FALSE)
}

########################################################################
##
## Simplify updates to status bar
##

setRattleTitle <- function(title=NULL)
{
  if (crv$appname == "RStat")
    standard <- "Developer Studio - [RStat]"
  else
    standard <- "R Data Miner - [Rattle]"
  if (is.null(title))
    theWidget("rattle_window")$setTitle(standard)
  else
    theWidget("rattle_window")$setTitle(sub("]",
                                            sprintf(" (%s)]", title),
                                            standard))
}

setStatusBar <- function(..., sep=" ")
{
  msg <- paste(sep=sep, ...)
  if (length(msg) == 0) msg <-""
  theWidget("statusbar")$push(1, msg)
  while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE) # Refresh status/windows
  invisible(NULL)
}

collectOutput <- function(command, use.print=FALSE, use.cat=FALSE,
                          width=getOption("width"), envir=parent.frame())
{
  # TODO Should this use cat or print? Cat translates the \n to a
  # newline and doesn't precede the output by [1].  For pretty output
  # with sprintf() you probably want cat(), but if you have a vector
  # of formatted text and you want to look at it (as data), print()
  # would be better.

  # TODO Should we be using collect.output?

  owidth <- getOption("width")
  options(width=width)
  if (use.print)
    command <- paste("print(", command, ")", sep="")
  else if (use.cat)
    command <- paste("cat(", command, ")", sep="")

  # 080829 - Let's try out capture.output as a simpler way of doing
  # sink. Seems to work okay!

  if (FALSE)
  {
    zz <- textConnection("commandsink", "w", TRUE)
    sink(zz)
    result <- try(eval(parse(text=command), envir=envir))
    sink()
    close(zz)
  }
  else
  {
    result <- try(commandsink <- capture.output(eval(parse(text=command), envir=envir)))
  }
  
  if (inherits(result, "try-error"))
  {
    errorDialog(sprintf("A command has failed: %s.", command),
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
  rattleGUI <- rattleGUI # Global - to avoid a "NOTE" from "R CMD check"
  
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
  # The use of .Platform$OS.type is as recommended in the R.version
  # manual page.
  return(.Platform$OS.type == "windows")
}

listBuiltModels <- function()
{
#  return(! (is.null(crs$glm) && is.null(crs$rpart) &&
#            is.null(crs$gbm) && is.null(crs$rf) &&
#            is.null(crs$svm)))
  models <- c()
  for (m in crv$MODELLERS)
    if (not.null(eval(parse(text=sprintf("crs$%s", m)))))
      models <- c(models, m)
  return(models)
}

## setDefaultPath <- function(filename)
## {
##   # REMOVE THIS FUNCTION - SEE NOTES BELOW. Simply assign direct to
##   # crs$dwd and don't setwd.
  
##   # Record the default location for data. Also set R's current working
##   # directory to the path. Note that I expect that for projects we
##   # record the path as crs$pwd outside of this function but we don't
##   # set R's cwd to it at any time. Note that we don't really need to
##   # do this, in that we are moving R's cwd without the user actually
##   # asking for this. Instead, we should perhaps not change cwd, but
##   # record it in crs$dwd and then use
##   # dialog$setCurrentFolder(crs$dwd), as I am doing now (080319) for
##   # projects.
  
##   if (not.null(filename))
##   {
##     crs$dwd <<- dirname(filename)
##     setwd(crs$dwd)
##   }
## }

########################################################################
##
## PLOTTING
##
## Callbacks

on_plot_save_button_clicked <- function(action)
{
  # To know which window we are called from we extract the plot
  # number from the window title!!!. This then ensures we save the
  # right device.
  #
  # Also, export to pdf (from Cairo) is not too good it seems. Gets a
  # grey rather than white background. PNG and JPEG look just fine.
  # This is being fixed by Michael Lawrence.  

  ttl <- action$getParent()$getParent()$getParent()$getParent()$getTitle()
  dev.num <- as.integer(sub("Rattle: Plot ", "", ttl))
  savePlotToFileGui(dev.num)
}

on_plot_copy_button_clicked <- function(action)
{
  ttl <- action$getParent()$getParent()$getParent()$getParent()$getTitle()
  dev.num <- as.integer(sub("Rattle: Plot ", "", ttl))
  startLog("COPY PLOT TO CLIPBOARD")
  appendLog(paste("Copy the plot on device", dev.num, "to the clipboard."),
            sprintf('copyPlotToClipboard(%s)', dev.num))
  copyPlotToClipboard(dev.num)
  infoDialog("The plot has been copied to the clipboard as a PNG.")
}

on_plot_print_button_clicked <- function(action)
{
  ## To know which window we are called from we extract the plot
  ## number from the window title!!!. This then ensures we save the
  ## right device.
    
  ttl <- action$getParent()$getParent()$getParent()$getParent()$getTitle()
  dev.num <- as.integer(sub("Rattle: Plot ", "", ttl))
  startLog("PRINT PLOT")
  appendLog(paste("Print the plot on device", dev.num),
            sprintf('printPlot(%s)', dev.num))
  printPlot(dev.num)
  infoDialog(sprintf("Plot %d has been sent to the printer.", dev.num))
}

on_plot_close_button_clicked <- function(action)
{
  ttl <- action$getParent()$getParent()$getParent()$getParent()$getTitle()
  devnum <- as.integer(sub(paste(crv$appname, ": Plot ", sep=""), "", ttl))
  dev.off(devnum)
  pw <- action$getParentWindow()
  pw$destroy()
}

########################################################################

newPlot <- function(pcnt=1)
{
  # Create a new device into which the plot is to go.
  
  # Trial the use of the Cairo device. This was the only place I
  # needed to change to switch over to the Cairo device. As backup,
  # revert to the x11() or windows() device.

  if (theWidget("use_cairo_graphics_device")$getActive() &&
      packageIsAvailable("cairoDevice", "display plots"))
  {
    require("cairoDevice", quietly=TRUE)
    result <- try(etc <- file.path(.path.package(package="rattle")[1], "etc"),
                  silent=TRUE)
    if (inherits(result, "try-error"))
      plotGUI <- gladeXMLNew("rattle.glade", root="plot_window")
    else
      plotGUI <- gladeXMLNew(file.path(etc,"rattle.glade"), root="plot_window")
    gladeXMLSignalAutoconnect(plotGUI)
    da <- plotGUI$getWidget("drawingarea")
    asCairoDevice(da)
    plotGUI$getWidget("plot_window")$setTitle(paste(crv$appname, ": Plot ",
                                                    dev.cur(), sep=""))
  }
  else if (.Platform$GUI %in% c("X11", "unknown"))
  {
    # Add "unknown" to handle the case with the littler script
    # interface which runs with an "unknown" GUI.

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

copyPlotToClipboard <- function(dev.num=dev.cur())
{
  # This is designed to be called from the Gtk window that displays
  # the Cairo device, to copy the plot displayed there into the
  # Clipboard. It has not been tested on non-Cairo devices.
  #
  # We can place a GdkPixbuf image into the CLIPBOARD using
  # GtkClipboardSetImage. I've not figure out yet how to get the image
  # directly from the Cairo device as a GdkPixbuf. So instead I save
  # to PNG file then load that file as a GdkPixmap then copy that to
  # the clipboard.
  #
  # This works for GNU/Linux and more recent MS/Windows (e.g., on my
  # recent Dell laptop but not on ATOnet computers). It has not been
  # tested on Mac/OSX. Perhaps it is a bug and needs to be reported to
  # Michael Lawrence. Michael has also mentioned a new version of
  # cairoDevice supporting cairo backends for PDF, PS, SVG, and PNG to
  # output in those formats directly (070406).
  #
  # Note that in oodraw, for example, you can select an object, then
  # grab the selection and have it available in R:
  #
  # im <- gtkClipboardGet("CLIPBOARD")$waitForImage()
  #
  # Of course you can also load the image from file:
  #
  # im <- gdkPixbufNewFromFile("audit_auto_plot3.png")$retval
  #
  # Once we have the image:
  #
  # gtkClipboardGet("CLIPBOARD")$setImage(im)
  #
  # Which can then be pasted into oowriter, for example.

  # On Windows I sometimes needed this:
  #
  # if (isWindows())
  # {cur <- dev.cur(); dev.set(dev.num);
  #  my.savePlot("clipboard"); dev.set(cur)} else {

  require("RGtk2")
  temp.name <- paste(tempfile(), ".png", sep="")
  savePlotToFile(temp.name, dev.num)
  im <- gdkPixbufNewFromFile(temp.name)$retval
  gtkClipboardGet("CLIPBOARD")$setImage(im)
  file.remove(temp.name)
}

savePlotToFileGui <- function(dev.num=dev.cur(), name="plot")
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

  dialog <- gtkFileChooserDialog(paste("Export Graphics (pdf, png, jpg, svg",
                                       ifelse(isWindows(), ", wmf", ""),
                                       ")", sep=""),
                                 NULL, "save",
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-save", GtkResponseType["accept"])
  
  if(not.null(crs$dataname))
    dialog$setCurrentName(paste(get.stem(crs$dataname),
                                "_", name, ".pdf", sep=""))
  
  ff <- gtkFileFilterNew()
  if (isWindows())
    ff$setName("Graphics Files (pdf png jpg svg wmf)")
  else
    ff$setName("Graphics Files (pdf png jpg svg)")
  ff$addPattern("*.pdf")
  ff$addPattern("*.png")
  ff$addPattern("*.jpg")
  ff$addPattern("*.svg")
  if (isWindows()) ff$addPattern("*.wmf")
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
  
  if (get.extension(save.name) == "")
    save.name <- sprintf("%s.pdf", save.name)
  
  if (file.exists(save.name))
    if (! questionDialog("A Graphics file of the name", save.name,
                         "already exists. \n\nDo you want to",
                         "overwrite this file?"))
      return()

  startLog("SAVE PLOT")
  appendLog(paste("Save the plot on device", dev.num, "to file."),
            sprintf('savePlotToFile("%s", %s)', save.name, dev.num))
  
  if (savePlotToFile(save.name, dev.num))
    infoDialog("Plot", dev.num, "has been exported to", save.name)
}

savePlotToFile <- function(file.name, dev.num=dev.cur())
{
  cur <- dev.cur()
  dev.set(dev.num)
  ext <- get.extension(file.name)
  if (ext == "pdf")
    ## Set version to 1.4 since dev.copy from a Cairo device needs
    ## this.  It is done automatically with a warning anyhow, but
    ## might as well avoid the warning so as not to worry anyone.
    dev.copy(pdf, file=file.name, width=10, height=10, version="1.4")
  else if (ext == "png")
    dev.copy(png, file=file.name, width=1000, height=1000)
  else if (ext == "jpg")
    dev.copy(jpeg, file=file.name, width=1000, height=1000)
  else if (ext == "svg")
    if (packageIsAvailable("RSvgDevice", "to save plot to SVG format"))
    {
      require("RSvgDevice")
      dev.copy(devSVG, file=file.name, width=10, height=10)
    }
    else
      return()
  else if (ext == "wmf")
    dev.copy(win.metafile, file=file.name, width=10, height=10)
  else
  {
    infoDialog(sprintf("The specified extension '%s' is not supported.", ext))
    return(FALSE)
  }
  dev.off()
  dev.set(cur)
  return(TRUE)
}

printPlot <- function(dev.num=dev.cur()) 
{
  cur <- dev.cur()
  dev.set(dev.num)
  if (isWindows())
    my.dev.print(win.print)
  else
    my.dev.print()
  dev.set(cur)
}
  
# The following is from dev.print, but dev.print does not list "Cairo"
# as a screen device. So just use their code until they change
# this. Ripley has implemented (070408) a fix for this and it is in
# 2.5.0 version of dev.print.

my.dev.print <- function (device = postscript, ...) 
{
    current.device <- dev.cur()
    nm <- names(current.device)[1]
    if (nm == "null device") 
        stop("no device to print from")
    if (!(nm %in% c("Cairo", "X11", "GTK", "gnome", "windows", "quartz"))) 
        stop("can only print from screen device")
    oc <- match.call()
    oc[[1]] <- as.name("dev.copy")
    oc$device <- device
    din <- graphics::par("din")
    w <- din[1]
    h <- din[2]
    if (missing(device)) {
        if (is.null(oc$file)) 
            oc$file <- ""
        hz0 <- oc$horizontal
        hz <- if (is.null(hz0)) 
            ps.options()$horizontal
        else eval.parent(hz0)
        paper <- oc$paper
        if (is.null(paper)) 
            paper <- ps.options()$paper
        if (paper == "default") 
            paper <- getOption("papersize")
        paper <- tolower(paper)
        switch(paper, a4 = {
            wp <- 8.27
            hp <- 11.69
        }, legal = {
            wp <- 8.5
            hp <- 14
        }, executive = {
            wp <- 7.25
            hp <- 10.5
        }, {
            wp <- 8.5
            hp <- 11
        })
        wp <- wp - 0.5
        hp <- hp - 0.5
        if (!hz && is.null(hz0) && h < wp && wp < w && w < hp) {
            hz <- TRUE
        }
        else if (hz && is.null(hz0) && w < wp && wp < h && h < 
            hp) {
            hz <- FALSE
        }
        else {
            h0 <- ifelse(hz, wp, hp)
            if (h > h0) {
                w <- w * h0/h
                h <- h0
            }
            w0 <- ifelse(hz, hp, wp)
            if (w > w0) {
                h <- h * w0/w
                w <- w0
            }
        }
        if (is.null(oc$pointsize)) {
            pt <- ps.options()$pointsize
            oc$pointsize <- pt * w/din[1]
        }
        if (is.null(hz0)) 
            oc$horizontal <- hz
        if (is.null(oc$width)) 
            oc$width <- w
        if (is.null(oc$height)) 
            oc$height <- h
    }
    else {
        devname <- deparse(substitute(device))
        if (devname %in% c("png", "jpeg", "bmp") && is.null(oc$width) && 
            is.null(oc$height)) 
            warning("need to specify one of 'width' and 'height'")
        if (is.null(oc$width)) 
            oc$width <- if (!is.null(oc$height)) 
                w/h * eval.parent(oc$height)
            else w
        if (is.null(oc$height)) 
            oc$height <- if (!is.null(oc$width)) 
                h/w * eval.parent(oc$width)
            else h
    }
    dev.off(eval.parent(oc))
    dev.set(current.device)
}

# This one seems to have some assumption about the device it is saving
# from and causes a memory fault if it is Cairo! Best not to use it
# for now, and the Gtk clipboard stuff does work under Windows.

my.savePlot <- function (filename = "Rplot",
                         type = c("wmf", "emf", "png", "jpeg",
                           "jpg", "bmp", "ps", "eps", "pdf"),
                         device = dev.cur(), restoreConsole = TRUE)
{
  type <- match.arg(type)
  devlist <- dev.list()
  devcur <- match(device, devlist, NA)
  if (is.na(devcur))
    stop("no such device")
  devname <- names(devlist)[devcur]
  #if (devname != "windows")
  #  stop("can only copy from 'windows' devices")
  if (filename == "clipboard" && type == "wmf")
    filename <- ""
  if (nchar(filename) > 0)
    filename <- paste(filename, type, sep = ".")
  invisible(.External("savePlot", device, filename, type, restoreConsole,
                      PACKAGE = "grDevices"))
}

########################################################################

genPlotTitleCmd <- function(..., vector=FALSE)
{
  # 080817 Use month name rather than number - less ambiguous.
  # 080516 For RStat do not brand the plots.

  if (! exists("crv"))
  {
    crv <- list()
    crv$appname <- "Rattle"
  }
  
  main = paste(...)
  if(vector)
  {
    if (crv$appname == "RStat")
      sub <- ""
    else
      sub <- sprintf("%s %s %s", crv$appname,
                     format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"])
    return(c(main, sub))
  }
  else
  {  
    if (crv$appname == "RStat")
      sub <- ""
    else
      sub <- sprintf(paste('paste("%s", format(Sys.time(),',
                           '"%%Y-%%b-%%d %%H:%%M:%%S"), Sys.info()["user"])'),
                     crv$appname)
    return(sprintf('title(main="%s", sub=%s)', main, sub))
  }
}

set.cursor <- function(cursor="left-ptr", message=NULL)
{
  if (! is.null(message)) setStatusBar(message)
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

  # Change the line widths to represent the magnitude of the flow.
  # Use a log transform to get integers for the line widths.

  flow.log <- log10(flow) # Log 10 to get magnitude
  flow.log[flow.log==0] <- 1 # Set any 0's to 1 as the base case
  flow.log[flow.log==-Inf] <- 0 # Set resulting -Infinty (log10(0)) values to 0
  flow.mag <- round(flow.log) # Round them to 

  # Add color to indicate the magnitude.  Use heat colours to
  # indicate the magnitude of the flow, from yellow to red.

  heat <- rev(heat.colors(max(flow.mag)))
  flow.col <- flow.mag
  for (i in 1:length(heat)) flow.col[flow.col==i] <- heat[i]
  flow.col <- sapply(flow.col, as.character)
  
  # Record the magnitude of flow coming into any label and use this to
  # scale the entity labels. 

  entity.sizes <- round(log10(apply(flow, 2, sum)))
  entity.sizes[entity.sizes==-Inf] <- 0
  entity.sizes <- 1 + entity.sizes-min(entity.sizes)
  entity.sizes <- 1 + entity.sizes/max(entity.sizes)

  # A warning that "par()$cxy * label.cex" have missmatched
  # dimensions. par()$cxy is of length 2? Should be 1?
  
  suppressWarnings(plot(flow.net, displaylabels=TRUE, usecurve=TRUE,
                        mode="circle",
                        edge.lwd=flow.mag*1.5, edge.col=flow.col,
                        label.cex=entity.sizes, label.border=0))

  eval(parse(text=genPlotTitleCmd("Network Map of Flows")))

}

########################################################################
#
# Shared callbacks
#

update_comboboxentry_with_dataframes <- function(action, window)
{
  # Update a combo box (Evaluate -> Score) with just the available
  # data frames and matrices.

  current <- theWidget("data_name_combobox")$getActiveText()
  
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

close_rattle <- function(action, window)
{
  # Don't remove the graphics for now. In moving to the Cairo device,
  # this blanks the device, but does not destroy the containing
  # window. I wonder if there is some way to get a list of the plot
  # windows, and destroy each one?

  # graphics.off() # for (i in dev.list()) dev.off(i)

  # 080523 When this is called as a callback from the destroy signal
  # of the GtkObject, the window has already been destroyed, so no
  # need to try again.

  rw <- theWidget("rattle_window")
  if (not.null(rw)) rw$destroy()

  # Communicate to R that Rattle has finished. This is used by the
  # rattle script on GNU/Linux using the littler package which allows
  # one to use R as a scripting language. But rattle dispatches
  # itself from R, and so normally the script immediately
  # terminates. Instead we can have a loop that checks if rattleGUI
  # is NULL, and when it is we finish! Seems to work.

  rattleGUI <<- NULL

  # 080511 Restore options to how they were when Rattle was started.

  options(crv$options)
  
  # if (crv$tooltiphack) gtkMainQuit() # Only needed if gtkMain is run.

}

quit_rattle <- function(action, window)
{
  # 080815 This function used to return NULL or "yess" and I alwaysed
  # tested whether it's results was NULL. But why not return a
  # logical? Start doing that now, by returning TRUE instead of "yes",
  # and look to return FALSE instead of NULL on a negative response to
  # the question.

  msg <- sprintf("Do you want to terminate %s?", crv$appname)
  
  if (questionDialog(msg))
  {
    close_rattle(action, window)
    quit(save="no")
  }
}

########################################################################
# TRANSFORM TAB

#----------------------------------------------------------------------
# Interface Actions

# When a radio button on the TRANSFORM tab is selected, display the
# appropriate option.

on_impute_radiobutton_toggled <- function(button)
{
  if (button$getActive()) 
  {
    crv$TRANSFORM$setCurrentPage(crv$TRANSFORM.IMPUTE.TAB)
  }
  setStatusBar()
}

# TODO 080423 Change to RESCALE
on_normalise_radiobutton_toggled <- function(button)
{
  if (button$getActive()) 
  {
    crv$TRANSFORM$setCurrentPage(crv$TRANSFORM.NORMALISE.TAB)
  }
  setStatusBar()
}

on_remap_radiobutton_toggled <- function(button)
{
  if (button$getActive()) 
  {
    crv$TRANSFORM$setCurrentPage(crv$TRANSFORM.REMAP.TAB)
  }
  setStatusBar()
}

on_cleanup_radiobutton_toggled <- function(button)
{
  if (button$getActive()) 
  {
    crv$TRANSFORM$setCurrentPage(crv$TRANSFORM.CLEANUP.TAB)
  }
  setStatusBar()
}

on_impute_constant_radiobutton_toggled <- function(button)
{
  theWidget("impute_constant_entry")$setSensitive(button$getActive())
}

#----------------------------------------------------------------------
# Execution

executeTransformTab <- function()
{
  # We can not do any transforms if there is no dataset.

  if (noDatasetLoaded()) return()

  # Dispatch to the appropriate option.

  # TODO 080423 Change NORMALISE to RESCALE
  
  if (theWidget("normalise_radiobutton")$getActive())
    executeTransformNormalisePerform()
  else if (theWidget("impute_radiobutton")$getActive())
    executeTransformImputePerform()
  else if (theWidget("remap_radiobutton")$getActive())
    executeTransformRemapPerform()
  else if (theWidget("cleanup_radiobutton")$getActive())
    executeTransformCleanupPerform()
}

modalvalue <- function(x, na.rm=FALSE)
{
    x = unlist(x);
    if(na.rm) x = x[!is.na(x)]
    u = unique(x);
    n = length(u);
    frequencies = rep(0, n);
    for(i in 1:n)
    {
        if(is.na(u[i]))
        {
            frequencies[i] = sum(is.na(x))
        } else
        {
            frequencies[i] = sum(x==u[i], na.rm=TRUE)
        }
    }
    u[which.max(frequencies)]
}

executeTransformNormalisePerform <- function()
{
  # TODO 080609 We should rename this in line with the interface change,
  # since it is not necessarily normalisation but is rescaling.
  
  # First determine which normalisation option has been chosen and the
  # prefix of the new variable that will be introduced.  Default to
  # NULL in the hope of picking up an error if something has gone wrong.

  # TODO 071124 The radio buttons could be checkbuttons, and we do
  # multiple imputations for the selected variables, but for now, stay
  # with radio buttons as it is simple, without loss of functionality.
  
  action <- NULL
  vprefix <- NULL
  if (theWidget("normalise_recenter_radiobutton")$getActive())
  {
    action <- "recenter"
    vprefix <- "RRC_"
  }
  else if (theWidget("normalise_scale01_radiobutton")$getActive())
  {
    action <- "scale01"
    vprefix <- "R01_"
  }
  else if (theWidget("normalise_rank_radiobutton")$getActive())
  {
    action <- "rank"
    vprefix <- "RRK_"
  }
  else if (theWidget("normalise_medianad_radiobutton")$getActive())
  {
    action <- "medianad"
    vprefix <- "RMD_"
  }
  else if (theWidget("normalise_bygroup_radiobutton")$getActive())
  {
    action <- "bygroup"
    vprefix <- "RBG"
  }
  else if (theWidget("rescale_matrix_radiobutton")$getActive())
  {
    action <- "matrix"
    vprefix <- "RMA_"
  }
  
  # Obtain the list of selected variables from the treeview.

  variables <- NULL
  selected <- theWidget("impute_treeview")$getSelection()
  selected$selectedForeach(function(model, path, iter, data)
  {
    variables <<- c(variables, model$get(iter, 1)[[1]])
  }, TRUE)

  # We check here if the action is rescale, and we have any
  # categoric variables to be normalised. If so put up an info
  # dialogue and remove the categorics from the list of variables to
  # be normalised.

  classes <- unlist(lapply(variables, function(x) class(crs$dataset[[x]])))

  # 080328 For any ordered factors class returns two values (since the
  # object inherits first from ordered and then factor), so we remove
  # the "ordered" from the list to hopefully get back to the right
  # length for classes (i.e., one class for each variable). We do note
  # though that objects can inherit from multiple classes, and the
  # order presented is the order in which they inherit. I should
  # probably work with the unlist above to turn multiple results into
  # one, like "ordered_factor".

  classes <- classes[classes!="ordered"]
  
  if (action %in% c("recenter", "scale01", "rank", "medianad", "matrix")
      && "factor" %in% classes)
  {
    infoDialog(sprintf(paste('We can not rescale using "%s"',
                             "on a categoric variable.",
                             "Ignoring: %s."),
                       action, paste(variables[which(classes == "factor")],
                                     collapse=", ")))
    variables <- variables[-which(classes == "factor")] # Remove the factors.
    if (length(variables) == 0) return()
  }

  # Check if, for a BYGROUP, we have at most one categoric and the
  # others are numeric. Then remove the categoric (if any) from the
  # list of variables and store its name in byvname. This allows us to
  # continue to use the loop below, having just the numeric variables
  # in the list. TODO Allow multiple categorics and then group
  # across all the cateogircals: MaleMarried MaleDivorced
  # FemaleMarried etc.

  if (action %in% c("bygroup"))
  {
    numfactors <- sum(classes=="factor")
    numnumerics <- sum(classes=="numeric" | classes=="integer")

    # Ensure we have just one categoric variable. [080315 gjw] Allow
    # the case where we have no categorics, and do the normalisation
    # over the whole population rather than stratifying.
    
    #if (numfactors == 0)
    #{
    #  infoDialog(paste("We must have a categoric variable to group by for",
    #                   "the By Group option. Please select one categoric",
    #                   "variable."))
    #  return()
    #}

    # Ensure we have at least one numeric variable.
    
    if (numnumerics == 0)
    {
      infoDialog(paste("We must have a numeric variable to normalise for the",
                       "By Group option. Please select one numeric variable."))
      return()
    }

    # Currently, only support grouping by a single categoric. TODO
    # Support a group by of multiple categorics.
    
    if (numfactors > 1)
    {
      infoDialog(paste("We only support By Group with a single categoric",
                       "variable for now. \n\nPlease select just one",
                       "categoric."))
      return()
    }

    # All looks okay, so let's set things up.

    if (numfactors == 0)
      byvname <- NULL
    else
    {
      byvname <- variables[which(classes=="factor")]
      variables <- variables[-which(classes == "factor")]
    }
  }
  
  startLog("RESCALE Variables")
  
  # Make sure we have the reshape library from where the rescaler
  # function comes.
  
  if (action %in% c("scale01", "rank", "medianad", "bygroup"))
  {
    if (! packageIsAvailable("reshape", "normalise data")) return()
    lib.cmd <- "require(reshape, quietly=TRUE)"
    appendLog("The reshape package provides the rescaler function.", lib.cmd)
    eval(parse(text=lib.cmd))
  }

  # Record the current variable roles so that we can maintain these,
  # modified appropriately by ignore'ing the imputed variables, and
  # input'ing the newly imputed variables.
  
  input <- getSelectedVariables("input")
  target <- getSelectedVariables("target")
  risk <- getSelectedVariables("risk")
  ident <- getSelectedVariables("ident")
  ignore <- getSelectedVariables("ignore")

  # For MATRIX obtain the matrix totla first and then divide each
  # column by this.

  if (action == "matrix")
  {
    matrix.total <- 0
    total.cmd <- sprintf(paste("matrix.total <- sum(crs$dataset[,",
                               'c("%s")],',
                               "na.rm=TRUE)"),
                         paste(variables, collapse='", "'))
    appendLog("Calculate matrix total", total.cmd)
    eval(parse(text=total.cmd))
  }
    
  for (v in variables)
  {
    # Create the new name for the variable.
    
    if (action %in% c("bygroup"))
      vname <- paste(vprefix, byvname, v, sep="_")
    else
      vname <- paste(vprefix, v, sep="")
    # Check variable specific preconditions, and if we fail then
    # proceed to next variable.

    if (action == "medianad")
    {
      # 080609 For audit$Deductions this returns all NaN or Inf
      # because the median is 0. We can see this with
      # rescaler((crs$dataset[["Deductions"]]), "robust") So check for
      # this and do nothing!

      median.cmd <- sprintf('median(crs$dataset[["%s"]], na.rm=TRUE)', v)
      if (eval(parse(text=median.cmd)) == 0)
      {
        warnDialog(sprintf('The variable "%s" has a median of 0.', v),
                   "We can not compute the Median/MAD Rescaler",
                   "for this variable.")
        next()
      }
    }
         
    # Generate the command to copy the current variable into a new
    # variable, prefixed appropraitely.

    copy.cmd <- sprintf('crs$dataset[["%s"]] <<- crs$dataset[["%s"]]',
                        vname, v)
    cl <- class(crs$dataset[[v]])

    # Take a copy of the variable to be imputed.
    
    appendLog(sprintf("RESCALE %s.", v), gsub("<<-", "<-", copy.cmd))
    eval(parse(text=copy.cmd))
    
    # Determine what action to perform.
    
    if (action == "recenter")
    {
      norm.cmd <- sprintf(paste('crs$dataset[["%s"]] <<-',
                                'scale(crs$dataset[["%s"]])[,1]'), vname, v)
      norm.comment <- "Recenter and rescale the data around 0."
    }
    else if (action == "scale01")
    {
      norm.cmd <- sprintf(paste('crs$dataset[["%s"]] <<- ',
                                'rescaler((crs$dataset[["%s"]]), "range")'),
                          vname, v)
      norm.comment <- "Rescale to [0,1]."

      # Record the transformation for inclusion in PMML.

      crs$transforms <<- union(crs$transform,
                               paste(vname,
                                     min(crs$dataset[[vname]]),
                                     max(crs$dataset[[vname]]), sep="_"))
    }
    else if (action == "rank")
    {
      norm.cmd <- sprintf(paste('crs$dataset[["%s"]] <<- ',
                                'rescaler((crs$dataset[["%s"]]), "rank")'),
                          vname, v)
      norm.comment <- "Convert values to ranks."
    }
    else if (action == "medianad")
    {
      norm.cmd <- sprintf(paste('crs$dataset[["%s"]] <<- ',
                                'rescaler((crs$dataset[["%s"]]), "robust")'),
                          vname, v)
      norm.comment <- paste("Rescale by subtracting median and dividing",
                            "by median abs deviation.")
    }
    else if (action == "bygroup")
    {
      # v <- current numeric variable name from variables
      # byvname <- categoric variable name (no longer in variables)
      # vname <-  the new variable name set up as above

      if (is.null(byvname))
        norm.cmd <- sprintf(paste('crs$dataset[["%s"]] <<- 0\n',
                                  'crs$dataset[, ',
                                  '"%s"] <<-\n',
                                  '    rescaler(crs$dataset[',
                                  ', "%s"], "range") * 99',
                                  sep=""),
                            vname, vname, v)
      else
        norm.cmd <- sprintf(paste('bylevels <- levels(crs$dataset[["%s"]])\n',
                                  'crs$dataset[["%s"]] <<- 0\n',
                                  'for (vl in bylevels) \n',
                                  '  crs$dataset[crs$dataset[["%s"]]==vl, ',
                                  '"%s"] <<-\n',
                                  '    rescaler(crs$dataset[crs$dataset',
                                  '[["%s"]]',
                                  '==vl, "%s"], "range") * 99\n',
                                  'crs$dataset[is.nan(crs$dataset[["%s"]]), ',
                                  '"%s"] <<- 99',
                                  sep=""),
                            byvname, vname, byvname, vname, byvname, v,
                            vname, vname)

      
      norm.comment <- "Rescale to 0-100 within each group."
    }
    else if (action == "matrix")
    {
      norm.cmd <- sprintf(paste('crs$dataset[["%s"]] <<- ',
                                'crs$dataset[["%s"]]/matrix.total'),
                          vname, v)
      norm.comment <- "Dvidie column values by matrix total."
    }

    appendLog(norm.comment, gsub("<<-", "<-", norm.cmd))
    eval(parse(text=norm.cmd))

    # Now update the variable roles.
    
    if (v %in% input)
    {
      input <- setdiff(input, v)
      input <- union(input, vname)
    }
    else if (v %in% target)
    {
      target <- setdiff(target, v)
      target <- union(target, vname)
    }
    else if (v %in% risk)
    {
      risk <- setdiff(risk, v)
      risk <- union(risk, vname)
    }
    else if (v %in% ident)
    {
      ident <- setdiff(ident, v)
      ident <- union(ident, vname)
    }
    else
    {
      # If the source variable was ignore, then leave it as such, and
      # put the new variable in as input.

      input <- union(input, vname)
    }
    ignore <- union(ignore, v)

    # Record the transformation for possible inclusion in PMML.

    # crs$transforms <<- union(crs$transform, vname)
  }
  
  if (length(variables) > 0)
  {
    
    # Reset the dataset views keeping the roles unchanged except for
    # those that have been normalised, wich have just been added as
    # inputs, with the originals now ignored.

    resetDatasetViews(input, target, risk, ident, ignore)

    # Update the status bar

    setStatusBar(sprintf(paste("Normalised variables added to the dataset",
                               "with '%s' prefix."), vprefix))
  }
  else
  {
    warnDialog(paste("No variables have been selected for normalisation.",
                     "Please select some variables and Execute again."))
    setStatusBar("No variables selected to be normalised.")
  }
}  

executeTransformImputePerform <- function()
{
  # First determine which imputation option has been chosen and the
  # prefix of the new variable that will be introduced.  Default to
  # NULL so that if the value is not changed, we may get error (it
  # should be an error if the value is not changed).

  # [TODO 071124] The rdaio buttons could be checkbuttons, and we do
  # multiple imputations for the selected variables, but for now, stay
  # with radio buttons as it is simply, without loss of functionality.
  
  action <- NULL
  vprefix <- NULL
  if (theWidget("impute_zero_radiobutton")$getActive())
  {
    action <- "zero"
    vprefix <- "IZR" # May want to distinguish ZERO and MISSING
  }
  else if (theWidget("impute_mean_radiobutton")$getActive())
  {
    action <- "mean"
    vprefix <- "IMN"
  }
  else if (theWidget("impute_median_radiobutton")$getActive())
  {
    action <- "median"
    vprefix <- "IMD"
  }
  else if (theWidget("impute_mode_radiobutton")$getActive())
  {
    action <- "mode"
    vprefix <- "IMO"
  }
  else if (theWidget("impute_constant_radiobutton")$getActive())
  {
    action <- "constant"
    vprefix <- "ICN"
  }
  
  # Obtain the list of selected variables from the treeview.

  imputed <- NULL
  selected <- theWidget("impute_treeview")$getSelection()
  selected$selectedForeach(function(model, path, iter, data)
  {
    imputed <<- c(imputed, model$get(iter, 1)[[1]])
  }, TRUE)

  if (is.null(imputed)) 
    warnDialog(paste("No variables have been selected for imputation.",
                     "Please select some variables and Execute again."))
  # We check here if the action is mean or median, and we have any
  # categoric variables to be imputed. If so put up an info dialogue
  # and remove the cateorigcals from the list of variables to be
  # imputed.

  classes <- unlist(lapply(imputed, function(x) class(crs$dataset[[x]])))
  if (action %in% c("mean", "median") && "factor" %in% classes)
  {
    infoDialog(sprintf(paste("We can not impute the %s for a",
                             "categoric variable. Ignoring: %s."),
                       action, paste(imputed[which(classes == "factor")],
                                     collapse=", ")))
    imputed <- imputed[-which(classes == "factor")] # Remove the factors.
  }
  
  # OLD CODE
  
  #zero   <- getSelectedVariables("zero")
  #mean   <- getSelectedVariables("mean")
  #median <- getSelectedVariables("median")

  #imputed <- union(zero, union(mean, median))
  
  # Record the current variable roles so that we can maintain these,
  # modified appropriately by ignore'ing the imputed variables, and
  # input'ing the newly imputed variables.
  
  input <- getSelectedVariables("input")
  target <- getSelectedVariables("target")
  risk <- getSelectedVariables("risk")
  ident <- getSelectedVariables("ident")
  ignore <- getSelectedVariables("ignore")

  if (length(imputed) > 0) startLog("MISSING VALUE IMPUTATION")

  # [TODO 071124] The following code could be tidied up quite a
  # bit. It has evolved. Bits of the code handling the categorics
  # were copied from the numeric parts and vice versa, and they do it
  # different ways. Should try to do it the same way. Works for now!
      
  startLog("IMPUTE Missing Values")
  for (z in imputed)
  {
    # Generate the command to copy the current variable into a new
    # variable, prefixed appropraitely.
    
    vname <- paste(vprefix, z, sep="_")
    copy.cmd <- sprintf('crs$dataset[["%s"]] <<- crs$dataset[["%s"]]',
                            vname, z)
    cl <- class(crs$dataset[[z]])
    if (cl == "factor")
    {
      # Mean and median are not supported for categorics!

      if (action == "zero")
      {

        # Take a copy of the variable to be imputed.
    
        appendLog(sprintf("IMPUTE %s.", z), gsub("<<-", "<-", copy.cmd))
        eval(parse(text=copy.cmd))
             
        # If "Missing" is not currently a category for this variable,
        # add it in.

        if ("Missing" %notin% levels(crs$dataset[[vname]]))
        {
          levels.cmd <- sprintf(paste('levels(crs$dataset[["%s"]]) <<-',
                                      'c(levels(crs$dataset[["%s"]]),',
                                      '"Missing")'),
                                vname, vname)
          appendLog('Add a new category "Missing" to the variable',
                    gsub("<<-", "<-", levels.cmd))
          eval(parse(text=levels.cmd))
        }
      
        # Change all NAs to Missing.
      
        missing.cmd <- sprintf(paste('crs$dataset[["%s"]][is.na(',
                                     'crs$dataset[["%s"]])] <<- "Missing"',
                                     sep=""),
                               vname, z)
        appendLog('Change all NAs to "Missing"',gsub("<<-", "<-", missing.cmd))
        eval(parse(text=missing.cmd))
      }
      else if (action == "mode")
      {
        # Take a copy of the variable to be imputed.
    
        appendLog(sprintf("IMPUTE %s.", z), gsub("<<-", "<-", copy.cmd))
        eval(parse(text=copy.cmd))
             
        imp.cmd <- sprintf(paste('crs$dataset[["%s"]]',
                                 '[is.na(crs$dataset[["%s"]])]',
                                 ' <<- modalvalue(crs$dataset[["%s"]], ',
                                 "na.rm=TRUE)", sep=""), vname, z, z)
        appendLog("Change all NAs to the modal value (not advisable).",
                  gsub("<<-", "<-", imp.cmd))
        eval(parse(text=imp.cmd))
      }
      else if (action == "constant")
      {
        # Take a copy of the variable to be imputed.
    
        appendLog(sprintf("IMPUTE %s.", z), gsub("<<-", "<-", copy.cmd))
        eval(parse(text=copy.cmd))
             
        val <- theWidget("impute_constant_entry")$getText()

        # If val is not currently a category for this variable, add it
        # in.

        if (val %notin% levels(crs$dataset[[vname]]))
        {
          levels.cmd <- sprintf(paste('levels(crs$dataset[["%s"]]) <<-',
                                      'c(levels(crs$dataset[["%s"]]),',
                                      sprintf('"%s")', val)),
                                vname, vname)
          appendLog(sprintf('Add a new category "%s" to the variable', val), 
                    gsub("<<-", "<-", levels.cmd))
          eval(parse(text=levels.cmd))
        }

        imp.cmd <- sprintf(paste('crs$dataset[["%s"]]',
                                 '[is.na(crs$dataset[["%s"]])]',
                                 ' <<- "%s"', sep=""), vname, z, val)
        appendLog(sprintf("Change all NAs to the constant value: %s", val),
                  gsub("<<-", "<-", imp.cmd))
        eval(parse(text=imp.cmd))
      }
      else
        infoDialog(sprintf(paste("The option to impute the %s for the",
                                 "categoric variable (%s) is not (yet)",
                                 "available."), action, z))
    }
    else
    {
      # Take a copy of the variable to be imputed.
    
      appendLog(sprintf("IMPUTE %s.", z), gsub("<<-", "<-", copy.cmd))
      eval(parse(text=copy.cmd))
      
      # Determine what action to perform.

      if (action == "zero")
      {
        imp.cmd <- sprintf(paste('crs$dataset[["%s"]]',
                                 '[is.na(crs$dataset[["%s"]])]',
                                 " <<- 0", sep=""), vname, z)
        imp.comment <- "Change all NAs to 0."
      }
      else if (action == "mean")
      {
        # Note that if z is an integer (e.g. audit$Age) then the
        # imputed column will be numeric.
        
        imp.cmd <- sprintf(paste('crs$dataset[["%s"]]',
                                 '[is.na(crs$dataset[["%s"]])]',
                                 ' <<- mean(crs$dataset[["%s"]], ',
                                 "na.rm=TRUE)", sep=""), vname, z, z)
        imp.comment <- "Change all NAs to the mean value (not advisable)."
      }
      else if (action == "median")
      {
        imp.cmd <- sprintf(paste('crs$dataset[["%s"]]',
                                 '[is.na(crs$dataset[["%s"]])]',
                                 ' <<- median(crs$dataset[["%s"]], ',
                                 "na.rm=TRUE)", sep=""), vname, z, z)
        imp.comment <- "Change all NAs to the median (not advisable)."
      }
      else if (action == "mode")
      {
        imp.cmd <- sprintf(paste('crs$dataset[["%s"]]',
                                 '[is.na(crs$dataset[["%s"]])]',
                                 ' <<- modalvalue(crs$dataset[["%s"]], ',
                                 "na.rm=TRUE)", sep=""), vname, z, z)
        imp.comment <- "Change all NAs to the modal value (not advisable)."
      }
      else if (action == "constant")
      {
        val <- theWidget("impute_constant_entry")$getText()
        if (is.na(as.numeric(val)))
        {
          errorDialog(sprintf(paste('The supplied value of "%s" for the variable "%s"',
                                    'is not numeric. Please supply a numeric value.'),
                              val, z))
          next
        }
        imp.cmd <- sprintf(paste('crs$dataset[["%s"]]',
                                 '[is.na(crs$dataset[["%s"]])]',
                                 ' <<- %s ', sep=""), vname, z, val)
        imp.comment <- sprintf("Change all NAs to the constant: %s.", val)
      }
        
      appendLog(imp.comment, gsub("<<-", "<-", imp.cmd))
      eval(parse(text=imp.cmd))
    }
    if (z %in% input)
    {
      input <- setdiff(input, z)
      input <- union(input, vname)
    }
    else if (z %in% target)
    {
      target <- setdiff(target, z)
      target <- union(target, vname)
    }
    else if (z %in% risk)
    {
      risk <- setdiff(risk, z)
      risk <- union(risk, vname)
    }
    else if (z %in% ident)
    {
      ident <- setdiff(ident, z)
      ident <- union(ident, vname)
    }
    else
    {
      # If the source variable was ignore, then leave it as such, and
      # put the new variable in as input.
      input <- union(input, vname)
    }
    ignore <- union(ignore, z)
  }
  
  if (length(imputed) > 0)
  {
    # Reset the dataset views keeping the roles unchanged except for
    # those that have been imputed, wich have just been added as
    # inputs, with the originals now ignored.

    resetDatasetViews(input, target, risk, ident, ignore)

    # Update the status bar

    setStatusBar(sprintf(paste("Imputed variables added to the dataset",
                               "with '%s_' prefix."), vprefix))
  }
  else
  {
    setStatusBar("No variables selected to be imputed.")
  }
}  

#-----------------------------------------------------------------------

binning <- function (x, bins=4, method=c("quantile", "kmeans"),
                     labels=NULL, ordered=TRUE)
{
  # From Daniele Medri 31 Jan 2007.

  # Set ordered to FALSE in Rattle since randomForests don't work when
  # the factor is ordered, for some reason (080406).
  
  # Best k for natural breaks

  varkmeans <- function (x, centers, iter.max=10, num.seeds=bins)
  {
    if (mode(x) == "numeric")
    {
      x <- data.frame(new.x=x)
    }
    KM <- kmeans(x=x, centers=centers, iter.max=iter.max)
    for (i in 1:num.seeds)
    {
      newKM <- kmeans(x=x, centers=centers, iter.max=iter.max)
      if (sum(newKM$withinss) < sum(KM$withinss))
      {
        KM <- newKM
      }
    }
    KM$tot.withinss <- sum(KM$withinss)
    xmean <- apply(x, 2, mean)
    centers <- rbind(KM$centers, xmean)
    bss1 <- as.matrix(dist(centers)^2)
    KM$betweenss <- sum(as.vector(bss1[nrow(bss1), ]) * c(KM$size, 0))
    return(KM)
  }

  method <- match.arg(method)
  if(is.factor(x)) stop("this var is already a factor")
  if (is.data.frame(x)) stop("is needed an object of class data.frame")
  if (length(x) < bins) stop("more classes than obs")
  
  # Binning

  x <- if (method == "quantile")
  {
    breaks <- c(quantile(x, probs = seq(0, 1, 1/bins), na.rm = TRUE, type=8))
    breaks <- unique(breaks)
    breaks[1] <- min(x)
    breaks[length(breaks)] <- max(x)
    # quantiles from quantile() can be non-unique, which cut() doesn't
    # like. This is handled above through unique(). The function
    # cut2() in Hmisc handles this situation gracefully and it could
    # be used, but it is not necessary.
    if(length(breaks) >= 2)
    {
      cut(x, breaks, include.lowest = TRUE, labels = labels)
    }
    else
    {
      cat("Warning: var not considered\n")
      return(NULL)
    }
  }
  else if(method == "kmeans")
  {
    xx <- na.omit(x)
    maxbins <-nlevels(as.factor(xx))
    if(maxbins < bins)
    { 
      bins <-maxbins
    }
    breaks <- c(min(xx), tapply(xx, varkmeans(xx, bins)$cluster, max))
    if (length(unique(breaks)) >= 2)
    {
      cut(x, unique(breaks), include.lowest = TRUE, labels = labels)	
    }
    else
    {
      cat("Warning: var not considered\n")
      return(NULL)	
    }
  }
  if(ordered == TRUE)
  {
    ordered(factor(x))
  }
  else
  {
    factor(x)
  }
}

executeTransformRemapPerform <- function()
{
  # Remap variables in some way.

  # Obtain the list of selected variables from the treeview.

  vars <- NULL
  selected <- theWidget("impute_treeview")$getSelection()
  selected$selectedForeach(function(model, path, iter, data)
  {
    vars <<- c(vars, model$get(iter, 1)[[1]])
  }, TRUE)

  if (length(vars) == 0)
  {
    infoDialog("Please select some variables to remap first. Then Execute.")
    return()
  }

  # Record the current variable roles so that we can maintain
  # these, modified appropriately.
  
  input <- getSelectedVariables("input")
  target <- getSelectedVariables("target")
  risk <- getSelectedVariables("risk")
  ident <- getSelectedVariables("ident")
  ignore <- getSelectedVariables("ignore")

  # Determine the action requested.

  if (theWidget("remap_quantiles_radiobutton")$getActive())
  {
    action <- "quantiles"
    num.bins <- theWidget("remap_bins_spinbutton")$getValue()
    remap.prefix <- sprintf("BQ%d", num.bins)
    remap.comment <- sprintf(paste("Bin the variable into %d bins",
                                   "using quantiles."), num.bins)
  }
  else if (theWidget("remap_kmeans_radiobutton")$getActive())
  {
    action <- "kmeans"
    num.bins <- theWidget("remap_bins_spinbutton")$getValue()
    remap.prefix <- sprintf("BK%d", num.bins)
    remap.comment <- sprintf(paste("Bin the variable into %d bins",
                                   "using kmeans."), num.bins)
  }
  else if (theWidget("remap_eqwidth_radiobutton")$getActive())
  {
    action <- "eqwidth"
    num.bins <- theWidget("remap_bins_spinbutton")$getValue()
    remap.prefix <- sprintf("BE%d", num.bins)
    remap.comment <- sprintf(paste("Bin the variable into %d bins",
                                   "using equal widths."), num.bins)
  }
  else if (theWidget("remap_indicator_radiobutton")$getActive())
  {
    action <- "indicator"
    remap.prefix <- "IN"
    remap.comment <- "Turn a factor into indicator variables"
  }
  else if (theWidget("remap_joincat_radiobutton")$getActive())
  {
    action <- "joincat"
    remap.prefix <- "JN"
    remap.comment <- "Turn two factors into one factor"
  }
  else if (theWidget("remap_log_radiobutton")$getActive())
  {
    action <- "log"
    remap.prefix <- "LG"
    remap.comment <- "Log transform."
  }
  else if (theWidget("remap_asfactor_radiobutton")$getActive())
  {
    action <- "asfactor"
    remap.prefix <- "FC"
    remap.comment <- "Transform into a Factor."
  }
  else if (theWidget("remap_asnumeric_radiobutton")$getActive())
  {
    action <- "asnumeric"
    remap.prefix <- "NM"
    remap.comment <- "Transform into a Numeric."
  }
  
  # Check if the action is one that only works on numeric data, and we
  # have any categoric variables selected. If so put up an info
  # dialogue and remove the categorics from the list of variables to
  # be imputed.

  classes <- unlist(lapply(vars, function(x) class(crs$dataset[[x]])))
  if (action %in% c("quantiles", "kmeans", "eqwidth", "log")
      && "factor" %in% classes)
  {
    infoDialog(sprintf(paste("We can only handle numeric data for %s.",
                             "Ignoring: %s."), action,
                       paste(vars[which(classes == "factor")], collapse=", ")))
    vars <- vars[-which(classes == "factor")] # Remove the factors.
  }
  if (action %in% c("indicator", "joincat")
      && ("numeric" %in% classes || "integer" %in% classes))
  {
    infoDialog(sprintf(paste("We can only handle non numeric data for %s.",
                             "Ignoring: %s."), action,
                       paste(vars[which(classes == "numeric" ||
                                        classes == "integer")],
                             collapse=", ")))
    vars <- vars[-which(classes == "numeric" || classes == "integer")]
  }

  # If, as a result of removing variables from consideration we end up
  # with no variables left, silenty exit as we have already popped up
  # a meassage about removing the categoric variables.
  
  if (length(vars) == 0) return()

  # Now that we know which variables we are remapping, we can specify
  # the actions. 080406 We set ordered=FALSE here for now because
  # randomForest does not handle them. Andy is working on it.
  
  if (action == "quantiles")
  {
    remap.cmd <- paste(sprintf(paste('crs$dataset[["%s_%s"]] <<- binning(crs$',
                                     'dataset[["%s"]], %d, method="quantile",',
                                     'ordered=FALSE)',
                                     sep=""),
                               remap.prefix, vars, vars, num.bins),
                       collapse="\n")
  }
  else if (action == "kmeans")
  {
    remap.cmd <- paste(sprintf(paste('crs$dataset[["%s_%s"]] <<- binning(crs$',
                                     'dataset[["%s"]], %d, method="kmeans",',
                                     'ordered=FALSE)',
                                     sep=""),
                               remap.prefix, vars, vars, num.bins),
                       collapse="\n")
  }
  else if (action == "eqwidth")
  {
    remap.cmd <- paste(sprintf(paste('crs$dataset[["%s_%s"]] <<- cut(crs$',
                                     'dataset[["%s"]], %d)',
                                     sep=""),
                               remap.prefix, vars, vars, num.bins),
                       collapse="\n")
  }
  else if (action == "indicator")
  {
    remap.cmd <- paste(sprintf(paste('crs$dataset[, paste("%s_%s_", levels(',
                                     'crs$dataset[["%s"]]), sep="")] ',
                                     '<<- diag(nlevels(',
                                     'crs$dataset[["%s"]]))[crs$dataset',
                                     '[["%s"]],]',
                                     sep=""),
                               remap.prefix, vars, vars, vars, vars),
                       collapse="\n")
  }
  else if (action == "joincat")
  {
    if (length(vars) != 2)
    {
      infoDialog("We only join two categorics at a time.",
                 "Please select just two.")
      return()
    }
      
    remap.cmd <- sprintf(paste('crs$dataset[, "%s_%s_%s"] <<- ',
                               'interaction(paste(crs$dataset[["%s"]], "_",',
                               'crs$dataset[["%s"]], sep=""))',
                               sep=""),
                         remap.prefix, vars[1], vars[2],
                         vars[1], vars[2])
  }
  else if (action == "log")
  {
    remap.cmd <- paste(sprintf(paste('crs$dataset[["%s_%s"]] <<- log(crs$',
                                     'dataset[["%s"]])', sep=""),
                               remap.prefix, vars, vars),
                       collapse="\n")
  }
  else if (action == "asfactor")
  {
    remap.cmd <- paste(sprintf(paste('crs$dataset[["%s_%s"]] <<- ',
                                     'as.factor(crs$',
                                     'dataset[["%s"]])', sep=""),
                               remap.prefix, vars, vars),
                       collapse="\n")
  }
  else if (action == "asnumeric")
  {
    remap.cmd <- paste(sprintf(paste('crs$dataset[["%s_%s"]] <<- ',
                                     'as.numeric(crs$',
                                     'dataset[["%s"]])', sep=""),
                               remap.prefix, vars, vars),
                       collapse="\n")
  }
  
  # Perform the remapping.

  startLog("REMAP Variables")
  appendLog(remap.comment, gsub("<<-", "<-", remap.cmd))
  eval(parse(text=remap.cmd))

  # Record the new variables as having an INPUT role. No other changes
  # as the original variables are probably still required for
  # modelling.

  if (action == "joincat")
    input <- union(input, paste(remap.prefix, vars[1], vars[2], sep="_"))
  else if (action == "indicator")
    input <- union(input, paste(remap.prefix, vars,
                                levels(crs$dataset[[vars]]), sep="_"))
  else
    input <- union(input, paste(remap.prefix, vars, sep="_"))

  # Reset the dataset views keeping the roles unchanged except for
  # those that have been created, wich have just been added as inputs.

  resetDatasetViews(input, target, risk, ident, ignore)
  
  # Update the status bar
  
  setStatusBar(sprintf(paste("Remapped variables added to the dataset",
                             "with '%s' prefix."), remap.prefix))
}

#-----------------------------------------------------------------------

executeTransformCleanupPerform <- function()
{
  # First, record the current variable roles so that we can maintain
  # these, modified appropriately.
  
  input <- getSelectedVariables("input")
  target <- getSelectedVariables("target")
  risk <- getSelectedVariables("risk")
  ident <- getSelectedVariables("ident")
  ignore <- getSelectedVariables("ignore")

  startLog("CLEANUP the Dataset")

  if (theWidget("delete_ignored_radiobutton")$getActive())
  {
    if (variablesHaveChanged("deleting the selected ignored variables")) return()
    to.delete <- getSelectedVariables("ignore")
  }    
  else if (theWidget("delete_selected_radiobutton")$getActive())
  {

    # Obtain the list of selected variables from the treeview.

    to.delete <- NULL
    selected <- theWidget("impute_treeview")$getSelection()
    selected$selectedForeach(function(model, path, iter, data)
    {
      to.delete <<- c(to.delete, model$get(iter, 1)[[1]])
    }, TRUE)

    if (length(to.delete) == 0)
    {
      infoDialog("Please select some variables to delete first. Then Execute.")
      return()
    }
  }
  else if (theWidget("delete_navars_radiobutton")$getActive())
  {
    # Get a list of all variables. For now (and perhaps always),
    # ignore the role.
    
    to.delete <- names(crs$dataset)

    # Remove from the list any variables that do not have missing
    # values.
    
    for (v in to.delete)
      if (sum(is.na(crs$dataset[[v]])) == 0)
        to.delete <- setdiff(to.delete, v)
  }
  else if (theWidget("delete_naents_radiobutton")$getActive())
  {
    # Here, ignore the variables that have a role of Ignore, so we
    # only delete entities that have missing values for non-ignored
    # variables.

    if (is.null(ignore))
    {
      cases <- complete.cases(crs$dataset)
      del.cmd <- "crs$dataset <<- crs$dataset[complete.cases(crs$dataset),]"
    }
    else
    {
      cases <- complete.cases(crs$dataset[,-getVariableIndicies(ignore)])
      del.cmd <- sprintf(paste("crs$dataset <<- crs$dataset[complete.cases(",
                               "crs$dataset[,-%s]),]", sep=""),
                         simplifyNumberList(getVariableIndicies(ignore)))
    }
    
    if (! questionDialog(sprintf(paste("We are about to delete %d",
                                       "entites from the dataset."),
                                 sum(!cases)),
                         "These have missing values for some of the",
                         "non-Ignore variables.\n\nAre you sure you",
                         "want to delete these entites?"))
      return()

    # Perform the deletions.
  
    appendLog("Remove rows with missing values", gsub("<<-", "<-", del.cmd))
    eval(parse(text=del.cmd))

  }

  if (!theWidget("delete_naents_radiobutton")$getActive())
  {
    
    if (! questionDialog("We are about to delete the following variables.",
                         "This will permanently remove them from",
                         "the memory copy of the data, but will not",
                         "affect any file system copy.\n\n",
                         "Delete:",
                         paste(to.delete, collapse=", "),
                         "\n\nAre you sure you want to delete these",
                         "variables?"))
      return()

    del.cmd <- paste(sprintf('crs$dataset$%s <<- NULL', to.delete),
                     collapse="\n")
    del.comment <- "Remove specific columns from the dataset."

    # Perform the deletions.
  
    appendLog(del.comment, gsub("<<-", "<-", del.cmd))
    eval(parse(text=del.cmd))

    # Ensure any delted variables are no longer included in the list
    # of transformed variables.

    crs$transforms <<- crs$transforms[! sapply(crs$transforms,
                                               function(x) sub('_[^_]*_[^_]*$', '', x))
                                      %in% to.delete]

  }
  
  # Reset the dataset views keeping the roles unchanged except for
  # those that have been delete.

  resetDatasetViews(input, target, risk, ident, ignore)

  # Update the status bar

  setStatusBar("The deletions from the dataset have been completed.")
}


########################################################################
# EXPLORE TAB

#----------------------------------------------------------------------
# Interface Actions

# TODO 080519 The following need to be simplified in much the same way
# that we handle the show/hide of the widegts on the Data Tab.

# When a radio button is selected, display the appropriate tab

on_summary_radiobutton_toggled <- function(button)
{
  #separator       <- theWidget("explore_vseparator")
  summary.button  <- theWidget("summary_checkbutton")
  describe.button <- theWidget("describe_checkbutton")
  basics.button   <- theWidget("basics_checkbutton")
  kurtosis.button <- theWidget("kurtosis_checkbutton")
  skewness.button <- theWidget("skewness_checkbutton")
  missing.button   <- theWidget("missing_checkbutton")
  if (button$getActive())
  {
    .EXPLORE$setCurrentPage(.EXPLORE.SUMMARY.TAB)
    #separator$show()
    summary.button$show()
    describe.button$show()
    basics.button$show()
    kurtosis.button$show()
    skewness.button$show()
    missing.button$show()
  }
  else
  {
    #separator$hide()
    summary.button$hide()
    describe.button$hide()
    basics.button$hide()
    kurtosis.button$hide()
    skewness.button$hide()
    missing.button$hide()
  }
  setStatusBar()
}

on_explot_radiobutton_toggled <- function(button)
{
  #separator <- theWidget("explore_vseparator")
  barbutton <- theWidget("benford_bars_checkbutton")
  absbutton <- theWidget("benford_abs_radiobutton")
  posbutton <- theWidget("benford_pos_radiobutton")
  negbutton <- theWidget("benford_neg_radiobutton")
  diglabel <- theWidget("benford_digits_label")
  digspin <- theWidget("benford_digits_spinbutton")

  if (button$getActive()) 
  {
    .EXPLORE$setCurrentPage(.EXPLORE.PLOT.TAB)
    #separator$show()
    barbutton$show()
    absbutton$show()
    posbutton$show()
    negbutton$show()
    diglabel$show()
    digspin$show()
  }
  else
  {
    #separator$hide()
    barbutton$hide()
    absbutton$hide()
    posbutton$hide()
    negbutton$hide()
    diglabel$hide()
    digspin$hide()
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
  #separator <- theWidget("explore_vseparator")
  nabutton    <- theWidget("correlation_na_checkbutton")
  ordbutton   <- theWidget("explore_correlation_ordered_checkbutton")
  methodlabel <- theWidget("explore_correlation_method_label")
  methodbox   <- theWidget("explore_correlation_method_combobox")
  if (button$getActive()) 
  {
    .EXPLORE$setCurrentPage(.EXPLORE.CORRELATION.TAB)
    #separator$show()
    nabutton$show()
    ordbutton$show()
    methodlabel$show()
    methodbox$show()
  }
  else
  {
    #separator$hide()
    nabutton$hide()
    ordbutton$hide()
    methodlabel$hide()
    methodbox$hide()
  }
  setStatusBar()
}

on_hiercor_radiobutton_toggled <- function(button)
{
  methodlabel <- theWidget("explore_correlation_method_label")
  methodbox   <- theWidget("explore_correlation_method_combobox")
  if (button$getActive())
  {
    .EXPLORE$setCurrentPage(.EXPLORE.HIERCOR.TAB)
    methodlabel$show()
    methodbox$show()
  }
  else
  {
    methodlabel$hide()
    methodbox$hide()
  }
  setStatusBar()
}

on_prcomp_radiobutton_toggled <- function(button)
{
  if (button$getActive()) .EXPLORE$setCurrentPage(.EXPLORE.PRCOMP.TAB)
  setStatusBar()
}

########################################################################

cat_toggled <- function(cell, path.str, model)
{
  ## A categoric variable's radio button has been toggled in the
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
  ## Ensure categoric all check boxes are unchecked.

  set.cursor("watch")

  ## Only clear selected rows.

  tree.selection <- theWidget("categorical_treeview")$getSelection()

  # Use the data parameter to avoid an RGtk2 bug in 2.12.1, fixed in
  # next release. 071117
  tree.selection$selectedForeach(function(model, path, iter, data)
  {
    columns <- .CATEGORICAL[["barplot"]]:.CATEGORICAL[["mosplot"]]
    for (c in columns) if (model$get(iter, c)[[1]]) model$set(iter, c, FALSE)
    return(FALSE) # Keep going through all rows
  }, TRUE)

  set.cursor()
}

on_continuous_clear_button_clicked <- function(action, window)
{
  # Ensure all continuous check boxes are unchecked.

  set.cursor("watch")

  # Only clear selected rows.

  tree.selection <- theWidget("continuous_treeview")$getSelection()

  # Use the data parameter to avoid an RGtk2 bug in 2.12.1, fixed in
  # next release. 071117
  tree.selection$selectedForeach(function(model, path, iter, data)
  {
    columns <- .CONTINUOUS[["boxplot"]]:.CONTINUOUS[["benplot"]]
    for (c in columns) if (model$get(iter, c)[[1]]) model$set(iter, c, FALSE)
    return(FALSE) # Keep going through all rows
  }, TRUE)

  set.cursor()
}

on_summary_find_button_clicked <- function(window)
{
  search.str <- theWidget("summary_find_entry")$getText()
  tv <- theWidget("summary_textview")
  start.iter <- tv$getBuffer()$getStartIter()
  summarySearch(tv, search.str, start.iter)
}

on_summary_next_button_clicked <- function(window)
{
  search.str <- theWidget("summary_find_entry")$getText()
  tv <- theWidget("summary_textview")
  last.search.pos <- tv$getBuffer()$getMark('last.search.pos')
  if (is.null(last.search.pos)) return()
  last.search.iter <- tv$getBuffer()$getIterAtMark(last.search.pos)
  summarySearch(tv, search.str, last.search.iter)
}

on_viewdata_find_button_clicked <- function(window)
{
  ## Need to get the root window of the button, and everything else is
  ## in terms of that.

  root <- window$getRootWindow()
  search.str <- root$getWidget("viewdata_find_entry")$getText()
  print(search.str)
  tv <- viewdataGUI$getWidget("viewdata_textview")
  start.iter <- tv$getBuffer()$getStartIter()
  summarySearch(tv, search.str, start.iter)
}

on_viewdata_next_button_clicked <- function(window)
{
  search.str <- viewdataGUI$getWidget("viewdata_find_entry")$getText()
  tv <- viewdataGUI$getWidget("viewdata_textview")
  last.search.pos <- tv$getBuffer()$getMark('last.search.pos')
  if (is.null(last.search.pos)) return()
  last.search.iter <- tv$getBuffer()$getIterAtMark(last.search.pos)
  summarySearch(tv, search.str, last.search.iter)
}

summarySearch <- function(tv, search.str, start.iter)
{
  found <- start.iter$iter$forwardSearch(search.str, 0)
  tvb <- tv$getBuffer()
  if (found$retval)
  {
    tvb$selectRange(found$match.start, found$match.end)
    last.search.pos <-tvb$createMark('last.search.pos', found$match.end)

    tv$scrollToMark(last.search.pos, 0.2)
    while(gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)

    setStatusBar(sprintf('The string "%s" was found.', search.str))
  }
  else
    setStatusBar(sprintf('The string "%s" was not found.', search.str))
}

##----------------------------------------------------------------------
##
## Execution
##

executeExploreTab <- function()
{
  
  # Can not explore the data if there is no dataset.

  if (noDatasetLoaded()) return()

  # [080315 gjw] Don't proceed if the variable selections have
  # changed. For example, two targets might have been selected,
  # resulting in a popup, but not yet resolved, and so many of the
  # plots will fail.
  
  if (variablesHaveChanged("building a model")) return()

  # Ensure Sample does not require executing.

  sampling <- theWidget("sample_checkbutton")$getActive()
  if (sampling && sampleNeedsExecute()) return()

  # We generate a string representing the subset of the dataset on
  # which the exploration is to be performed. This is then passed to
  # the individually dispatched functions.

  vars <- getIncludedVariables(risk=TRUE)
  dataset <- sprintf("%s[%s,%s]", "crs$dataset",
                     ifelse(sampling, "crs$sample", ""),
                     ifelse(is.null(vars),"", vars))

  # For the distribution plot, we do list all variables in the
  # interface, even if they are ignored. TODO 061006 We could instead
  # grey out the ignored ones (i.e., make them not sensitive). But
  # for now, for plots, allow all variables, even the ignored ones,
  # and thus we need a dataset that includes all variables - the
  # "avdataset".

  avdataset <- sprintf("%s[%s,]", "crs$dataset",
                     ifelse(sampling, "crs$sample", ""))
  
  vars <- getIncludedVariables(numonly=TRUE)
  # TODO 060606 The question here is whether NULL means all variables
  # or means none found?
  
  #if (is.null(vars))
  #  ndataset <- NULL
  #else
    ndataset <- sprintf("%s[%s,%s]", "crs$dataset",
                        ifelse(sampling, "crs$sample", ""),
                        ifelse(is.null(vars),"",vars))

  # Numeric input variables

  vars <- inputVariables(numonly=TRUE)
  nidataset <- sprintf("%s[%s,%s]", "crs$dataset",
                       ifelse(sampling, "crs$sample", ""),
                       ifelse(is.null(vars),"",vars))
  
  # Dispatch
  
  if (theWidget("summary_radiobutton")$getActive())
    executeExploreSummary(dataset)
  else if (theWidget("explot_radiobutton")$getActive())
    executeExplorePlot(avdataset)
  else if (theWidget("ggobi_radiobutton")$getActive())
    executeExploreGGobi(dataset, crs$dataname)
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
  else if (theWidget("playwith_radiobutton")$getActive())
    executeExplorePlaywith(dataset)
}

executeExploreSummary <- function(dataset)
{
  TV <- "summary_textview"

  # Get the current state of the relevant buttons.
  
  use.sample  <- theWidget("sample_checkbutton")$getActive()
  do.summary  <- theWidget("summary_checkbutton")$getActive()
  do.describe <- theWidget("describe_checkbutton")$getActive()
  do.basics   <- theWidget("basics_checkbutton")$getActive()
  do.kurtosis <- theWidget("kurtosis_checkbutton")$getActive()
  do.skewness <- theWidget("skewness_checkbutton")$getActive()
  do.missing  <- theWidget("missing_checkbutton")$getActive()

  ## Make sure something has been selected.
  
  if (! (do.summary || do.describe || do.basics ||
         do.kurtosis || do.skewness || do.missing))
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
  
  startLog()
  theWidget(TV)$setWrapMode("none")
  resetTextview(TV)

  ## Construct and execute the requested commands.

  if (do.summary)
  {
    ## Find the number of entities with any missing value for the
    ## non-ignored variables.
    
    missing.cmd <- sprintf('length(attr((na.omit(%s)), "na.action"))', dataset)
    result <- try(missing <- eval(parse(text=missing.cmd)), silent=TRUE)
    if (inherits(result, "try-error")) missing <- 0
    
    ## A basic summary.

    summary.cmd <- sprintf("summary(%s)", dataset)
    appendLog("SUMMARY OF DATASET.", summary.cmd)
    appendTextview(TV,
                   paste("Summary of the ",
                         ifelse(use.sample && sampling, "** sample **", "full"),
                         " dataset.\n\n", sep=""),
                   sprintf(paste("The data contains %d entities",
                                 "with missing values."),
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
      appendLog("The describe command comes from Hmisc.", lib.cmd)
      eval(parse(text=lib.cmd))
      
      describe.cmd <- sprintf("describe(%s)", dataset)
      appendLog("Generate a description of the dataset.", describe.cmd)
      appendTextview(TV,
                     paste("Description of the",
                           ifelse(use.sample && sampling,
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
      appendLog("Use functionality from the fBasics package.", lib.cmd)
      eval(parse(text=lib.cmd))
      
      if (do.basics)
      {
        basics.cmd <- sprintf("lapply(%s[,%s], basicStats)", dataset,
                              ifelse(is.null(nvars), "", nvars))
        appendLog("Generate a summary of the numeric data.", basics.cmd)
        appendTextview(TV,
                       paste("Basic statistics for each numeric variable",
                             "of the",
                             ifelse(use.sample && sampling,
                                    "** sample **", "full"),
                             "dataset.\n\n"),
                       collectOutput(basics.cmd, TRUE))
      }
      
      if (do.kurtosis)
      {
        kurtosis.cmd <- sprintf("kurtosis(%s[,%s], na.rm=TRUE)", dataset,
                                ifelse(is.null(nvars), "", nvars))

        appendLog("Summarise the kurtosis of the numeric data.", kurtosis.cmd)
        appendTextview(TV,
                       paste("Kurtosis for each numeric variable ",
                             "of the ",
                             ifelse(use.sample && sampling,
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

        appendLog("Summarise the skewness of the numeric data.", skewness.cmd)
        appendTextview(TV,
                       paste("Skewness for each numeric variable",
                             "of the",
                             ifelse(use.sample && sampling,
                                    "** sample **", "full"),
                             "dataset.\nPositive means the right tail",
                             "is longer.\n\n"),
                       collectOutput(skewness.cmd, TRUE))
      }
    }
  }
      
  if (do.missing)
  {
    ## Add in a summary of the missing values.
  
    if (packageIsAvailable("mice", "summarise missing values"))
    {
      ## Load the mice package into the library

      lib.cmd <- "require(mice, quietly=TRUE)"
      appendLog("Missing value summary is performed by mice.", lib.cmd)
      eval(parse(text=lib.cmd))
      
      ## Variables to be included, as a string of indicies.
  
      included <- getIncludedVariables()
      including <- not.null(included)

      ## Add radio buttons to choose: Full, Train, Test dataset to summarise.
  
      ## Build the summary command

      summary.cmd <- paste("md.pattern(crs$dataset[,",
                           if (including) included,
                           "])", sep="")

      appendLog("Generate a summary of the missing values in the dataset.",
                summary.cmd)
      appendTextview(TV,
                     "Missing Value Summary\n\n",
                     collectOutput(summary.cmd, TRUE))
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

calcInitialDigitDistr <- function(l, digit=1,
                                  split=c("none", "positive", "negative"))
{

  # From a list of numbers return a vector of first digit
  # frequencies. If DIGIT is given, then return the distribution for
  # that digit, rather than the default first digit. The default SPLIT
  # is none, meaning that both positive and negative numbers are
  # considered (ignoring the sign). Otherwise we return the
  # distribution for either only the positive numbers in the list or
  # for only the negative numbers in the list.

  if (split == "positive")
    l <- l[l>0]
  else if (split == "negative")
    l <- l[l<0]

  # Ignore all zeros.

  l <- l[l!=0]
  
  # If we don't have any numbers in the distrbution, return a list of
  # zeros.
  
  if (length(l) == 0)
  {
    if (digit == 1)
    {
      result <- rep(0, 9)
      names(result) <- 1:9
    }
    else
    {
      result <- rep(0, 10)
      names(result) <- 0:9
    }
    return(result)
  }

  # Note that we remove decimal points (i.e., the decimal dot itself,
  # not any digits) from real numbers.
  
  ds <- data.frame(digit=as.numeric(substr(gsub("\\.", "",
                     as.character(abs(l))), digit, digit)),
                   value=1)
#[071201  ds <- data.frame(digit=as.numeric(gsub("(.).*", "\\1",
#                     as.character(abs(l)))),
#                   value=1)
  
  # Ignore any zeros
  
  if (digit == 1) ds <- ds[ds$digit!=0,]

  # Add in any mising digits as value=0
  
  missing <- setdiff(ifelse(digit>1,0,1):9, unique(ds[,1]))
  if (length(missing) > 0)
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
  # Plot the data. The dataset is a string that defines the dataset
  # to use. Information about what variables to plot and the kind of
  # plots is obtained from the continuous_treeview and the
  # categorical_treeview which are displayed in the Explore tab's
  # Distribution option. The appropriate plots are displayed.

  # Obtain the selection of variables.

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

  mosplots  <- getSelectedVariables("mosplot")
  nmosplots <- length(mosplots)

  total.plots <- nboxplots + nhisplots + length(cumplots) +
    nbenplots + nbarplots + ndotplots + nmosplots
  
  pmax <- theWidget("plots_per_page_spinbutton")$getValue()
  pcnt <- 0

  # Don't waste real estate if we are plotting less than number
  # allowed per page.
  
  if (total.plots < pmax) pmax <- total.plots
  
  # Iterate over all target values if a target is defined and has
  # less than 10 values. The plots will then also display the
  # distributions per target value.

  target <- getSelectedVariables("target")

  if (is.null(target))
    targets <- NULL
  else
    targets <- levels(as.factor(crs$dataset[[crs$target]]))

  # For now, let's plot always, since I was wondering why the Benford
  # plot was not showing all the targets!
  
##   if (length(targets) > 10)
##   {
##     target <- NULL
##     targets <- NULL
##   }
  
  # Check for sampling.
  
  use.sample <- theWidget("sample_checkbutton")$getActive()
  sampling  <- use.sample && not.null(crs$sample)

  # Record other options.

  annotate <- theWidget("explot_annotate_checkbutton")$getActive()
  
  # Split the data, first for all values.

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
  
  # Finish off the command to create the dataset for plotting.
  
  bind.cmd <- sprintf("%s)", bind.cmd)

  # Build a list of generic datasets. This describes how to get the
  # relevant rows from the dataset for All the data, then each of the
  # levels of a target. Each contains a "%s" which is replace gor
  # specific chosen variables at the time of using this construct to
  # obtain the data for the plot. The form is:
  #
  # All = crs$dataset$%s
  #
  # or if sampling is enabled:
  #
  # All = crs$dataset[crs$sample,]$%s  
  #
  # For each level:
  #
  # '0' = crs$dataset[crs$dataset$Adjusted=="0",]$%s
  #
  # or if sampling is enabled:
  #
  # '0' = crs$dataset[crs$sample,][crs$dataset[crs$sample,]$Adjusted=="0",]$%s
  #
  # This is a newer alternative to identifying the dataset
  # segments. We build this list of target and a specification of the
  # correspending data subset. Eventually move all plotting to use
  # this approach rather than using bind.cmd.

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

  # Generate a plot for each variable. If there are too many
  # variables, ask the user if we want to continue.

  if (total.plots > 10 && pmax == 1)
    if (! questionDialog("We are about to generate", total.plots,
                         "individual plots. That's quite a few.",
                         "You could select fewer variables, or you",
                         "can change the number of plots per page,",
                         "but you can also proceed if you like.",
                         "\n\nWould you like to proceed?"))
      return()

  #---------------------------------------------------------------------

  if (nboxplots > 0)
  {
    # Show a box plot for numeric data. A box plot shows the
    # distribution of numeric data graphically. The box iteself
    # extends from the lower to the upper quartiles with the median
    # drawn in the box. The lines then extend to the maximum and
    # minimum points that are no more than 1.5 times the interquartile
    # range from the median. Outliers are then also plotted as
    # points. The notches indicate significant differences, in that if
    # nocthes do not overlap, then the distribution medians are
    # significantly different.")

    plot.cmd <- paste('bp <<- boxplot(dat ~ grp, ds,',
                     sprintf('col=rainbow(%d),', length(targets)+1),
                     ifelse(is.null(targets), "",
                            sprintf('xlab="%s",', target)),
                     'notch=TRUE)')

    # Based on an example from Jim Holtman on r-help 070406.
    
    annotate.cmd <- paste("for (i in seq(ncol(bp$stats)))",
                          "{text(i,",
                          "bp$stats[,i] - 0.02*(max(ds$dat, na.rm=TRUE)",
                          "- min(ds$dat, na.rm=TRUE)),",
                          "labels=bp$stats[,i])}")
    
    lib.cmd <- "require(doBy, quietly=TRUE)"
    
    # TODO: Try using "by" instead of needing another package to
    # provide summaryBy. Also, the new version of doBy (061006) seems
    # to be outputting extra status information that makes the R
    # Console a little chatty unneccessarily - perhaps this will
    # disappear again - it looks like debugging information!
    #
    # status:
    # lhsvar     : dat 
    # rhsvar     : grp 
    # idvar      :  
    # fun.names  : mean 
    # varPrefix  : mean 
    # newNames   : mean.dat 

    # Only use summaryBy if there is a target, because it fails if
    # there is actually only one group in the data. Might be a new
    # bug in the doBy package.
    
    if (length(targets) > 1)
      mean.cmd <- paste(sprintf("points(1:%d,", length(targets)+1),
                        "summaryBy(dat ~ grp, data=ds,",
                        "FUN=mean, na.rm=TRUE)$dat.mean,",
                        "pch=8)")
    else
      mean.cmd <- paste(sprintf("points(1:%d,", length(targets)+1),
                        "mean(ds$dat, na.rm=TRUE),",
                        "pch=8)")
    
    for (s in 1:nboxplots)
    {

      startLog()
      appendLog("BOX PLOT")

      cmd <- paste("sprintf(bind.cmd,",
                   paste(paste('"', rep(boxplots[s], length(targets)+1), '"',
                               sep=""),
                         collapse=","),
                   ")")
      cmd <- eval(parse(text=cmd))
      appendLog(paste("Generate just the data for a boxplot of ",
                    boxplots[s], ".", sep=""),
              paste("ds <-", cmd))
      ds <- eval(parse(text=cmd))

      if (pcnt %% pmax == 0) newPlot(pmax)
      pcnt <- pcnt + 1
      
      appendLog("Plot the data, grouped appropriately.",
                gsub("<<", "<", plot.cmd))
      eval(parse(text=plot.cmd))

      # Add a value for the mean to each boxplot.
      
      if (packageIsAvailable("doBy", "add means to box plots"))
      {
        appendLog("Use the doBy package to group the data for means.",
                 lib.cmd)
        eval(parse(text=lib.cmd))

        appendLog("Calculate the group means.", mean.cmd)
        eval(parse(text=mean.cmd))
      }
        
      # Optionally include annotations.

      if (annotate)
      {
        appendLog("Add annotations to the plot.", annotate.cmd)
        eval(parse(text=annotate.cmd))
      }        
      
      # Add a title to the plot.
      
      title.cmd <- genPlotTitleCmd(sprintf("Distribution of %s%s",
                                          boxplots[s],
                                          ifelse(sampling, " (sample)","")))
      appendLog("Add a title to the plot.", title.cmd)
      eval(parse(text=title.cmd))
    }
  }

  ##--------------------------------------------------------------------
  
  if (nhisplots > 0)
  {
    # Plot a histogram for numeric data.

    plot.cmd <- paste('hs <- hist(ds[ds$grp=="All",1], main="", xlab="", ',
                      'col=rainbow(10))\n',
                      'dens <- density(ds[ds$grp=="All",1], na.rm=TRUE)\n',
                      'rs <- max(hs$counts)/max(dens$y)\n',
                      'lines(dens$x, dens$y*rs, type="l")',
                      sep="")
    rug.cmd <- 'rug(ds[ds$grp=="All",1])'

    # If the data looks more categoric then do a more usual hist
    # plot. TODO 080811 Add in a density plot - just need to get the
    # maximum frequency as hs$count above. BUT the density makes no
    # sense, because the bars are the actual data, there is no
    # grouping.

    altplot.cmd <- paste('plot(as.factor(round(ds[ds$grp=="All",1], ',
                         'digits=2)), col=rainbow(10))\n',
                         #'dens <- density(ds[ds$grp=="All",1], na.rm=TRUE)\n',
                         #'rs<- max(summary(as.factor(round(ds[ds$grp=="All",',
                         #'1], digits=2))))/max(dens$y)\n',
                         #'lines(dens$x, dens$y*rs, type="l")',
                         sep="")

    for (s in 1:nhisplots)
    {
      startLog()
      appendLog("HISTOGRAM")
      
      cmd <- paste("sprintf(bind.cmd,",
                   paste(paste('"', rep(hisplots[s], length(targets)+1), '"',
                               sep=""),
                         collapse=","),
                   ")")
      cmd <- eval(parse(text=cmd))
      appendLog(paste("Generate just the data for a histogram of ",
                    hisplots[s], ".", sep=""),
              paste("ds <-", cmd))
      ds <- eval(parse(text=cmd))

      if (pcnt %% pmax == 0) newPlot(pmax)
      pcnt <- pcnt + 1
      
      # Determine whether to plot a histogram of the numeric data or
      # as a factor (is.integer and unique <= 20).

      dsmin <- eval(parse(text="min(ds[ds$grp=='All',1], na.rm=TRUE)"))
      dsmax <- eval(parse(text="max(ds[ds$grp=='All',1], na.rm=TRUE)"))
      dsuni <- eval(parse(text="unique(ds[ds$grp=='All',1], na.rm=TRUE)"))
      
      if (length(dsuni) <= 20 && dsmax - dsmin <= 20)
      {
        appendLog("Plot the data.", altplot.cmd)
        eval(parse(text=altplot.cmd))
      }
      else
      {
        appendLog("Plot the data.", plot.cmd)

        eval(parse(text=plot.cmd))
        appendLog("Add a rug to illustrate density.", rug.cmd)
        eval(parse(text=rug.cmd))
      }
      
      title.cmd <- genPlotTitleCmd(sprintf("Distribution of %s%s",
                                           hisplots[s],
                                           ifelse(sampling, " (sample)","")))
      appendLog("Add a title to the plot.", title.cmd)
      eval(parse(text=title.cmd))
    }
  }

  #---------------------------------------------------------------------
  
  if (not.null(cumplots))
  {
    # Cumulative plot for numeric data.

    nplots <- length(cumplots)

    lib.cmd <- "require(Hmisc, quietly=TRUE)"
    
    for (s in 1:nplots)
    {
      startLog()

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
      appendLog(paste("Generate just the data for an Ecdf plot of",
                    cumplots[s], "."),
              paste("ds <-", cmd))
       ds <- eval(parse(text=cmd))

      if (pcnt %% pmax == 0) newPlot(pmax)
      pcnt <- pcnt + 1

      if (! packageIsAvailable("Hmisc", "plot cumulative charts")) break()

      appendLog("Use Ecdf from the Hmisc package.", lib.cmd)
      eval(parse(text=lib.cmd))

      appendLog("Plot the data.", plot.cmd)
      eval(parse(text=plot.cmd))
      title.cmd <- genPlotTitleCmd(sprintf("Cumulative %s%s",
                                           cumplots[s],
                                           ifelse(sampling, " (sample)","")))

      if (not.null(targets))
      {
        appendLog("Add a legend to the plot.", legend.cmd)
        eval(parse(text=legend.cmd))
      }

      appendLog("Add a title to the plot.", title.cmd)
      eval(parse(text=title.cmd))
    }
  }

  ##---------------------------------------------------------------------

  if (nbenplots > 0)
  {
    ## Plot Benford's Law for numeric data.

    barbutton <- theWidget("benford_bars_checkbutton")$getActive()
    absbutton <- theWidget("benford_abs_radiobutton")$getActive()
    posbutton <- theWidget("benford_pos_radiobutton")$getActive()
    negbutton <- theWidget("benford_neg_radiobutton")$getActive()
    digspin <- theWidget("benford_digits_spinbutton")$getValue()

    benopts <- sprintf(', split="%s", digit=%d',
                       ifelse(absbutton, "none",
                              ifelse(posbutton, "positive", "negative")),
                       digspin)
    
    # Using barplot2 from gplots
    
    lib.cmd <- "require(gplots, quietly=TRUE)"

    ## Calculate the expected distribution according to Benford's Law

    if (digspin == 1)
      expect.cmd <- paste('unlist(lapply(1:9, function(x) log10(1 + 1/x)))')
    # see http://www.mathpages.com/home/kmath302/kmath302.htm
    else if (digspin > 1) 
      expect.cmd <- sprintf(paste('unlist(lapply(0:9, function(x) {sum(log10',
                                  '(1 + 1/(10*(seq(10^(%d-2), ',
                                  '(10^(%d-1))-1)) + x)))}))'),
                            digspin, digspin)

    ## Construct the command to plot the distribution.

    if (barbutton)
    {
      plot.cmd <- paste('barplot2(ds, beside=TRUE,',
                       'xlab="Distribution of the ',
                        paste(digspin, c("st", "nd",
                                         "rd", "th")[min(4, digspin)],
                              sep = ""),
                        'Digit", ylab="Probability")')
    }
    else
    {
      plot.cmd <- paste('plot(', ifelse(digspin==1, "1", "0"),
                        ':9, ds[1,], type="b", pch=19, col=rainbow(1), ',
                       'ylim=c(0,max(ds)), axes=FALSE, ',
                       'xlab="Distribtuion of the ',
                        paste(digspin, c("st", "nd",
                                         "rd", "th")[min(4, digspin)],
                              sep = ""),
                        ' Digit',
                        '", ylab="Probability")\n',
                        'axis(1, at=',
                        ifelse(digspin==1, "1", "0"),
                        ':9)\n', 'axis(2)\n',
                       sprintf(paste('points(%d:9, ds[2,],',
                                     'col=%s, pch=19, type="b")\n'),
                               ifelse(digspin==1, 1, 0),
                               ifelse(is.null(target), "rainbow(2)[2]",
                                      sprintf("rainbow(%d)[2]",
                                              length(targets)+2))),
                       sep="")
      if (not.null(targets))
        for (i in 1:length(targets))
        {
          plot.cmd <- sprintf(paste('%s\npoints(%d:9, ds[%d,],',
                                   'col=%s, pch=%d, type="b")'),
                             plot.cmd, ifelse(digspin==1, 1, 0), i+2,
                             sprintf("rainbow(%d)[%d]",
                                     length(targets)+2, i+2),
                             19)
        }
    }
    if (packageIsAvailable("gplots", "plot a bar chart for Benford's Law"))
    {
      startLog()
      appendLog("BENFORD'S LAW")
      
      appendLog("Use barplot2 from gplots to plot Benford's Law.", lib.cmd)
      eval(parse(text=lib.cmd))
      
      appendLog("Generate the expected distribution for Benford's Law",
               paste("expect <-", expect.cmd))
      expect <- eval(parse(text=expect.cmd))

      if (is.null(targets) && ! barbutton)
      {
        # Plot all Benford's plots on the one line graph

        startLog()

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
                                         '(ds[ds$grp=="%s", 1]%s)', sep=""),
                                   benplots[s], benplots[s], benopts),
                           sep="")
          plot.cmd <- paste(plot.cmd,
                           sprintf(paste('points(1:9, ds[%d,],',
                                         'col=%s, pch=19, type="b")\n'),
                                   s+1, sprintf("rainbow(%d)[%d]",
                                                nbenplots+1, s+1)),
                           sep="")
        }
        new.bind.cmd <- paste(substr(new.bind.cmd, 1,
                                     nchar(new.bind.cmd)-7), ")",
                            sep="")
        data.cmd <- paste(data.cmd, ")))", sep="")

        legend.cmd <- sprintf(paste('legend("%s", c(%s), ',
                                   'fill=rainbow(%d), title="%s")'),
                              ifelse(digspin>2, "botright", "topright"),
                              paste(sprintf('"%s"',
                                            c("Benford", benplots)),
                                    collapse=","),
                              nbenplots+1, "Variables")

        appendLog("Generate the required data.",
                 paste("ds <-", new.bind.cmd))
        ds <- eval(parse(text=new.bind.cmd))

        appendLog("Generate specific plot data.", paste("ds <-", data.cmd))
        ds <- eval(parse(text=data.cmd))

        if (pcnt %% pmax == 0) newPlot(pmax)
        pcnt <- pcnt + 1

        par(xpd=TRUE)
        
        appendLog("Now do the actual plot.", plot.cmd)
        eval(parse(text=plot.cmd))

        appendLog("Add a legend to the plot.", legend.cmd)
        eval(parse(text=legend.cmd))
        
        if (sampling)
          title.cmd <- genPlotTitleCmd("Benford's Law (sample)")
        else
          title.cmd <- genPlotTitleCmd("Benford's Law")

        appendLog("Add a title to the plot.", title.cmd)
        eval(parse(text=title.cmd))
        

      }
      else
      {
        # Plot multiple graphs since we have a target, and will split
        # each graph according to the target values.
        
        for (s in 1:nbenplots)
        {
          startLog()
          #
          # Record the sizes of the subsets for the legend
          #
          sizes.cmd <- paste('sizes <<- (function(x)(paste(names(x), " (",',
                             ' x, ")", sep="")))(by(ds, ds$grp, nrow))')
          
          data.cmd <- paste('t(as.matrix(data.frame(expect=expect,\n    ',
                           'All=calcInitialDigitDistr(ds[ds$grp=="All", 1]',
                            benopts, ')')
        
          if (not.null(targets))
            for (t in 1:length(targets))
              data.cmd <- paste(data.cmd, ",\n     ",
                               sprintf('"%s"=', targets[t]),
                               'calcInitialDigitDistr(ds[ds$grp==',
                               sprintf('"%s", ', targets[t]), '1]',
                                benopts, ')',
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
              legend.cmd <- sprintf(paste('legend("%s", c(%s), inset=.05,',
                                         'fill=rainbow(%d), title="%s")'),
                                    ifelse(digspin>2, "bottomright",
                                           "topright"),
                                    '"Benfords", sizes',
#                                   paste(sprintf('"%s"',
#                                                c("Benford", "All", targets)),
#                                         collapse=","),
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
          
          appendLog(paste("Generate just the data for the plot of",
                         benplots[s], "."),
                   paste("ds <-", cmd))
          ds <- eval(parse(text=cmd))

          appendLog("Generate legend entries with subset sizes.",
                    gsub("<<-", "<-", sizes.cmd))
          eval(parse(text=sizes.cmd))
          
          appendLog("Generate frequency of initial digit.",
                   paste("ds <-", data.cmd))
          ds <- eval(parse(text=data.cmd))

          nan.cmd <- "ds[is.nan(ds)] <- 0"
          appendLog("Ensure rows with no digits are treated as zeros.", nan.cmd)
          ds[is.nan(ds)] <- 0
          
          if (pcnt %% pmax == 0) newPlot(pmax)
          pcnt <- pcnt + 1

          par(xpd=TRUE)
          
          appendLog("Now do the actual Benford plot.", plot.cmd)
          eval(parse(text=plot.cmd))
          
          appendLog("Add a legend to the plot.", legend.cmd)
          eval(parse(text=legend.cmd))
          
          if (sampling)
            title.cmd <- genPlotTitleCmd(sprintf(paste("Benford's Law:",
                                                       "%s (sample)%s"),
                                                 benplots[s],
                ifelse(posbutton, " (positive values)",
                       ifelse(negbutton, " (negative values)", ""))))
          else
            title.cmd <- genPlotTitleCmd(sprintf("Benford's Law: %s%s",
                                                 benplots[s],
                ifelse(posbutton, " (positive values)",
                       ifelse(negbutton, " (negative values)", ""))))
          appendLog("Add a title to the plot.", title.cmd)
          eval(parse(text=title.cmd))
        }
      }
    }
  }

  ##---------------------------------------------------------------------

  if (nbarplots > 0)
  {
    # Plot a frequency plot for a categoric variable.

    # 080817 Use barchart from lattice instead of barplot2 from
    # ggplots.

    lib.cmd <- "require(lattice, quietly=TRUE)"
    
    # Construct a generic data command built using the genericDataSet
    # values. To generate a barplot we use the output of the summary
    # command on each element in the genericDataSet, and bring them
    # together into a single structure. The resulting generic.data.cmd
    # will have a number of "%s"s (one for the whole dataset, then one
    # for each level) from the original genericDataSet string that
    # will be replaced with the name of each variable as it is being
    # plotted.

    generic.data.cmd <- paste(lapply(genericDataSet,
                                     function(x) sprintf("summary(%s)", x)),
                              collapse=",\n    ")
    generic.data.cmd <- sprintf("cbind(%s)", generic.data.cmd)

    # If the lattice package is available then generate a plot for
    # each chosen vairable.
    
    if (packageIsAvailable("lattice", "display a bar chart"))
    {
      startLog()
      appendLog("Load lattice for the barchart function.", lib.cmd)
      eval(parse(text=lib.cmd))

      for (s in 1:nbarplots)
      {
        startLog()

        # Construct and evaluate a command string to generate the
        # data for the plot.

        ds.cmd <- paste(sprintf("sprintf('%s',", generic.data.cmd),
                       paste(paste('"', rep(barplots[s], length(targets)+1),
                                 '"', sep=""), collapse=","), ")")
        ds.cmd <- eval(parse(text=ds.cmd))
        appendLog(sprintf("Generate the summary data for plotting %s.", barplots[s]),
                 paste("ds <-", ds.cmd))
        ds <- eval(parse(text=ds.cmd))

        names.cmd <- sprintf('colnames(ds) <- c(%s)',
                             ifelse(length(targets)==0, '"Frequency"',
                                    paste('"Frequency"',
                                          paste(sprintf('"%s"', targets),
                                                collapse=", "),
                                          sep=", ")))
        appendLog("Set the appropriate column names.", names.cmd)
        eval(parse(text=names.cmd))

        # We don't have multiple plots on the one plot implemented yet
        # - should we? I would guess there is a simple way to do this
        # with lattice.
        
        #if (pcnt %% pmax == 0) newPlot(pmax)
        #pcnt <- pcnt + 1
        newPlot(pmax)

        # Construct and evaluate the command to determine the order in
        # which to print the catgories, from smallest (at the bottom)
        # to largest.

        ord.cmd <- 'order(ds[,1])'
        appendLog("Sort the entries.", paste("ord <-", ord.cmd))
        ord <- eval(parse(text=ord.cmd))

        plot.cmd <- sprintf(paste('print(barchart(ds[ord,%s]',
                                  'xlab="Frequency"',
                                  ifelse(length(targets)==0,
                                         'groups=NULL', # Just to have something!
                                         sprintf(paste('auto.key=list(title="%s",',
                                                       'cex=0.75,', 'columns=%d)'),
                                                 target, 2)),
                                  sprintf('sub="%s"', genPlotTitleCmd(vector=TRUE)),
                                  'main="Distribution of %s%s"))', sep=", "),
                            ifelse(length(targets)==0, "", "-1"),
                            barplots[s],
                            ifelse(sampling," (sample)",""))
                            
        appendLog("Plot the data.", plot.cmd)
        eval(parse(text=plot.cmd))

      }
    }
  }

  ##---------------------------------------------------------------------

  if (ndotplots > 0)
  {
    
    # 080817 Use dotplot(lattice) instead of dotchart.

    lib.cmd <- "require(lattice, quietly=TRUE)"

    # Construct a generic data command built using the genericDataSet
    # values. To generate a barplot we use the output of the summary
    # command on each element in the genericDataSet, and bring them
    # together into a single structure. The resulting generic.data.cmd
    # will have a number of "%s"s (one for the whole dataset, then
    # one for each level) from the original genericDataSet string
    # that will be replaced with the name of each variable as it is
    # being plotted.

    generic.data.cmd <- paste(lapply(genericDataSet,
                                   function(x) sprintf("summary(%s)", x)),
                            collapse=",\n    ")
    generic.data.cmd <- sprintf("cbind(%s)", generic.data.cmd)

    # If the lattice package is available then generate a plot for
    # each chosen vairable.
    
    if (packageIsAvailable("lattice", "display a dot plot"))
    {
      startLog()
      appendLog("Load lattice for the dotplot function.", lib.cmd)
      eval(parse(text=lib.cmd))

      for (s in 1:ndotplots)
      {
        startLog()

        # Construct and evaluate a command string to generate the data
        # for the plot.

        ds.cmd <- paste(sprintf("sprintf('%s',", generic.data.cmd),
                        paste(paste('"', rep(dotplots[s], length(targets)+1),
                                    '"', sep=""), collapse=","), ")")
        ds.cmd <- eval(parse(text=ds.cmd))
        appendLog(sprintf("Generate the summary data for plotting %s.", dotplots[s]),
                  paste("ds <-", ds.cmd))
        ds <- eval(parse(text=ds.cmd))

        names.cmd <- sprintf('colnames(ds) <- c(%s)',
                             ifelse(length(targets)==0, '"Frequency"',
                                    paste('"Frequency"',
                                          paste(sprintf('"%s"', targets),
                                                collapse=", "),
                                          sep=", ")))
        appendLog("Set the appropriate column names.", names.cmd)
        eval(parse(text=names.cmd))

        # Construct and evaluate the command to determine the order in
        # which to print the catgories, from smallest (at the bottom)
        # to largest.

        ord.cmd <- 'order(ds[,1])'
        appendLog("Sort the entries.", paste("ord <-", ord.cmd))
        ord <- eval(parse(text=ord.cmd))

        # Construct and evaluate the command to plot the distribution.
    
        #if (pcnt %% pmax == 0) newPlot(pmax)
        #pcnt <- pcnt + 1
        newPlot(pmax)
      
        plot.cmd <- sprintf(paste('print(dotplot(ds[ord,%s]',
                                  'xlab="Frequency"',
                                  'type=c("p", "h", "a")',
                                  ifelse(length(targets)==0,
                                         'groups=NULL', # Just to have something!
                                         sprintf(paste('auto.key=list(title="%s",',
                                                       'cex=0.75,', 'columns=%d)'),
                                                 target, 2)),
                                  sprintf('sub="%s"', genPlotTitleCmd(vector=TRUE)),
                                  'main="Distribution of %s%s"))', sep=", "),
                            ifelse(length(targets)==0, "", "-1"),
                            dotplots[s],
                            ifelse(sampling," (sample)",""))
        appendLog("Plot the data.", plot.cmd)
        eval(parse(text=plot.cmd))
      }
    }
  }

  #---------------------------------------------------------------------

  if (nmosplots > 0)
  {

    for (s in 1:nmosplots)
    {

      startLog()

      # Construct and evaluate a command string to generate the
      # data for the plot.

      if (is.null(target))
        ds.cmd <- sprintf("table(crs$dataset$%s)", mosplots[s])
      else
        ds.cmd <- paste(sprintf(paste("table(crs$dataset$%s,",
                                      "crs$dataset$%s)"), mosplots[s], target))
      appendLog("Generate the table data for plotting.",
                paste("ds <-", ds.cmd))
      ds <- eval(parse(text=ds.cmd))

      # Construct and evaluate the command to determin the order in
      # which to print the catgories, from larges to smallest.

      # Construct and evaluate the command to plot the
      # distribution.
    
      if (pcnt %% pmax == 0) newPlot(pmax)
      pcnt <- pcnt + 1

      if (is.null(target))
        titles <- genPlotTitleCmd(sprintf("Mosaic of %s",
                                          mosplots[s],
                                          ifelse(sampling," (sample)","")),
                                  vector=TRUE)
      else
        titles <- genPlotTitleCmd(sprintf("%s by %s%s",
                                          mosplots[s], target,
                                          ifelse(sampling," (sample)","")),
                                  vector=TRUE)

      plot.cmd <- sprintf(paste('mosaicplot(ds, main="%s", sub="%s",',
                                ' color=rainbow(%d), cex=0.7)'),
                          titles[1], titles[2], length(targets)+1)
      appendLog("Plot the data.", plot.cmd)
      eval(parse(text=plot.cmd))
    }
  }
  
  # Update the status bar.
  
  if (total.plots > 1)
    setStatusBar("All", total.plots, "plots generated.")
  else if (total.plots ==  1)
    setStatusBar("One plot generated.")
  else
    setStatusBar("No plots selected.")
}
  
executeExploreGGobi <- function(dataset, name=NULL)
{
  ## Based on code from Marco Lo
  
  ## Construct the commands.

  lib.cmd <- "require(rggobi, quietly=TRUE)"
  ggobi.cmd <- paste('gg <<- ggobi(', dataset,
                     ifelse(not.null(name), sprintf(', name="%s"', name), ""),
                     ')')
              
  ## Start logging and executing the R code.
  
  if (! packageIsAvailable("rggobi", "explore the data using GGobi")) return()

  startLog()
  appendLog("GGobi is accessed using the rggobi package.", lib.cmd)
  eval(parse(text=lib.cmd))
  appendLog("Launch GGobi data visualization.", gsub("<<-", "<-", ggobi.cmd))
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

  # Obtain user interface settings.

  ordered <- theWidget("explore_correlation_ordered_checkbutton")$getActive()
  method <- tolower(theWidget("explore_correlation_method_combobox")$getActiveText())
  
  # Warn if there are too many variables. An alternative is to offer
  # to just plot the variables with the highest amount of correlation.
  
  nvars <- eval(parse(text=sprintf("ncol(%s)", dataset)))
  if (nvars > crv$max.vars.correlation &&
      ! questionDialog("You have requested a Correlation plot.",
                       "\n\nWith", nvars, "variables the plot may take",
                       "some time to display, and the display will",
                       "be cramped.",
                       "Consider identifying up to only",
                       crv$max.vars.correlation,
                       "input variables.\n\n",
                       "Would you like to continue anyhow?"))
    return(FALSE)
  
  # Construct the commands.

  # Deal with showing the missing values plot.
  
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
  crscor.cmd  <- sprintf('%scrscor <- cor(%s, use="pairwise", method="%s")',
                         ifelse(nas, naids.cmd, ""),
                         ifelse(nas,
                                sprintf("is.na(%s[naids])", dataset),
                                dataset),
                         method)
  if (ordered)
    crsord.cmd  <- paste("crsord <- order(crscor[1,])",
                         "crscor  <- crscor[crsord, crsord]",
                         sep="\n")
    
  print.cmd   <- "print(crscor)"
  if (nas)
  {
    print.cmd <- paste(print.cmd,
                       "\ncat('\nCount of missing values:\n')\n",
                       sprintf("print(apply(is.na(%s[naids]),2,sum))",
                               dataset),
                       "\ncat('\nPercent missing values:\n')\n",
                       sprintf(paste("print(100*apply(is.na(%s[naids]),",
                                     "2,sum)/nrow(%s))"),
                               dataset, dataset),
                       sep="")
    
  }
  plot.cmd    <- paste("plotcorr(crscor, ",
                       'col=colorRampPalette(c("red", "white", "blue"))(11)',
                       '[5*crscor + 6])\n',
                       genPlotTitleCmd("Correlation",
                                       ifelse(nas, "of Missing Values", ""),
                                       crs$dataname, "using", method),
                       sep="")
  
  # Start logging and executing the R code.

  if (! packageIsAvailable("ellipse", "display a correlation plot")) return()
     
  startLog("Generate a correlation plot for the variables.")
  resetTextview(TV)

  appendLog("The correlation plot uses the ellipse package.", lib.cmd)
  eval(parse(text=lib.cmd))

  appendLog("Correlations work for numeric variables only.", crscor.cmd)
  if (ordered) appendLog("Order the correlations by their strength.", crsord.cmd)
  appendLog("Display the actual correlations.", print.cmd)
  appendLog("Graphically display the correlations.", plot.cmd)

  appendTextview(TV,
               ifelse(nas,
                      "Missing Values Correlation Summary:",
                      "Correlation Summary:"), " Using ", method, " method\n\n",
               "Note that only correlations between numeric variables ",
               "are reported.\n\n",
               collectOutput(paste(crscor.cmd,
                                    if (ordered) crsord.cmd,
                                    print.cmd,
                                    sep="\n")))

  newPlot()
  eval(parse(text=paste(crscor.cmd,
               if (ordered) crsord.cmd,
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
                "\n\nNo numeric variables were found in the dataset",
                "from amongst those that are not ignored.",
                "\n\nYou may want to use the transform tab to transform",
                "your categoric data into numeric data.")
    return()
  }

  # Obtain user interface settings.

  method <- tolower(theWidget("explore_correlation_method_combobox")$getActiveText())

  # Check that we have sufficient data

  ncols <- eval(parse(text=sprintf("NCOL(%s)", dataset)))
  if ( ncols < 2 )
  {
    errorDialog("The dataset contains less than two numeric variables.",
                "\n\nCorrelations are calculated only for numeric data.",
                "\n\nYou may want to select more numeric variables or",
                "use the transform tab to transform",
                "your categoric variables into numeric variables.")

    return()
  }
    
  # Construct the commands.
  
  cor.cmd    <- sprintf('cc <- cor(%s, use="pairwise", method="%s")', dataset, method)
  hclust.cmd <- 'hc <- hclust(dist(cc), "ave")'
  dend.cmd   <- "dn <- as.dendrogram(hc)"

  # Modification by Ed Cox 080130 to increase margin for long variable names

  fontsize <- .75
  if (ncols>45) {fontsize <- .60}
  
  labwidth <- eval(parse(text = paste('max(unlist(lapply(colnames(',
                           dataset, '), "nchar"))) + 2', sep="")))
  plot.cmd   <- paste('op <- par(mar = c(3, 4, 3, ',
                      round(labwidth/3.5, 2), '))\n',
                      'plot(dn, horiz = TRUE, ',
                      'nodePar = list(col = 3:2, ',
                      'cex = c(2.0, 0.75), pch = 21:22, ',
                      'bg=  c("light blue", "pink"), ',
                      'lab.cex = ', fontsize, ', lab.col = "tomato"), ',
                      'edgePar = list(col = "gray", lwd = 2)',
                      ')\n',
                      genPlotTitleCmd("Variable Correlation Clusters",
                                     crs$dataname, "using", method),'\n',
                      'par(op)\n',
                      sep="")

 # plot.cmd   <- paste('plot(dn, horiz=TRUE, ',
 #                     'nodePar=list(col=3:2, cex=c(2.0, 0.75), pch= 21:22, ',
 #                     'bg= c("light blue", "pink"), ',
 #                     'lab.cex = 0.75, lab.col = "tomato"), ',
 #                     'edgePar=list(col="gray", lwd=2)',
 #                     ')\n',
 #                     genPlotTitleCmd("Variable Correlation Clusters",
 #                                    crs$dataname),
 #                     sep="")

  # Start logging and executing the R code.

  startLog("HIERARCHICAL VARIABLE CORRELATION")

  appendLog("Generate the correlations (numerics only).", cor.cmd)
  eval(parse(text=cor.cmd))

  appendLog("Generate hierarchical cluster of variables.", hclust.cmd)
  eval(parse(text=hclust.cmd))

  appendLog("Generate the dendrogram.", dend.cmd)
  eval(parse(text=dend.cmd))

  appendLog("Now draw the dendrogram.", plot.cmd)
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

  # Construct the commands.
  
  prcomp.cmd  <- sprintf(paste('pc <<- prcomp(na.omit(%s),',
                               'scale=TRUE, center=TRUE, tol=0)'),
                         dataset)
  print.cmd   <- "pc"
  summary.cmd <- "summary(pc)"
  plot.cmd    <- paste('plot(pc, main="")',
                       genPlotTitleCmd("Principal Components Importance",
                                      crs$dataname),
                       paste("axis(1, at=seq(0.7, ncol(pc$rotation)*1.2, 1.2),",
                             "labels=colnames(pc$rotation), lty=0)"),
                       sep="\n")
  biplot.cmd  <- paste('biplot(pc, main="")',
                       genPlotTitleCmd("Principal Components",
                                      crs$dataname),
                       sep="\n")

  ## Start logging and executing the R code.

  startLog()
  resetTextview(TV)
  
  appendLog("Perform a principal components analysis (numerics only).",
          gsub("<<-", "<-", prcomp.cmd))
  eval(parse(text=prcomp.cmd))

  appendTextview(TV, "Note that principal components on only the numeric\n",
                  "variables is calculated, and so we can not use this\n",
                  "approach to remove categoric variables from ",
                  "consideration.\n\n",
                  "Any numeric variables with relatively large rotation\n",
                  "values (negative or positive) in any of the first few\n",
                  "components are generally variables that you may wish\n",
                  "to include in the modelling.")

  appendLog("Show the output of the analysis,", print.cmd)
  appendTextview(TV, collectOutput(print.cmd, TRUE))
  
  appendLog("Summarise the importance of the components found.", summary.cmd)
  appendTextview(TV, collectOutput(summary.cmd, TRUE))

  newPlot(1)
  appendLog("Display a plot showing the relative importance of the components.",
          plot.cmd)
  eval(parse(text=plot.cmd))
  
  newPlot(1)
  appendLog("Display a plot showing the two most principal components.",
          biplot.cmd)
  eval(parse(text=biplot.cmd))
  
  ## Report completion to the user through the Status Bar.
  
  setStatusBar("A principal components analysis has been completed.")

}

executeExplorePlaywith <- function(dataset)
{
  # Testing for now. This has great potential.

  if (! packageIsAvailable("playwith", "explore data")) return()

  startLog("EXPLORE DATA.")

  lib.cmd <- "require(playwith)"
  appendLog("The latticist command comes from the playwith package.", lib.cmd)
  eval(parse(text=lib.cmd))

  plot.cmd <- sprintf("latticist(%s)", dataset)
  appendLog("Call upon the latticist command written for Rattle.", plot.cmd)
  eval(parse(text=plot.cmd))
}


########################################################################
# EVALUATE TAB

#----------------------------------------------------------------------
# INTERFACE CALLBACKS

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
  if (button$getActive())
  {
    theWidget("score_include_label")$show()
    theWidget("score_idents_radiobutton")$show()
    theWidget("score_all_radiobutton")$show()
    if (not.null(crs$kmeans))
      theWidget("kmeans_evaluate_checkbutton")$setSensitive(TRUE)
    if (not.null(crs$hclust))
      theWidget("hclust_evaluate_checkbutton")$setSensitive(TRUE)
  }
  else
  {
    theWidget("score_include_label")$hide()
    theWidget("score_idents_radiobutton")$hide()
    theWidget("score_all_radiobutton")$hide()

#    theWidget("kmeans_evaluate_checkbutton")$setSensitive(FALSE)
#    theWidget("hclust_evaluate_checkbutton")$setSensitive(FALSE)
}    
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
  for (m in crv$MODELLERS)
    if (theWidget(paste(m, "_evaluate_checkbutton", sep=""))$getActive())
      models <- c(models, m)
  if (theWidget("kmeans_evaluate_checkbutton")$isSensitive() &&
      theWidget("kmeans_evaluate_checkbutton")$getActive())
    models <- c(models, "kmeans")
  return(models)
}

current.evaluate.tab <- function()
{
  cp <- .EVALUATE$getCurrentPage()
  return(.EVALUATE$getTabLabelText(.EVALUATE$getNthPage(cp)))
}


#----------------------------------------------------------------------
# Execution

executeEvaluateTab <- function()
{
  # Perform the requested action from the Execute tab.

  # Obtain some background information.
  
  mtypes <- getEvaluateModels() # The chosen model types in the Evaluate tab.
  
  # Check any pre-conditions.
  
  # Ensure a dataset exists.

  if (noDatasetLoaded()) return()

  # Ensure we have at least one model to evaluate, otherwise warn the
  # user and do nothing.
  
  if (is.null(mtypes))
  {
    warnDialog("No model has been specified.",
               "\n\nPlease select one or more from the",
               "list of models available.")
    return()
  }

  # Ensure we recognise the model type.
  
  if (length(setdiff(mtypes, union(crv$MODELLERS, c("kmeans")))) > 0)
  {
    errorDialog("E121: A model type is not recognised.",
                "We found the model types to be:", mtypes,
                "Yet, Rattle only knows about:", crv$MODELLERS,
                "This is a Rattle bug.",
                "Please report this to support@togaware.com.")
    return()
  }

  # Ensure there is a model for each model type that is selected.

  if (sum(sapply(mtypes, function(x) is.null(crs[[x]]))) > 0)
  {
    errorDialog("E120: Some model has not been built?",
                "We found the model types to be:", mtypes,
                "The models not built:",
                sapply(mtypes, function(x) is.null(crs[[x]])),
                "This is a Rattle bug.",
                "Please report this to support@togaware.com.")
    return()
  }

  #   Ensure the appropriate package is loaded (in the case, for
  #   example, when loading a project and going straight to Evaluate,
  #   and wanting to run predict.svm on new data).

  if (.ADA %in%  mtypes &&
      ! packageIsAvailable("ada", sprintf("evaluate a %s model",
                                          commonName(.ADA))))
    return()
  if (.KSVM %in%  mtypes &&
      ! packageIsAvailable("kernlab", sprintf("evaluate a %s model",
                                              commonName(.KSVM))))
    return()
  if (.RF %in%  mtypes &&
      ! packageIsAvailable("randomForest", sprintf("evaluate a %s model",
                                                   commonName(.RF))))
    return()
  if (crv$GLM %in%  mtypes && "multinom" %in% class(crs$glm) &&
      ! packageIsAvailable("nnet", sprintf("evaluate a Multinomial %s model",
                                           commonName(crv$GLM))))
    return()
  if (crv$NNET %in%  mtypes &&
      ! packageIsAvailable("nnet", sprintf("evaluate a %s model",
                                           commonName(crv$NNET))))
    return()

  if(theWidget("score_radiobutton")$getActive())
    startLog("SCORE A DATASET")
  else
    startLog("EVALUATE MODEL PERFORMANCE")

  # Identify the data on which evaluation is to be performed.

  testset0 <- "crs$dataset"
  testname <- crs$dataname
  included <- getIncludedVariables() # Need all vars, including risk.

  if (theWidget("evaluate_training_radiobutton")$getActive())
  {
    # EVALUATE ON TRAINING DATA

    if (crv$appname != "RStat" && theWidget("sample_checkbutton")$getActive())
      infoDialog("You are using the training dataset to evaluate your model.",
                 "This will give you an optimistic estimate",
                 "of the performance of your model.",
                 "\n\nYou may want to choose",
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
    # EVALUATE ON TEST DATA
    
    if (is.null(included))
      testset0 <- "crs$dataset[-crs$sample,]"
    else
      testset0 <- sprintf("crs$dataset[-crs$sample, %s]", included)
    testname <- sprintf("%s [test]", crs$dataname)
  }
  else if (theWidget("evaluate_csv_radiobutton")$getActive())
  {
    # EVALUATE ON CSV DATA

    # We need to allow for the case where the loaded csv data does not
    # have the risk and target columns when we are scoring the data
    # (i.e., not when we are generating confusion charts and other
    # evaluations. For scoring, it is only natural that we do not have
    # the risk and target variables.
    
    filename <- theWidget("evaluate_filechooserbutton")$getFilename()
    crs$dwd <<- dirname(filename)
    crs$mtime <<- urlModTime(filename)

    if (is.null(filename))
    {
      errorDialog("You have requested that a CSV file be used",
                  "as your testing dataset, but you have not",
                  "identified which file. Please use the file",
                  "chooser button to select the CSV file you wish",
                  "to use as your testset for evaluation.")
      return()
    }
                   
    # Load the testset from file, but only load it if it is not
    # already loaded.
    
    if (is.null(crs$testname) || (basename(filename) != crs$testname))
    {
      # Fix filename for MS/Windows - otherwise eval/parse strips the \\.

      if (isWindows()) filename <- gsub("\\\\", "/", filename)

      nastring <- ', na.strings=c(".", "NA", "", "?")'
      read.cmd <- sprintf('crs$testset <<- read.csv("%s"%s)',
                          filename, nastring)
      appendLog("Read a file for evaluating the model",
              gsub("<<-", "<-", read.cmd))
      eval(parse(text=read.cmd))

      testname <- basename(filename)
      crs$testname <<- testname
    }
    
    # TODO The following case for included assumes the same column
    # orders. Should really check this to make sure.  For scoring a
    # dataset we do not include the target or the risk in the
    # variables, since they may not be present in the csv file that is
    # being loaded (if that option is active). Thus, in this case it
    # is best to simply use the whole dataset for scoring. But, for
    # the case where there are lots of columns that are ignored in the
    # model building, if they have lots of NAs then the scoring is
    # going to give NAs for RF, etc. (Pointed out by Ed Cox 9 Feb
    # 2008.) In general, not sure how to handle this, except for now
    # say that the schema must be identical in the scoring dataset to
    # the training dataset (including the target, risk, and ignored
    # columns). In fact,, if the target etc are the last columns then
    # we can get away with it.

    if (is.null(included)) # || theWidget("score_radiobutton")$getActive())
      testset0 <- "crs$testset"
    else
      testset0 <- sprintf("crs$testset[,%s]", included)
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
    appendLog("Assign the R dataset to be used as the test set.", assign.cmd)
    eval(parse(text=assign.cmd))
  }

  # Ensure the test dataset has the same levels for each variable of
  # the training dataset. This can arise when we externally split a
  # dataset into a training and testing dataset, and the smaller
  # testing dataset may not have representatives of all of the
  # variables. Be sure to add any new levels to the end, otherwise
  # you'll end up renaming some of the other levels! This won't help a
  # model that uses the variable and does not find the particular
  # level, although it is okay if it is missing levels. TODO this
  # might need to check for the former and error out if it is the
  # case.

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

    predcmd[[.ADA]] <- genPredictAda(testset[[.ADA]])
    respcmd[[.ADA]] <- genResponseAda(testset[[.ADA]])
    probcmd[[.ADA]] <- genProbabilityAda(testset[[.ADA]])
  }

  if (crv$NNET %in%  mtypes)
  {
    testset[[crv$NNET]] <- testset0

    predcmd[[crv$NNET]] <- sprintf("crs$pr <<- predict(crs$nnet, %s)",
                                   testset[[crv$NNET]])
    respcmd[[crv$NNET]] <- predcmd[[crv$NNET]]
    if (binomialTarget())
      probcmd[[crv$NNET]] <- gsub(")$", ', type="raw")', predcmd[[crv$NNET]])
    else
      probcmd[[crv$NNET]] <- gsub(")$", ', type="prob")', predcmd[[crv$NNET]])
  }

  if (.RPART %in%  mtypes)
  {
    testset[[.RPART]] <- testset0
    predcmd[[.RPART]] <- sprintf("crs$pr <<- predict(crs$rpart, %s)",
                                testset[[.RPART]])

    # For .RPART, the default is to generate class probabilities for
    # each output class, so ensure we instead generate the response.
  
    respcmd[[.RPART]] <- gsub(")$", ', type="class")', predcmd[[.RPART]])

    # For RPART the default predict command generates the probabilities
    # for each class and we assume we are interested in the final class
    # (i.e., for binary classification we are interested in the 1's).
    
    if (theWidget("model_tree_rpart_radiobutton")$getActive())
      probcmd[[.RPART]] <- sprintf("%s[,2]", predcmd[[.RPART]])
    else # ctree
      probcmd[[.RPART]] <- sprintf("%s", predcmd[[.RPART]])
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
    
  if (crv$GLM %in%  mtypes)
  {
    # 080716 The multinom model has been moved to GLM, even though it
    # is using the nnet library. So we need to do the nnet predict
    # here.

    if ("multinom" %in% class(crs$glm))
    {
      testset[[crv$GLM]] <- testset0
      predcmd[[crv$GLM]] <- sprintf("crs$pr <<- predict(crs$glm, %s)",
                                     testset[[crv$GLM]])
      respcmd[[crv$GLM]] <- predcmd[[crv$GLM]]
      probcmd[[crv$GLM]] <- sub(")$", ', type="prob")', predcmd[[crv$GLM]])

      # Add on the actual class also. This is useful for Score but may
      # be a problem for other types of evaluations (of whe=ich there
      # are currently none that that use probcmd for multinom.

      probcmd[[crv$GLM]] <- sub("<<- ", "<<- cbind(",
                                sub(")$",sprintf("), crs$glm$lab[predict(crs$glm, %s)])",
                                                  testset[[crv$GLM]]),
                                    probcmd[[crv$GLM]]))
      
    }        
    else
    {
        
      # GLM's predict removes rows with missing values, so we also need
      # to ensure we remove rows with missing values here.
    
      testset[[crv$GLM]] <- sprintf("na.omit(%s)", testset0)

      predcmd[[crv$GLM]] <- sprintf("crs$pr <<- predict(crs$glm, %s)",
                                    testset[[crv$GLM]])

      # For GLM, a response is a figure close to the class, either close
      # to 1 or close to 0, so threshold it to be either 1 or 0. TODO
      # Simplify this like?
      #    response.cmd <- gsub("predict", "(predict",
      #                         gsub(")$", ")>0.5)*1", response.cmd))
      
      respcmd[[crv$GLM]] <- gsub("predict", "as.factor(as.vector(ifelse(predict",
                                 gsub(")$", ', type="response") > 0.5, 1, 0)))',
                                      predcmd[[crv$GLM]]))

      # For GLM, the response is a probability of the class.
      
      probcmd[[crv$GLM]] <- gsub(")$", ', type="response")', predcmd[[crv$GLM]])
    }
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

  # Currently (and perhaps permanently) the ROCR package deals only
  # with binary classification, as does my own Risk Chart.
  
  if (!(theWidget("confusion_radiobutton")$getActive()
        #theWidget("pvo_radiobutton")$getActive() || Not working for multiclass
        || theWidget("score_radiobutton")$getActive())
      && is.factor(crs$dataset[[crs$target]])
      && length(levels(crs$dataset[[crs$target]])) > 2)
  {
    errorDialog("The number of levels in the target variable is greater",
                "than 2.",
                "Currently, Risk charts and the ROCR package",
                "(which implements the Lift, ROC, Precision, and Specificity",
                "charts) and Scoring and PrvOb",
                "apply only to binary classification.",
                "Either restructure the data for binary classificaiton,",
                "or else please suggest an alternative method of evaluation",
                "to support@togaware.com.")
    return()
  }

  # DISPATCH
  
  if (theWidget("confusion_radiobutton")$getActive())
    msg <- executeEvaluateConfusion(respcmd, testset, testname)
  else if (theWidget("risk_radiobutton")$getActive())
    msg <- executeEvaluateRisk(probcmd, testset, testname)
  else if (theWidget("costcurve_radiobutton")$getActive())
    msg <- executeEvaluateCostCurve(probcmd, testset, testname)
  else if (theWidget("roc_radiobutton")$getActive())
    msg <- executeEvaluateROC(probcmd, testset, testname)
  else if (theWidget("lift_radiobutton")$getActive())
    msg <- executeEvaluateLift(probcmd, testset, testname)
  else if (theWidget("precision_radiobutton")$getActive())
    msg <- executeEvaluatePrecision(probcmd, testset, testname)
  else if (theWidget("sensitivity_radiobutton")$getActive())
    msg <- executeEvaluateSensitivity(probcmd, testset, testname)

  else if (theWidget("pvo_radiobutton")$getActive())
  {
    if (categoricTarget())
      msg <- executeEvaluatePvOplot(probcmd, testset, testname)
    else if (numericTarget())
      msg <- executeEvaluatePvOplot(predcmd, testset, testname)
  }

  else if (theWidget("score_radiobutton")$getActive())
  {
    if (theWidget("kmeans_evaluate_checkbutton")$getActive())
      msg <- executeEvaluateKmeansScore()
    else
    {
      if (categoricTarget())
        msg <- executeEvaluateScore(probcmd, testset, testname)
      else if  (numericTarget())
        msg <- executeEvaluateScore(predcmd, testset, testname)
    }
  }
  else
    msg <- "No appropriate evaluator found."

  if (not.null(msg)) setStatusBar(msg)
}

#----------------------------------------------------------------------
# EVALUATE CONFUSION TABLE
  
executeEvaluateConfusion <- function(respcmd, testset, testname)
{
  TV <- "confusion_textview"
  
  resetTextview(TV)

  for (mtype in getEvaluateModels())
  {

    setStatusBar("Applying the", commonName(mtype),
                 "model to the dataset to generate a classification table...")
    
    # Generate the command to show the confusion matrix.
    
    confuse.cmd <- paste(sprintf("table(crs$pr, %s$%s, ",
                                 testset[[mtype]], crs$target),
                         'dnn=c("Predicted", "Actual"))')
  
    percentage.cmd <- paste("round(100*table(crs$pr, ",
                            sprintf("%s$%s, ", testset[[mtype]], crs$target),
                            'dnn=c("Predicted", "Actual"))',
                            "/length(crs$pr))",
                            sep="")

    if (binomialTarget()) # 080528 TODO generalise to categoricTarget
      error.cmd <- paste("(function(x){return((x[1,2]+x[2,1])/sum(x))})",
                         "(table(crs$pr,",
                         sprintf("%s$%s, ", testset[[mtype]], crs$target),
                         'dnn=c("Predicted", "Actual")))')
    
    # Log the R commands and execute them.

    appendLog(sprintf("%sGenerate a Confusion Table for the %s model.",
                     .START.LOG.COMMENT, commonName(mtype)), no.start=TRUE)
    appendLog(sprintf("Obtain the response from the %s model.",
                      commonName(mtype)),
             gsub("<<-", "<-", respcmd[[mtype]]))
  
    result <- try(eval(parse(text=respcmd[[mtype]])), TRUE)

    # Check for errors - in particular, new levels in the test dataset.

    if (inherits(result, "try-error"))
    {
      if (any(grep("has new level", result)) || any(grep("New levels",result)))
        infoDialog("It seems that the dataset on which the predictions",
                   "from the", mtype, "model are required has a categoric",
                   "variable with levels not found in the training",
                   "dataset. The predictions can not be made in",
                   "this situation. You may need to either ensure",
                   "the training dataset has representatives of all levels",
                   "or else remove them from the testing dataset.",
                   "Alternatively, do not include that variable in the",
                   "modelling. \n\n The actual error message was:\n\n",
                   paste(result, "\n"))
      else if (any(grep("undefined columns", result)))
        infoDialog("It seems that the dataset on which the predictions",
                   "from the", mtype, "model are required has some columns",
                   "missing. This is often the case when your CSV",
                   "dataset does not have the target",
                   "column included (e.g., when your test dataset",
                   "is meant to be used as a scoring dataset, in which case",
                   "we can't perform an evaluation).",
                   "For producing a confusion table we need",
                   "to include the target variable.",
                   "Please load a CSV file which has",
                   "the risk and target variables included.",
                   "\n\n The actual error message was:\n\n",
                   paste(result, "\n"))
      else
        errorReport(respcmd, result)
      next()
    }
    
    appendLog("Now generate the confusion matrix.", confuse.cmd)

    confuse.output <- collectOutput(confuse.cmd, TRUE)
  
    appendLog("Generate confusion matrix showing percentages.", percentage.cmd)
    percentage.output <- collectOutput(percentage.cmd, TRUE)

    if (binomialTarget())
    {
      appendLog("Calucate overall error percentage.", error.cmd)
      error.output <- collectOutput(error.cmd, TRUE)
    }
    
    appendTextview(TV,
                   sprintf(paste("Confusion matrix for the %s model",
                                 "on %s (counts):\n\n"),
                           commonName(mtype), testname),
                   confuse.output,
                   "\n\n",
                   sprintf(paste("Confusion matrix for the %s model",
                                 "on %s (%%):\n\n"),
                           commonName(mtype), testname),
                   percentage.output,
                   if (binomialTarget())
                   sprintf("\n\nOverall error: %s", format(error.output)))
  }
  
  return(sprintf("Generated Confusion Tables.", mtype, testname))
}

#----------------------------------------------------------------------
#
# EVALUATE RISK CHART
#

executeEvaluateRisk <- function(probcmd, testset, testname)
{
  # Initial setup. 

  TV <- "risk_textview"
  resetTextview(TV)
  
  # Ensure a risk variable has been specified.
  
  risk <- crs$risk
  if (is.null(risk))
  {
    errorDialog("No risk variable has been specified.",
                "From the Data tab please identify one variable as",
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

  # Put 1 or 2 charts onto their own plots. Otherwise, put the
  # multiple charts onto one plot, keeping them all the same size
  # (thus if numplots is odd, leave a cell of the plot empty.
  
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

    setStatusBar("Applying", commonName(mtype),
                 "model to the dataset to generate a risk chart ...")
    
    # We need the base testset name here to get the risk variable, which
    # is not usually in the list of included columns.
  
    # testbase <- gsub(", ?c\\(.*\\]", ",]", testset)

    # Instead, obtain the column list, and if it exists, add the risk
    # variable to it, to avoid listing all columns, since this can
    # affect the na.omit function which will omit more rows if these
    # extra columns have NAs.

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
                      "crs$eval$Precision, crs$eval$Recall, crs$eval$Risk,",
                      'risk.name="', risk, '", recall.name="', crs$target,
                      '")',
                      "\n",
                      genPlotTitleCmd("Risk Chart", commonName(mtype),
                                      testname, risk),
                      sep="")

    appendLog("Generate a Risk Chart",
             "#The Rattle package provides evaluateRisk and plotRisk.\n\n",
             gsub("<<-", "<-", probcmd[[mtype]]), "\n",
             gsub("<<-", "<-", evaluate.cmd), "\n",
             plot.cmd, sep="")

    result <- try(eval(parse(text=probcmd[[mtype]])), TRUE)

    # Check for errors - in particular, new levels in the test dataset.
    
    if (inherits(result, "try-error"))
    {
      if (any(grep("has new level", result)) || any(grep("New levels",result)))
        infoDialog("It seems that the dataset on which the probabilities",
                   "from the", mtype, "model are required has a categoric",
                   "variable with levels not found in the training",
                   "dataset. The probabilities can not be determined in",
                   "this situation. You may need to either ensure",
                   "the training dataset has representatives of all levels",
                   "or else remove them from the testing dataset.",
                   "Alternatively, do not include that variable in the",
                   "modelling. \n\n The actual error message was:\n\n",
                   paste(result, "\n"))
      else if (any(grep("undefined columns", result)))
        infoDialog("It seems that the dataset on which the predictions",
                   "from the", mtype, "model are required has some columns",
                   "missing. This is often the case when your CSV",
                   "dataset does not have the risk or target",
                   "columns included (e.g., when your test dataset",
                   "is meant to be used as a scoring dataset, in which case",
                   "we can't perform an evaluation).",
                   "For producing a risk chart we need",
                   "to include the risk and target variables.",
                   "Please load a CSV file which has",
                   "the risk and target variables included.",
                   "\n\n The actual error message was:\n\n",
                   paste(result, "\n"))
      else
        errorReport(probcmd, result)
      next()
    }

    # Check for all results the same.
    
    if (length(levels(as.factor(crs$pr))) == 1)
    {
      errorDialog("The model predicts the same result for all records,",
                  "so there is nothing to plot!")
      return()
    }

    # Now generate a summary of the performance at various probability
    # cutoffs, with the result being stored in crs$eval.
  
    eval(parse(text=evaluate.cmd))

    # We want to display the numeric results of the evaluation. But if
    # there are too many rows, as produced by KSVM for example, it will
    # be too much, so limit it to 100 row, which need to be selected
    # every Nth.

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
    msg <- paste("Summary ", commonName(mtype), " model ",
                 sprintf("(built using %s)", mtype), " on ",
                 testname,
                 " by probability cutoffs.\n\n", msg, sep="")
    appendTextview(TV, msg, collectOutput(sprintf("crs$eval[%s,]", id), TRUE))

    # Display the AUC measures.

    #auc <- calculateRiskAUC(crs$eval)
    #print(auc)
    aucRisk <- calculateAUC(crs$eval$Caseload, crs$eval$Risk)
    aucRecall <- calculateAUC(crs$eval$Caseload, crs$eval$Recall)
    appendTextview(TV, paste("The area under the Risk and Recall curves for ",
                             commonName(mtype), " model\n\n",
                             "Area under the Risk   (red)   curve: ",
                             sprintf("%d%% (%0.3f)\n",
                                     round(100*aucRisk), aucRisk),
                             "Area under the Recall (green) curve: ",
                             sprintf("%d%% (%0.3f)\n",
                                     round(100*aucRecall), aucRecall),
                             sep=""))
    
    # Display the Risk Chart itself now.

    # For 2 plots, so as not to overwrite the first plot, if we are
    # about to plot the second plot, initiate a new plot.
    
    if (numplots == 2 && mtype == model.list[length(model.list)]) newPlot(1)

    eval(parse(text=plot.cmd))

  }
  
  # Restore par
  
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

  # With na.rm=TRUE we cater for the case when the actual data has
  # missing values for the target.
  
  if (min(actual, na.rm=TRUE) != 0 || max(actual, na.rm=TRUE) !=1 )
    stop("actual must be binary (0,1) but found (",
         min(actual, na.rm=TRUE), ",", max(actual, na.rm=TRUE), ").")

  # For KSVMs, and perhaps other modellers, the predictied values are
  # probabilites, which may be a very high level of precision (e.g.,
  # 0.999999999999996 or 2.58015830922886e-13), and thus, when
  # converted to a factor, we have almost a one-to-one match from an
  # entity to a probability. When converted to a data frame the
  # resulting row names (these probablities of being a 1) have
  # caseloads of 1, 2, or 3, thus there are very many, and sometimes,
  # the probablities are the same! We then get duplicate row names and
  # the assigning of new names to the columns below gives an error
  # about duplicate row names! We should aggregate up to three
  # significant figures in the probabilities to make everything much
  # easier. BUT this then lumps all of the 0.9999999.... together, and
  # leaves a very large jump at the right end of the plot! We really
  # might want to instead aggregate on caseload! But rounding it to 13
  # digits seems okay! We get a good plot.

  predicted <- as.factor(round(predicted, 13))
  
  ds.actual <- data.frame(Actual=actual,
                            Risk=as.numeric(risks), # Avoid integer overflow
                            Predict=as.factor(predicted))
  #Predict=as.factor(ds.predict[,2]))

  # With na.rm=TRUE in the first sum here we cater for the case when
  # the actual data has missing values for the target. 
  
  ds.evaluation <- as.data.frame(t(rbind(tapply(ds.actual$Actual,
                                                ds.actual$Predict,
                                                sum, na.rm=TRUE),
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
  # This is Michael's measure of performance.
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
  if (dev == "" && filename != "")
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
  if (not.null(ri) && all(ri <= 1.5)) ri <- ri * 100 # Can sometimes be just >1
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

#----------------------------------------------------------------------
# EVALUATE COST CURVE 080524 

executeEvaluateCostCurve <- function(probcmd, testset, testname)
{
  # 080524 Display Cost Curves (Drummond and Holte) 

  lib.cmd <- "require(ROCR, quietly=TRUE)"
  if (! packageIsAvailable("ROCR", "plot a cost curve")) return()

  # Put 1 or 2 charts onto their own plots. Otherwise, put the
  # multiple charts onto one plot, keeping them all the same size
  # (thus if numplots is odd, leave a cell of the plot empty.
  
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

  nummodels <- length(probcmd)
  mcolors <- rainbow(nummodels, 1, .8)
  mcount <- 0  
  
  model.list <- getEvaluateModels()

  for (mtype in model.list)
  {
    setStatusBar("Applying", commonName(mtype),
                 "model to the dataset to generate a cost curve ...")

    mcount <- mcount + 1
    plot.cmd <- paste("plot(0, 0, xlim=c(0, 1), ylim=c(0, 1),",
                      'xlab="Probability cost function",',
                      'ylab="Normalized expected cost")\n',
                      'lines(c(0,1),c(0,1))\n',
                      'lines(c(0,1),c(1,0))\n',
                      'pred <- prediction(crs$pr,',
                      sprintf("%s$%s)\n", testset[[mtype]], crs$target),
                      'perf1 <- performance(pred, "fpr", "fnr")\n',
                      'for (i in 1:length(perf1@x.values))\n{\n',
                      '\tfor (j in 1:length(perf1@x.values[[i]]))\n\t{\n',
                      '\t\tlines(c(0,1),c(perf1@y.values[[i]][j],\n',
                      '\t\t\t\tperf1@x.values[[i]][j]),\n',
                      '\t\t\t\tcol=terrain.colors(10)[i],lty=3)\n',
                      '\t}\n}\n',
                      'perf<-performance(pred,"ecost")\n',
                      "plot(perf, lwd=1.5, xlim=c(0,1), ylim=c(0,1), add=T)\n",
                      "op <- par(xpd=TRUE)\n",
                      'text(0, 1.07, "FPR")\n',
                      'text(1, 1.07, "FNR")\n',
                      "par(op)\n",
                      'text(0.12, 1, "Predict +ve")\n',
                      'text(0.88, 1, "Predict -ve")\n',
                      # TODO 080810 Add text AUC=... to plot
                      genPlotTitleCmd("Cost Curve", commonName(mtype),
                                      testname))
                      
    appendLog("Plot a cost curve using the ROCR package.", lib.cmd)
    eval(parse(text=lib.cmd))
  
    appendLog(sprintf("Generate a Cost Curve for the %s model on %s.",
                     commonName(mtype), testname),
             gsub("<<-", "<-", probcmd[[mtype]]), "\n", plot.cmd)

    result <- try(eval(parse(text=probcmd[[mtype]])), silent=TRUE)

    # Check for errors - in particular, new levels in the test dataset.
    
    if (inherits(result, "try-error"))
    {
      if (any(grep("has new level", result)) || any(grep("New levels",result)))
        infoDialog("It seems that the dataset on which the probabilities",
                   "from the", mtype, "model are required has a categoric",
                   "variable with levels not found in the training",
                   "dataset. The probabilities can not be determined in",
                   "this situation. You may need to either ensure",
                   "the training dataset has representatives of all levels",
                   "or else remove them from the testing dataset.",
                   "Alternatively, do not include that variable in the",
                   "modelling. \n\n The actual error message was:\n\n",
                   paste(result, "\n"))
      else
        errorReport(probcmd, result)
      next()
    }

    # Display the Cost Curve itself now.

    # For 2 plots, so as not to overwrite the first plot, if we are
    # about to plot the second plot, initiate a new plot.
    
    if (numplots == 2 && mtype == model.list[length(model.list)]) newPlot(1)

    eval(parse(text=plot.cmd))

#    # Report the area under the curve.
#  
#    auc.cmd <- paste("performance(prediction(crs$pr, ",
#                    sprintf("%s$%s),", testset[[mtype]], crs$target),
#                    '"auc")', sep="")
#    appendLog("Calculate the area under the curve for the plot.", auc.cmd)
#    auc <- eval(parse(text=auc.cmd))
#    appendTextview(TV, paste("Area under the ROC curve for the",
#                             sprintf("%s model on %s is %0.4f",
#                                     mtype, testname,
#                                     attr(auc, "y.values"))))
  }
#  lines(c(0,1), c(0,1)) # Baseline

  ## If just one model, and we are plotting the test dataset, then
  ## also plot the training dataset.

#  if (nummodels==1 && length(grep("\\[test\\]", testname))>0)
#  {
#    mcount <- mcount + 1
#    plot.cmd <- paste("plot(performance(prediction(crs$pr, ",
#                      sprintf("%s$%s),",
#                              sub("-crs\\$sample", "crs$sample",
#                                  testset[[mtype]]), crs$target),
#                      '"tpr", "fpr"), ',
#                      'col="#00CCCCFF", lty=2, ',
#                      sprintf("add=%s)\n", addplot),
#                      sep="")
#    appendLog(sprintf("Generate an ROC Curve for the %s model on %s.",
#                     mtype, sub('\\[test\\]', '[train]', testname)),
#             gsub("<<-", "<-", sub("-crs\\$sample", "crs$sample",
#                                   probcmd[[mtype]])), "\n", plot.cmd)
#
#    result <- try(eval(parse(text=sub("-crs\\$sample",
#                               "crs$sample", probcmd[[mtype]]))), silent=TRUE)
#    eval(parse(text=plot.cmd))
#    models <- c("Test", "Train")
#    nummodels <- 2
#    legtitle <- getEvaluateModels()
#    title <- sub('\\[test\\]', '', testname)
#  }
#  else
#  {
#    models <- getEvaluateModels()
#    legtitle <- "Models"
#    title <- testname
#  }

#  legendcmd <- paste('legend("bottomright",',
#                     sprintf("c(%s),",
#                             paste('"', models, '"',
#                                   sep="", collapse=",")),
#                     sprintf('col=rainbow(%d, 1, .8), lty=1:%d,',
#                             nummodels, nummodels),
#                     sprintf('title="%s", inset=c(0.05, 0.05))', legtitle))
#  appendLog("Add a legend to the plot.", legendcmd)
#  eval(parse(text=legendcmd))
  
#  decor.cmd <- paste(genPlotTitleCmd("ROC Curve", "", title),
#                    '\ngrid()', sep="")
#  appendLog("Add decorations to the plot.", decor.cmd)
#  eval(parse(text=decor.cmd))
  
  return(sprintf("Generated ROC Curves on %s.", testname))
}

  
##----------------------------------------------------------------------
##
## EVALUATE LIFT CHART
##

executeEvaluateLift <- function(probcmd, testset, testname)
{
  lib.cmd <- "require(ROCR, quietly=TRUE)"
  if (! packageIsAvailable("ROCR", "plot a lift chart")) return()

  newPlot()
  addplot <- "FALSE"

  nummodels <- length(probcmd)
  mcolors <- rainbow(nummodels, 1, .8)
  mcount <- 0  
  
  for (mtype in getEvaluateModels())
  {
    setStatusBar("Applying", mtype, "model to the dataset to generate",
                 "a lift chart ...")
    
    mcount <- mcount + 1
    plot.cmd <- paste("plot(performance(prediction(crs$pr, ",
                      sprintf("%s$%s),", testset[[mtype]], crs$target),
                      '"lift", "rpp"), ',
                      sprintf('col="%s", lty=%d, ', mcolors[mcount], mcount),
                      sprintf("add=%s)\n", addplot),
                      sep="")
    addplot <- "TRUE"
    
    appendLog("Display Lift Chart using the ROCR package.", lib.cmd)
    eval(parse(text=lib.cmd))
    
    appendLog(sprintf("Generate a Lift Chart for the %s model on %s.",
                     mtype, testname),
             gsub("<<-", "<-", probcmd[[mtype]]), "\n", plot.cmd)

    result <- try(eval(parse(text=probcmd[[mtype]])), silent=TRUE)

    # Check for errors - in particular, new levels in the test dataset.
    
    if (inherits(result, "try-error"))
    {
      if (any(grep("has new level", result)) || any(grep("New levels",result)))
        infoDialog("It seems that the dataset on which the probabilities",
                   "from the", mtype, "model are required has a categoric",
                   "variable with levels not found in the training",
                   "dataset. The probabilities can not be determined in",
                   "this situation. You may need to either ensure",
                   "the training dataset has representatives of all levels",
                   "or else remove them from the testing dataset.",
                   "Alternatively, do not include that variable in the",
                   "modelling. \n\n The actual error message was:\n\n",
                   paste(result, "\n"))
      else
        errorReport(probcmd, result)
      next()
    }

    eval(parse(text=plot.cmd))
    
  }

  # If just one model, and we are plotting the test dataset, then
  # also plot the training dataset.

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
    appendLog(sprintf("Generate a Lift Chart for the %s model on %s.",
                     mtype, sub('\\[test\\]', '[train]', testname)),
             gsub("<<-", "<-", sub("-crs\\$sample", "crs$sample",
                                   probcmd[[mtype]])), "\n", plot.cmd)

    result <- try(eval(parse(text=sub("-crs\\$sample",
                               "crs$sample", probcmd[[mtype]]))), silent=TRUE)
    eval(parse(text=plot.cmd))
    models <- c("Test", "Train")
    nummodels <- 2
    legtitle <- sapply(getEvaluateModels(), commonName)
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
  appendLog("Add a legend to the plot.", legendcmd)
  eval(parse(text=legendcmd))
  
  decorcmd <- paste(genPlotTitleCmd("Lift Chart", "", title),
                    '\ngrid()', sep="")
  appendLog("Add decorations to the plot.", decorcmd)
  eval(parse(text=decorcmd))
  
  return("Generated Lift Charts.")
}

##----------------------------------------------------------------------
##
## EVALUATE ROC PLOT
##

executeEvaluateROC <- function(probcmd, testset, testname)
{
  TV <- "roc_textview"
  resetTextview(TV)
  lib.cmd <- "require(ROCR, quietly=TRUE)"
  if (! packageIsAvailable("ROCR", "plot a ROC curve")) return()

  newPlot()
  addplot <- "FALSE"

  nummodels <- length(probcmd)
  mcolors <- rainbow(nummodels, 1, .8)
  mcount <- 0  
  
  for (mtype in getEvaluateModels())
  {
    setStatusBar("Applying", mtype, "model to the dataset to generate",
                 "a ROC plot ...")

    mcount <- mcount + 1
    plot.cmd <- paste("plot(performance(prediction(crs$pr, ",
                      sprintf("%s$%s),", testset[[mtype]], crs$target),
                      '"tpr", "fpr"), ',
                      sprintf('col="%s", lty=%d, ', mcolors[mcount], mcount),
                      sprintf("add=%s)\n", addplot),
                      sep="")
    addplot <- "TRUE"

    appendLog("Plot an ROC curve using the ROCR package.", lib.cmd)
    eval(parse(text=lib.cmd))
  
    appendLog(sprintf("Generate an ROC Curve for the %s model on %s.",
                     mtype, testname),
             gsub("<<-", "<-", probcmd[[mtype]]), "\n", plot.cmd)

    result <- try(eval(parse(text=probcmd[[mtype]])), silent=TRUE)

    ## Check for errors - in particular, new levels in the test dataset.
    
    if (inherits(result, "try-error"))
    {
      if (any(grep("has new level", result)) || any(grep("New levels",result)))
        infoDialog("It seems that the dataset on which the probabilities",
                   "from the", mtype, "model are required has a categoric",
                   "variable with levels not found in the training",
                   "dataset. The probabilities can not be determined in",
                   "this situation. You may need to either ensure",
                   "the training dataset has representatives of all levels",
                   "or else remove them from the testing dataset.",
                   "Alternatively, do not include that variable in the",
                   "modelling. \n\n The actual error message was:\n\n",
                   paste(result, "\n"))
      else
        errorReport(probcmd, result)
      next()
    }

    eval(parse(text=plot.cmd))

    ## Report the area under the curve.
  
    auc.cmd <- paste("performance(prediction(crs$pr, ",
                    sprintf("%s$%s),", testset[[mtype]], crs$target),
                    '"auc")', sep="")
    appendLog("Calculate the area under the curve for the plot.", auc.cmd)
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
    appendLog(sprintf("Generate an ROC Curve for the %s model on %s.",
                     mtype, sub('\\[test\\]', '[train]', testname)),
             gsub("<<-", "<-", sub("-crs\\$sample", "crs$sample",
                                   probcmd[[mtype]])), "\n", plot.cmd)

    result <- try(eval(parse(text=sub("-crs\\$sample",
                               "crs$sample", probcmd[[mtype]]))), silent=TRUE)
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
  appendLog("Add a legend to the plot.", legendcmd)
  eval(parse(text=legendcmd))
  
  decor.cmd <- paste(genPlotTitleCmd("ROC Curve", "", title),
                    '\ngrid()', sep="")
  appendLog("Add decorations to the plot.", decor.cmd)
  eval(parse(text=decor.cmd))

  return(sprintf("Generated ROC Curves on %s.", testname))
}
  
##----------------------------------------------------------------------
##
## EVALUATE PRECISION PLOT
##

executeEvaluatePrecision <- function(probcmd, testset, testname)
{
  lib.cmd <- "require(ROCR, quietly=TRUE)"
  if (! packageIsAvailable("ROCR", "plot a precision chart")) return()

  newPlot()
  addplot <- "FALSE"

  nummodels <- length(probcmd)
  mcolors <- rainbow(nummodels, 1, .8)
  mcount <- 0  
  
  for (mtype in getEvaluateModels())
  {
    setStatusBar("Applying", mtype, "model to the dataset to generate",
                 "a Precision/Recall plot ...")

    mcount <- mcount + 1

    plot.cmd <- paste("plot(performance(prediction(crs$pr, ",
                      sprintf("%s$%s),", testset[[mtype]], crs$target),
                      '"prec", "rec"), ',
                      sprintf('col="%s", lty=%d, ', mcolors[mcount], mcount),
                      sprintf("add=%s)\n", addplot),
                      sep="")
    addplot <- "TRUE"
  
    appendLog("Precision/Recall Plot using the ROCR package", lib.cmd)
    eval(parse(text=lib.cmd))

    appendLog(sprintf("Generate a Precision/Recall Plot for the %s model on %s.",
                     mtype, testname),
             gsub("<<-", "<-", probcmd[[mtype]]), "\n", plot.cmd)

    result <- try(eval(parse(text=probcmd[[mtype]])), silent=TRUE)

    ## Check for errors - in particular, new levels in the test dataset.
    
    if (inherits(result, "try-error"))
    {
      if (any(grep("has new level", result)) || any(grep("New levels",result)))
        infoDialog("It seems that the dataset on which the probabilities",
                   "from the", mtype, "model are required has a categoric",
                   "variable with levels not found in the training",
                   "dataset. The probabilities can not be determined in",
                   "this situation. You may need to either ensure",
                   "the training dataset has representatives of all levels",
                   "or else remove them from the testing dataset.",
                   "Alternatively, do not include that variable in the",
                   "modelling. \n\n The actual error message was:\n\n",
                   paste(result, "\n"))
      else
        errorReport(probcmd, result)
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
    appendLog(sprintf("Generate a Precision/Recall Plot for the %s model on %s.",
                     mtype, sub('\\[test\\]', '[train]', testname)),
             gsub("<<-", "<-", sub("-crs\\$sample", "crs$sample",
                                   probcmd[[mtype]])), "\n", plot.cmd)

    result <- try(eval(parse(text=sub("-crs\\$sample",
                               "crs$sample", probcmd[[mtype]]))), silent=TRUE)
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
  appendLog("Add a legend to the plot.", legendcmd)
  eval(parse(text=legendcmd))
  
  decor.cmd <- paste(genPlotTitleCmd("Precision/Recall Plot", "", title),
                    '\ngrid()', sep="")
  appendLog("Add decorations to the plot.", decor.cmd)
  eval(parse(text=decor.cmd))
  
  return(sprintf("Generated Precision/Recall Plot on %s.", title))
}

##----------------------------------------------------------------------
##
## EVALUATE SENSITIVITY PLOT
##

executeEvaluateSensitivity <- function(probcmd, testset, testname)
{
  lib.cmd <- "require(ROCR, quietly=TRUE)"
  if (! packageIsAvailable("ROCR", "plot a sensitivity chart")) return()

  newPlot()
  addplot <- "FALSE"

  nummodels <- length(probcmd)
  mcolors <- rainbow(nummodels, 1, .8)
  mcount <- 0  
  
  for (mtype in getEvaluateModels())
  {
    setStatusBar("Applying", mtype, "model to the dataset to generate",
                 "a Sensitivity plot ...")

    mcount <- mcount + 1
    plot.cmd <- paste("plot(performance(prediction(crs$pr, ",
                      sprintf("%s$%s),", testset[[mtype]], crs$target),
                      '"sens", "spec"), ',
                      sprintf('col="%s", lty=%d, ', mcolors[mcount], mcount),
                      sprintf("add=%s)\n", addplot),
                      sep="")
     addplot <- "TRUE"
 
    appendLog("Display a Sensitivity/Specificity Plot using the ROCR package",
             lib.cmd)
    eval(parse(text=lib.cmd))

    appendLog(sprintf("Generate Sensitivity/Specificity Plot for %s model on %s.",
                     mtype, testname),
             gsub("<<-", "<-", probcmd[[mtype]]), "\n", plot.cmd)

    result <- try(eval(parse(text=probcmd[[mtype]])), silent=TRUE)

    ## Check for errors - in particular, new levels in the test dataset.
    
    if (inherits(result, "try-error"))
    {
      if (any(grep("has new level", result)) || any(grep("New levels",result)))
        infoDialog("It seems that the dataset on which the probabilities",
                   "from the", mtype, "model are required has a categoric",
                   "variable with levels not found in the training",
                   "dataset. The probabilities can not be determined in",
                   "this situation. You may need to either ensure",
                   "the training dataset has representatives of all levels",
                   "or else remove them from the testing dataset.",
                   "Alternatively, do not include that variable in the",
                   "modelling. \n\n The actual error message was:\n\n",
                   paste(result, "\n"))
      else
        errorReport(probcmd, result)
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
    appendLog(sprintf("Generate a Lift Chart for the %s model on %s.",
                     mtype, sub('\\[test\\]', '[train]', testname)),
             gsub("<<-", "<-", sub("-crs\\$sample", "crs$sample",
                                   probcmd[[mtype]])), "\n", plot.cmd)

    result <- try(eval(parse(text=sub("-crs\\$sample",
                               "crs$sample", probcmd[[mtype]]))), silent=TRUE)
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
  appendLog("Add a legend to the plot.", legendcmd)
  eval(parse(text=legendcmd))
  
  decor.cmd <- paste(genPlotTitleCmd("Sensitivity/Specificity (tpr/tnr)", "",
                                    title),
                    '\ngrid()', sep="")
  appendLog("Add decorations to the plot.", decor.cmd)
  eval(parse(text=decor.cmd))

  return(sprintf("Generated Sensitivity/Specificity Plot on %s.", testname))
}

#----------------------------------------------------------------------

executeEvaluateKmeansScore <- function()
{

  # TODO 080718 Need to select the dataset to which we append the
  # "score". Currently, it is the training dataset ONLY.
  
  startLog("EXPORT KMEANS CLUSTER ASSIGNMENT AS CSV")
    
  # Obtain filename to write the clusters to.
  
  dialog <- gtkFileChooserDialog("Export CSV", NULL, "save",
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-save", GtkResponseType["accept"])
    
  if(not.null(crs$dataname))
    dialog$setCurrentName(paste(get.stem(crs$dataname), "_kmeans", sep=""))

  ff <- gtkFileFilterNew()
  ff$setName("CSV Files")
  ff$addPattern("*.csv")
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

  if (get.extension(save.name) != "csv")
    save.name <- sprintf("%s.csv", save.name)
  
  if (file.exists(save.name))
    if (! questionDialog("A file of the same name as", save.name,
                         "already exists. Do you want to overwrite",
                         "this file?"))
      return()

  # 080523 Output all original data plus the cluster number, taking
  # missing values into account. This gets a little complex, to say
  # the least. We need to put the cluster number with each input
  # record, then add in those that have missing values, giving them
  # a cluster number of NA, and then make sure we generate the CSV
  # file in the same numeric order as it was read in.

  clnm <- "names(crs$kmeans$cluster)"
  clna <- sprintf("setdiff(rownames(crs$dataset[%s, ]), %s)",
                  ifelse(theWidget("sample_checkbutton")$getActive(),
                         "crs$sample", ""), clnm)

  # Check if there are missing values, and if not we don't need to
  # be so complex!

  missing <- length(eval(parse(text=clna))) > 0

  if (theWidget("score_idents_radiobutton")$getActive())
    sinclude <- paste('c("', paste(getSelectedVariables("ident"), collapse='", "'), '")',
                      sep="")
  else if (theWidget("score_all_radiobutton")$getActive())
    sinclude <- paste('c("', paste(names(crs$dataset), collapse='", "'), '")', sep="")

  csv.cmd <-  sprintf(paste("rbind(data.frame(crs$dataset[%s, ][%s,%s],",
                            "kmeans=crs$kmeans$cluster)",
                            "%s", # If non missing this is empty.
                            ")[as.character(sort(as.integer(",
                            "rownames(crs$dataset[%s, ])))), ]"),
                      ifelse(theWidget("sample_checkbutton")$getActive(),
                             "crs$sample", ""),
                      clnm,
                      sinclude,
                      ifelse(missing,
                             sprintf(",data.frame(crs$dataset[%s, ][%s,], kmeans=NA)",
                                     ifelse(theWidget("sample_checkbutton")$
                                            getActive(), "crs$sample", ""),
                                     clna),
                             ""),
                      ifelse(theWidget("sample_checkbutton")$getActive(),
                             "crs$sample", "")
                      ##sprintf('"%s"', paste(idents, collapse='", "'))
                      )
  
  # We can't pass "\" in a filename to the parse command in
  # MS/Windows so we have to run the save/write command separately,
  # i.e., not inside the string thaat is being parsed.
  
  appendLog("Generate data frame and export the clusters to CSV.",
            sprintf('write.csv(%s, file="%s", row.names=FALSE)', csv.cmd, save.name))
  write.csv(eval(parse(text=csv.cmd)), file=save.name, row.names=FALSE)

  return(paste("Scores have been saved to the file", save.name))
}


executeEvaluateScore <- function(probcmd, testset, testname)
{
  # Apply each selected model to the selected dataset and save the
  # results to a file with columns containing the score (or scores in
  # the case of a multinomial model) from a specific model. Other
  # columns depend on the radio button options, and will either be
  # just the identifiers, or a copy of the full data, or else, the
  # score columns are written to the original file (assuming CSV).
  # TODO: Would this be better as the Export functionality for the
  # Evaluate tab?

  # Obtain information from the interface: what other data is to be
  # included with the scores.

  sinclude <- NULL
  if (theWidget("score_idents_radiobutton")$getActive())
    sinclude <- "idents"
  else if (theWidget("score_all_radiobutton")$getActive())
    sinclude <- "all"
  
  # Obtain the filename to write the scores to.  We ask the user for a
  # filename if RATTLE_SCORE and .RATTLE.SCORE.OUT are not provided.
  # TODO should we add getwd() to the RATTLE_SCORE or
  # .RATTLE.SCORE.OUT if a relative path.

  fname <- Sys.getenv("RATTLE_SCORE")
  if (fname == "" && exists(".RATTLE.SCORE.OUT")) fname <- .RATTLE.SCORE.OUT
  
  if (fname == "")
  {
    # The default filename is the testname with spaces replaced by
    # "_", etc., and then "_score" is appended, and then "_all" or
    # "_idents" to indicate what other columns are included, and then
    # ".csv".
    
    default <- sprintf("%s_score_%s.csv",
                       gsub(" ", "_",
                            gsub("\\.[[:alnum:]]*", "",
                                 gsub("(\\[|\\])", "",
                                      gsub("\\*", "", testname)))),
                       sinclude)
    # fname <- paste(getwd(), default, sep="/")
      
    dialog <- gtkFileChooserDialog("Score Files", NULL, "save",
                                   "gtk-cancel", GtkResponseType["cancel"],
                                   "gtk-save", GtkResponseType["accept"])
    
    if(not.null(testname)) dialog$setCurrentName(default)
    
    dialog$setCurrentFolder(crs$dwd)
    
    ff <- gtkFileFilterNew()
    ff$setName("CSV Files")
    ff$addPattern("*.csv")
    dialog$addFilter(ff)
    
    ff <- gtkFileFilterNew()
    ff$setName("All Files")
    ff$addPattern("*")
    dialog$addFilter(ff)
    
    if (dialog$run() == GtkResponseType["accept"])
    {
      fname <- dialog$getFilename()
      dialog$destroy()
    }
    else
    {
      dialog$destroy()
      return()
    }
  }
  
  # Score the data with each model, collect the outputs, and then
  # write them all at once to file.
  #
  # Note that there is at least one testset (hence, below we look at
  # just the first testset), but possibly others, and there is an
  # assumption that they are all of the forms:
  #
  # crs$dataset[-crs$sample, c(...)]
  # na.omit(crs$dataset[-crs$sample, c(...)])
  #
  # or else they are all of the forms:
  #
  # crs$testset[,c(...)]
  # na.omit(crs$testset[,c(...)])
  #
  # TODO 080425 I could test to make sure they are all of the same
  # form to make sure the assumption is not breached.
  #
  # We first remove the na.omit so we can get all row names. The
  # na.omit is there for those models, like glm and ksvm, which do not
  # handle NA's themselves.

  ts <- testset[[1]]
  if (substr(ts, 1, 7) == "na.omit") ts <- sub('na.omit\\((.*)\\)$', '\\1', ts)

  # Create the data frame to hold the scores, initialised to NA in
  # every cell.
  
  the.names <- eval(parse(text=sprintf("row.names(%s)", ts)))
  the.models <- getEvaluateModels()
  scores <- as.data.frame(matrix(nrow=length(the.names),
                                 ncol=length(the.models)))
  row.names(scores) <- the.names
  names(scores) <- the.models
  
  # Obtain a list of the identity vartiables and 080713 target to output.
    
  idents <- union(getSelectedVariables("ident"), getSelectedVariables("target"))

  setStatusBar("Scoring dataset ...")
  
  for (mtype in the.models)
  {
    setStatusBar("Scoring dataset using", mtype, "...")
  
    # Apply the model to the dataset.

    appendLog(sprintf(paste("%s: Obtain %s",
                            "for the %s model on %s."),
                      toupper(mtype),
                      if (categoricTarget())
                      "probability scores"
                      else if (numericTarget())
                      "predictions",
                      commonName(mtype), testname),
              gsub("<<-", "<-", probcmd[[mtype]]))
    
    result <- try(eval(parse(text=probcmd[[mtype]])), silent=TRUE)

    # Check for errors - in particular, new levels in the testset. If
    # an error is found we skip this mtype and proceed to the
    # next. This will leave NA's in the score file for this mtype.
    
    if (inherits(result, "try-error"))
    {
      if (any(grep("has new level", result)) || any(grep("New levels",result)))
        infoDialog("The dataset on which the", mtype,
                   "model is applied to has a categoric",
                   "variable with levels not found in the training",
                   "dataset. The model can not be applied in",
                   "this situation. You may need to either ensure",
                   "the training dataset has representatives of all levels",
                   "or else remove them from the testing dataset.",
                   "Alternatively, do not include that variable in the",
                   "modelling. \n\n The actual error message was:\n\n",
                   paste(result, "\n"))
      else
        errorReport(probcmd, result)
      next()
    }

    # 080417 Communicate the score file name. Note that originally I
    # intended to export the user's choice as an environment variable
    # to communicate that back to a calling process. But setenv
    # unfortunately does not export the name outside the R process so
    # it is of no use. TODO We could get a bit more sophisticated
    # here and add getwd() to the RATTLE_SCORE if it is a relative
    # path.

    # Transform the dataset expression into what we need to extract
    # the relevant columns.
    #
    # Various formats include:
    #
    #    train	crs$dataset[crs$sample, c(3:12,14)]
    #    test	crs$dataset[-crs$sample, c(3:12,14)]
    #    csv	crs$testset[,c(3:12,14)]
    #    df	crs$testset
    #
    # Want
    #    subset(crs$dataset[-crs$sample,], select=Idents) + crs$pr
    #

    scoreset <- testset[[mtype]]

    # If no comma in scoreset, leave as is, else find first comma,
    # remove everything after, and replace with "]". PROBLEM TODO If
    # the testset[[crv$MODEL]] includes na.omit, we need to do something
    # different because after the following step of replacing the
    # column list with nothing, it is very likely that new columns
    # are included that have NAs, and hence the na.omit will remove
    # even more rows for the subset command than it does for the
    # predict command. Yet we still want to ensure we have all the
    # appropriate columns available. So use
    # na.omit(crs$dataset[-crs$sample, c(2:4,6:10,13)])@na.action to
    # remove the rows from crs$dataset[-crs$sample,] that have
    # missing values with regard the columns c(2:4,6:10,13). Thus if
    # we have scoreset as:
    #
    #  na.omit(crs$dataset[-crs$sample, c(2:4,6:10,13)])
    #
    # we want to:
    #
    #  omitted <- na.omit(crs$dataset[-crs$sample, c(2:4,6:10,13)])@na.action
    #
    # and then scoreset should become:
    #
    #  crs$dataset[-crs$sample,][-omitted,]

    # First deal with the na.omit case, to capture the list of rows
    # omitted.

    # Mod by Ed Cox (080301) to fix error when no NAs in test set. The
    # error was:
    #
    #  Error in eval(expr, envir, enclos) :
    #    no slot of name "na.action" for this object of class "data.frame"
    #
    # The issue appears to be that it complains if there are no NAs to
    # remove so bypass the omission code.  This is not a general fix
    # because Ed has run into a case where it failed, but until we
    # have an insight as to what the real problem is we go with
    # this. It may be related to regression versus classification. Ed
    # was doing a regression (and testing the Pr v Ob plots).
    
    #scorevarlist <- c(getSelectedVariables("ident"),
    #                  getSelectedVariables("target"),
    #                  getSelectedVariables("input"),
    #                  getSelectedVariables("risk"))

    omitted <- NULL
    if (substr(scoreset, 1, 7) == "na.omit")
    {
      narm.dim <- eval(parse(text=sprintf("dim(%s)", scoreset)))[1]
      orig.dim <- eval(parse(text=sub('na.omit', 'dim', scoreset)))[1]
      if (narm.dim != orig.dim)
        
        # Ed had: && !dim(tmpset)[1]==dim(na.omit(tmpset))[1])
        
        # End of Ed's modification.
        # if (substr(scoreset, 1, 7) == "na.omit")
      {
        omit.cmd <- paste("omitted <- attr(", scoreset, ', "na.action")',
                          sep="")
        appendLog("Record rows omitted from predict command.", omit.cmd)
        eval(parse(text=omit.cmd))
      }
    }

    # Add the scores into the scores variable.

    if (is.null(omitted))
      scores[[mtype]] <- result
    else
      scores[[mtype]][-omitted] <- result
    
  }
  
  # Generate the other columns to be included in the score file.

  # Ensure we have all columns available in the dataset to start with,
  # so remove the " c(....)" selector from ts. We are then going to
  # include the identifiers or all columns in the output (depending on
  # the value of sinclude) so select those columns.
  
  if (length(grep(",", ts)) > 0) ts <- gsub(",.*]", ",]", ts)

  if (sinclude == "all")
    scoreset <- ts
  else if (sinclude == "idents")
    scoreset <- sprintf('subset(%s, select=c(%s))', ts,
                        ifelse(is.null(idents), "", 
                               sprintf('"%s"',
                                       paste(idents, collapse='", "'))))
  else
  {
    errorDialog("We should not be here! The value of sinclude should have",
                "been one of all or idents. We found:", sinclude,
                "\n\nPlease report this to support@togaware.com")
    return()
  }

  appendLog("Extract the relevant columns from the dataset.",
            sprintf("sdata <- %s", scoreset))
  
  sdata <- eval(parse(text=scoreset))
  
  write.csv(cbind(sdata, scores), file=fname, row.names=FALSE)

  # StatusBar is enough so don't pop up a dialog?
  # infoDialog("The scores have been saved into the file", fname)
  
  return(paste("Scores have been saved to the file", fname))
}

executeEvaluatePvOplot <- function(probcmd, testset, testname)
{
  print(probcmd)
  # This modification to executeEvaluateSave was provided by Ed Cox
  # (080201) to plot predictions vs. observed values. Graham added the
  # logging and some fine tuning. It includes a pseudo R-squared.

  # Put 1 or 2 charts onto their own plots. Otherwise, put the
  # multiple charts onto one plot, keeping them all the same size
  # (thus if numplots is odd, leave a cell of the plot empty.

  model.list <- getEvaluateModels()
  numplots <- length(model.list)

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

  for (mtype in model.list)
  {

    appendLog(sprintf(paste("%s: Generate a Predicted v Observed plot",
                            "for %s model on %s."),
                      toupper(mtype), mtype, testname),
              gsub("<<-", "<-", probcmd[[mtype]]))
    
    result <- try(eval(parse(text=probcmd[[mtype]])), silent=TRUE)

    # Check for errors - in particular, new levels in the test
    # dataset. TODO This should be factored into a separate function,
    # since it is used in a number of places, including
    # executeEvaluateSave.
    
    if (inherits(result, "try-error"))
    {
      if (any(grep("has new level", result)) || any(grep("New levels",result)))
        infoDialog("It seems that the dataset on which the probabilities",
                   "from the", mtype, "model are required has a categoric",
                   "variable with levels not found in the training",
                   "dataset. The probabilities can not be determined in",
                   "this situation. You may need to either ensure",
                   "the training dataset has representatives of all levels",
                   "or else remove them from the testing dataset.",
                   "Alternatively, do not include that variable in the",
                   "modelling. \n\n The actual error message was:\n\n",
                   paste(result, "\n"))
      else
        errorReport(probcmd, result)
      next()
    }

    # Obtain a list of the identity variables.
    
    idents <- getSelectedVariables("ident")
    
    # Transform the dataset expression into what we need to extract
    # the relevant columns.
    #
    # TODO This should be factored into a separate function, since it
    # is used in a number of places, including executeEvaluateSave.
    #
    #
    # Various formats include:
    #
    #    train	crs$dataset[crs$sample, c(3:12,14)]
    #    test	crs$dataset[-crs$sample, c(3:12,14)]
    #    csv	crs$testset[,c(3:12,14)]
    #    df	crs$testset
    #
    # Want
    #    subset(crs$dataset[-crs$sample,], select=Idents) + crs$pr
    #

    scoreset <- testset[[mtype]]

    # If no comma in scoreset, leave as is, else find first comma,
    # remove everything after, and replace with "]". PROBLEM TODO If
    # the testset[[crv$MODEL]] includes na.omit, we need to do something
    # different because after the following step of replacing the
    # column list with nothing, it is very likely that new columns are
    # included that have NAs, and hence the na.omit will remove even
    # more rows for the subset command than it does for the predict
    # command. Yet we still want to ensure we have all the appropriate
    # columns available. So use na.omit(crs$dataset[-crs$sample,
    # c(2:4,6:10,13)])@na.action to remove the rows from
    # crs$dataset[-crs$sample,] that have missing values with regard
    # the columns c(2:4,6:10,13). Thus if we have scoreset as:
    #
    #  na.omit(crs$dataset[-crs$sample, c(2:4,6:10,13)])
    #
    # we want to:
    #
    #  omitted <- na.omit(crs$dataset[-crs$sample, c(2:4,6:10,13)])@na.action
    #
    # and then scoreset should become:
    #
    #  crs$dataset[-crs$sample,][-omitted,]

    # First deal with the na.omit case, to capture the list of rows
    # omitted.
    
    # Mod by Ed Cox (080301) to fix error when no NAs in test set. The
    # error was:
    #
    #  Error in eval(expr, envir, enclos) :
    #    no slot of name "na.action" for this object of class "data.frame"
    #
    # The issue appears to be that it complains if there are no NAs to
    # remove so bypass the omission code.  This is not a general fix
    # because Ed has run into a case where it failed, but until we
    # have an insight as to what the real problem is we go with
    # this. It may be related to regression versus classification. Ed
    # was doing a regression (and testing the Pr v Ob plots).
    
    scorevarlist <- c(getSelectedVariables("ident"),
                      getSelectedVariables("target"),
                      getSelectedVariables("input"),
                      getSelectedVariables("risk"))
    tmpset <- crs$dataset[-crs$sample, scorevarlist]

    if (substr(scoreset, 1, 7) == "na.omit" &&
        !dim(tmpset)[1]==dim(na.omit(tmpset))[1])

    # End of Ed's modification.
    # if (substr(scoreset, 1, 7) == "na.omit")
    {
      omit.cmd <- paste("omitted <- attr(", scoreset, ', "na.action")', sep="")
      appendLog("Record rows omitted from predict command.", omit.cmd)
      eval(parse(text=omit.cmd))
    }
    else
      omitted <- NULL

    # Now clean out the column subsets.
    
    if (length(grep(",", scoreset)) > 0)
      scoreset <- gsub(",.*]", ",]", scoreset)

    # And finally, remove the na.omit if there is one, replacing it
    # with specifically removing just the rows that were removed in
    # the predict command.

    if (not.null(omitted))
      scoreset <- sub(")", "[-omitted,]", sub("na.omit\\(", "", scoreset))

    # Extract the actual (i.e., observed) values that are to be
    # compared to the probabilities (i.e., predictions) from the
    # model.
    
    obsset <- sprintf('subset(%s, select=crs$target)', scoreset)
    appendLog("Obtain the observed output for the dataset",
              paste("obs <-", obsset))
    obs <- eval(parse(text=obsset))

    # fitcorr is the so called psuedo-R-square. It has a maximum less
    # than 1 and is often used in either binary or multinomial
    # logistic regression. This is to be interpreted differently to
    # the standard R-square.
    
    fit.cmd <- "na.omit(cbind(obs, Predicted=crs$pr))"
    appendLog("Combine the observed values with the predicted",
              paste("fitpoints <-", fit.cmd))
    fitpoints <- eval(parse(text=fit.cmd))
    
    corr.cmd <- "format(cor(fitpoints[,1], fitpoints[,2]), digits=4)"
    appendLog("Obtain the pseudo R2 - a correlation",
              paste("fitcorr <-", corr.cmd))
    fitcorr <- eval(parse(text=corr.cmd))

    # Plot the points - observed versus predicted.
    
    # For 2 plots, so as not to overwrite the first plot, if we are
    # about to plot the second plot, initiate a new plot.
    
    if (numplots == 2 && mtype == model.list[length(model.list)]) newPlot(1)

    par.cmd <- 'par(c(lty="solid", col="blue"))'
    appendLog("Plot settings for the true points and best fit",
              paste("op <-", par.cmd))
    op <- eval(parse(text=par.cmd))

    # In the plot I originally limited the x and y to (0,1). Not sure
    # why needed. Ed Cox pointed out he was losing values when
    # predicting more than (0,1) (linear regression), so remove the limits 
    # for now (080301).

    vnames <- names(fitpoints)
    plot.cmd <-sprintf('plot(%s, fitpoints[[2]], asp=1, xlab="%s", ylab="%s")',
                       ifelse(length(unique(fitpoints[[1]])) < 5,
                              "jitter(fitpoints[[1]])",
                              "fitpoints[[1]]"),
                       ifelse(length(unique(fitpoints[[1]])) < 5,
                              paste(vnames[1], "(Jittered)"),
                              vnames[1]),
                       vnames[2])
    appendLog("Display the observed (X) versus predicted (Y) points",
              plot.cmd)
    eval(parse(text=plot.cmd))

    # Fit a linear model Predicted ~ Observed.

    lm.cmd <- paste("lm(fitpoints[,2] ~ fitpoints[,1])")
    appendLog("Generate a simple linear fit between predicted and observed",
              paste("prline <-", lm.cmd))
    prline <- eval(parse(text=lm.cmd))

    ab.cmd <- "abline(prline)"
    appendLog("Add the linear fit to the plot",
              ab.cmd)
    eval(parse(text=ab.cmd))

    diag.cmd <- paste('par(c(lty="dashed", col="black"))',
                      'abline(0, 1)', sep="\n")
    appendLog("Add a diagonal representing perfect correlation",
              diag.cmd)
    eval(parse(text=diag.cmd))

    legend("topleft", legend=c("Linear Fit to Points",
                        "Predicted=Observed"),
           lty=c(1, 2),col=c("blue", "black"), bty="n")
    
    par(op)

    # TODO Add to LOG

    # Add decorations
    
    decorcmd <- paste(genPlotTitleCmd("Predicted vs. Observed", mtype,
                                      testname, "\n", "Pseudo R-square=",
                                      fitcorr),
                    '\ngrid()', sep="")
    appendLog("Add decorations to the plot.", decorcmd)
    eval(parse(text=decorcmd))

  }
  return("Pr v Ob plot generated.")
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
    setCopyright(paste(VERSION.DATE, "\n\n", COPYRIGHT))
  gladeXMLSignalAutoconnect(about)
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
  crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                              crv$NOTEBOOK.DATA.NAME))
  switchToPage(crv$NOTEBOOK.DATA.NAME)
}

on_tools_variables_activate <- function(action, window)
{
  crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                              crv$NOTEBOOK.SELECT.NAME))
  switchToPage(crv$NOTEBOOK.SELECT.NAME)
}

on_tools_transform_activate <- function(action, window)
{
  crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                              crv$NOTEBOOK.TRANSFORM.NAME))
  switchToPage(crv$NOTEBOOK.TRANSFORM.NAME)
}

on_tools_explore_activate <- function(action, window)
{
  crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                              crv$NOTEBOOK.EXPLORE.NAME))
  switchToPage(crv$NOTEBOOK.EXPLORE.NAME)
}

on_tools_cluster_activate <- function(action, window)
{
  crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                              crv$NOTEBOOK.CLUSTER.NAME))
  switchToPage(crv$NOTEBOOK.CLUSTER.NAME)
}

on_tools_model_activate <- function(action, window)
{
  crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                              crv$NOTEBOOK.MODEL.NAME))
  switchToPage(crv$NOTEBOOK.MODEL.NAME)
}

on_tools_evaluate_activate <- function(action, window)
{
  crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                              crv$NOTEBOOK.EVALUATE.NAME))
  switchToPage(crv$NOTEBOOK.EVALUATE.NAME)
}

on_tools_log_activate <- function(action, window)
{
  crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                              crv$NOTEBOOK.LOG.NAME))
  switchToPage(crv$NOTEBOOK.LOG.NAME)
}

switchToPage <- function(page)
{

  # Blank the status bar whenever we change pages
  
  setStatusBar()

  # This function used to accept numeric pages, so check for that and
  # convert to the page name rather than the now changing page number
  # (page numbers used to be fixed).

  if (is.numeric(page))
    page <- crv$NOTEBOOK$getTabLabelText(crv$NOTEBOOK$getNthPage(page))

  if (page == crv$NOTEBOOK.EVALUATE.NAME)
  {
    
    # On moving to the EVALUATE page, ensure each built model's
    # checkbox is active, and check the active model's checkbox, but
    # leave all the other as they are.

    mtypes <- listBuiltModels()

    if (not.null(mtypes))
    {
      # We have some models, so make sure their checkboxes are
      # sensitive.

      lapply(mtypes,
             function(x) theWidget(paste(x, "_evaluate_checkbutton",
                                            sep=""))$setSensitive(TRUE))
      
      if (is.null(crs$page) || crs$page == crv$NOTEBOOK.MODEL.NAME)
      {
        # By default check the current model's check button if we
        # have just come from the MODEL page. This makes it easy when
        # swaping from the Model page to this page to evaluate the
        # just built model (usually). The NULL test on crs$page
        # simply covers the loading of a project that did not save
        # the crs$page, as was the case for old project files.
        if (currentModelTab() %in% mtypes)
          theWidget(paste(currentModelTab(), "_evaluate_checkbutton",
                          sep=""))$setActive(TRUE)
      }
    }
  }


  # When changing to the LOG page desensitise the Execute button. Not
  # sure why anyone would push the execute button anyhow, so maybe
  # this is just better to result in an errorDialog rather than extra
  # logic here to greyt out the button?
  
  if (page == crv$NOTEBOOK.LOG.NAME)
  {
    theWidget("execute_button")$setSensitive(FALSE)
    theWidget("execute_menu")$setSensitive(FALSE)
  }
  else
  {
    theWidget("execute_button")$setSensitive(TRUE)
    theWidget("execute_menu")$setSensitive(TRUE)
  }
    
  # Record the current page so when we change we know which was last.

  crs$page <<- page

}

########################################################################
#
# HELP

popupTextviewHelpWindow <- function(topic)
{
  collectOutput(sprintf("help(%s, htmlhelp=TRUE)", topic), TRUE)
}

showHelpPlus <- function(msg)
{
  if (! questionDialog(paste(gsub(" <<>> ", "\n\n",
                                  gsub("\n", " ", msg)),
                             "Would you like to view the R help?",
                             sep="\n\n")))
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
  showHelp(paste(ifelse(crv$appname=="RStat",
                  paste("RStat is the WebFOCUS data mining application",
                        "developed by Information Builders on top of",
                        "Rattle and R. "),
                  ""),
           "Rattle is a graphical user interface for data mining
written in GNOME and R. R is an environment for statistical computing.
They are all free software licensed under the GNU General
Public License (GPL).
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
mulitnomial classification and regression tasks. but is become stable
in those areas also, over time.
<<>>
The most we can guarantee about this
code is that there are bugs! When you find one, or a misfeature or
something else you would like Rattle to do, please do email
support@togaware.com.
<<>>
Enjoy.", sep=""))
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
either of which may be categoric or numeric.
<<>>
dataset = A collection of data.
<<>>
entity = An object of interest, descibed by variables.
Also called a record, object, row or observation.
<<>>
variable = The data items used to describe an enitity.
Also called an attribute, feature or column.
<<>>
input variable = A measured or preset data item.
Also called predictor, independent variable, observed variable,
or descriptive variable.
<<>>
output variable = A variable possibly influenced by the input variables.
Also called response or dependent variable.
<<>>
categoric variable = A variable that takes on a value from a fixed
set of values. In R these are called factors and the possible values
are refered to as the levels of the factor.
<<>>
numeric variable = A variable that has values that are integers or real
numbers.")
}

on_help_csv_activate <- function(action, window)
{
  if (showHelpPlus("Data can be loaded from
a comma separated value (CSV) file, as might be generated
by spreadsheets and databases,
including Excel, Gnumeric, SAS/EM, QueryMan, and many other applications.
This is a good option for importing data.
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
A URL can be supplied in the Location: text box so that a CSV file can be
loaded from the network.
<<>>
The corresponding R code uses the simple read.csv() function."))
    popupTextviewHelpWindow("read.csv") }

on_help_arff_activate <- function(action, window)
{
  if (showHelpPlus("Data can be loaded from
an Attribute-Relation File Format, or ARFF, file
(beginning with version 2.5.0 of R).
ARFF is an ASCII text file format
that is essentially a CSV file with a header that describes the
meta-data. ARFF was developed for use in the Weka machine learning
software and there are quite a few datasets in this format now.
<<>>
The corresponding R code uses the read.arff() function from the
foreign package."))
  {
    require(foreign, quietly=TRUE)
    popupTextviewHelpWindow("read.arff")
  }
}

on_help_rdata_file_activate <- function(action, window)
{
  showHelp("Choose this if you have data stored in an R dataset
(usually with a filename extension of .Rdata).
The named file will be loaded and any data frames found in there will
be listed for selection.")
}

on_help_rdataset_activate <- function(action, window)
{
  showHelp("Datasets already loaded into R can be used
(although a copy is taken, with memory implications).
Only data frames are currently supported, and 
the names of all of the available data frames will be lsited.
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
  showHelp("The Data tab allows you to select roles for the
variables.
<<>>
By default, all variables have an Input role, except for any variables
that have constant value, or categorics with as many values as there
are rows (identifier variables). These will be marked as Ignore.
<<>>
One variable may also be identified as the Target (the first or last
categoric by default).
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

on_help_normalise_activate <- function(action, window)
{
  if (showHelpPlus("Rescaling options transforms a variable by remapping its
values to another set of values, such as a set that has a mean of 0 and
standard deviation of 1. Often we do this so that all of our variables
have a very similar spread, and perhaps distribution. This can then avoid
biases in various algorithms, such as in clustering where a distance measure
is often used.
<<>>
Various rescalings are supported, with the rescaler function from the reshape
package used in a number of cases.
<<>>
The Nolan Transform segments and remaps a numeric variable to the 0-100 range.
"))
  {
    if (packageIsAvailable("reshape", "display information about rescaler"))
      popupTextviewHelpWindow("rescaler")
  }
}

on_help_impute_activate <- function(action, window)
{
  showHelp("Imputation is used to fill in the missing values in the data.
The Zero/Missing imputation is a very simple method. Any missing numeric data
is simply assigned 0 and any missing categoric data is put into a new
category, Missing. Mean, Median and Mode replace missing values with the population
mean, median, or mode. This is not recommended.")
}

on_help_nolan_activate <- function(action, window)
{
  if (showHelpPlus("The Nolan Groups transformation segments the selected numeric variables
by a selected categoric variable, and then within each segment rescales the numeric
variable's range to the 0-100 range, using the range option of the rescale(rehsape)
function. This transform was proposed by Anthony Nolan."))
  {
    if (packageIsAvailable("reshape", "display information about rescaler"))
      popupTextviewHelpWindow("rescaler")
  }
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
For categoric data the frequency distribution across the values is listed.
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
      if (packageIsAvailable("Hmisc", "display help about describe"))
      {
        require(Hmisc, quietly=TRUE)
        popupTextviewHelpWindow("describe")
      }
      if (packageIsAvailable("fBasics", "display help about basic stats"))
      {
        require(fBasics, quietly=TRUE)
        popupTextviewHelpWindow("basicStats")
      }
    }
}

on_help_distributions_activate <- function(action, window)
{
  if (showHelpPlus( "Choose from various plot types to display
information about the distributions of data."))
    popupTextviewHelpWindow("boxplot")
}

on_help_ggobi_activate <- function(action, window)
{
  if (showHelpPlus( "Run the GGobi application to visually explore
your data. GGobi is a very powerful interactive visualiser.
You will need to have the separate GGobi application installed,
as well as the rggobi R package."))
    popupTextviewHelpWindow("ggobi")
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

on_help_log_activate <- function(action, window)
{
  showHelp("The Log tab records the underlying commands that
Rattle generates and passes over to R to execute.
You can save the Log commands to file to run at a later stage,
or you can paste the commands into the current R console to do
more than Rattle can support.")
}
