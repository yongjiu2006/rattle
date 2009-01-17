# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2009-01-15 15:21:34 Graham Williams>
#
# Copyright (c) 2009 Togaware Pty Ltd
#
# The Rattle package is made of the following R source files:
#
# cluster.R	KMeans and Hierachical Clustering.
# data.R	Handle Data management tasks.
# execute.R	The Execute functionality.
#

MAJOR <- "2"
MINOR <- "4"
REVISION <- unlist(strsplit("$Revision$", split=" "))[2]
VERSION <- paste(MAJOR, MINOR, REVISION, sep=".")
VERSION.DATE <- "Released 17 Jan 2009"
COPYRIGHT <- "Copyright (C) 2009 Togaware Pty Ltd"

SUPPORT <- "Contact support@togaware.com."

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

isRStat <- function() 
{
  return(exists("crv") && crv$appname == "RStat")
}

isRattle <- function() 
{
  return(crv$appname == "Rattle")
}

RStat <- function(csvname=NULL, ...) 
{
  rattle(csvname, appname="RStat", ...)
}

rattle <- function(csvname=NULL,
                   appname="Rattle",
                   tooltiphack=FALSE,
                   close="close")
{
  # If "tooltiphack" is TRUE then gtkMain is called on focus, blocking
  # the R console, but at least tooltips work, and on losing focus
  # gtkMainQuit is called, and thus the console is no longer blocked!
  # A bit ugly, but seems to work. This was suggested by Felix Andrew,
  # 080705. I notice that to load the supplied audit dataset I need to
  # change focus out of Rattle.

  # 080906 If close="quit" then when the window close is pressed, we
  # also quit R.
  
  # [080319 gjw] Create GLOBAL to avoid many "no visible binding" from
  # "R CMD check" by adding all hidden variables to it. Previously
  # they all began with "." as in crv$ADA used to be .ADA. "R CMD
  # check" complained a lot, once for each of these, so putting them
  # all into crv means only one complaint each time!

  if (TRUE) # 080907 Experiment with new.env()
    crv <<- new.env()
  else
    crv <<- list()
  crv$appname <<- appname
  crv$tooltiphack <<- tooltiphack # Record the value globally
  crv$.gtkMain <<- FALSE # Initially gtkMain is not running.
  crv$close <<- close
  crv$verbose <<- TRUE
  
  # Some global constants

  crv$max.vars.correlation <<- 40

  # Load gloablly required packages.
  
  if (! packageIsAvailable("RGtk2", "display the Rattle GUI"))
    stop("RGtk2 package is not available but is required for the GUI.")

  if (packageIsAvailable("vcd"))
  {
    # 080921 Load each individually so we can keep the loading quiet!
    
    require("MASS", quietly=TRUE)
    require("grid", quietly=TRUE)
    require("colorspace", quietly=TRUE)
    require("vcd", quietly=TRUE)
  }
  
  require(RGtk2, quietly=TRUE) # From http://www.ggobi.org/rgtk2/

  # Check to make sure libglade is available.

  if (! exists("gladeXMLNew"))
    stop("The RGtk2 package did not find libglade installed. ",
         "Please install it.")

  # On the Macintosh we seem to need to initialise all of the types
  # for the GTK widgets. So do that here.

  if (Sys.info()["sysname"] == "Darwin")
  {
    # Use the following to extract all widgets from the glade file:
    #
    # $ grep '<widget' rattle.glade | sed 's|^.*widget class="||' |\
    #   sed 's|".*$||' | sort -u | sed 's|^Gtk|gtk|' |\
    #   awk '{printf("%sGetType()\n", $1)}'
    
    gtkAboutDialogGetType()
    gtkAlignmentGetType()
    gtkButtonGetType()
    gtkCheckButtonGetType()
    gtkCheckMenuItemGetType()
    gtkComboBoxEntryGetType()
    gtkComboBoxGetType()
    gtkDrawingAreaGetType()
    gtkEntryGetType()
    gtkFileChooserButtonGetType()
    gtkFileChooserDialogGetType()
    gtkHBoxGetType()
    gtkHButtonBoxGetType()
    gtkHSeparatorGetType()
    gtkHandleBoxGetType()
    gtkImageGetType()
    gtkImageMenuItemGetType()
    gtkLabelGetType()
    gtkMenuBarGetType()
    gtkMenuGetType()
    gtkMenuItemGetType()
    gtkMiscGetType()
    gtkNotebookGetType()
    gtkRadioButtonGetType()
    gtkScrolledWindowGetType()
    gtkSeparatorMenuItemGetType()
    gtkSeparatorToolItemGetType()
    gtkSpinButtonGetType()
    gtkStatusbarGetType()
    gtkTableGetType()
    gtkTextViewGetType()
    gtkToolButtonGetType()
    gtkToolItemGetType()
    gtkToolbarGetType()
    gtkTreeViewGetType()
    gtkVBoxGetType()
    gtkVSeparatorGetType()
    gtkWidgetGetType()
    gtkWindowGetType()
  }
  
  # Ensure the About dialog will respond to the Quit button.
  
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

  # Really need an second untouched rattleGUI
  
  Global_rattleGUI <<-rattleGUI
    
  # Tune the interface to suit RStat

  setRattleTitle()

  if (isRStat())
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

  # 080924 Load of a supplied data file occurs here, but may take time
  # and whilst the UI is not fully set up yet, we see the Welcome
  # screen in Rattle displayed in RStat for 30 seconds or so. So
  # perhaps move it to later in the process.
  
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
    else
    {
      # 081020 gjw If the csvname is supplied then prefix it with
      # file:/// to make it conform to the filename obtained from the
      # file chooser button. Without doing this crs$dwd does not
      # include file:/// and when compared in changedDataTab to the
      # filename obtained with getUri they don't match, and hence the
      # data is reloaded! Take care of MS/Windows where the csvname
      # will be prefixed by the drive, so we add three slashes in
      # front.

      if (isWindows())
        csvname <- paste("file:///", csvname, sep="")
      else
        csvname <- paste("file://", csvname, sep="")
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
  # but not all are done yet. 081227 Also note that kmeans, hclust and
  # apriori will also be migrating into being treated as first class
  # models.

  crv$KMEANS <<- "kmeans"
  crv$HCLUST <<- "hclust"
  crv$APRIORI <<- "apriori"
  
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

  if (TRUE)
    crs <<- new.env()
  else
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
               apriori=NULL,
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

  crv$NOTEBOOK.TEST.NAME <<- "Test"

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

  # 080921 Define the DATA tab pages

  .DATA.NOTEBOOK 	<<- theWidget("data_notebook")
  .DATA.CORPUS.TAB      <<- getNotebookPage(.DATA.NOTEBOOK, "corpus")
  .DATA.CSV.TAB         <<- getNotebookPage(.DATA.NOTEBOOK, "csv")

  .DATA.DISPLAY.NOTEBOOK       <<- theWidget("data_display_notebook")
  .DATA.DISPLAY.TREEVIEW.TAB   <<- getNotebookPage(.DATA.DISPLAY.NOTEBOOK, "treeview")
  .DATA.DISPLAY.WELCOME.TAB    <<- getNotebookPage(.DATA.DISPLAY.NOTEBOOK, "welcome")

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
  .EXPLORE.PLAYWITH.TAB    <<- getNotebookPage(.EXPLORE, "playwith")
  
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
  
  # Turn off the sub-notebook tabs.

  # Sys.sleep(5) 080924 to test delays....
  
  .DATA.NOTEBOOK$setShowTabs(FALSE)
  .DATA.DISPLAY.NOTEBOOK$setShowTabs(FALSE)
  .EXPLORE$setShowTabs(FALSE)
  crv$TRANSFORM$setShowTabs(FALSE)
  .CLUSTER$setShowTabs(FALSE)
  crv$MODEL$setShowTabs(FALSE)
  .EVALUATE$setShowTabs(FALSE)

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

  if (isRattle())
  {
    .DATA.DISPLAY.NOTEBOOK$setCurrentPage(.DATA.DISPLAY.WELCOME.TAB)
    resetTextview("rattle_welcome_textview",
                  paste("Welcome to Rattle (rattle.togaware.com).\n",
                        "\nRattle is a free graphical user",
                        "interface for Data Mining, developed using R.",
                        "R is a free software environment",
                        "for statistical computing and graphics.",
                        "Together they provide a sophisticated",
                        "environments for data mining,",
                        "statistical analyses, and data visualisation.",
                        "\n\nSee the Help menu for extensive support in",
                        "using Rattle.",
                        "The Togaware Desktop Data Mining Survival Guide",
                        "includes Rattle documentation",
                        "and is available from",
                        "datamining.togaware.com",
                        "\n\nRattle is licensed under the",
                        "GNU General Public License, Version 2.",
                        "Rattle comes with ABSOLUTELY NO WARRANTY.",
                        "See Help -> About for details.",
                        "\n\nRattle version", VERSION,
                        "Copyright (C) 2009 Togaware Pty Ltd"),
                  tvsep=FALSE)
  }
  
## PUT THE MAIN TEXT HERE INTO THE ABOUT.
##
##   if (isRattle())
##   {
##     resetTextview("data_textview", "Welcome to Rattle.\n\n", tvsep=FALSE)
##     resetTextview("log_textview", "# Rattle Log File.\n\n", tvsep=FALSE)
##   }
##   else if (isRStat())
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
##                        "\nCopyright (C) 2009 Togaware Pty Ltd"),
##                  tvsep=FALSE)
  appendTextview("log_textview",
                 paste("# Rattle is Copyright (C) 2009",
                       "Togaware Pty Ltd"),
                 tvsep=FALSE)

  initiateLog()
  
  # Make sure the text is shown on startup.
  
  while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
  
  # Now deal with any arguments to rattle.

  if (not.null(csvname))
  {
    if (!theWidget("data_filechooserbutton")$setUri(csvname))
      infoDialog("Internal Error: The setting of the filename box",
                 "failed.", SUPPORT)
    # Make sure GUI updates
    while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
    executeDataTab(csvname)
  }

###   # Tune the interface to suit RStat

###   if (isRStat())
###     tuneRStat()
###   else
###     tuneOthers()
  
  ## theWidget("csv_filechooserbutton")$setFilename("audi.csv")

  invisible()
}

tuneRStat <- function()
{
  # Tune the user interface to suit the requirements for RStat. Often,
  # we have added functionality to Rattle that is not yet we tested
  # and tuned to for release as RStat.

  SUPPORT <- "Contact Information Builders Technical Support."
  
  ## Toolbar
  
  theWidget("report_toolbutton")$hide()
  theWidget("rattle_menu")$hide()

  ## Data
  
  # Data -> R Dataset

  theWidget("data_rdataset_radiobutton")$hide()
  theWidget("data_corpus_radiobutton")$hide()
  
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
  theWidget("model_tree_include_missing_checkbutton")$hide()
 
  # Model -> Linear

  # 080815 I've moved to using the "Linear" label for the lm/glm
  # family. Regression is perhaps a more general term. I've not
  # approval for this from IBI so retaining Regression there for now.
  theWidget("model_linear_radiobutton")$setLabel("Regression")
  theWidget("glm_linear_radiobutton")$setLabel("Linear")
  theWidget("model_linear_probit_radiobutton")$hide()
  theWidget("model_linear_plot_button")$hide()
  
  # Model -> All

  theWidget("all_models_radiobutton")$hide()

  # Evaluate -> Linear

  theWidget("glm_evaluate_checkbutton")$setLabel("Regression")
  
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

write.rstat <- function(x, file="", ...)
{
  # 081026 The RStat standard for missing values uses "." for
  # categorics and "" for numeric. The simplest approach to handling
  # this is to add "." as a new level to categoric variables to
  # replace the missing values, then to tell write.csv to use "" for
  # missing, which then should only be numeric variables.

  # 081101 IBI request "." for all missing!
  
###   # Replace missing with "." in each categoric.

###   factors <- which(sapply(1:ncol(x), function(y) is.factor(x[,y])))
###   x[,factors] <- sapply(factors,
###                          function(y)
###                          {
###                            levels(x[,y]) <- c(levels(x[,y]), ".")
###                            x[,y][is.na(x[,y])] <- "."
###                            x[,y]
###                          })

###   # Write to file with missing as "" for the remaining numerics
  
  write.csv(x, file=file, row.names=FALSE, na=".", ...)
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
  crs$apriori  <<- NULL
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

    # Reset Cluster

    theWidget("kmeans_clusters_spinbutton")$setValue(10)
    theWidget("kmeans_seed_spinbutton")$setValue(123)
    theWidget("kmeans_runs_spinbutton")$setValue(1)
    theWidget("kmeans_stats_button")$setSensitive(FALSE)
    theWidget("kmeans_data_plot_button")$setSensitive(FALSE)
    theWidget("kmeans_discriminant_plot_button")$setSensitive(FALSE)

    theWidget("hclust_clusters_spinbutton")$setValue(10)
    theWidget("hclust_nbproc_spinbutton")$setValue(1)
    theWidget("hclust_dendrogram_button")$setSensitive(FALSE)
    theWidget("hclust_stats_button")$setSensitive(FALSE)
    theWidget("hclust_data_plot_button")$setSensitive(FALSE)
    theWidget("hclust_discriminant_plot_button")$setSensitive(FALSE)

    
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
  
  errorDialog("An error occured with", cmd, SUPPORT, "\n\n",
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
  if (isRStat())
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
  rattleGUI <- Global_rattleGUI # Global - to avoid a "NOTE" from "R CMD check"
  
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

listBuiltModels <- function(exclude=NULL)
{
  # Build a list of models that have been built. 
  models <- c()
  for (m in setdiff(c(crv$MODELLERS, crv$KMEANS, crv$HCLUST, crv$APRIORI),
                    exclude))
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
  setStatusBar(sprintf("Plot %d copied to the clipboard as a PNG.", dev.num))
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
  setStatusBar(sprintf("Plot %d sent to printer: %s", dev.num,
                       options("printcmd")))
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
  # GtkClipboardSetImage. I've not figured out yet how to get the
  # image directly from the Cairo device as a GdkPixbuf. So instead I
  # save to PNG file then load that file as a GdkPixmap then copy that
  # to the clipboard.
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
               "Please Execute (F2) to obtain a plot to export.")
    return()
  }

  # Obtain a filename to save to. Ideally, this would also prompt for
  # the device to export, and the fontsize, etc.

  dialog <- gtkFileChooserDialog(paste("Export Graphics (.pdf, .png, .jpg, .svg",
                                       ifelse(isWindows(), ", wmf", ""),
                                       ")", sep=""),
                                 NULL, "save",
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-save", GtkResponseType["accept"])
  dialog$setDoOverwriteConfirmation(TRUE)
  
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
  
#  if (get.extension(save.name) == "")
#    save.name <- sprintf("%s.pdf", save.name)
  
  startLog("SAVE PLOT")
  appendLog(paste("Save the plot on device", dev.num, "to file."),
            sprintf('savePlotToFile("%s", %s)', save.name, dev.num))
  
  if (savePlotToFile(save.name, dev.num))
    setStatusBar("Plot", dev.num, "exported to", save.name)
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
    crv$verbose <- TRUE
  }
  
  main = paste(...)
  if(vector)
  {
    if (isRStat() || ! crv$verbose)
      sub <- ""
    else
      sub <- sprintf("%s %s %s", crv$appname,
                     format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"])
    return(c(main, sub))
  }
  else
  {  
    if (isRStat() || ! crv$verbose)
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
  for (i in seq_along(heat)) flow.col[flow.col==i] <- heat[i]
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

  # 080906 Deal with R not finishing up when rattle is called from
  # littler or R CMD BATCH and we close rather than quit.

  if (crv$close == "quit") quit(save="no")
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
    crv$close <- "quit"
    close_rattle(action, window)
  }
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

on_playwith_radiobutton_toggled <- function(button)
{
  if (button$getActive()) .EXPLORE$setCurrentPage(.EXPLORE.PLAYWITH.TAB)
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

  # Make sure something has been selected.
  
  if (! (do.summary || do.describe || do.basics ||
         do.kurtosis || do.skewness || do.missing))
  {
    infoDialog("No summary type has been selected.",
               "Please choose at least one to get some output.")
    return()
  }
    
  # Other useful information:
  #   is there a sample
  #   list of numeric variables
  
  sampling  <- not.null(crs$sample)

  numeric.cmd <- sprintf(paste("seq(1,ncol(%s))",
                               "[as.logical(sapply(%s, is.numeric))]",
                               sep=""), dataset, dataset)
  nvars <- simplifyNumberList(eval(parse(text=numeric.cmd)))

  # Start the trace to the log.
  
  startLog()
  theWidget(TV)$setWrapMode("none")
  resetTextview(TV)

  # Construct and execute the requested commands.

  if (do.summary)
  {
    # Find the number of entities with any missing value for the
    # non-ignored variables.
    
    missing.cmd <- sprintf('length(attr((na.omit(%s)), "na.action"))', dataset)
    result <- try(missing <- eval(parse(text=missing.cmd)), silent=TRUE)
    if (inherits(result, "try-error")) missing <- 0
    
    # Use Hmisc's contents to summarise the data frame, if Hmisc is
    # available.

    contents.cmd <- ""
    if (packageIsAvailable("Hmisc", "describe the contents of a data frame"))
    {
      lib.cmd <- "require(Hmisc, quietly=TRUE)"
      appendLog("The contents command comes from Hmisc.", lib.cmd)
      eval(parse(text=lib.cmd))
      contents.cmd <- sprintf("contents(%s)", dataset)
    }
    summary.cmd <- sprintf("summary(%s)", dataset)
    
    appendLog("SUMMARISE THE DATASET", contents.cmd, summary.cmd)
    appendTextview(TV,
                   paste("Below is a summary of ",
                         ifelse(use.sample && sampling, "a SAMPLE of ", ""),
                         "the dataset.\n\n", sep=""),
                   "In reading the simple distribution tables the 1st and 3rd Qu.\n",
                   "refer to the first and third quartiles, indicating that 25% of\n",
                   "the entities have values of that variable which are less than\n",
                   "or greater than (respectively) the value listed.\n\n",
                   if (missing > 0)
                   paste("We also note that the data contains", missing, "entities",
                         "with missing values.\nCheck",
                         "the Show Missing check box for details.\n\n"),
                   collectOutput(contents.cmd),
                   "\n\n",
                   collectOutput(summary.cmd))
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

  for (i in seq_along(targets))
  {
    bind.cmd <- sprintf("%s,\n            data.frame(dat=%s",
                        bind.cmd, dataset)
    
    bind.cmd <- sprintf('%s[crs$dataset%s$%s=="%s","%%s"], grp="%s")',
                        bind.cmd,
                        ifelse(sampling, "[crs$sample,]", ""),
                        target, targets[i], targets[i])
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
  for (i in seq_along(targets))
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

    # 081122 TODO ggplot2 will simplify this substantially:
    #
    # p <- ggplot(crs$dataset, aes(factor(TARGET_Adjusted), Age))
    # p + geom_boxplot(aes(fill=factor(TARGET_Adjusted)))
    #
    # Thus, no deriving the ds dataset, and no playing with colours.

#    if (crv$appname == "Rattle")
#    {
#      lib.cmd <- "require(ggplot2, quietly=TRUE)"
#      for (s in 1:nboxplots)
#      {
#        plot.cmd <- sprintf(paste("p <- ggplot(crs$dataset,",
#                                  "aes(factor(TARGET_Adjusted), %s))\n",
#                                  "p + geom_boxplot(aes(fill=",
#                                  "factor(TARGET_Adjusted)))"),
#                            boxplots[s])
#        eval(parse(text=lib.cmd))
#        print(eval(parse(text=plot.cmd))) # Very slow and not in Cairo device!!!!
#      }
#    }
#    else
#    {
    
    # 080918 Use the vcd package to get a better colour map. See
    # http://epub.wu-wien.ac.at/dyn/virlib/wp/eng/showentry?ID=epub-wu-01_c87
    
    if (packageIsAvailable("vcd"))
      cols <- "col=rainbow_hcl(%d, start = 270, end = 150),"
    else
      cols <- "col=rainbow(%d),"
    
    plot.cmd <- paste('bp <<- boxplot(dat ~ grp, ds,',
                     sprintf(cols, length(targets)+1),
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
    
    for (s in seq_len(nboxplots))
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
#  }
  

  ##--------------------------------------------------------------------
  
  if (nhisplots > 0)
  {
    # Plot a histogram for numeric data.

    if (packageIsAvailable("vcd"))
      cols <- "col=rainbow_hcl(%s, start = 270, end = 150)"
    else
      cols <- "col=rainbow(%s)"

    plot.cmd <- paste('hs <- hist(ds[ds$grp=="All",1], main="", xlab="", ',
                      cols, ', breaks="scott", border=FALSE)\n',
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

    if (packageIsAvailable("vcd"))
      cols <- "col=rainbow_hcl(30, start = 270, end = 150)"
    else
      cols <- "col=rainbow(30)"

    altplot.cmd <- paste('plot(as.factor(round(ds[ds$grp=="All",1], ',
                         'digits=2)), ', cols, ')\n',
                         #'dens <- density(ds[ds$grp=="All",1], na.rm=TRUE)\n',
                         #'rs<- max(summary(as.factor(round(ds[ds$grp=="All",',
                         #'1], digits=2))))/max(dens$y)\n',
                         #'lines(dens$x, dens$y*rs, type="l")',
                         sep="")

    for (s in seq_len(nhisplots))
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

      # 080925 Determine the likely number of bars for the plot. This
      # does not always seem to get it correct.
      
      nbars <- nclass.scott(na.omit(ds[ds$grp=="All",1]))

      if (length(dsuni) <= 20 && dsmax - dsmin <= 20)
      {
        appendLog("Plot the data.", altplot.cmd)
        eval(parse(text=altplot.cmd))
      }
      else
      {
        plot.cmd <- sprintf(plot.cmd, nbars)
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
    
    for (s in seq_len(nplots))
    {
      startLog()

      if (packageIsAvailable("vcd"))
        col <- rainbow_hcl(length(targets)+1, start = 30, end = 300)
      else
        col <- rainbow(length(targets)+1)
      
      plot.cmd <- paste('Ecdf(ds[ds$grp=="All",1],',
                       sprintf('col="%s",', col[1]),
                       'xlab="",',
                       'subtitles=FALSE)\n')
      if (not.null(targets))
      for (t in seq_len(targets))
      {
        plot.cmd <- paste(plot.cmd,
                         sprintf('Ecdf(ds[ds$grp=="%s",1], ', targets[t]),
                         sprintf('col="%s", lty=%d, ', col[t+1], t+1),
                         'xlab="", subtitles=FALSE, add=TRUE)\n',
                         sep="")
      }

    if (packageIsAvailable("vcd"))
      cols <- "col=rainbow_hcl(%d, start = 30, end = 300)"
    else
      cols <- "col=rainbow(%d)"

      if (not.null(targets))
        legend.cmd <- sprintf(paste('legend("bottomright", c(%s), ',
                                   cols, ", lty=1:%d,",
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
                       'xlab="Distribution of the ',
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
        for (i in 1:seq_along(targets))
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
        for (s in seq_len(nbenplots))
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
        
        for (s in seq_len(nbenplots))
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
            for (t in seq_along(targets))
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
    ## Plot a frequency plot for a categoric variable.

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
      startLog()
      appendLog("Use barplot2 from gplots for the barchart.", lib.cmd)
      eval(parse(text=lib.cmd))

      for (s in seq_len(nbarplots))
      {

        startLog()

        ## Construct and evaluate a command string to generate the
        ## data for the plot.

        ds.cmd <- paste(sprintf("sprintf('%s',", generic.data.cmd),
                       paste(paste('"', rep(barplots[s], length(targets)+1),
                                 '"', sep=""), collapse=","), ")")
        ds.cmd <- eval(parse(text=ds.cmd))
        appendLog("Generate the summary data for plotting.",
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
        appendLog("Sort the entries.", paste("ord <-", ord.cmd))
        ord <- eval(parse(text=ord.cmd))

        cols <- sprintf(ifelse(packageIsAvailable("vcd"),
                               "rainbow_hcl(%s, start = 270, end = 150)",
                               "rainbow(%s)"),
                       length(targets)+1) 
        
        maxFreq <- max(ds)
        plot.cmd <- sprintf(paste('barplot2(ds[,ord], beside=TRUE,',
                                 'ylim=c(0, %d), col=%s)'),
                           round(maxFreq+maxFreq*0.20), cols)
        appendLog("Plot the data.", paste("bp <- ", plot.cmd))
        bp <- eval(parse(text=plot.cmd))

        ## Construct and evaluate a command to add text to the top of
        ## the bars in the bar chart. Only do this if there are not
        ## too many values for the category, otherwise the numbers
        ## look bad. I could, alternatively, scale the font?

        if (ncol(bp) <= 5)
        {
          text.cmd <- sprintf("text(bp, ds[,ord]+%d, ds[,ord])",
                             round(maxFreq*0.040))
          appendLog("Add the actual frequencies.", text.cmd)
          eval(parse(text=text.cmd))
        }

        ## Construct and evaluate a command to add a legend to the
        ## plot, but only if there is a target, optherwise it is
        ## obvious.
        
        if (not.null(targets))
        {
          legend.cmd <- sprintf(paste('legend("topright", c(%s), ',
                                     "fill=%s, ",
                                     'title="%s")'),
                               paste(sprintf('"%s"', c("All", targets)),
                                     collapse=","),
                               cols,
                               target)
          appendLog("Add a legend to the plot.", legend.cmd)
          eval(parse(text=legend.cmd))
        }
        
        ## Construct and evaluate a command to add the title to the
        ## plot.
        
        title.cmd <- genPlotTitleCmd(sprintf("Distribution of %s%s",
                                            barplots[s],
                                            ifelse(sampling," (sample)","")))
        appendLog("Add a title to the plot.", title.cmd)
        eval(parse(text=title.cmd))
      }
    }
  }

### REMOVE 080925 - Until work out multiple plots on one device issue.
###   if (nbarplots > 0)
###   {
###     # Plot a frequency plot for a categoric variable.

###     # 080817 Use barchart from lattice instead of barplot2 from
###     # ggplots.

###     lib.cmd <- "require(lattice, quietly=TRUE)"
    
###     # Construct a generic data command built using the genericDataSet
###     # values. To generate a barplot we use the output of the summary
###     # command on each element in the genericDataSet, and bring them
###     # together into a single structure. The resulting generic.data.cmd
###     # will have a number of "%s"s (one for the whole dataset, then one
###     # for each level) from the original genericDataSet string that
###     # will be replaced with the name of each variable as it is being
###     # plotted.

###     generic.data.cmd <- paste(lapply(genericDataSet,
###                                      function(x) sprintf("summary(%s)", x)),
###                               collapse=",\n    ")
###     generic.data.cmd <- sprintf("cbind(%s)", generic.data.cmd)

###     # If the lattice package is available then generate a plot for
###     # each chosen vairable.
    
###     if (packageIsAvailable("lattice", "display a bar chart"))
###     {
###       startLog()
###       appendLog("Load lattice for the barchart function.", lib.cmd)
###       eval(parse(text=lib.cmd))

###       for (s in 1:nbarplots)
###       {
###         startLog()

###         # Construct and evaluate a command string to generate the
###         # data for the plot.

###         ds.cmd <- paste(sprintf("sprintf('%s',", generic.data.cmd),
###                        paste(paste('"', rep(barplots[s], length(targets)+1),
###                                  '"', sep=""), collapse=","), ")")
###         ds.cmd <- eval(parse(text=ds.cmd))
###         appendLog(sprintf("Generate the summary data for plotting %s.", barplots[s]),
###                  paste("ds <-", ds.cmd))
###         ds <- eval(parse(text=ds.cmd))

###         names.cmd <- sprintf('colnames(ds) <- c(%s)',
###                              ifelse(length(targets)==0, '"Frequency"',
###                                     paste('"Frequency"',
###                                           paste(sprintf('"%s"', targets),
###                                                 collapse=", "),
###                                           sep=", ")))
###         appendLog("Set the appropriate column names.", names.cmd)
###         eval(parse(text=names.cmd))

###         # We don't have multiple plots on the one plot implemented yet
###         # - should we? I would guess there is a simple way to do this
###         # with lattice.
        
###         #if (pcnt %% pmax == 0) newPlot(pmax)
###         #pcnt <- pcnt + 1
###         newPlot(pmax)

###         # Construct and evaluate the command to determine the order in
###         # which to print the catgories, from smallest (at the bottom)
###         # to largest.

###         ord.cmd <- 'order(ds[,1])'
###         appendLog("Sort the entries.", paste("ord <-", ord.cmd))
###         ord <- eval(parse(text=ord.cmd))

###         plot.cmd <- sprintf(paste('print(barchart(ds[ord,%s]',
###                                   'xlab="Frequency"',
###                                   ifelse(length(targets)==0,
###                                          'groups=NULL', # Just to have something!
###                                          sprintf(paste('auto.key=list(title="%s",',
###                                                        'cex=0.75,', 'columns=%d)'),
###                                                  target, 2)),
###                                   sprintf('sub="%s"', genPlotTitleCmd(vector=TRUE)),
###                                   'main="Distribution of %s%s"))', sep=", "),
###                             ifelse(length(targets)==0, "", "-1"),
###                             barplots[s],
###                             ifelse(sampling," (sample)",""))
                            
###         appendLog("Plot the data.", plot.cmd)
###         eval(parse(text=plot.cmd))

###       }
###     }
###   }

  ##---------------------------------------------------------------------

### REMOVE 080925 - Until work out multiple plots on one device issue.
###   if (ndotplots > 0)
###   {
    
###     # 080817 Use dotplot(lattice) instead of dotchart. 080925 But not
###     # yet since it uses a different mechanism to get multiple plots on
###     # one device and I've not set that up yet.

###     # lib.cmd <- "require(lattice, quietly=TRUE)"

###     # Construct a generic data command built using the genericDataSet
###     # values. To generate a barplot we use the output of the summary
###     # command on each element in the genericDataSet, and bring them
###     # together into a single structure. The resulting generic.data.cmd
###     # will have a number of "%s"s (one for the whole dataset, then
###     # one for each level) from the original genericDataSet string
###     # that will be replaced with the name of each variable as it is
###     # being plotted.

###     generic.data.cmd <- paste(lapply(genericDataSet,
###                                    function(x) sprintf("summary(%s)", x)),
###                             collapse=",\n    ")
###     generic.data.cmd <- sprintf("cbind(%s)", generic.data.cmd)

###     # If the lattice package is available then generate a plot for
###     # each chosen vairable.
    
###     if (packageIsAvailable("lattice", "display a dot plot"))
###     {
###       startLog()
###       appendLog("Load lattice for the dotplot function.", lib.cmd)
###       eval(parse(text=lib.cmd))

###       for (s in 1:ndotplots)
###       {
###         startLog()

###         # Construct and evaluate a command string to generate the data
###         # for the plot.

###         ds.cmd <- paste(sprintf("sprintf('%s',", generic.data.cmd),
###                         paste(paste('"', rep(dotplots[s], length(targets)+1),
###                                     '"', sep=""), collapse=","), ")")
###         ds.cmd <- eval(parse(text=ds.cmd))
###         appendLog(sprintf("Generate the summary data for plotting %s.", dotplots[s]),
###                   paste("ds <-", ds.cmd))
###         ds <- eval(parse(text=ds.cmd))

###         names.cmd <- sprintf('colnames(ds) <- c(%s)',
###                              ifelse(length(targets)==0, '"Frequency"',
###                                     paste('"Frequency"',
###                                           paste(sprintf('"%s"', targets),
###                                                 collapse=", "),
###                                           sep=", ")))
###         appendLog("Set the appropriate column names.", names.cmd)
###         eval(parse(text=names.cmd))

###         # Construct and evaluate the command to determine the order in
###         # which to print the catgories, from smallest (at the bottom)
###         # to largest.

###         ord.cmd <- 'order(ds[,1])'
###         appendLog("Sort the entries.", paste("ord <-", ord.cmd))
###         ord <- eval(parse(text=ord.cmd))

###         # Construct and evaluate the command to plot the distribution.
    
###         #if (pcnt %% pmax == 0) newPlot(pmax)
###         #pcnt <- pcnt + 1
###         newPlot(pmax)
      
###         plot.cmd <- sprintf(paste('print(dotplot(ds[ord,%s]',
###                                   'xlab="Frequency"',
###                                   'type=c("p", "h", "a")',
###                                   ifelse(length(targets)==0,
###                                          'groups=NULL', # Just to have something!
###                                          sprintf(paste('auto.key=list(title="%s",',
###                                                        'cex=0.75,', 'columns=%d)'),
###                                                  target, 2)),
###                                   sprintf('sub="%s"', genPlotTitleCmd(vector=TRUE)),
###                                   'main="Distribution of %s%s"))', sep=", "),
###                             ifelse(length(targets)==0, "", "-1"),
###                             dotplots[s],
###                             ifelse(sampling," (sample)",""))
###         appendLog("Plot the data.", plot.cmd)
###         eval(parse(text=plot.cmd))
###       }
###     }
###   }

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

    # This should have been removed at some stage! We seem to be using
    # dotchart from grpahics now.
    #
    #    appendLog("Use dotplot from lattice for the plots.", lib.cmd)
    #    eval(parse(text=lib.cmd))

    for (s in seq_len(ndotplots))
    {

      startLog()

      ## Construct and evaluate a command string to generate the
      ## data for the plot.

      ds.cmd <- paste(sprintf("sprintf('%s',", generic.data.cmd),
                     paste(paste('"', rep(dotplots[s], length(targets)+1),
                                 '"', sep=""), collapse=","), ")")
      ds.cmd <- eval(parse(text=ds.cmd))
      appendLog("Generate the summary data for plotting.",
               paste("ds <-", ds.cmd))
      ds <- eval(parse(text=ds.cmd))

      ## Construct and evaluate the command to determin the order in
      ## which to print the catgories, from larges to smallest.

      if (is.null(target))
        ord.cmd <- 'order(ds[1,])'
      else
        ord.cmd <- 'order(ds[1,], decreasing=TRUE)'
      appendLog("Sort the entries.",
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

      cols <- sprintf(ifelse(packageIsAvailable("vcd"),
                             "rainbow_hcl(%s, start = 270, end = 150)",
                             "rainbow(%s)"),
                      length(targets)+1) 

      plot.cmd <- sprintf(paste('dotchart(%s, main="%s", sub="%s",',
                               'col=%s,%s',
                               'xlab="Frequency", pch=19)'),
                         "ds[,ord]", titles[1], titles[2], cols,
                         ifelse(is.null(target), "", ' labels="",'))
      appendLog("Plot the data.", plot.cmd)
      eval(parse(text=plot.cmd))

      if (not.null(target))
      {
        legend.cmd <- sprintf(paste('legend("bottomright", bg="white",',
                                   'c("All","0","1"), col=%s,',
                                   'pch=19, title="%s")'),
                             cols, target)
        appendLog("Add a legend.", legend.cmd)
        eval(parse(text=legend.cmd))
      }
    }
  }

  #---------------------------------------------------------------------

  for (s in seq_len(nmosplots))
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

    if (packageIsAvailable("vcd"))
      cols <- "color=rainbow_hcl(%d, start = 270, end = 150)"
    else
      cols <- "color=rainbow(%d)"

    plot.cmd <- sprintf(paste('mosaicplot(ds, main="%s", sub="%s",',
                              ' ', cols, ', cex=0.7)'),
                        titles[1], titles[2], length(targets)+1)
    appendLog("Plot the data.", plot.cmd)
    eval(parse(text=plot.cmd))
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
  # Based on code from Marco Lo

  # 081128 Obtain info from a collection of radio buttons as to
  # whether to brush the data.  E.g., if hclust then:
  #
  # brush.cmd <- "glyph_colour(gg[1]) <- cutree(crs$hclust, 10)"
  #
  # Note also the embed=TRUE option of the ggobi display
  # function. This allows the display to be embedded within a RGtk
  # window, and hence seemlessly become part of Rattle.  
  
  # Construct the commands.

  lib.cmd <- "require(rggobi, quietly=TRUE)"
  ggobi.cmd <- paste('gg <<- ggobi(', dataset,
                     ifelse(not.null(name), sprintf(', name="%s"', name), ""),
                     ')')

  # Start logging and executing the R code.
  
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

  if (! packageIsAvailable("latticist", "explore data")) return()

  startLog("EXPLORE DATA.")

  lib.cmd <- "require(latticist)"
  appendLog("The latticist command comes from the latticist package.", lib.cmd)
  eval(parse(text=lib.cmd))

  latopts <- ""
  if (! is.null(crs$target))
    latopts <- sprintf(', spec=list(groups = "%s")', crs$target)
  plot.cmd <- sprintf("latticist(%s%s)", dataset, latopts)
  appendLog("Call upon latticist.", plot.cmd)
  eval(parse(text=plot.cmd))
}


########################################################################

## General Menu Callbacks

on_rattle_menu_activate <- function(action, window)
{
  browseURL("http://rattle.togaware.com")
}

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
  if (isRStat())
  {
    # 081004 setProgramName is only available in GTK+ 2.12 and
    #above. But the MS/Wdinows version of RGtk2 is compiled for 2.10,
    #and it has a compile time check for version, not run time, and so
    #even though 2.12 is installed, it won't run the function.
    #
    #if(exists("gtkAboutDialogSetProgramName"))
    #  about$getWidget("aboutdialog")$setProgramName("RStat")
    ab <- about$getWidget("aboutdialog")
    ab["program-name"] <- "RStat"
    ab["comments"] <- NULL
    about$getWidget("aboutdialog")$setWebsite(paste("http://www.togaware.com",
                                                    "\n     http://www.ibi.com"))
  }
  
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

on_verbose_menuitem_toggled <- function(action, window)
{
  crv$verbose <<- theWidget("verbose_menuitem")$getActive()
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

on_tools_test_activate <- function(action, window)
{
  crv$NOTEBOOK$setCurrentPage(getNotebookPage(crv$NOTEBOOK,
                                              crv$NOTEBOOK.TEST.NAME))
  switchToPage(crv$NOTEBOOK.TEST.NAME)
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

    mtypes <- listBuiltModels(exclude=crv$APRIORI)

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

