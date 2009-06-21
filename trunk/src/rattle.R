# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2009-06-01 18:02:54 Graham Williams>
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
GENERATION <- unlist(strsplit("$Revision$", split=" "))[2]
REVISION <- as.integer(GENERATION)-380
VERSION <- paste(MAJOR, MINOR, REVISION, sep=".")
VERSION.DATE <- "Released 21 Jun 2009"
COPYRIGHT <- "Copyright (C) 2006-2009 Togaware Pty Ltd."

# Acknowledgements: Frank Lu has provided much feedback and has
# extensively tested early versions of Rattle. Many colleagues at the
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
#   Original design placed state variables into the crs list and
#   global constants into . variables that then moved into the crv
#   list instead, after R CMD check started complaining about possibly
#   unbound variables. The real solution seems to be
#   environments. This was implemented temporarily simply by replacing
#   crv and crs with environments. The list notation then continued to
#   work for them! 090316 Finally removed all <<- assignments into the
#   environments, since, as Chambers (2008) page 124 points out a
#   reference to the environemt ralways refers to the same
#   environment. 
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

overwritePackageFunction <- function(fname, fun, pkg)
{
  # 090207 This allows a plugin to easily overwrite any Rattle funtion
  # with their own functionality. Simply define your own FUN that is
  # to overwrite the Rattle defined function FNAME. 090517 We do it
  # this way rather than having to export the function to be
  # overridden. Note that the override only happens within the
  # namespace of the package. Thus it does not make sense to use this
  # overwrite function to overwrite an exported function, since the
  # overwrite will not be seen externally to the package.
  
  re <- eval(parse(text=sprintf("environment(%s)", pkg)))
  unlockBinding(fname, re)
  assign(fname, fun, re)
  lockBinding(fname, re)
}

rattle <- function(csvname=NULL)
{
  # 090517 Require pmml. Now that there is an indication on the Data
  # tab as to whether the varaiable (i.e., a transformed variable) can
  # be exported to PMML we need pmml to be loaded. Thus pmml is now a
  # "Depends:" in the DESCRIPTION file.
  
  # If crv$tooltiphack is TRUE then gtkMain is called on focus,
  # blocking the R console, but at least tooltips work. On losing
  # focus gtkMainQuit is called, and thus the console is no longer
  # blocked!  A bit ugly, but seems to work. This was suggested by
  # Felix Andrew, 080705. I notice that to load the supplied audit
  # dataset I need to change focus out of Rattle.

  # 080906 If crv$close="quit" then when the window close is pressed, we
  # also quit R.
  
  # 080319 Create global crv and crs to avoid many "no visible
  # binding" messages from "R CMD check" by adding all hidden
  # variables to crs and crv. Previously they all began with "." as in
  # crv$ADA used to be .ADA. "R CMD check" complained a lot, once for
  # each of these, so putting them all into crv means only one
  # complaint each time! Then defining crv in .onLoad remoaves the
  # NOTE altogether.

  # 090303 Make sure crv has been defined. This was necessitated
  # because CHECK does not run .onLoad in checking.

  if (! exists("crv")) 
  {
    .onLoad()
    .onAttach()
  }

  if (not.null(crv$show.timestamp) && crv$show.timestamp)
    cat(crv$appname, "timestamp:",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  
  # 090309 Reset the environment, crs, which stores the curret Rattle
  # state and used extensively throughout Rattle as a global
  # state. Not ideal for functional programming and only a hopefully
  # small deviation from Chamber's (2008) Prime Directive principle,
  # and similar to the "option" exception to the Prime Directive!
  
  crs <<- new.env()

  # crv$tooltiphack <<- tooltiphack # Record the value globally

  # 090525 Move to having the Setting option work on Linux. This
  # remove all this tooltip stuff.
  
  # if (crv$tooltiphack) crv$load.tooltips <- TRUE

  crv$.gtkMain <- FALSE # Initially gtkMain is not running.
  
  # Load gloablly required packages.
  
  if (! packageIsAvailable("RGtk2", "display the Rattle GUI"))
    stop("RGtk2 package is not available but is required for the GUI.")

  if (packageIsAvailable("colorspace"))
  {
    # 080921 Load each individually so we can keep the loading quiet!

    # 090524 I think we are only after the colorspace package
    # here. Perhaps remove these others from the Suggests list.
    
#    require("MASS", quietly=TRUE)
#    require("grid", quietly=TRUE)
    require("colorspace", quietly=TRUE)
#    require("vcd", quietly=TRUE)
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

  # 090206 Tune the interface to suit needs, and in particular allow
  # packages to overwrite these functions so that the interface can be
  # tuned to suit plugins.

  setMainTitle()
  configureGUI()
  if (crv$load.tooltips) loadTooltips()
  
  # 080511 Record the current options and set the scientific penalty
  # to be 5 so we generally get numerics pinted using fixed rather
  # than exponential notation. We reset all options to what they were
  # at the startup of Rattle on closing Rattle. Not necessarily a good
  # idea since the knowing user may actually also change options
  # whilst Rattle is running.
  
  crv$options <- options(scipen=5)

  # 080924 Load of a supplied data file occurs here, but may take time
  # and whilst the UI is not fully set up yet, we see the Welcome
  # screen in Rattle displayed in plugins for 30 seconds or so. So
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
    
    if (! is.null(.RATTLE.DATA)) csvname <- .RATTLE.DATA

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
      # include file:/// and when compared in dataNeedsLoading to the
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
  
  crv$COLUMN <- c(number = 0, variable = 1, type = 2, input = 3,
                   target = 4, risk = 5, ident = 6, ignore = 7, comment = 8)
  
  crv$IMPUTE <- c(number=0, variable=1, comment=2)
  
  crv$CATEGORICAL <- c(number = 0, variable = 1, barplot = 2,
                        dotplot = 3, mosplot = 4, comment = 5)
  
  crv$CONTINUOUS <-  c(number = 0, variable = 1, boxplot = 2,
                        hisplot = 3, cumplot = 4, benplot = 5, comment = 6)
  
  # Create constants naming the MODELLERS (i.e., the model
  # builders). Note that these are migrating into the crv variable,
  # but not all are done yet. 081227 Also note that kmeans, hclust and
  # apriori will also be migrating into being treated as first class
  # models.

  crv$KMEANS 	<- "kmeans"
  crv$HCLUST 	<- "hclust"
  crv$APRIORI 	<- "apriori"
  
  crv$GLM   	<- "glm"
  crv$RPART 	<- "rpart"
  #GBM <- "gbm"
  crv$ADA   	<- "ada"
  crv$RF    	<- "rf"
  crv$SVM   	<- "svm"
  crv$KSVM  	<- "ksvm"
  crv$NNET  	<- "nnet"

  crv$MODELLERS <- c(crv$RPART, crv$ADA, crv$RF, crv$KSVM, crv$GLM, crv$NNET)
  
  # PACKAGE STATE VARIABLE
  
  # 090309 The following is now taken care of in .onLoad as defined in
  # zzz.R. 

  ## if (TRUE)
  ##   crs <<- new.env()
  ## else
  ##   crs <<- list(dataset=NULL,
  ##              dataname=NULL,
  ##              dwd=NULL, 	# Data Working Directory
  ##              mtime=NULL,	# Modification time of file
  ##              pwd=NULL,	# Project Working Directory
  ##              input=NULL,
  ##              target=NULL,
  ##              weights=NULL,
  ##              risk=NULL,
  ##              ident=NULL,
  ##              ignore=NULL,
  ##              nontargets=NULL, # 080426 Started but not yet implemented
  ##              sample=NULL,
  ##              sample.seed=NULL,
  ##              kmeans=NULL,
  ##              kmeans.seed=NULL,
  ##              hclust=NULL,
  ##              page="",
  ##              smodel=NULL, # Record whether the sample has been modelled
  ##              glm=NULL,
  ##              rpart=NULL,
  ##              ada=NULL,
  ##              apriori=NULL,
  ##              rf=NULL,
  ##              svm=NULL,
  ##              ksvm=NULL,
  ##              perf=NULL,
  ##              eval=NULL,
  ##              testset=NULL,
  ##              testname=NULL,
  ##              alog=NULL,	# Record of interaction - useful?
  ##              transforms=NULL  # Record of variable transforms for inclusion in PMML
  ##              )

  # Main notebook related constants and widgets.  Track the widgets
  # that are needed for removing and inserting tabs in the notebook,
  # depending on the selected paradigm. TODO Paradigms have gone as of
  # 080519 so we may not need all this machinery now!
  
  crv$NOTEBOOK <- theWidget("notebook")

  crv$NOTEBOOK.DATA.NAME <- "Data"

  crv$NOTEBOOK.TEST.NAME <- "Test"

  crv$NOTEBOOK.EXPLORE.NAME <- "Explore"

  crv$NOTEBOOK.TRANSFORM.NAME <- "Transform"

  crv$NOTEBOOK.CLUSTER.NAME    <- "Cluster"
  crv$NOTEBOOK.CLUSTER.WIDGET <- theWidget("cluster_tab_widget")
  crv$NOTEBOOK.CLUSTER.LABEL  <- theWidget("cluster_tab_label")

  crv$NOTEBOOK.ASSOCIATE.NAME    <- "Associate"
  crv$NOTEBOOK.ASSOCIATE.WIDGET <- theWidget("associate_tab_widget")
  crv$NOTEBOOK.ASSOCIATE.LABEL  <- theWidget("associate_tab_label")

  crv$NOTEBOOK.MODEL.NAME     <- "Model"
  crv$NOTEBOOK.MODEL.WIDGET  <- theWidget("model_tab_widget")
  crv$NOTEBOOK.MODEL.LABEL   <- theWidget("model_tab_label")

  crv$NOTEBOOK.EVALUATE.NAME    <- "Evaluate"
  crv$NOTEBOOK.EVALUATE.WIDGET <- theWidget("evaluate_tab_widget")
  crv$NOTEBOOK.EVALUATE.LABEL  <- theWidget("evaluate_tab_label")

  crv$NOTEBOOK.LOG.NAME       <- "Log"

  # 080921 Define the DATA tab pages

  crv$DATA.NOTEBOOK 	<- theWidget("data_notebook")
  crv$DATA.CORPUS.TAB      <- getNotebookPage(crv$DATA.NOTEBOOK, "corpus")
  crv$DATA.CSV.TAB         <- getNotebookPage(crv$DATA.NOTEBOOK, "csv")

  crv$DATA.DISPLAY.NOTEBOOK     <- theWidget("data_display_notebook")
  crv$DATA.DISPLAY.TREEVIEW.TAB <- getNotebookPage(crv$DATA.DISPLAY.NOTEBOOK, "treeview")
  crv$DATA.DISPLAY.WELCOME.TAB  <- getNotebookPage(crv$DATA.DISPLAY.NOTEBOOK, "welcome")

  # Define the TRANSFORM tab pages
  
  crv$TRANSFORM               <- theWidget("transform_notebook")
  # TODO 080423 Change to RESCALE
  crv$TRANSFORM.NORMALISE.TAB <- getNotebookPage(crv$TRANSFORM, "normalise")
  crv$TRANSFORM.IMPUTE.TAB    <- getNotebookPage(crv$TRANSFORM, "impute")
  crv$TRANSFORM.REMAP.TAB     <- getNotebookPage(crv$TRANSFORM, "remap")
  crv$TRANSFORM.OUTLIER.TAB   <- getNotebookPage(crv$TRANSFORM, "outlier")
  crv$TRANSFORM.CLEANUP.TAB   <- getNotebookPage(crv$TRANSFORM, "cleanup")

  crv$EXPLORE                 <- theWidget("explore_notebook")
  crv$EXPLORE.SUMMARY.TAB     <- getNotebookPage(crv$EXPLORE, "summary")
  crv$EXPLORE.PLOT.TAB        <- getNotebookPage(crv$EXPLORE, "explot")
  crv$EXPLORE.CORRELATION.TAB <- getNotebookPage(crv$EXPLORE, "correlation")
  crv$EXPLORE.PRCOMP.TAB      <- getNotebookPage(crv$EXPLORE, "prcomp")
  crv$EXPLORE.INTERACTIVE.TAB <- getNotebookPage(crv$EXPLORE, "interactive")
  
  crv$CLUSTER            <- theWidget("cluster_notebook")
  crv$CLUSTER.KMEANS.TAB <- getNotebookPage(crv$CLUSTER, "kmeans")
  crv$CLUSTER.HCLUST.TAB <- getNotebookPage(crv$CLUSTER, "hclust")
  
  crv$MODEL           <- theWidget("model_notebook")
  crv$MODEL.RPART.TAB <- getNotebookPage(crv$MODEL, crv$RPART)
  crv$MODEL.GLM.TAB   <- getNotebookPage(crv$MODEL, crv$GLM)
  crv$MODEL.ADA.TAB   <- getNotebookPage(crv$MODEL, crv$ADA)
  ## crv$MODEL.GBM.TAB   <- getNotebookPage(crv$MODEL, .GBM)
  crv$MODEL.RF.TAB    <- getNotebookPage(crv$MODEL, crv$RF)
  crv$MODEL.SVM.TAB   <- getNotebookPage(crv$MODEL, crv$SVM)
  crv$MODEL.NNET.TAB   <- getNotebookPage(crv$MODEL, crv$NNET)

  crv$SVMNB           <- theWidget("svm_notebook")
  crv$SVMNB.ESVM.TAB  <- getNotebookPage(crv$SVMNB, "esvm")
  crv$SVMNB.KSVM.TAB  <- getNotebookPage(crv$SVMNB, "ksvm")
  
  crv$EVALUATE                 <- theWidget("evaluate_notebook")
  crv$EVALUATE.CONFUSION.TAB   <- getNotebookPage(crv$EVALUATE, "confusion")
  crv$EVALUATE.RISK.TAB        <- getNotebookPage(crv$EVALUATE, "risk")
  crv$EVALUATE.LIFT.TAB        <- getNotebookPage(crv$EVALUATE, "lift")
  crv$EVALUATE.ROC.TAB         <- getNotebookPage(crv$EVALUATE, "roc")
  crv$EVALUATE.PRECISION.TAB   <- getNotebookPage(crv$EVALUATE, "precision")
  crv$EVALUATE.SENSITIVITY.TAB <- getNotebookPage(crv$EVALUATE, "sensitivity")
  crv$EVALUATE.COSTCURVE.TAB   <- getNotebookPage(crv$EVALUATE, "costcurve")
  crv$EVALUATE.PVO.TAB         <- getNotebookPage(crv$EVALUATE, "pvo")
  crv$EVALUATE.SCORE.TAB       <- getNotebookPage(crv$EVALUATE, "score")
  
  # Turn off the sub-notebook tabs.

  # Sys.sleep(5) 080924 to test delays....
  
  crv$DATA.NOTEBOOK$setShowTabs(FALSE)
  crv$DATA.DISPLAY.NOTEBOOK$setShowTabs(FALSE)
  crv$EXPLORE$setShowTabs(FALSE)
  crv$TRANSFORM$setShowTabs(FALSE)
  crv$CLUSTER$setShowTabs(FALSE)
  crv$MODEL$setShowTabs(FALSE)
  crv$EVALUATE$setShowTabs(FALSE)

  ########################################################################
  # Connect the callbacks.
  
  gladeXMLSignalAutoconnect(rattleGUI)

  # Enable the tooltips Settings option on GNU/Linux. Under MS/Windows
  # tooltips have always worked so this option is not relevant.

  if (isLinux() && crv$load.tooltips)
  {
    theWidget("tooltips_menuitem")$show()
    theWidget("tooltips_menuitem")$setActive(FALSE)
  }

  ########################################################################
  # User interface initialisations.
  
  initialiseVariableViews()
  
  # Ensure the filechooserbutton by default will filter CSVs.

  updateFilenameFilters("data_filechooserbutton", "CSV")
  
  # Do not enable ARFF option for versions before 2.5.0 where it was
  # not included in the foreign package.

  if (!exists("getRversion", baseenv()) || getRversion() <= "2.4.0")
    theWidget("arff_radiobutton")$hide()
  
  theWidget("model_tree_include_missing_checkbutton")$setActive(FALSE)
  #theWidget("glm_family_comboboxentry")$setActive(0)
  theWidget("svm_kernel_comboboxentry")$setActive(0)

  ## Check if some external applications are available and if not
  ## de-sensitise their functionality.

  # How to test if ggobi is actually available?

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

  displayWelcomeTabMessage()
  
  initiateLog()
  
  # Make sure the text is shown on startup.
  
  while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
  
  # Now deal with any arguments to rattle.

  if (not.null(csvname))
  {
    if (!theWidget("data_filechooserbutton")$setUri(csvname))
      infoDialog("Internal Error: The setting of the filename box",
                 "failed.", crv$support.msg)
    # Make sure GUI updates
    while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
    executeDataTab(csvname)
  }

  ## theWidget("csv_filechooserbutton")$setFilename("audi.csv")

  # Call resetRattle to ensure all textviews get their default texts
  
  resetRattle(FALSE)
  
  invisible()
}

########################################################################
# Configurable functions - these are here because plugins may want to
# overwrite them.

configureGUI <- function()
{

  # Toolbar

  theWidget("report_toolbutton")$show()
  
  id.string <- paste('<span foreground="blue">',
                     '<i>', crv$appname, '</i> ',
                     '<i>Version ', crv$version, '</i> ',
                     '<i><span underline="single">togaware.com</span></i>',
                     '</span>', sep="")

  rattle.menu <- theWidget("rattle_menu")
  rattle.menu$SetRightJustified(TRUE)
  rattle.menu$getChild()$setMarkup(id.string)

}

displayWelcomeTabMessage <- function()
{
  crv$DATA.DISPLAY.NOTEBOOK$setCurrentPage(crv$DATA.DISPLAY.WELCOME.TAB)
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
                      "\n\nRattle version", crv$version,
                      "Copyright (C) 2006-2009 Togaware Pty Ltd",
                      "\nRattle is a registered trademark of Togaware Pty Ltd"),
                tvsep=FALSE)
}

writeCSV <- function(x, file="", ...)
{
  write.csv(x, file=file, row.names=FALSE, ...)
}

#-----------------------------------------------------------------------
# MAINLOOP ITERATION
#
# Attempt to get tooltips working forGNU/Linux by starting up gtkMain
# on the window getting focus, and stopping it when it loses
# focus. Based on idea from Felix Andrews.

gtkmain_handler <- function(widget, event)
{
  # 090525 Can't get this one working yet - to be able to turn
  # tooltips on and off. playwith does it?
  
  #if (! theWidget("tooltip_menuitem")$getActive())
  #  return(gtkmainquit_handler(widget, event))
  
  # Switch to GTK event loop while the window is in focus (for tooltips)
  
  if (! crv$.gtkMain)
  {
    crv$.gtkMain <- TRUE
    gtkMain()
  }
  return(FALSE)
}

gtkmainquit_handler <- function(widget, event)
{
  if (crv$.gtkMain)
  {
    crv$.gtkMain <- FALSE
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

  if (new.dataset) setMainTitle()
  
  if (new.dataset)
  {
    # Initialise CRS

    crs$dataset  <- NULL
    crs$dataname <- NULL
    # crs$dwd      <- NULL
    crs$mtime    <- NULL
    crs$input    <- NULL
    crs$target   <- NULL
    crs$weights  <- NULL
    crs$risk     <- NULL
    crs$ident    <- NULL
    crs$ignore   <- NULL
    crs$nontargets <- NULL # 080426 Started but not yet implemented.
    crs$sample   <- NULL
    crs$sample.seed <- NULL
    crs$testset  <- NULL
    crs$testname <- NULL
    crs$transforms <- NULL
  }

  # Clear out all current models.
  
  crs$kmeans   <- NULL
  crs$kmeans.seed <- NULL
  crs$hclust   <- NULL
  crs$apriori  <- NULL
  crs$page     <- ""
  crs$smodel   <- NULL
  crs$glm      <- NULL
  crs$rpart    <- NULL
  crs$ada      <- NULL
  crs$rf       <- NULL
  crs$svm      <- NULL
  crs$ksvm     <- NULL
  crs$nnet     <- NULL
  crs$perf     <- NULL
  crs$eval     <- NULL

  # Clear all now outdated text views

  resetTextviews()

  # Set all sub tabs back to the default tab page and reflect this in
  # the appropriate radio button.

  # TODO 080423 Change name to RESCALE
  crv$TRANSFORM$setCurrentPage(crv$TRANSFORM.NORMALISE.TAB)
  theWidget("normalise_radiobutton")$setActive(TRUE)
  theWidget("impute_zero_radiobutton")$setActive(TRUE)
  theWidget("impute_constant_entry")$setText("")
  theWidget("remap_quantiles_radiobutton")$setActive(TRUE)
  theWidget("delete_ignored_radiobutton")$setActive(TRUE)
  
  crv$EXPLORE$setCurrentPage(crv$EXPLORE.SUMMARY.TAB)
  theWidget("summary_radiobutton")$setActive(TRUE)

  crv$CLUSTER$setCurrentPage(crv$CLUSTER.KMEANS.TAB)
  theWidget("kmeans_radiobutton")$setActive(TRUE)
  
  crv$MODEL$setCurrentPage(crv$MODEL.RPART.TAB)
  theWidget("rpart_radiobutton")$setActive(TRUE)
  #theWidget("all_models_radiobutton")$setActive(TRUE)

  crv$EVALUATE$setCurrentPage(crv$EVALUATE.CONFUSION.TAB)
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

    # Reset Test tab

    theWidget("test_distr_radiobutton")$setActive(TRUE)
    theWidget("test_vars1_combobox")$getModel()$clear()
    theWidget("test_vars2_combobox")$getModel()$clear()
    #theWidget("test_vars1_combobox")$setActive(-1)
    #theWidget("test_vars2_combobox")$setActive(-1)
    theWidget("test_groupby_checkbutton")$setActive(TRUE)
    theWidget("test_groupby_target_label")$setText("No Target")
    theWidget("test_groupby_checkbutton")$setSensitive(TRUE)
    theWidget("test_groupby_target_label")$setSensitive(TRUE)
    
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
    theWidget("rpart_minsplit_spinbutton")$setValue(crv$rpart.minsplit.default)
    theWidget("rpart_maxdepth_spinbutton")$setValue(crv$rpart.maxdepth.default)
    theWidget("model_tree_cp_spinbutton")$setValue(crv$rpart.cp.default)
    theWidget("rpart_minbucket_spinbutton")$setValue(crv$rpart.minbucket.default)
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
  
    if (! is.null(.RATTLE.SCORE.IN))
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

          .RATTLE.SCORE.IN <<- NULL
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
  theWidget("hclust_link_combobox")$setActive(1)
  theWidget("hclust_dendrogram_button")$setSensitive(FALSE)
  theWidget("hclust_clusters_label")$setSensitive(FALSE)
  theWidget("hclust_clusters_spinbutton")$setSensitive(FALSE)
  theWidget("hclust_stats_button")$setSensitive(FALSE)
  theWidget("hclust_data_plot_button")$setSensitive(FALSE)
  theWidget("hclust_discriminant_plot_button")$setSensitive(FALSE)
  
}

########################################################################
# UTILITIES

"%notin%" <- function(x,y) ! x %in% y

not.null <- function(x) ! is.null(x)

uri2file <- function(u)
{
  sub("^file://", "", u)
}

listVersions <- function(file="", ...)
{
  result <- installed.packages()[,c("Package", "Version")]
  row.names(result) <- NULL
  write.csv(result, file=file, ...)
  invisible(result)
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
                                sprintf("\n\n%s %s", crv$appname, crv$version))
  connectSignal(dialog, "response", gtkWidgetDestroy)
}

errorReport <- function(cmd, result)
{
  # A standard command error report that is not being captured by
  # Rattle. Eventually, all of these should be identified by Rattle
  # and a sugggestion given as to how to avoid the error.
  
  errorDialog("An error occured with", cmd, crv$support.msg, "\n\n",
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
  # Popup an error dialog if no dataset has been loaded, and return
  # TRUE, otherwise return FALSE.

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

setMainTitle <- function(title=NULL)
{
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

isLinux <- function()
{
  return(.Platform$OS.type == "unix")
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
##     crs$dwd <- dirname(filename)
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
    dev.print(win.print)
  else
    dev.print()
  dev.set(cur)
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

  if (! exists("crv"))
  {
    crv <- list()
    crv$appname <- "Rattle"
    crv$verbose <- TRUE
  }
  
  main = paste(...)
  if(vector)
  {
    if (! crv$verbose)
      sub <- ""
    else
      sub <- sprintf("%s %s %s", crv$appname,
                     format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"])
    return(c(main, sub))
  }
  else
  {  
    if (! crv$verbose)
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

on_rattle_window_delete_event <- function(action, window)
{
  if (crv$close %in% c("quit", "ask"))
  {
    msg <- sprintf("Do you want to terminate %s?", crv$appname)
    if (!questionDialog(msg))
      return(TRUE)
    else
      if (crv$close == "quit")
        quit(save="no")
      else
        return(FALSE)
  }
}

close_rattle <- function(action, window)
{
  # 090401 This callback seems to be called after the window is
  # destroyed!!!  So the question serves no purpose... Not clear how
  # to fix that.
  
  closeRattle()
}

quit_rattle <- function(action, window)
{
  # 080815 This function used to return NULL or "yes" and I always
  # tested whether it's results was NULL. But why not return a
  # logical? Start doing that now, by returning TRUE instead of "yes",
  # and look to return FALSE instead of NULL on a negative response to
  # the question.

  closeRattle(TRUE)
}

closeRattle <- function(ask=FALSE)
{
  if (ask || crv$close %in% c("quit", "ask"))
  {  
    msg <- sprintf("Do you want to terminate %s?", crv$appname)
    if (!questionDialog(msg)) return(FALSE)
  }
  
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

  ab <- about$getWidget("aboutdialog")
  ab$setVersion(crv$version)

  configureAbout(ab)
  
  gladeXMLSignalAutoconnect(about)
}

configureAbout <- function(ab)
{
  ab["program-name"] <- "Rattle"
  ab$setCopyright(paste(VERSION.DATE, "\n\n", COPYRIGHT, "\n" ,
                        "All rights reserved."))
}
 

on_paste1_activate <- notImplemented
on_copy1_activate <- notImplemented

on_tooltips_activate <- function(action, window)
{
  
  ## infoDialog("Currently this functionality is not implemented.",
  ##             "It is awaiting some insight into how to get hold of",
  ##             "the glade GtkTooltips group, which can then be",
  ##             "disabled or enabled as requested.")

  if(action$getActive())
  {
    myWin <- theWidget("rattle_window")
    myWin$addEvents(GdkEventMask["focus-change-mask"])
    gSignalConnect(myWin, "focus-in-event", gtkmain_handler)
    gSignalConnect(myWin, "focus-out-event", gtkmainquit_handler)
    gSignalConnect(myWin, "delete-event", gtkmainquit_handler)
  }
  ## else
  ## {
  ##   infoDialog("Currently the functionality to turn tooltips off",
  ##              "is not implemented.")
  ## }    
}

on_verbose_menuitem_toggled <- function(action, window)
{
  crv$verbose <- theWidget("verbose_menuitem")$getActive()
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

  crs$page <- page

}

