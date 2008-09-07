# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2008-09-07 15:25:17 Graham Williams>
#
# DATA TAB
#
# Copyright (c) 2008 Togaware Pty Ltd
#
# This file is part of Rattle.
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
#
########################################################################
#
# I've removed the Data Entry radio button because why, really, would
# anyone be interested in manually entering some data - use Gnumeric
# or some other spreadsheet to do that.
#

########################################################################
# UTILITIES

overwriteModel <- function()
{
  # 080523 If a model exists then warn the user about losing the model
  # on loading a new dataset. Perhaps this could be generalised to any
  # kind of opration that replaces the current model.
  
  if (not.null(listBuiltModels()))
    return(questionDialog("You have chosen to load a dataset.",
                          "This will clear the current project",
                          "(dataset and models).",
                          "If you choose not to continue",
                          "you can save the project, and then load",
                          "the new dataset.\n",
                          "\nDo you wish to continue and so overwrite",
                          "the current project?"))
  else
    return(TRUE)
}

dataTabShow <- function(...)
{
  # A support function to display the indicated widgets and hide all
  # others, within the data tab. When new widgets are added to the tab
  # through editting the XML file with glade, be sure to add it to the
  # list of known widgets here.
  
  widgets <- c(...)
  known <- c("data_filename_label",
             "data_filechooserbutton",
             "data_separator_label",
             "data_separator_entry",
             "data_header_checkbutton",
             "data_name_label",
             "data_name_combobox",
             "data_odbc_dsn_label",
             "data_odbc_dsn_entry",
             "data_odbc_table_label",
             "data_odbc_table_combobox",
             "data_odbc_limit_label",
             "data_odbc_limit_spinbutton",
             "data_odbc_believeNRows_checkbutton")
  for (w in widgets) theWidget(w)$show()
  for (w in setdiff(known, widgets)) theWidget(w)$hide()
}

showDataViewButtons <- function(action=TRUE)
{
  # Rattle starts up with the View and Edit buttons of the Data tab
  # not sensitive. Once data has been loaded we make these tabs
  # sensitive. The ACTION option allows for the case where we might
  # want to make them not sensitive. This option (action=FALSE) is not
  # currently used.

  if (! is.logical(action)) warning("action must be a logical")
    
  theWidget("data_view_button")$setSensitive(action)
  theWidget("data_edit_button")$setSensitive(action)
}  

urlModTime <- function(filename)
{
  # Return the modification time of the file. Strip out any "file://"
  # prefix to the filename. We note that this will not work for
  # http:// urls.
  
  return(file.info(gsub("file:///", "/", filename))$mtime)
}

changedDataTab <- function()
{
  # 080520 Determine whether any of the data source aspects of the
  # Data tab have changed. This is probably limited to checking things
  # relevant to the currently selected data source radio button.

  # 080712 If there is no dataname stored, then don't bother testing
  # any other conditions. The dataset should be loaded.
  
  if (is.null(crs$dataname)) return(TRUE)
  
  # 080712 Check what data source is active, and act
  # appropriately. For those I have yet to work on, simply return TRUE
  # so that at least the data always gets loaded. But this does then
  # wipe out any changes the user makes to selections.

  if (theWidget("data_csv_radiobutton")$getActive()
      || theWidget("data_arff_radiobutton")$getActive())
  {
  
    filename <- theWidget("data_filechooserbutton")$getUri()  

    if (is.null(filename) || is.null(crs$dwd))
      return(TRUE)
  
    if (URLdecode(basename(filename)) != crs$dataname ||
        dirname(URLdecode(filename)) != crs$dwd)
      return(TRUE)

    # 080606 TODO Test if file date has changed, and if so, return
    # TRUE.  file.info does not handle URLs so this is no good at
    # present. Note that under MS/Windows this returns NA so we don't
    # get a chance to notice updated files.

    now.mtime <- urlModTime(filename)
    if (! is.null(crs$mtime) && ! is.null(now.mtime) && now.mtime > crs$mtime)
      return(TRUE)
  
  }
  else if (theWidget("data_rdataset_radiobutton")$getActive())
  {
    dataname <- theWidget("data_name_combobox")$getActiveText()

    if (is.null(dataname) || crs$dataname != dataname)
      return(TRUE)
  }
  else if (theWidget("data_library_radiobutton")$getActive())
  {
    dataname <- theWidget("data_name_combobox")$getActiveText()
    if (is.null(crs$datapkg) || is.null(dataname))
      return(TRUE)
    adsname <- gsub('([^ :]*).*$', '\\1', unlist(strsplit(dataname, ":"))[1])
    dspkg <- unlist(strsplit(dataname, ":"))[2]
    if (crs$dataname != adsname
        || crs$datapkg != dspkg)
      return(TRUE)
  }
  else if (theWidget("data_rdata_radiobutton")$getActive())
  {
    dataname <- theWidget("data_name_combobox")$getActiveText()

    if (is.null(dataname) || crs$dataname != dataname) return(TRUE)
  }
  else if (theWidget("data_odbc_radiobutton")$getActive())
  {
   table <- theWidget("data_odbc_table_combobox")$getActiveText()

   if (is.null(table) || crs$dataname != table) return(TRUE)
  }

  # Return FALSE if we did not detect any changes.

  return(FALSE)
}

updateFilenameFilters <- function(button, fname)
{
  # Add the filters appropriate to the filter name (fname) supplied.

  button <- theWidget(button)
  filters <- button$listFilters()

  if (fname == "CSV")
  {
    if (! (length(filters) > 0 && filters[[1]]$getName() == "CSV Files"))
    {
      lapply(filters, function(x) button$removeFilter(x))

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
  else if (fname == "ARFF")
  {
    if (! (length(filters) > 0 && filters[[1]]$getName() == "ARFF Files"))
    {
      lapply(filters, function(x) button$removeFilter(x))

      ff <- gtkFileFilterNew()
      ff$setName("ARFF Files")
      ff$addPattern("*.arff")
      button$addFilter(ff)
      
      ff <- gtkFileFilterNew()
      ff$setName("All Files")
      ff$addPattern("*")
      button$addFilter(ff)
    }
  }
  else if (fname == "Rdata")
  {
    if (! (length(filters) > 0 && filters[[1]]$getName() == "Rdata Files"))
    {
      lapply(filters, function(x) button$removeFilter(x))

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

  # Kick the GTK event loop otherwise you end up waiting until the
  # mouse is moved, for example.
  
  while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
}

########################################################################
# CALLBACKS

on_data_csv_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    dataTabShow("data_filename_label",
                "data_filechooserbutton",
                "data_separator_label",
                "data_separator_entry",
                "data_header_checkbutton")
    updateFilenameFilters("data_filechooserbutton", "CSV")
  }
}

on_data_arff_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    
    dataTabShow("data_filename_label",
                "data_filechooserbutton")
    updateFilenameFilters("data_filechooserbutton", "ARFF")
  }
}

on_data_rdata_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    dataTabShow("data_filename_label",
                "data_filechooserbutton",
                "data_name_label",
                "data_name_combobox")
    updateFilenameFilters("data_filechooserbutton", "Rdata")
    cbox <- theWidget("data_name_combobox")
    cbox$getModel()$clear()
  }
}

on_data_rdataset_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    dataTabShow("data_name_label", "data_name_combobox")
    updateRDatasets()
  }
}

# 080907 Trying to get an event that will auto update the combobox
# without having to move to another radio button and then back again.

on_data_name_combobox_button_press_event <- function(button)
{
  print("Button Press")
  updateRDatasets()
}

on_data_name_combobox_enter_notify_event <- function(button)
{
  print("Enter Notify")
  updateRDatasets()
}

on_data_name_combobox_focus <- function(button)
{
  print("Focus")
  updateRDatasets()
}

on_data_name_combobox_set_focus_child<- function(direction, data)
{
  print("Focus Child")
  #print(direction)
  print(data)
  #updateRDatasets()
}

on_data_name_combobox_focus_in_event<- function(direction, data)
{
  print("Focus In")
  #print(direction)
  #updateRDatasets()
}

#

on_data_library_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    dataTabShow("data_name_label", "data_name_combobox")
    updateDataLibrary()
  }
}

on_data_odbc_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    dataTabShow("data_odbc_dsn_label",
                "data_odbc_dsn_entry",
                "data_odbc_table_label",
                "data_odbc_table_combobox",
                "data_odbc_limit_label",
                "data_odbc_limit_spinbutton",
                "data_odbc_believeNRows_checkbutton")
}

updateRDatasets <- function()
{
  # Update a combo box with just the available data frames and matrices.

  set.cursor("watch", "Determining the available datasets....")
  
  dl <- unlist(sapply(ls(sys.frame(0)),
                      function(x)
                      {
                        cmd <- sprintf(paste("is.data.frame(%s) ||",
                                             'inherits(%s,',
                                             '"sqlite.data.frame")'), x, x)
                        var <- try(ifelse(eval(parse(text=cmd), sys.frame(0)),
                                          x, NULL), silent=TRUE)
                        if (inherits(var, "try-error"))
                          var <- NULL
                        return(var)
                      }))

  cbox <- theWidget("data_name_combobox")
  
  if (not.null(dl))
  {
    cbox$getModel()$clear()
    lapply(dl, cbox$appendText)
    ## Set the selection to that which was already selected, if possible.
#    if (not.null(current) && current %in% dl)
#      action$setActive(which(sapply(dl, function(x) x==current))[1]-1)
  }
  set.cursor(message="")
}

########################################################################
# EXECUTE

executeDataTab <- function(csvname=NULL)
{
  # Dispatch to the task indicated by the selected radio button within
  # the Data tab. If there is no change to the data source, or the
  # data type label is not sensisteive (i.e., we have loaded a
  # project), then simply update the variable roles instead, without
  # reloading the data.  080520 This is now required as a result of
  # merging the Data and the Select tabs.

#  if (! is.null(csvname))
#  {    
#    if (! executeDataCSV(csvname)) return(FALSE)
#  }
  if (theWidget("data_type_label")$isSensitive() && changedDataTab())
  {
    if (theWidget("data_csv_radiobutton")$getActive())
    {
      if (! executeDataCSV(csvname)) return(FALSE)
    }
    else if (theWidget("data_arff_radiobutton")$getActive())
    {
      if (! executeDataARFF()) return(FALSE)
    }
    else if (theWidget("data_odbc_radiobutton")$getActive())
    {
      if (! executeDataODBC()) return(FALSE)
    }
    else if (theWidget("data_rdata_radiobutton")$getActive())
    {
      if (! executeDataRdata()) return()
    }
    else if (theWidget("data_rdataset_radiobutton")$getActive())
    {
      if (! executeDataRdataset()) return()
    }
    else if (theWidget("data_library_radiobutton")$getActive())
    {
      if (! executeDataLibrary()) return()
    }

    # Update the select treeview. This is done on a Data execute only
    # when a new dataset has been loaded. If the user has simply
    # changed some of the roles or the sampling then we do not do a
    # reset, just an update.

    createVariablesModel(colnames(crs$dataset)) 

    # Whether we have changed the dataset or not we need to generate the
    # sample and then record the variable roles.
    
    # Turn sampling on, set range bounds and generate the default 70%
    # sample. Do the range bounds first since otherwise the value gets
    # set back to 1. Also, need to set both the percentage and the count
    # since if the old percentage is 70 and the new is 70, then no
    # change in value is noticed, and thus the count is not
    # automatically updated.
    
    nrows <- nrow(crs$dataset)
    per <- 70
    srows <- round(nrows * per / 100)
    theWidget("sample_checkbutton")$setActive(!exists(".RATTLE.SCORE.IN"))
    theWidget("sample_count_spinbutton")$setRange(1,nrows)
    theWidget("sample_count_spinbutton")$setValue(srows)
    theWidget("sample_percentage_spinbutton")$setValue(per)
  }
  else
    resetRattle(new.dataset=FALSE)
  
#  else
#  {
#    resetRattle(new.dataset=FALSE)
#
#    if (changedDataTab())
#    {
#        
#      # Just duplicate above for now to get this working.
#      createVariablesModel(colnames(crs$dataset)) # BUT THIS REVERTS TO DEFAULTS
#      nrows <- nrow(crs$dataset)
#      per <- 70
#      srows <- round(nrows * per / 100)
#      theWidget("sample_checkbutton")$setActive(!exists(".RATTLE.SCORE.IN"))
#      theWidget("sample_count_spinbutton")$setRange(1,nrows)
#      theWidget("sample_count_spinbutton")$setValue(srows)
#      theWidget("sample_percentage_spinbutton")$setValue(per)
#    }
#    
#  }

  # TODO 080520 Change the name to updateSample.
  
  ## 080603 NOT NEEDED AS DONE IN executeSelectTab
  ## executeSelectSample()

  # Execute the SELECT tab. Changes have bene made and we need to
  # ensure the cached role variables are updated, or else we might see
  # unexpected warnings about changes having been made but not
  # EXECTUEd. [071125]

  # TODO 080520 Change the name to updateRoles.
  
  executeSelectTab()

  # Set the risk label appropriately.
  
  theWidget("evaluate_risk_label")$setText(crs$risk)

  # Enable the Data View and Edit buttons.

  showDataViewButtons()

  return()
}

#-----------------------------------------------------------------------
# EXECUTE DATA CSV

executeDataCSV <- function(filename=NULL)
{

  # Either a filename is supplied in the function call or a filename
  # is expected to be available in the data_filechooserbutton. This
  # could be either a CSV or TXT file. If no filename is supplied,
  # then give the user the option to load a sample dataset (for now,
  # the audit dataset).
  
  # Begin by collecting the relevant data from the interface. 080511
  # The file chooser button has a getFilename to retrieve the
  # filename. The getUri also retrieves the file name, but as a
  # URL. So we use this, since R can handle the
  # "file:///home/kayon/audit.csv" just fine. Thus I have now allowed
  # the filechooser button to accept non-local files (i.e.,
  # URLs). Unfortunately I can't yet get the basename of the URL to be
  # displayed in the button text. 080512 The URLdecode will replace
  # the %3F with "?" and %3D with "=", etc, as is required for using
  # this with the read.csv function.

  if (is.null(filename))
    filename <- theWidget("data_filechooserbutton")$getUri()
  
  # If no filename has been supplied give the user the option to use
  # the Rattle supplied sample dataset.
    
  if (is.null(filename))
  {
    if (! questionDialog("No CSV filename has been provided.\n",
                         "\nWe require a dataset to be loaded.\n",
                         "\nWould you like to use the sample audit",
                         "dataset?"))

      # If no filename is given and the user decides not to go with
      # the sample dataset then return without doing anything.
      
      return(FALSE)

    else
    {
      # 080515 Use the Rattle provided sample dataset.
      
      filename <- system.file("csv", "audit.csv", package="rattle")
      theWidget("data_filechooserbutton")$setFilename(filename)

      # Make sure we end up with a URI since a URI is otherwise used
      # when retrieving the information from the filechooserbutton
      # widget. If we don't do this then the crs$dwd does not include
      # the "file://" bit, and thus changedDataTab returns TRUE the
      # next time, which is not right! 080520 TODO This may not work
      # for MS/Windows.

      filename <- paste("file://", filename, sep="")
      
      # 080713 We still need the events flush with tootiphack set
      # since otherwise we have to lose focus before the screen gets
      # updated.
      
      while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)

      #gtkmainquit_handler(NULL, NULL)
      #gtkmain_handler(NULL, NULL)
    }
  }
  else
    filename <- URLdecode(filename)

  crs$dwd <<- dirname(filename)
  crs$mtime <<- urlModTime(filename)
  
  # If there is a model warn about losing it.

  if (! overwriteModel()) return(FALSE)

  # Fix filename for MS - otherwise eval/parse strip the \\.

  if (isWindows()) filename <- gsub("\\\\", "/", filename)

  # Get the separator to use.

  sep = theWidget("data_separator_entry")$getText()
  if (sep != ",")
    sep <- sprintf(', sep="%s"', sep)
  else
    sep <- ""

  # Check whether we expect a header or not.

  if (theWidget("data_header_checkbutton")$getActive())
    hdr <- ""
  else
    hdr <- ", header=FALSE"
  
  nastring <- ', na.strings=c(".", "NA", "", "?")'
  
  # Generate commands to read the data.

  read.cmd <- sprintf('crs$dataset <<- read.csv("%s"%s%s%s)',
                      filename, hdr, sep, nastring)
  
  # Start logging and executing the R code.

  startLog()
  
  appendLog("LOAD CSV FILE", gsub('<<-', '<-', read.cmd))
  resetRattle()
  result <- try(eval(parse(text=read.cmd)), silent=TRUE)
  if (inherits(result, "try-error"))
  {
    if (any(grep("cannot open the connection", result)))
    {
      errorDialog("The file you specified could not be found:\n\n  ",
                  filename, "\n\nPlease check the filename and try again.")
      return(FALSE)
    }
    else if (any(grep("no lines available in input", result)))
    {
      errorDialog("The file you specified:\n\n\t",
                  filename, "\n\nis empty. ",
                  "Please check the filename and try again.")
      return(FALSE)
    }
    else
      errorReport(read.cmd, result)
  }
    
  crs$dataname <<- basename(filename)
  setRattleTitle(crs$dataname)

  # Update the Data Tab Treeview and Samples.

##  resetVariableRoles(colnames(crs$dataset), nrow(crs$dataset)) 

  # Enable the Data View and Edit buttons.

##  showDataViewButtons()
  
  setStatusBar("The CSV file has been loaded:", crs$dataname)

  return(TRUE)
}

########################################################################
# OLD DATA TAB STUFF MIGRATING TO THE ABOVE
#

on_data_view_button_clicked <- function(button)
{
  viewData()
}

on_data_edit_button_clicked <- function(button)
{
  editData()
}

on_data_filechooserbutton_file_set <- function(button)
{
  # When the filename has been changed on the Data tab check if
  # further action is required. If RData File is active, then load the
  # corresponding .Rdata file and extract the dataset names to be
  # chosen from.

  if (theWidget("data_rdata_radiobutton")$getActive())
  {
    # Collect relevant data

    filename <- theWidget("data_filechooserbutton")$getFilename()
    crs$dwd <<- dirname(filename)
    crs$mtime <<- urlModTime(filename)

    # Fix filename for MS - otherwise eval/parse strip the \\.

    if (isWindows()) filename <- gsub("\\\\", "/", filename)

    # Generate commands to read the data and then display the structure.

    load.cmd <- sprintf('load("%s")', filename)

    # Start logging and executing the R code.

    startLog()

    appendLog("Load an Rdata file containing R objects.", load.cmd)
    set.cursor("watch")
    eval(parse(text=paste("new.objects <- ", load.cmd)), baseenv())
    set.cursor()
    
    # Add new dataframes to the combo box.
    
    combobox <- theWidget("data_name_combobox")
    if (not.null(new.objects))
    {
      combobox$getModel()$clear()
      lapply(new.objects, combobox$appendText)
    }
  
    setStatusBar()
  }
  
}

#-----------------------------------------------------------------------
# DATA LIBRAY
#

# 080522 Migrated this from old interface to new interface. Maybe this
# is now called whenever the Library radio button is activated.
#
# OLD: Update the library combo box with all of the available
# datasets. Can take a little time the first time to generate the
# list. I've associated this with the focus callback, but then it is
# called also when it loses focus!!!

updateDataLibrary <- function()
{
  # OLD: TODO How to tell that this is a "gain focus" action and not a
  # "lose focus" action, since we only want to build the list on
  # gaining focus.

  data.name.combobox <- theWidget("data_name_combobox")
  
  # Record the current selection so that we can keep it as the default.
  
  current <- data.name.combobox$getActiveText()

  ## if (! is.null(current)) return()

  # This could take a little while, so use to watch cursor to indicate
  # we are busy.
  
  set.cursor("watch", "Determining the available datasets from all packages...")
  
  da <- data(package = .packages(all.available = TRUE))
  dl <- sort(paste(da$results[,'Item'], ":", da$results[,'Package'], 
                   ":", da$results[,'Title'], sep=""))
  # Add the entries to the combo box.
  
  if (not.null(dl))
  {
    data.name.combobox$getModel()$clear()
    lapply(dl, data.name.combobox$appendText)
    
    # Set the selection to that which was already selected, if possible.

    if (not.null(current) && current %in% dl)
      data.name.combobox$setActive(which(sapply(dl, function(x) x==current))[1]-1)
  }

  set.cursor(message="")

}

#-----------------------------------------------------------------------

open_odbc_set_combo <- function(button)
{
  # This is a callback for when the ODBC DSN name has changed
  # (associated with "activate").  Load the corresponding tables from
  # the specified ODBC database.

  # Obtain name of the DSN.

  DSNname <- theWidget("data_odbc_dsn_entry")$getText()
  
  # Generate commands to connect to the database and retrieve the tables.

  lib.cmd <- sprintf("require(RODBC, quietly=TRUE)")
  connect.cmd <- sprintf('crs$odbc <<- odbcConnect("%s")', DSNname)
  tables.cmd  <- sprintf('sqlTables(crs$odbc)$TABLE_NAME')
  
  # Start logging and executing the R code.

  if (! packageIsAvailable("RODBC", "connect to an ODBC database")) return(FALSE)
      
  startLog("ODBC CONNECTION")

  appendLog("Require the RODBC library", lib.cmd)
  set.cursor("watch")
  eval(parse(text=lib.cmd))
  set.cursor("")
       
  # Close all currently open channels. This assumes that the user is
  # not openning channelse themselves. Could be a bad choice, but
  # assume we are addressing the usual Rattle user.

  odbcCloseAll()
  
  appendLog("Open the connection to the ODBC service.",
          gsub('<<-', '<-', connect.cmd))
  result <- try(eval(parse(text=connect.cmd)))
  if (inherits(result, "try-error"))
  {
    errorDialog("The attempt to open the ODBC connection failed.",
                "Please check that the DSN is correct.",
                "See the R Console for further details.")
    return(FALSE)
  }
  
  appendLog("Load the names of available tables.", tables.cmd)
  set.cursor("watch")
  result <- try(eval(parse(text=paste("tables <<- ", tables.cmd))))
  set.cursor()
  if (inherits(result, "try-error"))
  {
    errorDialog("The attempt to query the ODBC connection failed.",
                "Please check that the DSN is correct.",
                "See the R Console for further details.")
    return(FALSE)
  }

  # Add list of tables to the combo box.

  combobox <- theWidget("data_odbc_table_combobox")
  if (not.null(tables))
  {
    combobox$getModel()$clear()
    lapply(tables, combobox$appendText)
  }
  
  setStatusBar("ODBC connection to database established. Now select a table.")

  return(TRUE)
}

#----------------------------------------------------------------------
#
# Execution
#
resetVariableRoles <- function(variables, nrows, input=NULL, target=NULL,
                               risk=NULL, ident=NULL, ignore=NULL,
                               zero=NULL, mean=NULL,
                               boxplot=NULL,
                               hisplot=NULL, cumplot=NULL, benplot=NULL,
                               barplot=NULL, dotplot=NULL, mosplot=NULL,
                               resample=TRUE, autoroles=TRUE)
{
  # Update the SELECT treeview with the dataset variables.

  createVariablesModel(variables, input, target, risk, ident, ignore, zero,
                       mean, boxplot, hisplot, cumplot, benplot, barplot,
                       dotplot, mosplot, autoroles=autoroles)

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
    theWidget("sample_checkbutton")$setActive(!exists(".RATTLE.SCORE.IN"))
    theWidget("sample_count_spinbutton")$setRange(1,nrows)
    theWidget("sample_count_spinbutton")$setValue(srows)
    theWidget("sample_percentage_spinbutton")$setValue(per)

    executeSelectSample()
  }

  # Execute the SELECT tab. Changes have bene made and we need to
  # ensure the cached role variables are updated, or else we might see
  # unexpected warnings about changes having been made but not
  # EXECTUEd. [071125]
  
  executeSelectTab()

  # Set the risk label appropriately.
  
  theWidget("evaluate_risk_label")$setText(crs$risk)
}

resetDatasetViews <- function(input, target, risk, ident, ignore)
{
  
  # Reset the treeviews.

  theWidget("select_treeview")$getModel()$clear()
  theWidget("impute_treeview")$getModel()$clear()
  theWidget("categorical_treeview")$getModel()$clear()
  theWidget("continuous_treeview")$getModel()$clear()

  # Recreate the treeviews, setting the roles as provided.

  resetVariableRoles(colnames(crs$dataset), nrow(crs$dataset),
                     input=input, target=target, risk=risk,
                     ident=ident, ignore=ignore,
                     resample=FALSE, autoroles=FALSE)

}

executeDataARFF <- function()
{

  if (R.version$minor < "4.0")
  {
    infoDialog("Support for ARFF is only available in R 2.5.0 and beyond.")
    return(FALSE)
  }

  # Collect relevant data

  filename <- theWidget("data_filechooserbutton")$getUri()

  # If no filename is given then return without doing anything.

  if (is.null(filename))
  {
    errorDialog("No ARFF Filename has been chosen yet.",
                "You must choose one before execution.")
    return(FALSE)
  }

  filename <- URLdecode(filename)
  
  crs$dwd <<- dirname(filename)
  crs$mtime <<- urlModTime(filename)

  # We need the foreign package to read ARFF data.
  
  if (! packageIsAvailable("foreign", "read an ARFF dataset")) return(FALSE)
  lib.cmd <- "require(foreign, quietly=TRUE)"
  
  # If there is a model warn about losing it.

  if (! overwriteModel()) return(FALSE)

  # Fix filename for MS - otherwise eval/parse strip the \\.

  if (isWindows()) filename <- gsub("\\\\", "/", filename)

  # Generate commands to read the data and then display the structure.

  read.cmd <- sprintf('crs$dataset <<- read.arff("%s")', filename)
  str.cmd  <- "str(crs$dataset)"
  
  # Start logging and executing the R code.

  startLog()
  ##theWidget(TV)$setWrapMode("none") # On for welcome msg
  ##resetTextview(TV)
  
  appendLog("The foreign package provides a function to read arff.", lib.cmd)
  eval(parse(text=lib.cmd))

  appendLog("LOAD ARFF FILE", gsub('<<-', '<-', read.cmd))
  resetRattle()
  eval(parse(text=read.cmd))
  crs$dataname <<- basename(filename)
  setRattleTitle(crs$dataname)

  appendLog("Display a simple summary (structure) of the dataset.", str.cmd)
  ##appendTextview(TV, sprintf("Structure of %s.\n\n", filename),
  ##                collectOutput(str.cmd))
  
  ## Update the select treeview and samples.

##  resetVariableRoles(colnames(crs$dataset), nrow(crs$dataset)) 

  # Enable the Data View button.

##  showDataViewButtons()
  
  setStatusBar("The ARFF data has been loaded:", crs$dataname)

  return(TRUE)
}

executeDataODBC <- function()
{

  # Retrieve information. Note that there is no standard LIMIT option
  # in SQL, but it is LIMIT in Teradata, so perhaps we go with that
  # for now?
  
  dsn.name <- theWidget("data_odbc_dsn_entry")$getText()
  table <- theWidget("data_odbc_table_combobox")$getActiveText()
  row.limit <- theWidget("data_odbc_limit_spinbutton")$getValue()
  believe.nrows <- theWidget("data_odbc_believeNRows_checkbutton")$getActive()
  sql.query <- "" # theWidget("odbc_sql_entry")$getText()
  
  # If the ODBC channel has not been openned, then tell the user how
  # to do so.

  if (class(crs$odbc) != "RODBC")
  {
    errorDialog("A connection to an ODBC data source name (DSN) has not been",
                "established.",
                "Please enter the DSN and press Enter.",
                "This will also populate the list of tables to choose from.",
                "After establishing the connection you can choose a table",
                "or else enter a specific SQL query to retrieve a dataset.")
    return(FALSE)
  }
  
  # Error if no table from the database has been chosen.
  
  if (sql.query == "" && is.null(table))
  {
    errorDialog("No table nor SQL query has been specified.",
                "Please identify the name of the table you wish to load.",
                "All tables in the connected database are listed",
                "once a connection is made.",
                "\n\nAlternatively, enter a query to retrieve a dataset.")
    return(FALSE)
  }

  # If there is a model warn about losing it.

  if (! overwriteModel()) return(FALSE)

  if (sql.query != "")
    sql <- sql.query
  else
  {
    sql <- sprintf("SELECT * FROM %s", table)
    if (row.limit > 0) sql <- paste(sql, "LIMIT", row.limit)
  }
  
  #assign.cmd <- "crs$dataset <<- sqlFetch(crs$odbc, table)"
  assign.cmd <- paste("crs$dataset <<- sqlQuery(crs$odbc, ", '"', sql, '"',
                      ifelse(believe.nrows, "", ", believeNRows=FALSE"),
                      ")", sep="")
  str.cmd  <- "str(crs$dataset)"

  if (row.limit == 0)
  {
    ## Double check with the user if we are abuot to extract a large
    ## number of rows.
    
    numRows <- sqlQuery(crs$odbc, sprintf("SELECT count(*) FROM %s", table))
    if (numRows > 50000)
      if (! questionDialog("You are about to extract", numRows,
                           "rows from the table", table,
                           "of the", dsn.name, "ODBC connection.",
                           "That's quite a few to load into memory.",
                           "\n\nDo you wish to continue?"))
        return()
  }
  
  ## Start logging and executing the R code.

  startLog()
  appendLog("LOAD FROM DATABASE TABLE",
           gsub('<<-', '<-', assign.cmd))
  resetRattle()
  eval(parse(text=assign.cmd))
  crs$dataname <<- table
  setRattleTitle(crs$dataname)

  appendLog("Display a simple summary (structure) of the dataset.", str.cmd)
  
  ## Update the select treeview and samples.
  
##  resetVariableRoles(colnames(crs$dataset), nrow(crs$dataset)) 
  
  # Enable the Data View button.

##  showDataViewButtons()
  
  setStatusBar("The ODBC data has been loaded:", crs$dataname)

  return(TRUE)
}

executeDataRdata <- function()
{
  
  # Collect relevant data.

  filename <- theWidget("data_filechooserbutton")$getFilename()
  dataset <- theWidget("data_name_combobox")$getActiveText()

  # Error exit if no filename is given.

  if (is.null(filename))
  {
    errorDialog("No Rdata Filename has been chosen yet.",
                 "You must choose one before execution.")
    return(FALSE)
  }

  crs$dwd <<- dirname(filename)
  crs$mtime <<- urlModTime(filename)

  # Error if no dataset from the Rdata file has been chosen.
  
  if (is.null(dataset))
  {
    errorDialog("No R dataset name has been specified.",
                "Please identify the name of the R dataset.",
                "Any data frames that were found in the loaded Rdata",
                "file are available to choose from in the Data Name",
                "combo box.")
    return(FALSE)
  }

  # If there is a model warn about losing it.

  if (! overwriteModel()) return(FALSE)

  # Generate commands.
  
  assign.cmd <- sprintf('crs$dataset <<- %s', dataset)
  str.cmd  <- "str(crs$dataset)"
  
  # Start logging and executing the R code.

  startLog()
  
  appendLog("LOAD RDATA FILE", gsub('<<-', '<-', assign.cmd))
  resetRattle()
  eval(parse(text=assign.cmd))
  crs$dataname <<- dataset
  setRattleTitle(crs$dataname)

  setStatusBar("The data has been loaded:", crs$dataname)

  return(TRUE)
}

executeDataRdataset <- function()
{
  
  # Collect relevant data

  dataset <- theWidget("data_name_combobox")$getActiveText()

  # 080907 Can we do this here each time? I haven't work out a way to
  # update the combobox when it is clicked - this is what would be
  # best! But at least having it in here means we can update it when
  # it is executed.
  
  updateRDatasets()
  
  if (is.null(dataset))
  {
    errorDialog("No R dataset name has been specified.",
                "Please identify the name of the R dataset.",
                "Any data frames that exist in the R Console",
                "are available to choose from in the Data Name",
                "combo box.")
    return(FALSE)
  }

  # If there is a model then warn about losing it.

  if (! overwriteModel()) return(FALSE)

  # Generate commands.

  assign.cmd <- sprintf('crs$dataset <<- %s', dataset)
  str.cmd <- "str(crs$dataset)"

  # Start logging and executing the R code.

  startLog()
  #theWidget(TV)$setWrapMode("none") # On for welcome msg
  #resetTextview(TV)
  
  appendLog("LOAD R DATA FRAME", gsub('<<-', '<-', assign.cmd))
  resetRattle()
  eval(parse(text=assign.cmd))
  crs$dataname <<- dataset
  setRattleTitle(crs$dataname)

  # 080328 Fix up any non-supported characters in the column names,
  # otherwise they cause problems, e.g. "a-b" when used as ds$a-b is
  # interpreted as (ds$a - b)!
  
  names(crs$dataset) <<- make.names(names(crs$dataset))

  appendLog("Display a simple summary (structure) of the dataset.", str.cmd)

  setStatusBar("The R dataset is now available.")

  return(TRUE)
}

executeDataLibrary <- function()
{
  # 080521 Load a dataset from a particular R package.
  
  # Collect relevant data.
  
  dataset <- theWidget("data_name_combobox")$getActiveText()

  if (is.null(dataset))
  {
    errorDialog("No dataset from the R libraries has been specified.",
                "\n\nPlease identify the name of the dataset",
                "you wish to load using the Data Name chooser.")
    return(FALSE)
  }

  # Actual dataset name as known when loaded.
  
  adsname <- gsub('([^ :]*).*$', '\\1', unlist(strsplit(dataset, ":"))[1])

  # Some datasets are loaded through loading another name (which
  # appears in parentheses. Extract the actual name of the dataset
  # that has to be named to be loaded.
  
  dsname <- gsub('.* \\((.*)\\)$', '\\1', unlist(strsplit(dataset, ":"))[1])

  # Extract the name of the package from which the dataset is loaded.

  dspkg <- unlist(strsplit(dataset, ":"))[2]

  # If there is a model then warn about losing it.

  if (! overwriteModel()) return()

  # Generate commands.

  assign.cmd <- sprintf(paste('data(list = "%s", package = "%s")\n',
                              'crs$dataset <<- %s', sep=""),
                        dsname, dspkg, adsname)
  
  # Start logging and executing the R code.

  startLog()
  
  appendLog("LOAD R DATASET", gsub('<<-', '<-', assign.cmd))
  resetRattle()
  eval(parse(text=assign.cmd))
  if (class(crs$dataset) != "data.frame")
  {
    errorDialog(sprintf("The selected dataset, '%s', from the '%s' package",
                        adsname, dspkg),
                "is not of class data frame (the data type).",
                sprintf("Its data class is '%s.'", class(crs$dataset)),
                "This is not currently supported by", crv$appname,
                "and so it  can not be loaded. Perhaps choose a different",
                "dataset from the library.")
    return(FALSE)
  }
  
  crs$dataname <<- adsname
  crs$datapkg <<- dspkg
  setRattleTitle(crs$dataname)
  
  setStatusBar("The R package data is now available.")

  return(TRUE)
}

executeDataEntry <- function()
{
  # 080523 The DATA ENTRY option has been removed and so we should
  # remove this function sometime soon.
  
  # Check if there is a model first and then warn about losing it.

  if (! overwriteModel()) return()

  ## Generate commands.

  assign.cmd <- paste('crs$dataset <<- data.frame()',
                      'crs$dataset <<- edit(crs$dataset)', sep="\n")
  str.cmd <- "str(crs$dataset)"
  
  ## Start logging and executing the R code.

  startLog()
  theWidget(TV)$setWrapMode("none") # On for welcome msg
  resetTextview(TV)
  
  appendLog("ENTER A DATA SET MANUALLY",
          gsub('<<-', '<-', assign.cmd))
  resetRattle()
  eval(parse(text=assign.cmd))
  crs$dataname <<- "dataset"
  setRattleTitle(crs$dataname)
  
  appendLog("Display a simple summary (structure) of the dataset.", str.cmd)
  setTextview(TV, sprintf("Structure of %s.\n\n", crs$dataset),
               collectOutput(str.cmd), sep="")

  ## Update the select treeview and samples.

##  resetVariableRoles(colnames(crs$dataset), nrow(crs$dataset)) 

  # Enable the Data View button.

##  showDataViewButtons()
  
  setStatusBar("The provided data is now available.")
}

viewData <- function()
{
  result <- try(etc <- file.path(.path.package(package="rattle")[1], "etc"),
                silent=TRUE)
  if (inherits(result, "try-error"))
    viewdataGUI <- gladeXMLNew("rattle.glade", root="viewdata_window")
  else
    viewdataGUI <- gladeXMLNew(file.path(etc,"rattle.glade"),
                               root="viewdata_window")
  gladeXMLSignalAutoconnect(viewdataGUI)
  tv <- viewdataGUI$getWidget("viewdata_textview")
  tv$modifyFont(pangoFontDescriptionFromString("monospace 10"))
  op <- options(width=10000)
  tv$getBuffer()$setText(collectOutput("print(crs$dataset)"))
  options(op)
  ## For IBI viewdataGUI$getWidget("viewdata_window")$setTitle("Fred")
}
    
editData <- function()
{
  
  # Check if there is a model first and then warn about losing it.

  if (! overwriteModel()) return()

  # Generate commands.

  assign.cmd <- 'crs$dataset <<- edit(crs$dataset)'
  str.cmd <- "str(crs$dataset)"
  
  # Start logging and executing the R code.

  startLog()
  ##theWidget(TV)$setWrapMode("none") # On for welcome msg
  ##resetTextview(TV)
  
  appendLog("EDIT A DATA SET MANUALLY", gsub('<<-', '<-', assign.cmd))
  
  # These are needed because resetRattle clears everything

  ds <- crs$dataset
  # TODO fn <- theWidget("data_filechooserbutton")$getFilename()
  
  resetRattle()
  crs$dataset <<- ds
  eval(parse(text=assign.cmd))

  crs$dataname <<- "dataset"
  # TODO fn <- theWidget("data_filechooserbutton")$getValue()


  setRattleTitle(crs$dataname)
  
  appendLog("Display a simple summary (structure) of the dataset.", str.cmd)
  ##setTextview(TV, sprintf("Structure of %s.\n\n", crs$dataset),
  ##             collectOutput(str.cmd), sep="")

  # Update the select treeview and samples.

  ## resetVariableRoles(colnames(crs$dataset), nrow(crs$dataset)) 
  createVariablesModel(colnames(crs$dataset)) 

  # Enable the Data View button.

##  showDataViewButtons()
  
  setStatusBar("The supplied data is now available.")

}

exportDataTab <- function()
{
  # Obtain filename to write the dataaset to.
  
  dialog <- gtkFileChooserDialog("Export Dataset", NULL, "save",
                                 "gtk-cancel", GtkResponseType["cancel"],
                                 "gtk-save", GtkResponseType["accept"])

  if(not.null(crs$dataname))
    dialog$setCurrentName(paste(get.stem(crs$dataname), "_saved", sep=""))

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
    if (! questionDialog("The data file", save.name,
                         "already exists. Are you sure you want to overwrite",
                         "this file?"))
      return()

  write.csv(crs$dataset, save.name, row.names=FALSE)

  setStatusBar("The dataset has been exported to", save.name)

}  

########################################################################
# DATA ROLES
#
# The DATA Execute will perform a sampling of the data and stores
# the indicies in crs$sample. It will also build the list of variable
# roles and stores these in crs$input, crs$ident, crs$ignore,
# crs$target, and crs$risk. This is then used in MODEL to limit the
# dataset in the call to rpart to just the crs$input variables.  In
# EVALUATE the crs$risk is used for the Risk Chart.

#------------------------------------------------------------------------
# Interface

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
    theWidget("explore_sample_label")$show()
    theWidget("explore_vseparator")$show()
  }
  else
  {
    theWidget("sample_percentage_spinbutton")$setSensitive(FALSE)
    theWidget("sample_percentage_label")$setSensitive(FALSE)
    theWidget("sample_count_spinbutton")$setSensitive(FALSE)
    theWidget("sample_count_label")$setSensitive(FALSE)
    theWidget("sample_seed_spinbutton")$setSensitive(FALSE)
    theWidget("sample_seed_button")$setSensitive(FALSE)
    theWidget("explore_sample_label")$hide()
    theWidget("explore_vseparator")$hide()
  }
  crs$sample <<- NULL
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
  # Set the ignore flag for all selected variables, and ensure all
  # other roles are unchecked.

  #ptm <- proc.time()
  set.cursor("watch")
  tree.selection <- theWidget("select_treeview")$getSelection()

  # Under MS/Windows with Terminal Services to the host we get very
  # slow redraws? Tried fixing it with freezeUpdates and thawUpdates
  # but it had no impact. Changing 500 variables takes 5
  # seconds. When connected over terminal services the elapsed time
  # is 16 seconds, still with 5 seconds user time.
  
  # theWidget("rattle_window")$getWindow()$freezeUpdates()

  # Use the data parameter to avoid an RGtk2 bug in 2.12.1, fixed in
  # next release. 071113
  tree.selection$selectedForeach(function(model, path, iter, data)
  {
    model$set(iter, .COLUMN[["ignore"]], TRUE)

    columns <- setdiff(.COLUMN[["input"]]:.COLUMN[["ignore"]],
                       .COLUMN[["ignore"]])

    # Timing indicates the for loop is slower on GNU/Linux but faster
    # on MS/Windows 500! But the extra test also slows things down,
    # so best not to conditionalise for now.

    #if (isWindows())
      for (c in columns)
        if (model$get(iter, c)[[1]]) model$set(iter, c, FALSE)
    #else
    #  lapply(columns, function(x) model$set(iter, x, FALSE))

    return(FALSE) # Keep going through all rows
  }, data=TRUE)

  #cat("->Ig", proc.time() - ptm, "\n")
  set.cursor()

  # theWidget("rattle_window")$getWindow()$thawUpdates()
}

on_variables_toggle_input_button_clicked <- function(action, window)
{
  # Set the input flag for all selected variables within the Select
  # tab, and ensure all other roles for these variables are unchecked.

  #ptm <- proc.time()
  set.cursor("watch")

  treeview <- theWidget("select_treeview")
  tree.selection <- treeview$getSelection()
  #theWidget("rattle_window")$getWindow()$freezeUpdates()

  # Use the data parameter to avoid an RGtk2 bug in 2.12.1, fixed in
  # next release. 071113
  tree.selection$selectedForeach(function(model, path, iter, data)
  {
    model$set(iter, .COLUMN[["input"]], TRUE)
    columns <- setdiff(.COLUMN[["input"]]:.COLUMN[["ignore"]],
                       .COLUMN[["input"]])

    #if (isWindows())
      for (c in columns)
        if (model$get(iter, c)[[1]]) model$set(iter, c, FALSE)
    #else
    #  lapply(columns, function(x) model$set(iter, x, FALSE))

    return(FALSE) # Keep going through all rows
  }, data=TRUE)

  #cat("->In", proc.time() - ptm, "\n")
  set.cursor()
  #theWidget("rattle_window")$getWindow()$thawUpdates()
}

#----------------------------------------------------------------------
#
# Execution
#

executeSelectTab <- function()
{
  # 080520 TODO May want to rename this as SELECT is no longer a tab
  # but is not part of the DATA tab. Perhaps we call it
  # resetSelections.
  
  # Check for pre-requisites.
  
  # Can not do any preparation if there is no dataset.

  if (noDatasetLoaded()) return()

  executeSelectSample()

  input   <- getSelectedVariables("input")
  target  <- getSelectedVariables("target")
  risk    <- getSelectedVariables("risk")
  ident   <- getSelectedVariables("ident")
  ignore  <- getSelectedVariables("ignore")
  weights <- theWidget("weight_entry")$getText()
  if (weights == "") weights <- NULL
  
  # Fail if there is more than one target.

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
  
  # Ask if the Target does not look like a target.

  if (not.null(target))
    target.levels <- length(levels(as.factor(crs$dataset[[target]])))
  else
    target.levels <- 0

  # Fail if there is more than one risk.

  if (length(risk) > 1)
  {
    errorDialog("More than a single risk column has been identified (",
                 paste(sprintf("%s:%s",
                               getSelectedVariables("risk", FALSE),
                               risk), collapse=" "),
                 "). \n\nOnly a single risk column is allowed.",
                 sep="")
    return()
  }

  # Fail if the Risk column is not numeric.

  if (not.null(risk) && ! is.numeric(crs$dataset[[risk]]))
  {
    errorDialog("The column selected for your risk",
                 sprintf("(%s)", crs$dataset[[risk]]),
                 "is not numeric.",
                "\n\nPlease select a numeric column as your Risk vairable.")
    return()
  }

  # Obtain a list of variables and R functions in the Weight Calculator

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
      # Check for any missing variables

      if (identifiers[vars][i] %notin% allvars)
      {
        errorDialog("The Weight Calculator contains the variable",
                     identifiers[vars][i], "which is not known in the",
                     "dataset.")
        return()
      }

      # Check if Weight variables are not ignored, and inform user if not

      if (identifiers[vars][i] %notin%
                        union(ident, union(target, union(ignore, risk))))
      {
        infoDialog("You have used the variable",
                    identifiers[vars][i],
                    "in the weights formula but it is an input.",
                    "This is unusual since it is both an input variable",
                    "and used to weight the outputs.",
                    "It is suggested that you ignore the variable.")
      }
      
      # For each Weights variable, replace with full reference to
      # crs$dataset, since the variable is ignored.

      weights <- gsub(identifiers[vars][i],
                      sprintf("crs$dataset$%s", identifiers[vars][i]),
                      weights)
    
    }
  }
  
  # Record appropriate information.
  
  crs$input   <<- input
  crs$target  <<- target
  crs$risk    <<- risk
  crs$ident   <<- ident
  crs$ignore  <<- ignore
  crs$weights <<- weights
  
  # Update MODEL targets

  the.target <- sprintf("Target: %s", ifelse(is.null(target),
                                             "None", target))

  theWidget("explot_target_label")$setText(the.target)

  theWidget("rpart_target_label")$setText(the.target)
  theWidget("rf_target_label")$setText(the.target)
  theWidget("svm_target_label")$setText(the.target)
  # theWidget("gbm_target_label")$setText(the.target)
  theWidget("ada_target_label")$setText(the.target)
  theWidget("glm_target_label")$setText(the.target)
  theWidget("nnet_target_label")$setText(the.target)

  # Update MODEL weights

  if (not.null(weights))
  {
    weights.display <- gsub('crs\\$dataset\\$', '', weights)
    the.weight <- sprintf("Weights: %s", weights.display)
    # 080815 Just display Weights if there is a weights value, and
    # empty otherwise.
    # theWidget("model_tree_rpart_weights_label")$setText(the.weight)
    theWidget("model_tree_rpart_weights_label")$setText("Weights in use.")
  }
  else
  {
    theWidget("model_tree_rpart_weights_label")$
    setText("")
  }    

  # 080413 Update MODEL types that are available.

  # With more than two classes we can't use AdaBoost since the current
  # package does not support more than 2 classes.

  if (categoricTarget() && target.levels <= 2)
    theWidget("boost_radiobutton")$setSensitive(TRUE)
  else
    theWidget("boost_radiobutton")$setSensitive(FALSE)
  
  # Update various MODEL options

  if (categoricTarget())
  {
    theWidget("rpart_radiobutton")$setSensitive(TRUE)
    theWidget("rf_radiobutton")$setSensitive(TRUE)
    theWidget("svm_radiobutton")$setSensitive(TRUE)

    theWidget("model_linear_radiobutton")$setSensitive(TRUE)

    # Always sensitive? theWidget("all_models_radiobutton")$setSensitive(TRUE)


    # For linear models, if it is categoric and binomial then assume
    # logistic regression (default to binmoial distribution and the
    # logit link function) otherwise it is multinomial so assume
    # poisson regression (default o poisson distribution and log link
    # function).

    if (binomialTarget())
    {
      theWidget("model_linear_builder_label")$setText("glm (Logistic)")
      theWidget("glm_linear_radiobutton")$setSensitive(FALSE)
      theWidget("glm_gaussian_radiobutton")$setSensitive(FALSE)
      theWidget("glm_logistic_radiobutton")$setSensitive(TRUE)
      theWidget("glm_logistic_radiobutton")$setActive(TRUE)
      theWidget("model_linear_probit_radiobutton")$setSensitive(TRUE)
      theWidget("glm_multinomial_radiobutton")$setSensitive(FALSE)

      theWidget("nnet_radiobutton")$setSensitive(TRUE)
      # I don't think these need tgo be done. We can't see the options
    # when the nnet button is not sensitive
    #theWidget("nnet_hidden_nodes_label")$setSensitive(FALSE)
    #theWidget("nnet_hidden_nodes_spinbutton")$setSensitive(FALSE)
      theWidget("nnet_builder_label")$setText("nnet (0/1)")

    }
    else
    {
      theWidget("model_linear_builder_label")$setText("multinom")
      theWidget("glm_linear_radiobutton")$setSensitive(FALSE)
      theWidget("glm_gaussian_radiobutton")$setSensitive(FALSE)
      theWidget("glm_logistic_radiobutton")$setSensitive(FALSE)
      theWidget("model_linear_probit_radiobutton")$setSensitive(FALSE)
      theWidget("glm_multinomial_radiobutton")$setSensitive(TRUE)
      theWidget("glm_multinomial_radiobutton")$setActive(TRUE)

      theWidget("nnet_radiobutton")$setSensitive(FALSE)
      # I don't think these need tgo be done. We can't see the options
      # when the nnet button is not sensitive
      #theWidget("nnet_hidden_nodes_label")$setSensitive(FALSE)
      #theWidget("nnet_hidden_nodes_spinbutton")$setSensitive(FALSE)
      #theWidget("nnet_builder_label")$setText("")
    }
  }
  else if (numericTarget())
  {
    theWidget("rpart_radiobutton")$setSensitive(TRUE)
    theWidget("rf_radiobutton")$setSensitive(FALSE)
    theWidget("svm_radiobutton")$setSensitive(FALSE)

    # For linear models, if it is numeric we are probably going to use
    # a lm so set the default family to nothing! This is becasue lm
    # simply does gaussian and an identity link function.

#    theWidget("glm_family_comboboxentry")$setActive(0)

    theWidget("model_linear_radiobutton")$setSensitive(TRUE)
    theWidget("model_linear_builder_label")$setText("lm")
    theWidget("glm_linear_radiobutton")$setSensitive(TRUE)
    theWidget("glm_linear_radiobutton")$setActive(TRUE)
    theWidget("glm_gaussian_radiobutton")$setSensitive(TRUE)
    theWidget("glm_logistic_radiobutton")$setSensitive(FALSE)
    theWidget("model_linear_probit_radiobutton")$setSensitive(FALSE)
    theWidget("glm_multinomial_radiobutton")$setSensitive(FALSE)

    theWidget("nnet_radiobutton")$setSensitive(TRUE)
    theWidget("nnet_hidden_nodes_label")$setSensitive(TRUE)
    theWidget("nnet_hidden_nodes_spinbutton")$setSensitive(TRUE)
    theWidget("nnet_builder_label")$setText("nnet (Regression)")


    # Always sensitive? theWidget("all_models_radiobutton")$setSensitive(TRUE)

  }
  else # What else could it be? No target!
  {
    theWidget("rpart_radiobutton")$setSensitive(FALSE)
    theWidget("rf_radiobutton")$setSensitive(FALSE)
    theWidget("svm_radiobutton")$setSensitive(FALSE)
    theWidget("model_linear_radiobutton")$setSensitive(FALSE)
    theWidget("nnet_radiobutton")$setSensitive(FALSE)
    theWidget("all_models_radiobutton")$setSensitive(FALSE)
    theWidget("nnet_hidden_nodes_label")$setSensitive(FALSE)
    theWidget("nnet_hidden_nodes_spinbutton")$setSensitive(FALSE)
    # 080719 - remove, or else we can't sample and cluster!!
    # theWidget("sample_checkbutton")$setActive(FALSE)
    theWidget("glm_linear_radiobutton")$setSensitive(FALSE)
    theWidget("glm_gaussian_radiobutton")$setSensitive(FALSE)
    theWidget("glm_logistic_radiobutton")$setSensitive(FALSE)
    theWidget("model_linear_probit_radiobutton")$setSensitive(FALSE)
    theWidget("glm_multinomial_radiobutton")$setSensitive(FALSE)
  }
  
  # Update EVALUATE risk variable
  
  theWidget("evaluate_risk_label")$setText(crs$risk)

  # Update defaults that rely on the number of variables.
  
  .RF.MTRY.DEFAULT <<- floor(sqrt(length(crs$input)))
  theWidget("rf_mtry_spinbutton")$setValue(.RF.MTRY.DEFAULT)
  
  # 080505 We auto decide whether the target looks like a categorical
  # or numeric, but if it ends up being a categoric (the user
  # overrides with the type radio button) with vary many classes,
  # then complain!
  
  if (not.null(target)
      && categoricTarget()
      && target.levels > 10)
  {
    if (! questionDialog("The column selected as a Target",
                         sprintf("(%s)", target),
                         "will be treated as a categorical variable",
                         "since Target Type is set to Categoric.",
                         "\n\nThe variable has more than 10 distinct",
                         "values",
                         sprintf("(%d in fact).", target.levels),
                         "That is unusual and some model builders will",
                         "take a long time.\n\nConsider using fewer",
                         "classes for the target categorical variable",
                         "or select Target Type as Numeric.",
                         "\n\nDo you want to continue anyhow?"))
      return()
  }

  # Finished - update the status bar.
  
  setStatusBar("Variable roles noted.",
               "There are", length(crs$input), "input variables.",
               ifelse(length(crs$target) == 0,
                      paste("NO target thus no predictive modelling enabled",
                            "and no sampling."),
                      paste("Target is", crs$target, "treated as a",
                            ifelse(categoricTarget(),
                                   sprintf("Categoric %d for classification.",
                                           target.levels),
                                   "Numeric for regression."))))
}

executeSelectSample <- function()
{
  # Identify if there are entities without a target value. TODO
  # 080426. I started looking at noting those entities with missing
  # target values. This is recorded in crs$nontargets. Currently I'm
  # not using it. The intention was to only sample from those with
  # targets, etc. But the impacts need to be carefuly thought through.
  #
  # Perhaps the philosophy should go back to the fact that the user
  # can split the dataset up themselves quite easily, and I do
  # provide a mechanism for them to load their dataset for scoring.
  
  #target <- getSelectedVariables("target")
  #print(target)
  #crs$nontargets <<- which(is.na(crs$dataset[[target]]))
  
  # Record that a random sample of the dataset is desired and the
  # random sample itself is loaded into crs$sample. 080425 Whilst we
  # are at it we also set the variable crs$targeted to be those row
  # indicies that have a non NA target.

  if (theWidget("sample_checkbutton")$getActive())
  {
    #ssize <- theWidget("sample_percentage_spinbutton")$getValue()
    #ssize <- floor(nrow(crs$dataset)*ssize/100)
    ssize <- theWidget("sample_count_spinbutton")$getValue()

    seed <- theWidget("sample_seed_spinbutton")$getValue()
    
    sample.cmd <- paste(sprintf("set.seed(%d)\n", seed),
                        "crs$sample <<- sample(nrow(crs$dataset), ", ssize,
                        ")", sep="")

    appendLog("Build a random sample for modelling.",
            gsub("<<-", "<-", sample.cmd))
    eval(parse(text=sample.cmd))

    # When we have sampling, assume the remainder is the test set and
    # so enable the Testing radio button in Evaluate.
    
    theWidget("evaluate_testing_radiobutton")$setSensitive(TRUE)
    theWidget("evaluate_testing_radiobutton")$setActive(TRUE)
  }
  else
  {
    crs$sample <<- NULL

    theWidget("evaluate_testing_radiobutton")$setSensitive(FALSE)
    if (exists(".RATTLE.SCORE.IN"))
      theWidget("evaluate_csv_radiobutton")$setActive(TRUE)
    else
      theWidget("evaluate_training_radiobutton")$setActive(TRUE)
  }

  crs$smodel <<- vector()

  # TODO For test/train, use sample,split from caTools?

  ## Set some defaults that depend on sample size.
  
  #if (is.null(crs$sample))
  #  .RF.SAMPSIZE.DEFAULT <<- length(crs$dataset)
  #else
  #  .RF.SAMPSIZE.DEFAULT <<- length(crs$sample)
  #theWidget("rf_sampsize_spinbutton")$setValue(.RF.SAMPSIZE.DEFAULT)

  ## 080520 Don't set the status bar - it is overwritten by the
  ## message about variable roles being noted.

##  setStatusBar()

##  if (theWidget("sample_checkbutton")$getActive())
##    setStatusBar("The sample has been generated.",
##                  "There are", length(crs$sample), "entities.")
##  else
##    setStatusBar("Sampling is inactive.")
}

getSelectedVariables <- function(role, named=TRUE)
{
  # DESCRIPTION
  # Generate a list of variables marked with the specified role.
  #
  # ARGUMENTS
  # role  = a string naming the role to query on
  # named = if TRUE return variable names as strings, if FALSE, numbers
  #
  # DETAILS The select_treeview, categorical_treeview and
  # continuous_treeview are places where a variable can be identified
  # as having a given role. Whilst the role of "ignore" is common
  # across all three treeviews, only the ignore from the main
  # select_treeview is considered. If a role is not found, simply
  # return NULL, rather than an error (for no particular reason).
  #
  # ASSUMPTIONS The variable and number columns are assumed to be the
  # same in each of .COLUMNS, .CATEGORICAL, and .CONTINUOUS.

  variables <- NULL
  type <- "logical"

  if (role %in% c("input", "target", "risk", "ident", "ignore"))
  {
    model <- theWidget("select_treeview")$getModel()
    rcol  <- .COLUMN[[role]]
  }

  else if (role %in% c("boxplot", "hisplot", "cumplot", "benplot"))
  {
    model <- theWidget("continuous_treeview")$getModel()
    rcol  <- .CONTINUOUS[[role]]
  }

  else if (role %in% c("barplot", "dotplot", "mosplot"))
  {
    model <- theWidget("categorical_treeview")$getModel()
    rcol  <- .CATEGORICAL[[role]]
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
#                  if (type=="character")
#                  {
#                    if (role == "zero" && flag == "Zero/Missing")
#                      variables <<- c(variables, variable)
#                    if (role == "mean" && flag == "Mean")
#                      variables <<- c(variables, variable)
#                    if (role == "median" && flag == "Median")
#                      variables <<- c(variables, variable)
#                  }
#                  else
                    if (flag) variables <<- c(variables, variable)
                  return(FALSE) # Keep going through all rows
                }, TRUE)
  # Set the data parameter to TRUE to avoid an RGtk2 bug in 2.12.1, fixed in
  # next release. 071117

  return(variables)
}

initialiseVariableViews <- function()
{
  # Define the data models for the various treeviews.

  model <- gtkListStoreNew("gchararray", "gchararray", "gchararray",
                           "gboolean", "gboolean", "gboolean", "gboolean",
                           "gboolean", "gchararray")

  impute <- gtkListStoreNew("gchararray", "gchararray", "gchararray")
  
  continuous <- gtkListStoreNew("gchararray", "gchararray",
                                "gboolean", "gboolean",
                                "gboolean", "gboolean", "gchararray")
  
  
  categorical <- gtkListStoreNew("gchararray", "gchararray",
                                 "gboolean", "gboolean", "gboolean",
                                 "gchararray")
  
  
  # View the model through the treeview in the DATA tab

  treeview <- theWidget("select_treeview")
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
  
  # Add the INPUT column.

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

  ## Add the barplot and dotplot and mosplot.

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
  
  renderer <- gtkCellRendererToggleNew()
  renderer$set(xalign = 0.0)
  renderer$set(width = 60)
  renderer$setData("column", .CATEGORICAL["mosplot"])
  connectSignal(renderer, "toggled", cat_toggled, categorical)
  cat.offset <-
    catview$insertColumnWithAttributes(-1,
                                       "Mosaic",
                                       renderer,
                                       active = .CATEGORICAL[["mosplot"]])
  
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
                                 zero=NULL, mean=NULL,
                                 boxplot=NULL,
                                 hisplot=NULL, cumplot=NULL, benplot=NULL,
                                 barplot=NULL, dotplot=NULL, mosplot=NULL,
                                 autoroles=TRUE)
{
  # Set up initial information about variables throughout Rattle,
  # including the Data tab's variable model, the Explore tab's
  # categorical and continuous models, and the Modelling tab defaults
  # where they depend on the dataset sizes.
  #
  # Any values supplied for input, target, risk, ident, ignore,
  # boxplot, hisplot, cumplot, benplot, barplot, dotplot, and
  # mosplot, arguments should be lists of variable names (list of
  # strings).

  # Retrieve the models.
  
  model <- theWidget("select_treeview")$getModel()
  impute <- theWidget("impute_treeview")$getModel()
  categorical <- theWidget("categorical_treeview")$getModel()
  continuous  <- theWidget("continuous_treeview")$getModel()

  # Automatically identify a default target if none are identified as
  # a target (by beginning with TARGET) in the variables
  # (080303). Heuristic is - the last or first if it's a factor with
  # few levels, or has only a few values. Then the treeview model will
  # record this choice, and we set the appropriate labels with this,
  # and record it in crs.

  given.target <- which(substr(variables, 1, 6) == "TARGET")
  if (autoroles && length(given.target) > 0) target <- variables[given.target[1]]
  
  if (autoroles && is.null(target))
  {
    # Find the last variable that is not an IMP (imputed). This is
    # just a general heuristic, and works particularly for imputation
    # performed in Rattle. Should also do this for first, and also for
    # IGNORE variables.
    
    last.var <- length(variables)
    while (last.var > 1 && substr(variables[last.var], 1, 4) == "IMP_")
    {
      last.var <- last.var - 1
    }
    
    target <- -1
    if ((is.factor(crs$dataset[,last.var]) &&
         length(levels(crs$dataset[,last.var])) > 1 &&
         length(levels(crs$dataset[,last.var])) < 5)
        || (length(levels(as.factor(crs$dataset[,last.var]))) < 5
            && length(levels(as.factor(crs$dataset[,last.var]))) > 1))
      target <- last.var
    else if ((is.factor(crs$dataset[,1]) &&
              length(levels(crs$dataset[,1])) > 1 &&
              length(levels(crs$dataset[,1])) < 5)
             || (length(levels(as.factor(crs$dataset[,1]))) < 5
                 && length(levels(as.factor(crs$dataset[,1]))) > 1))
      target <- 1
    else
      for (i in 2:length(variables)-1)
      {
        if ((is.factor(crs$dataset[,i]) &&
             length(levels(crs$dataset[,i])) > 1 &&
              length(levels(crs$dataset[,i])) < 5)
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

  # Determine the list of input variables so far (i.e., not dealing
  # with ignore and risk yet).
  
  if (is.null(input)) input <- variables
  input <- setdiff(input, target)
  
  # Update the Model tab with the selected default target

  the.target <- sprintf("Target: %s", ifelse(is.null(target), "None", target))

  theWidget("explot_target_label")$setText(the.target)

  theWidget("glm_target_label")$setText(the.target)
  theWidget("rpart_target_label")$setText(the.target)
  ## theWidget("gbm_target_label")$setText(the.target)
  theWidget("ada_target_label")$setText(the.target)
  theWidget("rf_target_label")$setText(the.target)
  theWidget("svm_target_label")$setText(the.target)
  theWidget("nnet_target_label")$setText(the.target)

  plots <- union(boxplot,
                 union(hisplot,
                       union(cumplot,
                             union(benplot,
                                   union(barplot,
                                         union(dotplot, mosplot))))))
  
  ## Build the Variables treeview model with each variable's INPUT set
  ## to TRUE and all else FALSE. If the variable has only a single
  ## value then it defaults to IGNORE, and if it is a factor and has
  ## as many distinct values as there are rows, then also default to
  ## IGNORE.

  for (i in 1:length(variables))
  {
    #used <- union(target, union(risk, union(ident, ignore)))
    
    iter <- model$append()$iter

    cl <- class(crs$dataset[[variables[i]]])
    if (length(cl) == 2 && cl[1] == "ordered" && cl[2] == "factor")
    {
      cl <- "factor"
      cl <- "Categorical"
    }
    
    # First check for special variable names. 

    if (autoroles)
    {
      if (paste("IMP_", variables[i], sep="") %in% variables)
      {
        # This works with SAS/EM IMPutations and Rattle's imputations,
        # which add the IMP_ at the beginning of the name of any imputed
        # variables.
        
        ignore <- c(ignore, variables[i])
        
        # Be sure to also remove any other role for the original
        # variable?
      }
      else if (substr(variables[i], 1, 2) == "ID")
      {
        ident <- c(ident, variables[i])
      }
      # No longer needed as this is handled prior to the target
      # heuristics. Remove this code eventually if all looks
      # okay. (080303)
      #
      # else if (substr(variables[i], 1, 6) == "TARGET")
      # {
      #   target <- variables[i]
      # }
      else if (substr(variables[i], 1, 6) == "IGNORE")
      {
        ignore <- c(ignore, variables[i])
      }
      else if (substr(variables[i], 1, 4) == "RISK")
      {
        risk <- c(risk, variables[i])
      }
      else if ("factor" %in% cl)
      {
        lv <- length(levels(crs$dataset[[variables[i]]]))
        if (lv == nrow(crs$dataset))
        {
          cl <- "ident"
          ident <- c(ident, variables[i])
        }
        else if (lv == 1)
        {
          cl <- "constant"
          ignore <- c(ignore, variables[i])
        }
      }
      else
      {
        lv <- length(levels(as.factor(crs$dataset[[variables[i]]])))
        if ("integer" %in% cl && lv == nrow(crs$dataset))
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
    }

    # Fix any doubling up

    input <- setdiff(input, target)
    if (length(target) > 0 && length(ident) > 0 && target %in% ident)
      target <- NULL
    
    # Always change a "factor" to "factor lvls"
    
    if ("factor" %in% cl)
    {
      lv <- length(levels(crs$dataset[[variables[i]]]))
      if (lv > 1)
        cl <- paste(cl, lv)
    }
    
    input <- setdiff(setdiff(setdiff(input, ignore), ident), risk)

    missing.count <- sum(is.na(crs$dataset[[variables[i]]]))

    unique.count <- length(unique(crs$dataset[[variables[i]]]))

    numeric.var <- is.numeric(crs$dataset[[variables[i]]])
    possible.categoric <- (unique.count <= 10 ||
                           theWidget("target_categoric_radiobutton")$
                           getActive())
    
    # Convert internal class to printable form.
    
    prcl <- cl
    prcl <- gsub('factor', 'Categoric', prcl)
    prcl <- gsub('integer', 'Numeric', prcl)
    prcl <- gsub('numeric', 'Numeric', prcl)
    
    # Every variable goes into the VARIABLES treeview.

    model$set(iter,
              .COLUMN["number"], i,
              .COLUMN["variable"], variables[i],
              .COLUMN["type"], prcl,
##              .COLUMN["numeric"], cl %in% c("integer", "numeric"),
##              .COLUMN["categorical"], cl %in% c("factor"),
              .COLUMN["input"], variables[i] %in% input,
              .COLUMN["target"], variables[i] %in% target,
              .COLUMN["risk"], variables[i] %in% risk,
              .COLUMN["ident"], variables[i] %in% ident,
              .COLUMN["ignore"], variables[i] %in% ignore,
              .COLUMN["comment"], paste(ifelse(missing.count > 0,
                                               sprintf("Missing: %d ",
                                                       missing.count), ""),
                                        ifelse(numeric.var,# &&
                                               #possible.categoric,
                                               sprintf("Unique: %d ",
                                                       unique.count), ""),
                                        sep=""))

    # Selected variables go into the other treeviews.

    if (missing.count > -1)# Ignore IGNOREd variables. But crs$ignore
                           # is not yet set. Need to remove
                           # later. Also, this treeview has become
                           # used for all TRANSFORM operations, so
                           # must include all variables, not just ones
                           # with missing values.
    {
      # Generate correct Rattle terminology for the variable class.
      
      dtype <- paste("A ", cl, " variable")
      if (cl == "integer")
        dtype <- sprintf("Integer [%d to %d; mean=%d; median=%d]",
                         min(crs$dataset[[variables[i]]], na.rm=TRUE),
                         max(crs$dataset[[variables[i]]], na.rm=TRUE),
                         as.integer(mean(crs$dataset[[variables[i]]],
                                         na.rm=TRUE)),
                         as.integer(median(crs$dataset[[variables[i]]],
                                         na.rm=TRUE)))
      else if (cl == "numeric")
        dtype <- sprintf("Numeric [%.2f to %.2f; mean=%.2f; median=%.2f]",
                         min(crs$dataset[[variables[i]]], na.rm=TRUE),
                         max(crs$dataset[[variables[i]]], na.rm=TRUE),
                         mean(crs$dataset[[variables[i]]], na.rm=TRUE),
                         median(crs$dataset[[variables[i]]], na.rm=TRUE))
      else if (substr(cl, 1, 6) == "factor")
        dtype <- sprintf("Categorical [%s levels]",
                         length(levels(crs$dataset[[variables[i]]])))

      # Generate text for the missing values bit.

      if (missing.count > 0)
        mtext <- sprintf(" %d missing values", missing.count)
      else
        mtext <- ""
      
      imp.options <- gtkListStoreNew("gchararray")
      imp.options.iter <- imp.options$append()$iter
      imp.options$set(imp.options.iter, 0, "xx")
      combo <- gtkComboBoxNewWithModel(imp.options, 0)
      impiter <- impute$append()$iter
      impute$set(impiter,
                 .IMPUTE["number"], i,
                 .IMPUTE["variable"], variables[i],
                 .IMPUTE["comment"], sprintf("%s%s.",
                                            dtype, mtext))
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
                      .CATEGORICAL["mosplot"], variables[i] %in% mosplot,
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

#----------------------------------------------------------------------
#
# Support
#

getIncludedVariables <- function(numonly=FALSE, listall=FALSE, risk=FALSE, target=TRUE)
{
  # DESCRIPTION
  # Generate a numeric list of variables not ignored.
  #
  # ARGUMENTS
  # numonly = Only include numeric variables
  # listall = Don't simplify a full list to NULL
  # risk =  Include any risk variable in the returned list
  #
  # RETURNS
  # A string of comma separated numbers
  #
  # DETAILS Generates a list of input variable indicies and the
  # target variable index and, optionally, the risk variable index.
  # If the list contains all variables, then return NULL (as the
  # dataset does not then need to be indexed to subset the variables).
  #
  # TODO This last assumption of returning NULL causes problems since we
  # don't know whether this means all variables or no variables!

  fi <- getVariableIndicies(crs$input)
  if (target)
    ti <- getVariableIndicies(crs$target)
  else
    ti <- NULL
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
  # Return, as a comma separated list (as a string) the list of all
  # variable indicies for those that are not ignored. If the list
  # contains all variables except for the ignored variables, then
  # return NULL.

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

getCategoricVariables <- function(type="string")
{
  # Return a list of categoric variables from amongst those with an
  # INPUT role. If type is "names" than return the list of variable
  # names.
  
  include <- NULL
  cats <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.factor))]
  if (length(cats) > 0)
  {
    indicies <- getVariableIndicies(crs$input)
    included <- intersect(cats, indicies)
    if (type=="names")
      include <- names(crs$dataset)[included]
    else
      include <- simplifyNumberList(included)
  }
  return(include)
}

getNumericVariables <- function(type="string")
{
  # Returns a list of numeric variables. 080803 Add support to return
  # a list of indicies rather than the default string that needs to be
  # executed to identfy the indicies.
  
  nums <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.numeric))]
  if (length(nums) > 0)
  {
    indicies <- intersect(nums, getVariableIndicies(crs$input))
    if (type == "string")
      indicies <- simplifyNumberList(indicies)
  }
  else
    indicies <- NULL

 return(indicies)
}

