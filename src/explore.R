# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2009-03-28 12:50:44 Graham Williams>
#
# Implement EXPLORE functionality.
#
# Copyright (c) 2009 Togaware Pty Ltd
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

########################################################################
# EXECUTION

executeExploreTab <- function()
{
  
  # Can not explore the data if there is no dataset.

  if (noDatasetLoaded()) return()

  # 080315 Don't proceed if the variable selections have changed. For
  # example, two targets might have been selected, resulting in a
  # popup, but not yet resolved, and so many of the plots will fail.
  
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
  else if (theWidget("explore_distr_radiobutton")$getActive())
    executeExplorePlot(avdataset)
  else if (theWidget("explore_interactive_radiobutton")$getActive())
    if (theWidget("explore_interactive_latticist_radiobutton")$getActive())
      executeExplorePlaywith(dataset)
    else
      executeExploreGGobi(dataset, crs$dataname)
  else if (theWidget("correlation_radiobutton")$getActive())
    if (theWidget("explore_hiercor_checkbutton")$getActive())
        executeExploreHiercor(ndataset)
    else
    {
      if (theWidget("correlation_na_checkbutton")$getActive())
        executeExploreCorrelation(dataset)
      else
        executeExploreCorrelation(ndataset)
    }
  
# 090328 Remove separate Hier Correlation
# else if (theWidget("hiercor_radiobutton")$getActive())
#    executeExploreHiercor(ndataset)

  else if (theWidget("prcomp_radiobutton")$getActive())
    executeExplorePrcomp(nidataset)
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
    # Find the number of observations with any missing value for the
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
    
    appendLog("SUMMARISE THE DATASET", contents.cmd, "\n", summary.cmd)
    appendTextview(TV,
                   paste("Below is a summary of ",
                         ifelse(use.sample && sampling, "a SAMPLE of ", ""),
                         "the dataset.\n\n", sep=""),
                   "In reading the simple distribution tables the 1st and 3rd Qu.\n",
                   "refer to the first and third quartiles, indicating that 25% of\n",
                   "the observations have values of that variable which are less than\n",
                   "or greater than (respectively) the value listed.\n\n",
                   if (missing > 0)
                   paste("We also note that the data contains", missing, "observations",
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

executeExplorePlot <- function(dataset,
                               boxplots = getSelectedVariables("boxplot"),
                               hisplots = getSelectedVariables("hisplot"),
                               cumplots = getSelectedVariables("cumplot"),
                               benplots = getSelectedVariables("benplot"),
                               barplots = getSelectedVariables("barplot"),
                               dotplots = getSelectedVariables("dotplot"),
                               mosplots = getSelectedVariables("mosplot"))
{
  # Plot the data. The DATASET is a character string that defines the
  # dataset to use. Information about what variables to plot and the
  # kind of plots is obtained from the continuous_treeview and the
  # categorical_treeview which are displayed in the Explore tab's
  # Distribution option. The appropriate plots are displayed. 090323
  # By having the list of varaiables to display for each type of plot
  # set in the parameter list we can call this function to plot
  # variables from the command line, as in using Sweave. The function
  # remains an internal Rattle function though - only for those who
  # know!

  # Obtain the selection of variables.

  # 090323 REMOVE boxplots  <- getSelectedVariables("boxplot")
  nboxplots <- length(boxplots)

  # 090323 REMOVE hisplots <- getSelectedVariables("hisplot")
  nhisplots <- length(hisplots)

  # 090323 REMOVE cumplots <- getSelectedVariables("cumplot")
  # 090323 REMOVE benplots  <- getSelectedVariables("benplot")
  nbenplots <- length(benplots)
  
  # 090323 REMOVE barplots  <- getSelectedVariables("barplot")
  nbarplots <- length(barplots)
  
  # 090323 REMOVE dotplots  <- getSelectedVariables("dotplot")
  ndotplots <- length(dotplots)

  # 090323 REMOVE mosplots  <- getSelectedVariables("mosplot")
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

  if (length(targets) > 10) targets <- NULL

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
    if (length(targets))
      plot.cmd <- paste(plot.cmd, "\n",
                        paste(sprintf(paste('dens <- density(ds[ds$grp=="%s",',
                                            '1], na.rm=TRUE)\n',
                                            'lines(dens$x, dens$y*rs, ',
                                            'type="l", ',
                                            'col=rainbow(%s)[%s])', sep=""),
                                      targets,
                                      length(targets)+1,
                                      seq_along(targets)),
                                      #eval(parse(text=sprintf(cols,
                                      #             length(targets))))),
                              collapse="\n"),
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
        if (length(targets))
        {
          legend.cmd <- sprintf(paste('legend("topright", c(%s),',
                                      'fill=c("black", rainbow(%s)))'),
                                paste(sprintf('"%s"', c("All", targets)),
                                      collapse=","),
                                length(targets)+1)
          appendLog("Add a legend to the plot.", legend.cmd)
          eval(parse(text=legend.cmd))
        }
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
        for (t in seq_along(targets))
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

    if (packageIsAvailable("vcd"))
      cols <- "rainbow_hcl(%d, start = 30, end = 300)"
    else
      cols <- "rainbow(%d)"

    # Calculate the expected distribution according to Benford's Law

    if (digspin == 1)
      expect.cmd <- paste('unlist(lapply(1:9, function(x) log10(1 + 1/x)))')
    # see http://www.mathpages.com/home/kmath302/kmath302.htm
    else if (digspin > 1) 
      expect.cmd <- sprintf(paste('unlist(lapply(0:9, function(x) {sum(log10',
                                  '(1 + 1/(10*(seq(10^(%d-2), ',
                                  '(10^(%d-1))-1)) + x)))}))'),
                            digspin, digspin)

    # Construct the command to plot the distribution.

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
                        ':9, ds[1,], type="b", pch=19, col=',
                        sprintf(cols, 1), ', ',
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
                                     'col=%s[2], pch=19, type="b")\n'),
                               ifelse(digspin==1, 1, 0),
                               ifelse(is.null(target),
                                      sprintf(cols, 2),
                                      sprintf(cols, length(targets)+2))),
                       sep="")
      if (not.null(targets))
        for (i in seq_along(targets))
        {
          plot.cmd <- sprintf(paste('%s\npoints(%d:9, ds[%d,],',
                                   'col=%s[%d], pch=%d, type="b")'),
                             plot.cmd, ifelse(digspin==1, 1, 0), i+2,
                             sprintf(cols, length(targets)+2),
                             i+2, 19)
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
                         'pch=19, col=',
                          sprintf(cols, 1),
                          ', ',
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
                                         'col=%s[%d], pch=19, type="b")\n'),
                                   s+1, sprintf(cols, nbenplots+1), s+1),
                           sep="")
        }
        new.bind.cmd <- paste(substr(new.bind.cmd, 1,
                                     nchar(new.bind.cmd)-7), ")",
                            sep="")
        data.cmd <- paste(data.cmd, ")))", sep="")

        legend.cmd <- sprintf(paste('legend("%s", c(%s), ',
                                   'fill=%s, title="%s")'),
                              ifelse(digspin>2, "botright", "topright"),
                              paste(sprintf('"%s"',
                                            c("Benford", benplots)),
                                    collapse=","),
                              sprintf(cols, nbenplots+1), "Variables")

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
                                         'fill=%s, title="%s")'),
                                    ifelse(digspin>2, "bottomright",
                                           "topright"),
                                    '"Benfords", sizes',
#                                   paste(sprintf('"%s"',
#                                                c("Benford", "All", targets)),
#                                         collapse=","),
                                   sprintf(cols, length(targets)+2), target)
          else
            if (barbutton)
              legend.cmd <- paste('legend("topright", c("Benford", "All"),',
                                 'fill=heat.colors(2))')
            else
              legend.cmd <- paste('legend("topright", c("Benford", "All"), ',
                                  'fill=', sprintf(cols, 2), sep="")
          
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
    naids <- NULL
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
      errorDialog("The data contains only one variable with missing values,",
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

  # Report completion to the user through the Status Bar.
  
  setStatusBar("Hierarchical cluster of correlations plotted.")

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

  # User interface options
  
  svd <- theWidget("explore_prcomp_pr_radiobutton")$getActive()
  
  # Construct the commands.
  
  prcomp.cmd  <- sprintf(paste('pc <<- %s(na.omit(%s),',
                               'scale=TRUE, center=TRUE, tol=0)'),
                         ifelse(svd, "prcomp", "princomp"),
                         dataset)
  print.cmd   <- "pc"
  summary.cmd <- "summary(pc)"
  plot.cmd    <- paste('plot(pc, main="")',
                       genPlotTitleCmd("Principal Components Importance",
                                      crs$dataname),
                       if (svd) # 090328 Add X axis labels for prcomp version.
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
  
  appendLog("PRINCIPAL COMPONENTS ANALYSIS (on numerics only).",
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
# CALLBACKS

#-----------------------------------------------------------------------
# DISPLAY APPRORIATE TAB
#
# When an Option radio button is selected, display the appropriate tab
#
# 090328 Move to using sub tabs to house the various widgets, rather
# than turning them on and off!


on_summary_radiobutton_toggled <- function(button)
{
  if (button$getActive()) crv$EXPLORE$setCurrentPage(crv$EXPLORE.SUMMARY.TAB)
  setStatusBar()
}

on_explore_distr_radiobutton_toggled <- function(button)
{
  if (button$getActive()) crv$EXPLORE$setCurrentPage(crv$EXPLORE.PLOT.TAB)
  setStatusBar()
}

on_explore_interactive_radiobutton_toggled <- function(button)
{
  if (button$getActive()) crv$EXPLORE$setCurrentPage(crv$EXPLORE.INTERACTIVE.TAB)
  setStatusBar()
}

on_correlation_radiobutton_toggled <- function(button)
{
  if (button$getActive()) crv$EXPLORE$setCurrentPage(crv$EXPLORE.CORRELATION.TAB)
  setStatusBar()
}

on_prcomp_radiobutton_toggled <- function(button)
{
  if (button$getActive()) crv$EXPLORE$setCurrentPage(crv$EXPLORE.PRCOMP.TAB)
  setStatusBar()
}

#-----------------------------------------------------------------------
# DISTRIBUTIONS

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
    columns <- crv$CATEGORICAL[["barplot"]]:crv$CATEGORICAL[["mosplot"]]
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
    columns <- crv$CONTINUOUS[["boxplot"]]:crv$CONTINUOUS[["benplot"]]
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

#-----------------------------------------------------------------------
# CORRELATION

on_explore_hiercor_checkbutton_toggled <- function(button)
{
  if (button$getActive())
  {
    theWidget("explore_correlation_ordered_checkbutton")$setSensitive(FALSE)
    theWidget("correlation_na_checkbutton")$setSensitive(FALSE)
  }
  else
  {
    theWidget("explore_correlation_ordered_checkbutton")$setSensitive(TRUE)
    theWidget("correlation_na_checkbutton")$setSensitive(TRUE)
  }
}  


