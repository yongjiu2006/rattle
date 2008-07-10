# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2008-07-10 11:34:37 Graham Williams>
#
# Implement cluster functionality.
#
# Copyright (c) 2008 Togaware Pty Ltd
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
# CALLBACKS

# When a radio button is selected, display the appropriate tab page.

on_kmeans_radiobutton_toggled <- function(button)
{
  if (button$getActive())
  {
    .CLUSTER$setCurrentPage(.CLUSTER.KMEANS.TAB)
    if (not.null(crs$hclust))
      theWidget("kmeans_hclust_centers_checkbutton")$setSensitive(TRUE)
    else
      theWidget("kmeans_hclust_centers_checkbutton")$setSensitive(FALSE)
  }
  setStatusBar()
}

on_hclust_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    .CLUSTER$setCurrentPage(.CLUSTER.HCLUST.TAB)
  setStatusBar()
}

on_kmeans_hclust_centers_checkbutton_toggled <- function(button)
{
  #
  # When the hclust centers checkbutton is on, we should not allow any
  # use of the runs spin button because it does not make sens to do
  # both, nor does kmeansruns support starting points for clusters.
  #
  if (button$getActive())
  {
    theWidget("kmeans_runs_spinbutton")$setSensitive(FALSE)
    theWidget("kmeans_runs_label")$setSensitive(FALSE)
  }
  else
  {
    theWidget("kmeans_runs_spinbutton")$setSensitive(TRUE)
    theWidget("kmeans_runs_label")$setSensitive(TRUE)
  }
}    

on_kmeans_iterate_checkbutton_toggled <- function(button)
{
  #
  # When the iterate checkbutton is on, we should not allow any use of
  # the runs spin button because it does not make sense to do both.
  #
  if (button$getActive())
  {
    theWidget("kmeans_runs_spinbutton")$setSensitive(FALSE)
    theWidget("kmeans_runs_label")$setSensitive(FALSE)
  }
  else
  {
    theWidget("kmeans_runs_spinbutton")$setSensitive(TRUE)
    theWidget("kmeans_runs_label")$setSensitive(TRUE)
  }
}    


########################################################################
# EXECUTION

executeClusterTab <- function()
{
  ## Can not cluster without a dataset.

  if (noDatasetLoaded()) return()

  ## If it looks like the VARIABLES page has not been executed, complain..

  if (variablesHaveChanged()) return()

  ## Check if sampling needs executing.

  if (sampleNeedsExecute()) return()
    
  ## Kmeans only works for numeric data, so identify variables to
  ## include.  Only work with the INPUT/TARGET/RISK
  ## variables. That is, only exclude the IGNORE and IDENT variables.

  include <- getNumericVariables()
  if (length(include) == 0)
  {
    errorDialog("Clusters are currently calculated only for numeric data.",
                "No numeric variables were found in the dataset",
                "from amongst those having an input/target/risk role.")
    return()
  }

  # Dispatch.

  if (theWidget("kmeans_radiobutton")$getActive())
    executeClusterKMeans(include)
  else if (theWidget("hclust_radiobutton")$getActive())
    executeClusterHClust(include)
}

#----------------------------------------------------------------------
#
# KMEANS
#

executeClusterKMeans <- function(include)
{
  TV <- "kmeans_textview"
  sampling  <- not.null(crs$sample)

  # Obtain interface information.
  
  nclust <- theWidget("kmeans_clusters_spinbutton")$getValue()
  seed <- theWidget("kmeans_seed_spinbutton")$getValue()
  nruns <- theWidget("kmeans_runs_spinbutton")$getValue()
  usehclust <- theWidget("kmeans_hclust_centers_checkbutton")$getActive()
  useIterate <- theWidget("kmeans_iterate_checkbutton")$getActive()
  
  startLog("KMEANS CLUSTER")

  # SEED: Log the R command and execute.

  seed.cmd <- sprintf('set.seed(%d)', seed)
  appendLog("Set the seed to get the same clusters each time.", seed.cmd)
  eval(parse(text=seed.cmd))

  # Calculate the centers

  if (usehclust)
    centers <- sprintf("centers.hclust(crs$dataset[%s,%s], crs$hclust, %d)",
                       ifelse(sampling, "crs$sample", ""), include, nclust)
  else
    centers <- nclust
  
  # KMEANS: Log the R command and execute.

  if (! useIterate)
  {
    if (nruns > 1)
    {
      lib.cmd <- "require(fpc, quietly=TRUE)"
      if (! packageIsAvailable("fpc", "kmeans runs")) return()
      appendLog("The kmeansruns functionality is provided by the fpc package.",
                lib.cmd)
      eval(parse(text=lib.cmd))

      kmeans.cmd <- sprintf(paste('crs$kmeans <<-',
                                  'kmeansruns(crs$dataset[%s,%s],',
                                  '%s, runs=%s)'),
                            ifelse(sampling, "crs$sample", ""),
                            include, centers, nruns)
    }
    else
    {
      kmeans.cmd <- sprintf(paste('crs$kmeans <<- kmeans(',
                                  'na.omit(crs$dataset[%s,%s]), %s)', sep=""),
                            ifelse(sampling, "crs$sample", ""),
                            include, centers)
    }
    
    appendLog(sprintf("Generate a kmeans cluster of size %s%s%s.", nclust,
                      ifelse(nruns>1, " choosing the best from ", ""),
                      ifelse(nruns>1, nruns, "")),
              gsub("<<-", "<-", kmeans.cmd))
    start.time <- Sys.time()
    eval(parse(text=kmeans.cmd))
    time.taken <- Sys.time()-start.time

    # SUMMARY: Show the resulting model.

    appendLog("\n\n## REPORT ON CLUSTER CHARACTERISTICS", no.start=TRUE)
    appendLog("Cluster sizes:", "paste(crs$kmeans$size, collapse=' ')")
    appendLog("Cluster centers:", "crs$kmeans$centers")
    appendLog("Within cluster sum of squares:", "crs$kmeans$withinss")
    resetTextview(TV)
    setTextview(TV, "Cluster Sizes\n\n",
                collectOutput("paste(crs$kmeans$size, collapse=' ')", TRUE),
                "\n\nCluster centroids.\n\n",
                collectOutput("crs$kmeans$centers", TRUE),
                "\n\nWithin cluster sum of squares.\n\n",
                collectOutput("crs$kmeans$withinss", TRUE),
                "\n")

    # Ensure the kmeans buttons are now active

    theWidget("kmeans_stats_button")$setSensitive(TRUE)
    theWidget("kmeans_data_plot_button")$setSensitive(TRUE)
    theWidget("kmeans_discriminant_plot_button")$setSensitive(TRUE)
  }
  else # Iterate over the clusters.
  {
    start.time <- Sys.time()
    css <- vector()
    css[1] <- 0
    for (i in 2:nclust)
    {
      kmeans.cmd <- sprintf(paste('crs$kmeans <<-',
                                  'kmeans(na.omit(crs$dataset[%s,%s]), %s)'),
                            ifelse(sampling, "crs$sample", ""), include, i)
      eval(parse(text=seed.cmd))
      eval(parse(text=kmeans.cmd))
      css[i] <- sum(crs$kmeans$withinss)
    }
    time.taken <- Sys.time()-start.time
    resetTextview(TV)
    setTextview(TV, "We have iterated over multiple cluster sizes ",
                "from 2 to ", nclust, " clusters.\n\n",
                "The plot displays the sum(withinss) for each clustering\n",
                "and the change of this from the previous clustering\n",
                "with the plot starting with 3 clusters\n")
    newPlot()
    plot(3:nclust, c(css[3:nclust]), ylim=c(0, max(css[3:nclust])),
         type="b", lty=1, col="blue",
         xlab="Number of Clusters", ylab="Sum of WithinSS",
         main="Sum of WithinSS Over Number of Clusters")
    points(3:nclust, css[2:(nclust-1)]-css[3:nclust],
           type="b", pch=4, lty=2, col="red")
    legend("topright", c("Sum(WithinSS)", "Diff previous Sum(WithinSS)"),
           col=c("blue", "red"), lty=c(1, 2), pch=c(1,4), inset=0.05)
  }

  time.msg <- sprintf("Time taken: %0.2f %s", time.taken,
                      attr(time.taken, "units"))
  addTextview(TV, "\n", time.msg, textviewSeparator())
  appendLog(time.msg)
  setStatusBar("The K Means cluster has been generated.",
               time.msg )
  
}

on_kmeans_seed_button_clicked <- function(button)
{
  rseed <- as.integer(runif(1, 0, 1000000))
  theWidget("kmeans_seed_spinbutton")$setValue(rseed)
}

on_help_kmeans_activate <- function(action, window)
{
  if (showHelpPlus("KMeans is a traditional approach to clustering.
In addition to building a cluster, a discriminant coordinates plot
can be generated, using the package fpc, as a display of the clusters."))
  {
    popupTextviewHelpWindow("kmeans")
    if (packageIsAvailable("fpc", "view documentation for plotcluster"))
    {
      require(fpc, quietly=TRUE)
      popupTextviewHelpWindow("plotcluster")
    }
  }
}

on_kmeans_stats_button_clicked <- function(button)
{
  ## Make sure there is a cluster first.
  
  if (is.null(crs$kmeans))
  {
    errorDialog("E124: Should not be here. Plaes report to",
                "support@togaware.com")
    return()
  }

  ## LIBRARY: Ensure the appropriate package is available for the
  ## plot, and log the R command and execute.
  
  lib.cmd <- "require(fpc, quietly=TRUE)"
  appendLog("The plot functionality is provided by the fpc package.", lib.cmd)
  eval(parse(text=lib.cmd))

  ## Some background information.  Assume we have already built the
  ## cluster, and so we don't need to check so many conditions.

  TV <- "kmeans_textview"
  sampling  <- not.null(crs$sample)

  include <- getNumericVariables()
  if (length(include) == 0)
  {
    errorDialog("Clusters are currently calculated only for numeric data.",
                "No numeric variables were found in the dataset",
                "from amongst those having an input/target/risk role.")
    return()
  }

  # STATS: Log the R command and execute. 080521 TODO Fix a bug by
  # adding the na.omit here (since by default that is done in building
  # the clusters). Not sure if this is generally correct.

  stats.cmd <- sprintf(paste("cluster.stats(dist(na.omit(crs$dataset[%s,%s])),",
                             "crs$kmeans$cluster)\n"),
                       ifelse(sampling, "crs$sample", ""), include)
  appendLog("Generate cluster statistics using the fpc package.", stats.cmd)
  appendTextview(TV, "General cluster statistics:\n\n",
                 collectOutput(stats.cmd, use.print=TRUE))

  setStatusBar("K Means cluster statistics have been generated.")
}

on_kmeans_data_plot_button_clicked <- function(button)
{

  # Make sure there is a cluster first.

  if (is.null(crs$kmeans))
  {
    errorDialog("E132: Should not be here. Please report to",
                "support@togaware.com")
    return()
  }

  # Some background information.  Assume we have already built the
  # cluster, and so we don't need to check so many conditions.

  sampling  <- not.null(crs$sample)
  nums <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.numeric))]
  if (length(nums) > 0)
  {
    indicies <- getVariableIndicies(crs$input)
    include <- simplifyNumberList(intersect(nums, indicies))
  }

  # We can only plot if there is more than a single variable.
  
  if (length(intersect(nums, indicies)) == 1)
  {
    infoDialog("A data plot of the clusters can not be constructed",
               "because there is only one numeric variable available",
               "in the data.")
    return()
  }

  # PLOT: Log the R command and execute. 080521 TODO I've added in
  # na.omit here, since when we cluster the audit data, with missing
  # values for Age we need to ensure the data points correspond to the
  # cluster numbers. Otherwise we get a bad looking plot!!!! But do we
  # always need na.omit. It is not always used on bulding clusters.

  ##  plot.cmd <- sprintf(paste("plot(crs$dataset[%s,%s], ",
  plot.cmd <- sprintf(paste("plot(na.omit(crs$dataset[%s,%s]), ",
                            "col=crs$kmeans$cluster)\n",
                            genPlotTitleCmd(""), sep=""),
                      ifelse(sampling, "crs$sample", ""), include)
  appendLog("Generate a data plot.", plot.cmd)

  set.cursor("watch", "Rendering the plot. Please wait...")
  newPlot()
  eval(parse(text=plot.cmd))
  set.cursor("left-ptr", "Data plot has been generated.")
}

on_kmeans_discriminant_plot_button_clicked <- function(button)
{

  ## Make sure there is a cluster first.

  if (is.null(crs$kmeans))
  {
    errorDialog("E125: Should not be here. Please report to",
                "support@togaware.com")
    return()
  }

  ## LIBRARY: Ensure the appropriate package is available for the
  ## plot, and log the R command and execute.
  
  lib.cmd <- "require(fpc, quietly=TRUE)"
  appendLog("The plot functionality is provided by the fpc package.", lib.cmd)
  eval(parse(text=lib.cmd))

  ## Some background information.  Assume we have already built the
  ## cluster, and so we don't need to check so many conditions.

  sampling  <- not.null(crs$sample)
  nums <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.numeric))]
  if (length(nums) > 0)
  {
    indicies <- getVariableIndicies(crs$input)
    include <- simplifyNumberList(intersect(nums, indicies))
  }

  if (length(nums) == 0 || length(indicies) == 0)
  {
    errorDialog("Clusters are currently calculated only for numeric data.",
                "No numeric variables were found in the dataset",
                "from amongst those having an input/target/risk role.")
    return()
  }

  ## We can only plot if there is more than a single variable.
  
  if (length(intersect(nums, indicies)) == 1)
  {
    infoDialog("A discriminant coordinates plot can not be constructed",
               "because there is only one numeric variable available",
               "in the data.")
    return()
  }

  # PLOT: Log the R command and execute. 080521 Add the na.omit since
  # kmeans is usually built with this.

  plot.cmd <- sprintf(paste("plotcluster(na.omit(crs$dataset[%s,%s]), ",
                            "crs$kmeans$cluster)\n",
                            genPlotTitleCmd("Discriminant Coordinates",
                                            crs$dataname), sep=""),
                      ifelse(sampling, "crs$sample", ""), include)
  appendLog("Generate a discriminant coordinates plot.", plot.cmd)
  newPlot()
  eval(parse(text=plot.cmd))

  setStatusBar("Discriminant coordinates plot has been generated.")
}

##------------------------------------------------------------------------
##
## HCLUST
##

executeClusterHClust <- function(include)
{
  # Initial setup. Ensure the textview is monospace for fixed width
  # output of the centers and other information (so the columns line
  # up).

  TV <- "hclust_textview"
  theWidget(TV)$modifyFont(pangoFontDescriptionFromString("monospace 10"))
  
  # TODO : If data is large put up a question about wanting to
  # continue?
  
  sampling  <- not.null(crs$sample)

  # The amap library needs to be loaded for hcluster. Also note that
  # hcluster takes about 0.33 seconds, compared to hclust taking 11
  # seconds!

  lib.cmd <- "require(amap, quietly=TRUE)"
  if (packageIsAvailable("amap"))
  {
    amap.available <- TRUE
    appendLog("The hcluster function is provided by the amap package.",
              lib.cmd)
    eval(parse(text=lib.cmd))
  }
  else
    amap.available <- FALSE
  
  # Obtain interface information.

  dist <- theWidget("hclust_distance_combobox")$getActiveText()
  link <- theWidget("hclust_link_combobox")$getActiveText()
  nbproc <- theWidget("hclust_nbproc_spinbutton")$getValue()

  # Check if user has requested more than a single processor, and if
  # so but amap is not available, inform the user and exit.
  
  if (nbproc != 1 && ! amap.available)
  {
    errorDialog("The amap package is not available and so the efficient",
                "and parallel hcluster is not available.",
                "Please set the number of processors to 1 to proceed",
                "with using the single processor hclust instead.",
                "Be aware that the amap version is over 10 times faster.")
    return(FALSE)
  }
  
  # Determine which hclust to use for clustering.

  if (amap.available)

    # Use the more efficient hcluster for clustering.
  
    hclust.cmd <- paste("crs$hclust <<- ",
                        sprintf(paste('hclusterpar(crs$dataset[%s,%s],',
                                      'method="%s", link="%s",',
                                      'nbproc=%d)'),
                                ifelse(sampling, "crs$sample", ""),
                                include, dist, link, nbproc),
                        sep="")
  else

    # Use the standard hclust for clustering.
    
    hclust.cmd <- paste("crs$hclust <<- ",
                        sprintf(paste('hclust(dist(crs$dataset[%s,%s],',
                                      'method="%s"),',
                                      'method="%s")'),
                                ifelse(sampling, "crs$sample", ""),
                                include, dist, link),
                        sep="")

  # Log the R command.

  startLog("HIERARCHICAL CLUSTER")
  appendLog("Generate a hierarchical cluster of the data.",
          gsub("<<-", "<-", hclust.cmd))
  
  # Perform the commands.

  start.time <- Sys.time()
  result <- try(eval(parse(text=hclust.cmd)), silent=TRUE)
  time.taken <- Sys.time()-start.time
  if (inherits(result, "try-error"))
  {
    if (any(grep("[cC]annot allocate (vector|memory)", result)))
    {
      errorDialog("The call to hclust appears to have failed.",
                   "This is often due, as in this case,",
                   "to running out of memory",
                   "as hclust is rather memory hungry.",
                   "A quick solution is to sample the dataset, through the",
                   "Transform tab. On 32bit machines you may be limited to",
                   "less than 2000 entities.")
      setTextview(TV)
    }
    else
      errorDialog("The call to hclust appears to have failed.",
                   "The error message was:", result,
                   "I am not familiar with this error, and you may",
                   "want to report it to support@togaware.com")
    return()
  }

  setTextview(TV, "Hiearchical Cluster\n", collectOutput("crs$hclust", TRUE))

  theWidget("hclust_dendrogram_button")$setSensitive(TRUE)
  theWidget("hclust_clusters_label")$setSensitive(TRUE)
  theWidget("hclust_clusters_spinbutton")$setSensitive(TRUE)
  theWidget("hclust_stats_button")$setSensitive(TRUE)
  theWidget("hclust_data_plot_button")$setSensitive(TRUE)
  theWidget("hclust_discriminant_plot_button")$setSensitive(TRUE)

  time.msg <- sprintf("Time taken: %0.2f %s", time.taken,
                      attr(time.taken, "units"))
  addTextview(TV, "\n", time.msg, textviewSeparator())
  appendLog(time.msg)
  setStatusBar("A hierarchical cluster has been generated.", time.msg)
  
}

centers.hclust <- function(x, h, nclust=10, use.median=FALSE)
{
  if (!inherits(h, "hclust")) stop("Not a legitimate hclust opbject")
    
  if (class(x) != "matrix") x <- as.matrix(x)
  if (use.median)
    centres <- round(tapply(x, list(rep(cutree(h, nclust), ncol(x)),
                                    col(x)), median))
  else
    centres <- tapply(x, list(rep(cutree(h, nclust), ncol(x)),
                              col(x)), mean)
  dimnames(centres) <- list(NULL, dimnames(x)[[2]])
  return(centres)
}

on_hclust_dendrogram_button_clicked <- function(button)
{
  plotDendrogram()
}

plotDendrogram <- function()
{

  ## Make sure there is a hclust object first.

  if (is.null(crs$hclust))
  {
    errorDialog("E126: Should not be here.",
                "There is no Hierarchical Cluster yet we are",
                "trying to plot it.",
                "Please report to support@togaware.com")
    return()
  }

  ## Load the required package into the library.  The library, cba,
  ## should already be loaded. But check anyhow.

  lib.cmd <- "require(cba, quietly=TRUE)"
  if (! packageIsAvailable("cba", "plot a dendrogram")) return(FALSE)
  appendLog("The plot functionality is provided by the cba package.", lib.cmd)
  eval(parse(text=lib.cmd))

  ## Generate the plot command to not print the xaxis labels if there
  ## are too many entities.

  if (length(crs$hclust$order) > 100)
    limit <- ", labels=FALSE, hang=0"
  else
    limit <- ""
  plot.cmd <- paste(sprintf('plot(crs$hclust, main="", sub="", xlab=""%s)\n',
                            limit),
                    genPlotTitleCmd("Cluster Dendrogram", crs$dataname),
                    sep="")

  ## Log the R command and execute.
  
  appendLog("Generate a dendrogram plot.", plot.cmd)
  newPlot()
  eval(parse(text=plot.cmd))

  ## Identify the clusters in the plot, if specified.

  nclust <- theWidget("hclust_clusters_spinbutton")$getValue()
  if (nclust > 1 && nclust <= length(crs$hclust$height))
  {
    rect.cmd <- sprintf("rect.hclust(crs$hclust, k=%d)", nclust)
    appendLog("Add in rectangles to show the clusters.", rect.cmd)
    eval(parse(text=rect.cmd))
  }
  
  setStatusBar("Dendrogram plot completed.")
}

on_hclust_stats_button_clicked <- function(button)
{
  displayHClustStats()
}

displayHClustStats <- function()
{
  # Initial setup.
  
  TV <- "hclust_textview"

  # Make sure there is a cluster first.
  
  if (is.null(crs$hclust))
  {
    errorDialog("E127: Should not be here. Please report to",
                "support@togaware.com")
    return()
  }

  # Ensure the appropriate package is available for the plot, and log
  # the R command and execute.
  
  lib.cmd <- "require(fpc, quietly=TRUE)"
  appendLog("The plot functionality is provided by the fpc package.", lib.cmd)
  eval(parse(text=lib.cmd))

  resetTextview(TV)

  # Some background information.  Assume we have already built the
  # cluster, and so we don't need to check so many conditions.

  nclust <- theWidget("hclust_clusters_spinbutton")$getValue()
  sampling  <- not.null(crs$sample)
  nums <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.numeric))]
  if (length(nums) > 0)
  {
    indicies <- getVariableIndicies(crs$input)
    include <- simplifyNumberList(intersect(nums, indicies))
  }

  if (length(nums) == 0 || length(indicies) == 0)
  {
    errorDialog("Clusters are currently calculated only for numeric data.",
                "No numeric variables were found in the dataset",
                "from amongst those having an input/target/risk role.")
    return()
  }

  # Cluster centers.

  centers.cmd <- sprintf("centers.hclust(crs$dataset[%s,%s], crs$hclust, %d)",
                       ifelse(sampling, "crs$sample", ""), include, nclust)
  appendLog("List the suggested cluster centers for each cluster", centers.cmd)
  appendTextview(TV, "Cluster means:\n\n",
                 collectOutput(centers.cmd, use.print=TRUE))
  
  # STATS: Log the R command and execute.

  stats.cmd <- sprintf(paste("cluster.stats(dist(crs$dataset[%s,%s]),",
                             "cutree(crs$hclust, %d))\n"),
                       ifelse(sampling, "crs$sample", ""), include,
                       nclust)
  appendLog("Generate cluster statistics using the fpc package.", stats.cmd)
  appendTextview(TV, "General cluster statistics:\n\n",
                 collectOutput(stats.cmd, use.print=TRUE))

  setStatusBar("HClust cluster statistics have been generated.")
}

on_hclust_data_plot_button_clicked <- function(button)
{

  ## Make sure there is a cluster first.

  if (is.null(crs$hclust))
  {
    errorDialog("E133: Should not be here. Please report to",
                "support@togaware.com")
    return()
  }

  ## Some background information.  Assume we have already built the
  ## cluster, and so we don't need to check so many conditions.

  sampling  <- not.null(crs$sample)
  num.clusters <- theWidget("hclust_clusters_spinbutton")$getValue()
  nums <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.numeric))]
  if (length(nums) > 0)
  {
    indicies <- getVariableIndicies(crs$input)
    include <- simplifyNumberList(intersect(nums, indicies))
  }

  if (length(nums) == 0 || length(indicies) == 0)
  {
    errorDialog("Clusters are currently calculated only for numeric data.",
                "No numeric variables were found in the dataset",
                "from amongst those having an input/target/risk role.")
    return()
  }

  ## We can only plot if there is more than a single variable.
  
  if (length(intersect(nums, indicies)) == 1)
  {
    infoDialog("A data plot can not be constructed",
               "because there is only one numeric variable available",
               "in the data.")
    return()
  }

  ## PLOT: Log the R command and execute.

  plot.cmd <- sprintf(paste("plot(crs$dataset[%s,%s], ",
                            "col=cutree(crs$hclust, %d))\n",
                            genPlotTitleCmd(""), sep=""),
                      ifelse(sampling, "crs$sample", ""), include,
                      num.clusters)
  appendLog("Generate a data plot.", plot.cmd)
  newPlot()
  eval(parse(text=plot.cmd))

  setStatusBar("Data plot has been generated.")
}

on_hclust_discriminant_plot_button_clicked <- function(button)
{

  ## Make sure there is a cluster first.

  if (is.null(crs$hclust))
  {
    errorDialog("E128: Should not be here. Please report to",
                "support@togaware.com")
    return()
  }

  ## LIBRARY: Ensure the appropriate package is available for the
  ## plot, and log the R command and execute.
  
  lib.cmd <- "require(fpc, quietly=TRUE)"
  appendLog("The plot functionality is provided by the fpc package.", lib.cmd)
  eval(parse(text=lib.cmd))

  ## Some background information.  Assume we have already built the
  ## cluster, and so we don't need to check so many conditions.

  sampling  <- not.null(crs$sample)
  num.clusters <- theWidget("hclust_clusters_spinbutton")$getValue()
  nums <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.numeric))]
  if (length(nums) > 0)
  {
    indicies <- getVariableIndicies(crs$input)
    include <- simplifyNumberList(intersect(nums, indicies))
  }

  if (length(nums) == 0 || length(indicies) == 0)
  {
    errorDialog("Clusters are currently calculated only for numeric data.",
                "No numeric variables were found in the dataset",
                "from amongst those having an input/target/risk role.")
    return()
  }

  ## We can only plot if there is more than a single variable.
  
  if (length(intersect(nums, indicies)) == 1)
  {
    infoDialog("A discriminant coordinates plot can not be constructed",
               "because there is only one numeric variable available",
               "in the data.")
    return()
  }

  ## PLOT: Log the R command and execute.

  plot.cmd <- sprintf(paste("plotcluster(crs$dataset[%s,%s], ",
                            "cutree(crs$hclust, %d))\n",
                            genPlotTitleCmd("Discriminant Coordinates",
                                            crs$dataname), sep=""),
                      ifelse(sampling, "crs$sample", ""), include,
                      num.clusters)
  appendLog("Generate a discriminant coordinates plot.", plot.cmd)
  newPlot()
  eval(parse(text=plot.cmd))

  setStatusBar("Discriminant coordinates plot has been generated.")
}

## THIS IS NOT EVEN RELATED TO hclust!!!! USES PAM

## on_hclust_seriation_button_clicked <- function(button)
## {

##   ## Make sure there is a hclust object first.

##   if (is.null(crs$hclust))
##   {
##     errorDialog("SHOULD NOT BE HERE. REPORT TO",
##                 "support@togaware.com")
##     return()
##   }

##   ## The library, cba, should already be loaded. But check anyhow. I
##   ## think this is required for the seriation. Need to check.

##   lib.cmd <- "require(cba, quietly=TRUE)"
##   if (! packageIsAvailable("cba", "generate a seriation plot")) return()
##   appendLog("Seriation is provided by the cba package.", lib.cmd)
##   eval(parse(text=lib.cmd))
  
##   ## Some background information.

##   sampling  <- not.null(crs$sample)
##   nums <- seq(1,ncol(crs$dataset))[as.logical(sapply(crs$dataset, is.numeric))]
##   if (length(nums) > 0)
##   {
##     indicies <- getVariableIndicies(crs$input)
##     include <- simplifyNumberList(intersect(nums, indicies))
##   }

##   plot.cmd <- paste("d <- dist(as.matrix(crs$dataset",
##                     sprintf("[%s,%s]",
##                             ifelse(sampling, "crs$sample", ""),
##                             include),
##                     "))\n",
##                     "l <- pam(d, 10, cluster.only = TRUE)\n",
##                     "res <- cluproxplot(d, l, method = ",
##                     'c("Optimal", "Optimal"), plot = FALSE)\n',
##                     'plot(res, plotOptions = list(main = "PAM + ',
##                     'Seriation (Optimal Leaf ordering)", ',
##                     'col = terrain.colors(64)))', sep="")

##   appendLog("Generate a seriation plot.", plot.cmd)
##   newPlot()
##   eval(parse(text=plot.cmd))
  
##   setStatusBar("Seriation plot completed.")
## }

########################################################################
# EXPORT

exportClusterTab <- function()
{
  if (noDatasetLoaded()) return()

  if (theWidget("kmeans_radiobutton")$getActive())
  {
    exportKMeansTab()
  }
  else
  {
    errorDialog("PMML export for this model is not yet implemented.")
    return()
  }
}

exportKMeansTab <- function(file)
{
  # Make sure we have a model first!
  
  if (is.null(crs$kmeans))
  {
    errorDialog("No kmeans cluster model is available. Be sure to build",
                "the model before trying to export it! You will need",
                "to press the Execute button (F5) in order to build the",
                "model.")
    return()
  }

  # Do we export a model to PMML? If not then we export the acutal
  # cluster assignment to CSV.
  
  exportModel <- theWidget("kmeans_export_model_radiobutton")$getActive()

  # TODO 080703 request a file, allowing either .csv or .xml
  # extension, and then take the appropriate action, rather than
  # having that as a radio button choice.
  
  if (exportModel)
  {
    startLog("EXPORT KMEANS AS PMML")
  
    # Require the pmml package
  
    lib.cmd <- "require(pmml, quietly=TRUE)"
    if (! packageIsAvailable("pmml", "export kmeans clusters")) return(FALSE)
    appendLog("Load the PMML package to export a kmeans cluster.", lib.cmd)
    eval(parse(text=lib.cmd))
  
    # Obtain filename to write the clusters to.
  
    dialog <- gtkFileChooserDialog("Export PMML", NULL, "save",
                                   "gtk-cancel", GtkResponseType["cancel"],
                                   "gtk-save", GtkResponseType["accept"])
    
    if(not.null(crs$dataname))
      dialog$setCurrentName(paste(get.stem(crs$dataname), "_kmeans", sep=""))

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

    if (get.extension(save.name) == "")
      save.name <- sprintf("%s.xml", save.name)
    
    if (file.exists(save.name))
      if (is.null(questionDialog("A file of the same name as", save.name,
                                 "already exists. Do you want to overwrite",
                                 "this file?")))
        return()

    pmml.cmd <- "pmml(crs$kmeans)"

    # We can't pass "\" in a filename to the parse command in
    # MS/Windows so we have to run the save/write command separately,
    # i.e., not inside the string thaat is being parsed.

    appendLog("Export the cluster as PMML.",
              sprintf('saveXML(%s, "%s")', pmml.cmd, save.name))
    saveXML(eval(parse(text=pmml.cmd)), save.name)
  
    setStatusBar("The PMML file", save.name, "has been written.")
  }
  else # Export clusters to CSV, augmenting the original data.
  {
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
      if (is.null(questionDialog("A file of the same name as", save.name,
                                 "already exists. Do you want to overwrite",
                                 "this file?")))
        return()

    ## 080523 No longer used      idents <- getSelectedVariables("ident")

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
    
    csv.cmd <-  sprintf(paste("rbind(data.frame(crs$dataset[%s, ][%s, ],",
                              "kmeans=crs$kmeans$cluster)",
                              "%s", # If non missing this is empty.
                              ")[as.character(sort(as.integer(",
                              "rownames(crs$dataset[%s, ])))), ]"),
                        ifelse(theWidget("sample_checkbutton")$getActive(),
                               "crs$sample", ""),
                        clnm,
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
  
    setStatusBar("The CSV file", save.name, "has been written.")
  }
}
