# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2009-02-07 07:40:53 Graham Williams>
#
# Implement kmeans functionality.
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

on_kmeans_seed_button_clicked <- function(button)
{
  rseed <- as.integer(runif(1, 0, 1000000))
  theWidget("kmeans_seed_spinbutton")$setValue(rseed)
}

on_kmeans_stats_button_clicked <- function(button)
{
  ## Make sure there is a cluster first.
  
  if (is.null(crs$kmeans))
  {
    errorDialog("E124: Should not be here.", crv$support.msg)
    return()
  }

  ## LIBRARY: Ensure the appropriate package is available for the
  ## plot, and log the R command and execute.
  
  if (!packageIsAvailable("fpc", "plot a cluster")) return()
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

  setStatusBar("K Means cluster statistics have been generated. Scroll to view.")
}

on_kmeans_data_plot_button_clicked <- function(button)
{

  # Make sure there is a cluster first.

  if (is.null(crs$kmeans))
  {
    errorDialog("E132: Should not be here.", crv$support.msg)
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

  # Alternative plot commands that could be considered:
  #
  #    plot3d (rgl) with type="s"
  #    plotmatrix (ggplot2)
  #    splom (lattice)
  #
  # I think the default plot is quite good. plotmatrix is good, but
  # does not include the scales and takes a long time to render.
  
  ##  plot.cmd <- sprintf(paste("plot(crs$dataset[%s,%s], ",
  plot.cmd <- sprintf(paste("plot(na.omit(crs$dataset[%s,%s]), ",
                            "col=crs$kmeans$cluster)\n%s", sep=""),
                      ifelse(sampling, "crs$sample", ""), include, genPlotTitleCmd(""))
  appendLog("Generate a data plot.", plot.cmd)

  set.cursor("watch", "Rendering the plot. Please wait...")
  newPlot()
  eval(parse(text=plot.cmd))
  set.cursor("left-ptr", "Data plot has been generated.")
}

on_kmeans_discriminant_plot_button_clicked <- function(button)
{

  # Make sure there is a cluster first.

  if (is.null(crs$kmeans))
  {
    errorDialog("E125: No cluster to plot.",
                "The button should not have been sensitive.",
                crv$support.msg)
    return()
  }

  # The fpc package provides the plotcluster command execute.
  
  if (!packageIsAvailable("fpc", "plot a cluster")) return()
  lib.cmd <- "require(fpc, quietly=TRUE)"
  appendLog("The plot functionality is provided by the fpc package.", lib.cmd)
  eval(parse(text=lib.cmd))

  # Some background information.  Assume we have already built the
  # cluster, and so we don't need to check so many conditions.

  sampling <- not.null(crs$sample)
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

  # We can only plot if there is more than a single variable.
  
  if (length(intersect(nums, indicies)) == 1)
  {
    infoDialog("A discriminant coordinates plot can not be constructed",
               "because there is only one numeric variable available",
               "in the data.")
    return()
  }

  # PLOT: Log the R command and execute. 080521 Add the na.omit since
  # kmeans is usually built with this.

  plot.cmd <- paste(sprintf("plotcluster(na.omit(crs$dataset[%s,%s]), ",
                            ifelse(sampling, "crs$sample", ""), include),
                    "crs$kmeans$cluster)\n",
                    genPlotTitleCmd("Discriminant Coordinates",
                                    crs$dataname), sep="")
  appendLog("Generate a discriminant coordinates plot.", plot.cmd)
  newPlot()
  eval(parse(text=plot.cmd))

  setStatusBar("Discriminant coordinates plot has been generated.")
}

########################################################################
# EXECUTION

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
    centers <- sprintf("centers.hclust(na.omit(crs$dataset[%s,%s]), crs$hclust, %d)",
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

# 080913 The tryCatch is neat, but the simle try is sufficient.
#    
#    errorHandler <- function(condition)
#    {
#     if (conditionMessage(condition)=="more cluster centers than distinct data points.")
#       errorDialog("The data does not support the number of clusters requested.",
#                    "Reduce the number of clusters and try again.")
#      else
#        message(sprintf("Error in call to %s %s",
#                        conditionCall(condition),
#                        conditionMessage(condition)))
#      return(FALSE)
#    }
#    if (! tryCatch(eval(parse(text=kmeans.cmd)), error=errorHandler))
#      return(FALSE)
    
    result <- try(eval(parse(text=kmeans.cmd)), TRUE)
    time.taken <- Sys.time()-start.time

    if (inherits(result, "try-error"))
    {
      if (any(grep("more cluster centers than distinct data points", result))||
          any(grep("cannot take a sample larger than the population", result)))
        errorDialog("The data does not support the number of clusters",
                    "requested. Reduce the number of clusters and try again.")
      else
        errorDialog("The cluster build failed.",
                    "The error was:", result)
      return(FALSE)
    }

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

########################################################################
# EXPORT

exportKMeansTab <- function(file)
{
  # Make sure we have a model first!
  
  if (is.null(crs$kmeans))
  {
    errorDialog("No kmeans cluster model is available. Be sure to build",
                "the model before trying to export it! You will need",
                "to press the Execute button (F2) in order to build the",
                "model.")
    return()
  }

  startLog("EXPORT KMEANS")
  
  save.name <- getExportSaveName("kmeans")
  if (is.null(save.name)) return(FALSE)
  ext <- tolower(get.extension(save.name))

  # Generate appropriate code.

  pmml.cmd <- sprintf("pmml(crs$kmeans%s)",
                      ifelse(length(crs$transforms) > 0,
                             ", transforms=crs$transforms", ""))

  # We can't pass "\" in a filename to the parse command in
  # MS/Windows so we have to run the save/write command separately,
  # i.e., not inside the string that is being parsed.

  if (ext == "xml")
  {
    appendLog("Export cluster as PMML.",
              sprintf('saveXML(%s, "%s")', pmml.cmd, save.name))
    saveXML(eval(parse(text=pmml.cmd)), save.name)
  }
  else if (ext == "c")
  {
    save.name <- tolower(save.name)
    model.name <- sub("\\.c", "", basename(save.name))
    appendLog("Export a cluster model as C code for WebFocus.",
              sprintf('cat(pmmltoc(toString(%s), name="%s", %s, %s, %s), file="%s")',
                      pmml.cmd, model.name,
                      attr(save.name, "includePMML"),
                      attr(save.name, "includeMetaData"),
                      attr(save.name, "exportClass"),
                      save.name))
    cat(pmmltoc(toString(eval(parse(text=pmml.cmd))), model.name,
                attr(save.name, "includePMML"),
                attr(save.name, "includeMetaData"),
                attr(save.name, "exportClass")), file=save.name)
  }
  
  setStatusBar("The", toupper(ext), "file", save.name, "has been written.")

}

########################################################################
# SCORE

predict.kmeans <- function(model, data)
{
  # 081228 Initial work on a predict.kmeans function, to allow using a
  # kmeans model to allocate new data to pre-existing clusters using
  # the common model interface function, predict. This makes it easy
  # to use the Rattle modelling code on kmeans. TODO Currently, no
  # support for alternative distance measures. This will be needed
  # eventually
  
  num.clusters <- nrow(model$centers)
  cluster.names <- rownames(model$centers)
  cluster.vars <- colnames(model$centers)
  num.rows <- nrow(data)
  cluster.row.nums <- seq(num.rows+1, num.rows+num.clusters)

  # 081228 Put the data first, to maintain rownames. If there are
  # conflicts in rownames then rbind creates new rownames, and this
  # will be a surprise for the rownames of the returned data. We avoid
  # this by having the original data first, thus its rownames are
  # maintained. This may well change the cluster names, which we then
  # need to change back correctly.

  # 081228 Simply calculate the distance between all points - this is
  # simpler to code, but perhaps less efficient?
  
  d <- as.matrix(dist(rbind(data[cluster.vars], model$centers)))
  d <- d[-cluster.row.nums,cluster.row.nums]
  colnames(d) <- cluster.names
  
  out <- apply(d, 1, which.min)
  miss <- attr(na.omit(data[cluster.vars]), "na.action")
  out[miss] <- NA
  return(out)
}

genPredictKmeans <- function(dataset)
{
  # 081227 Generate a command to obtain the prediction results when
  # applying the model to new data.
  
  return(sprintf("crs$pr <<- predict(crs$kmeans, %s)", dataset))
}

genResponseKmeans <- function(dataset)
{
  # 081227 Generate a command to obtain the response when applying the
  # model to new data.
  
  return(genPredictKmeans(dataset))
}

genProbabilityKmeans <- function(dataset)
{
  # 081227 Generate a command to obtain the probability when applying
  # the model to new data. There is probably a prblem with simply
  # using the cluster label as the output, since it won't be a
  # probability or even look like it. Let's do it for now though -
  # should be okay.
  
  return(genPredictKmeans(dataset))
}
