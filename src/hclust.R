# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2010-01-20 07:58:56 Graham Williams>
#
# Implement hclust functionality.
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

on_hclust_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    crv$CLUSTER$setCurrentPage(crv$CLUSTER.HCLUST.TAB)
  setStatusBar()
}

on_hclust_dendrogram_button_clicked <- function(button)
{
  plotDendrogram()
}

on_hclust_stats_button_clicked <- function(button)
{
  set.cursor("watch", Rtxt("Determining the cluster statistics...."))
  try(displayHClustStats())
  set.cursor("left-ptr", "Cluster stistics displayed. Scroll to see all.")
}

on_hclust_data_plot_button_clicked <- function(button)
{

  # Make sure there is a cluster first.

  if (is.null(crs$hclust))
  {
    errorDialog("E133: No cluster to plot.",
                "The button should not have been sensitive.",
                crv$support.msg)
    return()
  }

  # Some background information.  Assume we have already built the
  # cluster, and so we don't need to check so many conditions.

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

  # We can only plot if there is more than a single variable.
  
  if (length(intersect(nums, indicies)) == 1)
  {
    infoDialog("A data plot can not be constructed",
               "because there is only one numeric variable available",
               "in the data.")
    return()
  }

  # PLOT: Log the R command and execute.

  set.cursor("watch", Rtxt("Determining the cluster statistics...."))
  plot.cmd <- paste(sprintf(paste("plot(crs$dataset[%s,%s], ",
                                  "col=cutree(crs$hclust, %d))\n",
                                  sep=""),
                            ifelse (sampling, "crs$sample", ""), include,
                            num.clusters),
                    genPlotTitleCmd(""))
  appendLog("Generate a data plot.", plot.cmd)
  newPlot()
  eval(parse(text=plot.cmd))

  set.cursor("left-ptr", "Data plot has been generated.")
}

on_hclust_discriminant_plot_button_clicked <- function(button)
{

  # Make sure there is a cluster first.

  if (is.null(crs$hclust))
  {
    errorDialog("E128: No cluster to plot.",
                "The button should not have been sensitive.",
                crv$support.msg)
    return()
  }

  # The fpc package provides the plotcluster command.
  
  if (!packageIsAvailable("fpc", "plot the cluster")) return()
  lib.cmd <- "require(fpc, quietly=TRUE)"
  appendLog(packageProvides("fpc", "plotcluster"), lib.cmd)
  eval(parse(text=lib.cmd))

  # Some background information.  Assume we have already built the
  # cluster, and so we don't need to check so many conditions.

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

  # We can only plot if there is more than a single variable.
  
  if (length(intersect(nums, indicies)) == 1)
  {
    infoDialog("A discriminant coordinates plot can not be constructed",
               "because there is only one numeric variable available",
               "in the data.")
    return()
  }

  # PLOT: Log the R command and execute.

  plot.cmd <- paste(sprintf(paste("plotcluster(na.omit(crs$dataset[%s,%s]), ",
                                  "cutree(crs$hclust, %d))\n"),
                            ifelse(sampling, "crs$sample", ""), include,
                            num.clusters),
                    genPlotTitleCmd("Discriminant Coordinates",
                                    crs$dataname), sep="")
  appendLog("Generate a discriminant coordinates plot.", plot.cmd)
  newPlot()
  eval(parse(text=plot.cmd))

  setStatusBar("Discriminant coordinates plot has been generated.")
}

########################################################################
# EXECUTION

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
  if (packageIsAvailable("amap", "perform an efficient hcluster"))
  {
    amap.available <- TRUE
    appendLog(packageProvides("amap", "hcluster"), lib.cmd)
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
  
    hclust.cmd <- paste("crs$hclust <- ",
                        sprintf(paste('hclusterpar(na.omit(crs$dataset[%s,%s]),',
                                      'method="%s", link="%s",',
                                      'nbproc=%d)'),
                                ifelse(sampling, "crs$sample", ""),
                                include, dist, link, nbproc),
                        sep="")
  else

    # Use the standard hclust for clustering.
    
    hclust.cmd <- paste("crs$hclust <- ",
                        sprintf(paste('hclust(dist(crs$dataset[%s,%s],',
                                      'method="%s"),',
                                      'method="%s")'),
                                ifelse(sampling, "crs$sample", ""),
                                include, dist, link),
                        sep="")

  # Log the R command.

  startLog("HIERARCHICAL CLUSTER")
  appendLog("Generate a hierarchical cluster of the data.",
          hclust.cmd)
  
  # Perform the commands.

  start.time <- Sys.time()
  result <- try(eval(parse(text=hclust.cmd)), silent=TRUE)
  time.taken <- Sys.time()-start.time
  if (inherits(result, "try-error"))
  {
    if (any(grep("[cC]annot allocate (vector|memory)", result)))
    {
      errorDialog("E143: The call to hclust appears to have failed.",
                  "This is often due, as is the case here,",
                  "to running out of memory",
                  "as hclust is rather memory hungry.",
                  "A quick solution is to sample the dataset, through the",
                  "Data tab. On 32bit machines you may be limited to",
                  "less than 2000 observations.")
      setTextview(TV)
    }
    else
      errorDialog(errorMessageFun("hclust", result))
    return(FALSE)
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
  return(TRUE)
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

plotDendrogram <- function()
{

  # Make sure there is a hclust object first.

  if (is.null(crs$hclust))
  {
    errorDialog("E126: Should not be here.",
                "There is no Hierarchical Cluster yet we are",
                "trying to plot it.", crv$support.msg)
    return()
  }

  # Load the required package into the library.  The library, cba,
  # should already be loaded. But check anyhow.

  lib.cmd <- "require(cba, quietly=TRUE)"
  if (! packageIsAvailable("cba", "plot a dendrogram")) return(FALSE)
  appendLog(packageProvides("cba", "plot"), lib.cmd)
  eval(parse(text=lib.cmd))

  # Show a busy cursor whilst drawing the plot.

  set.cursor("watch", Rtxt("Rendering the hierarchical cluster dendrogram...."))
  
  # Generate the plot command to not print the xaxis labels if there
  # are too many observations.

  if (length(crs$hclust$order) > 100)
    limit <- ", labels=FALSE, hang=0"
  else
    limit <- ""
  plot.cmd <- paste(sprintf('plot(crs$hclust, main="", sub="", xlab=""%s)\n',
                            limit),
                    genPlotTitleCmd("Cluster Dendrogram", crs$dataname),
                    sep="")

  # Log the R command and execute.
  
  appendLog("Generate a dendrogram plot.", plot.cmd)
  newPlot()
  eval(parse(text=plot.cmd))

  # Identify the clusters in the plot, if specified.

  nclust <- theWidget("hclust_clusters_spinbutton")$getValue()
  if (nclust > 1 && nclust <= length(crs$hclust$height))
  {
    rect.cmd <- sprintf("rect.hclust(crs$hclust, k=%d)", nclust)
    appendLog("Add in rectangles to show the clusters.", rect.cmd)
    eval(parse(text=rect.cmd))
  }
  
  set.cursor("left-ptr", "")
}

displayHClustStats <- function()
{
  # Initial setup.
  
  TV <- "hclust_textview"

  # Make sure there is a cluster first.
  
  if (is.null(crs$hclust))
  {
    errorDialog("E127: No cluster to plot.",
                "The button should not have been sensitive.",
                crv$support.msg)
    return()
  }

  # The fpc package provides is available for cluster.stats function.
  
  if (!packageIsAvailable("fpc", "calculate cluster statistics")) return()
  lib.cmd <- "require(fpc, quietly=TRUE)"
  appendLog(packageProvides("fpc", "cluster.stats"), lib.cmd)
  eval(parse(text=lib.cmd))

  # 090323 Don't reset the textview since we want to reatin the build
  # information.

  # 090323 REMOVE resetTextview(TV)

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

  centers.cmd <- sprintf("centers.hclust(na.omit(crs$dataset[%s,%s]), crs$hclust, %d)",
                       ifelse(sampling, "crs$sample", ""), include, nclust)
  appendLog("List the suggested cluster centers for each cluster", centers.cmd)
  appendTextview(TV, "Cluster means:\n\n",
                 collectOutput(centers.cmd, use.print=TRUE))
  
  # STATS: Log the R command and execute.

  stats.cmd <- sprintf(paste("cluster.stats(dist(na.omit(crs$dataset[%s,%s])),",
                             "cutree(crs$hclust, %d))\n"),
                       ifelse(sampling, "crs$sample", ""), include,
                       nclust)
  appendLog("Generate cluster statistics using the fpc package.", stats.cmd)
  appendTextview(TV, "General cluster statistics:\n\n",
                 collectOutput(stats.cmd, use.print=TRUE))

  setStatusBar("HClust cluster statistics have been generated.")
}

## THIS IS NOT EVEN RELATED TO hclust!!!! USES PAM

## on_hclust_seriation_button_clicked <- function(button)
## {

##   ## Make sure there is a hclust object first.

##   if (is.null(crs$hclust))
##   {
##     errorDialog("SHOULD NOT BE HERE.", crv$support.msg)
##     return()
##   }

##   ## The library, cba, should already be loaded. But check anyhow. I
##   ## think this is required for the seriation. Need to check.

##   lib.cmd <- "require(cba, quietly=TRUE)"
##   if (! packageIsAvailable("cba", "generate a seriation plot")) return()
##   appendLog(packageProvides("cba", "Seriation"), lib.cmd)
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

exportHClustTab <- function(file)
{
  # Make sure we have a model first!
  
  if (is.null(crs$hclust))
  {
    errorDialog("No hierarchical cluster model is available. Be sure to build",
                "the model before trying to export it! You will need",
                "to press the Execute button (F2) in order to build the",
                "model.")
    return()
  }

  # Get some required information

  sampling  <- not.null(crs$sample)
  nclust <- theWidget("hclust_clusters_spinbutton")$getValue()
  include <- getNumericVariables()
  
  startLog("EXPORT HCLUST")
  
  save.name <- getExportSaveName("hclust")
  if (is.null(save.name)) return(FALSE)
  ext <- tolower(get.extension(save.name))

  # Generate appropriate code.

  pmml.cmd <- sprintf(paste("pmml(crs$hclust, centers=centers.hclust(",
                           "na.omit(crs$dataset[%s,%s]), crs$hclust, %d)%s)",
                            sep=""),
                      ifelse(sampling, "crs$sample", ""), include, nclust,
                      ifelse(length(crs$transforms) > 0,
                             ", transforms=crs$transforms", ""))

  # We can't pass "\" in a filename to the parse command in
  # MS/Windows so we have to run the save/write command separately,
  # i.e., not inside the string that is being parsed.

  if (ext == "xml")
  {
    appendLog("Export hierarchical cluster as PMML.",
              sprintf('saveXML(%s, "%s")', pmml.cmd, save.name))
    saveXML(eval(parse(text=pmml.cmd)), save.name)
  }
  else if (ext == "c")
  {
    save.name <- tolower(save.name)
    model.name <- sub("\\.c", "", basename(save.name))
    appendLog("Export hieracrchical cluster model as C code for WebFocus.",
              sprintf('cat(pmmltoc(toString(%s), name="%s", %s, %s, %s), file="%s")',
                      pmml.cmd, model.name, 
                      attr(save.name, "includePMML"),
                      attr(save.name, "includeMetaData"),
                      attr(save.name, "exportClass"),
                      save.name))
    cat(pmmltoc(toString(eval(parse(text=pmml.cmd))), model.name,
                attr(save.name, "includePMML"),
                ifelse(attr(save.name, "includeMetaData"),
                       getTextviewContent("hclust_textview"),
                       "\"Not Included\""),
                attr(save.name, "exportClass")), file=save.name)
  }
  
  setStatusBar("The", toupper(ext), "file", save.name, "has been written.")

}

########################################################################
# SCORE

predict.hclust <- function(object, data, x, nclust=10, ...)
{
  # 090126 Initial work on a predict.hclust function, to allow using a
  # hclust model to allocate new DATA to pre-existing clusters that
  # are built from another dataset X. This uses the common model
  # interface function, predict. This makes it easy to use the Rattle
  # modelling code on kmeans. We use a kmeans encoding to generate the
  # clusters. This is only an approximation. Gets pretty close for
  # ward link and euclidean distance.

  object$centers <- centers.hclust(x, object, nclust=nclust, use.median=FALSE)
  rownames(object$centers) <- seq_len(nclust)
  return(predict.kmeans(object, data))
}

genPredictHclust <- function(dataset)
{
  # 081227 Generate a command to obtain the prediction results when
  # applying the model to new data.

  nclust <- theWidget("hclust_clusters_spinbutton")$getValue()
  sampling  <- not.null(crs$sample)
  include <- getNumericVariables()

  return(sprintf("crs$pr <- predict(crs$hclust, %s, na.omit(crs$dataset[%s,%s]), %s)",
                 dataset, ifelse(sampling, "crs$sample", ""), include, nclust))
}

genResponseHclust <- function(dataset)
{
  # 081227 Generate a command to obtain the response when applying the
  # model to new data.
  
  return(genPredictHclust(dataset))
}

genProbabilityHclust <- function(dataset)
{
  # 081227 Generate a command to obtain the probability when applying
  # the model to new data. There is probably a prblem with simply
  # using the cluster label as the output, since it won't be a
  # probability or even look like it. Let's do it for now though -
  # should be okay.
  
  return(genPredictHclust(dataset))
}
