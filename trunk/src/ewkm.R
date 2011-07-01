# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2011-06-24 06:11:13 Graham Williams>
#
# Implement biclust functionality.
#
# Copyright (c) 2011 Togaware Pty Ltd
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
# Callbacks

# When a radio button is selected, display the appropriate tab page.

on_ewkm_radiobutton_toggled <- function(button)
{
  if (button$getActive())
    crv$CLUSTER$setCurrentPage(crv$CLUSTER.KMEANS.TAB)
  setStatusBar()
}

########################################################################
# Execution

executeClusterEwkm <- function(include)
{
  TV <- "kmeans_textview"
  sampling  <- not.null(crs$sample)

  # Obtain interface information.
  
  nclust <- theWidget("kmeans_clusters_spinbutton")$getValue()
  seed <- theWidget("kmeans_seed_spinbutton")$getValue()
  if (seed == crv$seed) seed <- "crv$seed"
  nruns <- theWidget("kmeans_runs_spinbutton")$getValue()
  usehclust <- theWidget("kmeans_hclust_centers_checkbutton")$getActive()
  useIterate <- theWidget("kmeans_iterate_checkbutton")$getActive()
  
  startLog(commonName(crv$KMEANS))

  # Load the required package.
  
  lib.cmd <- "require(siatclust, quietly=TRUE)"
  if (! packageIsAvailable("siatclust", Rtxt("perform subspace cluster analysis")))
    return(FALSE)
  appendLog(packageProvides('siatclust', 'ewkm'), lib.cmd)
  eval(parse(text=lib.cmd))

  # Set the seed so we can repeat.

  seed.cmd <- sprintf('set.seed(%s)', seed)
  appendLog(Rtxt("Reset the random number seed to obtain the same results each time."),
            seed.cmd)
  eval(parse(text=seed.cmd))

  # Determine the dataset to use.

  ds <- sprintf("na.omit(crs$dataset[%s, %s])",
                ifelse(sampling, "crs$sample", ""), include)
  
  # Calculate the centers

  if (usehclust)
    centers <- sprintf("centers.hclust(%s, crs$hclust, %d)", ds, nclust)
  else
    centers <- nclust
  
  # KMEANS: Log the R command and execute.

  ewkm.cmd <- sprintf('crs$kmeans <- ewkm(%s, %s)', ds, centers)
    
  appendLog(sprintf(Rtxt("Generate a ewkm cluster of size %s."), nclust),
            ewkm.cmd)

  start.time <- Sys.time()

  result <- try(eval(parse(text=ewkm.cmd)), TRUE)
  time.taken <- Sys.time()-start.time

  # Show the resulting model.

  size.cmd <- "paste(crs$kmeans$size, collapse=' ')"
  means.cmd <- sprintf("mean(%s)", ds)
  centres.cmd <- "crs$kmeans$centers"
  withinss.cmd <- "crs$kmeans$withinss"
    
  startLog(Rtxt("Report on the cluster characteristics."))
  appendLog(Rtxt("Cluster sizes:"), size.cmd)
  appendLog(Rtxt("Data means:"), means.cmd)
  appendLog(Rtxt("Cluster centers:"), centres.cmd)
  appendLog(Rtxt("Within cluster sum of squares:"), withinss.cmd)

  resetTextview(TV)
  setTextview(TV, Rtxt("Cluster sizes:"), "\n\n",
              collectOutput(size.cmd, TRUE),
              "\n\n", Rtxt("Data means:"), "\n\n",
              collectOutput(means.cmd),
              "\n\n", Rtxt("Cluster centers:"), "\n\n",
              collectOutput(centres.cmd, TRUE),
              "\n\n", Rtxt("Within cluster sum of squares:"), "\n\n",
              collectOutput(withinss.cmd, TRUE),
              "\n")

  # Ensure the kmeans information buttons are now active.

  showModelKMeansExists()

  reportTimeTaken(TV, time.taken, model=commonName(crv$KMEANS))

  return(TRUE)
}

