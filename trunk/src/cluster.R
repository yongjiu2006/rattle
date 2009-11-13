# Gnome R Data Miner: GNOME interface to R for Data Mining
#
# Time-stamp: <2009-11-12 22:19:49 Graham Williams>
#
# Implement cluster functionality.
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

executeClusterTab <- function()
{
  # Can not cluster without a dataset.

  if (noDatasetLoaded()) return()

  # If it looks like the VARIABLES page has not been executed, complain..

  if (variablesHaveChanged()) return()

  # Check if sampling needs executing.

  if (sampleNeedsExecute()) return()
    
  # Kmeans and hclust only work for numeric data, so identify
  # variables to include.  Only work with the INPUT/TARGET/RISK
  # variables. That is, only exclude the IGNORE and IDENT variables.

  include <- getNumericVariables()
  if (! length(include))
  {
    errorDialog("Clusters are currently calculated only for numeric data.",
                "No numeric variables were found in the dataset",
                "from amongst those having an input/target/risk role.")
    return()
  }

  # Dispatch.

  if (theWidget("kmeans_radiobutton")$getActive())
  {
    if (executeClusterKMeans(include))
      theWidget("evaluate_kmeans_checkbutton")$setActive(TRUE)
  }
  else if (theWidget("hclust_radiobutton")$getActive())
  {
    if (executeClusterHClust(include))
      theWidget("evaluate_hclust_checkbutton")$setActive(TRUE)
  }
}

########################################################################
# EXPORT

exportClusterTab <- function()
{
  
  if (noDatasetLoaded()) return()

  if (theWidget("kmeans_radiobutton")$getActive())
  {
    exportKMeansTab()
  }
  else if (theWidget("hclust_radiobutton")$getActive())
  {
    exportHClustTab()
  }
  else
  {
    errorDialog("PMML export for this model is not yet implemented.")
    return()
  }
}

