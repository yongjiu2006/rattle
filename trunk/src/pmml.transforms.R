# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Time-stamp: <2008-08-27 22:04:51 Graham Williams>
#
# Copyright (c) 2008 Togaware Pty Ltd
#
# This files is part of the Rattle suite for Data Mining in R.
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
# Transformations PMML exporter
#
# Should the variable be named same as in the data dictionary (i.e.,
# include the R01_) - probably it should.
#
# Variable name codes:
#
#   R01_vname_min_max

pmml.transform <- function(transform)
{
  require("R4X")
  
  type <- strsplit(transform, "_")[[1]]
  var <-  sub("^[^_]*_", "", transform)
  type <- type[1]

  pmml <- NULL
  if (type == "R01")
    pmml <- pmml.R01(transform)
  else
    warning("Can not currently handle ", type, " transform as in ", i)
  return(pmml)
}

pmml.R01 <- function(var)
{
  # For a R01_Age_15_90 transform, the PMML is:
  #
  # <NormContinuous field="Age">
  #   <LinearNorm orig="15" norm="0"/>
  #   <linearNorm orig="90" norm="1"/>
  # </NormContinuous>
  #
  # The code using "range" in rescaler from the reshape package is:
  #
  #    x-min(x)/(abs(max(x)-min(x)))

  spl <- strsplit(var, "_")[[1]]
  var <- paste(spl[2:(length(spl)-2)], collapse="_")
  minvar <- spl[length(spl)-1]
  maxvar <- spl[length(spl)]
  
  pmml <- xml('<NormContinuous field="{var}">
  <LinearNorm orig="{minvar}" norm="0"/>
  <LinearNorm orig="{maxvar}" norm="1"/>
</NormContinuous>')

  return(pmml)
}

