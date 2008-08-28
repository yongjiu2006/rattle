# PMML: Predictive Modelling Markup Language
#
# Part of the Rattle package for Data Mining
#
# Time-stamp: <2008-08-27 22:16:09 Graham Williams>
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

pmml.kmeans <- function(model,
                        transforms=NULL,
                        model.name="KMeans_Model",
                        app.name="Rattle/PMML",
                        description="KMeans cluster model",
                        copyright=NULL, ...)
{
  require(XML, quietly=TRUE)
  
  if (! inherits(model, "kmeans")) stop("Not a legitimate kmeans object")

  # Collect the required information.

  field <- NULL
  field$name <-  colnames(model$centers)
  orig.fields <- field$name
  if (! is.null(transforms))
    field$name <- unifyTransforms(field$name, transforms)
  number.of.fields <- length(field$name)

  field$class <- rep("numeric", number.of.fields) # All fields are numeric
  names(field$class) <- field$name

  number.of.clusters <- length(model$size)
  cluster.names <- rownames(model$centers)

  # PMML

  pmml <- pmmlRootNode("3.2")

  # PMML -> Header

  pmml <- append.XMLNode(pmml, pmmlHeader(description, copyright, app.name))

  # PMML -> DataDictionary

  pmml <- append.XMLNode(pmml, pmmlDataDictionary(field))

  # PMML -> ClusteringModel

  cl.model <- xmlNode("ClusteringModel",
                      attrs=c(modelName=model.name,
                        functionName="clustering", # Required
                        algorithmName="KMeans: Hartigan and Wong",
                        modelClass="centerBased", # Required
                        numberOfClusters=number.of.clusters)) # Required

  # PMML -> ClusteringModel -> MiningSchema

  cl.model <- append.XMLNode(cl.model, pmmlMiningSchema(field))

  # PMML -> ClusteringModel -> LocalTransformations -> DerivedField -> NormContiuous

  if (! is.null(transforms))
  {
    ltrans <- xmlNode("LocalTransformations")
    for (i in transforms)
    {
      dfield <- xmlNode("DerivedField",
                        attrs=c(name=sub("^R01_(.*)_[^_]*_[^_]*$", "R01_\\1", i),
                          optype="continuous",
                          dataType="double"))
      ltrans <- append.XMLNode(ltrans, append.XMLNode(dfield, pmml.transform(i)))
    }
    
    cl.model <- append.XMLNode(cl.model, ltrans)
  }
  
  # PMML -> ClusteringModel -> ComparisonMeasure
  
  cl.model <- append.XMLNode(cl.model,
                             append.XMLNode(xmlNode("ComparisonMeasure",
                                                    attrs=c(kind="distance")),
                                            xmlNode("squaredEuclidean")))

  # PMML -> ClusteringField - These exist if there are LocalTransforms

  for (i in orig.fields)
  {
    cl.model <- append.xmlNode(cl.model,
                               xmlNode("ClusteringField",
                                       attrs=c(field=i)))
  }
  
  # PMML -> ClusteringModel -> Cluster -> Array
  
  clusters <- list()
  for (i in 1:number.of.clusters)
  {
    cl.model <- append.XMLNode(cl.model,
                               xmlNode("Cluster",
                                       attrs=c(name=cluster.names[i],
                                         size=model$size[i]),
                                       xmlNode("Array",
                                               attrs=c(n=number.of.fields,
                                                 type="real"),
                                               paste(model$centers[i,],
                                                     collapse=" "))))
  }
  pmml <- append.XMLNode(pmml, cl.model)

  return(pmml)
}

unifyTransforms <- function(vars, transforms)
{
  for (i in transforms)
  {
    ibase <- sub("^R01_(.*)_[^_]*_[^_]*$", "R01_\\1", i)
    if (ibase %in% vars)
    {
      index <- which(ibase == vars)
      vars[index] <- sub("^R01_", "", vars[index])
    }
  }
  return(vars)
}

