loadTooltips <- function()
{
  if (! packageIsAvailable("XML", "load GUI tooltips"))
  {
    warning("The XML package is not available. Tooltips will not be available.")
    return(FALSE)
  }

  require(XML, quietly=TRUE)

  result <- try(etc <- file.path(.path.package(package="rattle")[1], "etc"),
                silent=TRUE)
  if (inherits(result, "try-error"))
    doc <- xmlTreeParse("tooltips.xml", useInternalNodes=TRUE)
  else
    doc <- xmlTreeParse(file.path(etc, "tooltips.xml"), useInternalNodes=TRUE)

  for (tt in getNodeSet(doc, "//tooltip"))
    theWidget(xmlGetAttr(tt, 'widget'))$setTooltipText(xmlValue(tt))
}
