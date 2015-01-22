#####################################################################
# 
#  Package summaryTables provides functions to create common summary 
#  tables for biomedical manuscripts.

#  Copyright (C) 2015 Sarah M. Kreidler.
# 
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
# 
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
# 
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#
#####################################################################

#
# Convenient function to build a mean + sd row
# in Table 1
#
pasteContinuous <- function(varname, digits=2) {
  return(paste(round(mean(varname, na.rm=TRUE), digits=digits), "\u00B1", round(sd(varname, na.rm=TRUE), digits=digits)))
}

#
# Convenience function to build a count (percent) row
# in Table 1
#
pasteCategorical <- function(tableRow) {  
  return(paste(sep="", tableRow[1], " (", round(tableRow[2]*100, digits=1), "%)"))  
}


#
# summaryTable
#
# Build a summary table, optionally stratified by a 
#
#
summaryTable <- function(data, continuousVars=NULL, categoricalVars=NULL, stratifyBy=NA,
                         main="", 
                         format="data.frame", continuousNames=NULL, categoricalNames=NULL) {
  
  #---------------
  # error checking
  #---------------
  if (is.null(continuousVars) && is.null(categoricalVars)) {
    stop("No variables specified")
  }
  # if names are specified, make sure they match the number of variables
  if (!is.null(continuousNames) && length(continuousNames) != length(continuousVars)){
    stop("length of continuousNames vector must match length of continuousVars vector")
  }
  # if names are specified, make sure they match the number of variables
  if (!is.null(categoricalNames) && length(categoricalNames) != length(categoricalVars)){
    stop("length of categoricalNames vector must match length of categoricalVars vector")
  }
  
  #----------------------------------
  # Build data frame for table output
  #----------------------------------
  # create the row labels for continuous variables
  if (!is.null(continuousNames)) {
    continuousLabels = continuousNames
  } else {
    continuousLabels = continuousVars
  }
  # create the row labels for categorical variables
  categoricalLabels = sapply(categoricalVars, function(varname) {
    varAsFactor = as.factor(data[,varname])
    return(c(varname, levels(varAsFactor)))
  })
  
  # add the labels to the table
  tableData = data.frame(labels=c(continuousLabels, categoricalLabels))
  
  
  # bind the summary rows for the data overall
  tableData$overall = c(sapply(continuousVars, function(varname) {
      return(pasteContinuous(data[,varname]))
    }),
    sapply(categoricalVars, function(varname) {
      varAsFactor = as.factor(varname)
      varFreq = table(data[,varname])
      varFreq = cbind(varFreq,prop.table(varFreq))
      
      return(c("", sapply(1:nrow(varFreq), function(i) {
        return(pasteCategorical(varFreq[i,]))
      })))
    })
  )
  
  
  # add stratified columns
  if (!is.na(stratifyBy)) {
    
  }
  
  #---------------------------------------
  # Output the table in the desired format
  #---------------------------------------
  
  if (format == "data.frame") {
    return(tableData)
  } else if (format == "rtf") {
    
  } else if (format == "latex") {
    return (xtable(tableData))
  }
  
}

tmp = summaryTable(mtcars, continuousVars=c("mpg", "disp"), categoricalVars=c("cyl"))
