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
# Build a single column in the data table
#
buildColumn <- function(data, vars, isCategorical, stratifyBy=NA, level=NA) {
  return (
    unlist(sapply(1:length(vars), function(i) {
      varname = vars[i]
      if (isCategorical[i]) {
        # categorical var
        varLevels = levels(as.factor(data[,varname]))
        if (!is.na(stratifyBy) && !is.na(level)) {
          varFreq = table(data[data[,stratifyBy] == level,varname])
        } else {
          varFreq = table(data[,varname]) 
        }
        varFreq = cbind(varFreq,prop.table(varFreq))
        
        return(c("", sapply(1:length(varLevels), function(i) {
          return(pasteCategorical(tryCatch(varFreq[varLevels[i],], 
                                           error=function(e) {return(c(0,0))})))
        })))
        
      } else {
        # continuous var
        if (!is.na(stratifyBy) && !is.na(level)) {
          return(pasteContinuous(data[data[,stratifyBy] == level,varname]))
        } else {
          return(pasteContinuous(data[,varname]))
        }
      }
    }))
  )
}

#
# summaryTable
#
# Build a summary table, optionally stratified by a 
#
#
summaryTable <- function(data, vars=NULL, isCategorical=NULL, names=NULL,stratifyBy=NA) {
  
  #---------------
  # error checking
  #---------------
  if (is.null(vars)) {
    stop("No variables specified")
  }
  # if names are specified, make sure they match the number of variables
  if (!is.null(names) && length(names) != length(vars)){
    stop("length of names vector must match length of vars vector")
  }
  # if names are specified, make sure they match the number of variables
  if (!is.null(isCategorical) && length(isCategorical) != length(vars)){
    stop("length of isCategorical vector must match length of vars vector")
  }
  
  #----------------------------------
  # Build data frame for table output
  #----------------------------------
  # create the labels
  labels = unlist(sapply(1:length(vars), function(i) {
    varname = vars[i]
    if (isCategorical[i]) {
      varAsFactor = as.factor(data[,varname])
      return(c(ifelse(is.null(names),varname,names[i]), levels(varAsFactor)))
    } else {
      # continuous var
      return(ifelse(is.null(names),varname,names[i]))
    }
  }))
  
  # add the labels to the table
  tableData = data.frame(labels=labels)
  
  
  # bind the summary rows for the data overall
  tableData$overall = buildColumn(data, vars, isCategorical)
  
  
  # add stratified columns
  if (!is.na(stratifyBy)) {
    stratifyFactor = as.factor(data[, stratifyBy])
    for(level in levels(stratifyFactor)) {
      tableData[,paste(c(stratifyBy, "_", level), collapse="")] = 
        buildColumn(data, vars, isCategorical, stratifyBy=stratifyBy, level=level)
    }
  }
  
  #---------------------------------------
  # return the table 
  #---------------------------------------
  return (tableData)
  
}

tmp = summaryTable(mtcars, vars=c("mpg", "cyl", "disp"), isCategorical=c(0,1,0), stratifyBy="am")
