# ---------
# Important
# ---------

# use effects 3.1-2 (or older) or 4.1-2 (or newer) for proper CLMM probability estimates

#library(devtools)
#install_version("effects", version = "3.1-2", repos = "http://cran.us.r-project.org")
#install.packages("effects", repos="http://R-Forge.R-project.org")

# -----
# To-Do
# -----

# Support more than 2-way interactions
# Check if polynomials are working as expected
# Allow for changing the faceting variable (for interactions)

# ---------
# Libraries
# ---------

library(effects)
library(ordinal)
library(reshape2)
library(ggplot2)
library(MASS)

# ---------
# Functions
# ---------

# build ordinal plot data
BuildOrdinalPlotData <- function(theEffect, theModel, theData, unscale = TRUE) {
  
  # set up empty list for returning unscaled variables (if applicable)
  unscaled <- list()
  
  # set up basic plotting data.frame from effect
  theTerms <- as.character(sapply(theEffect$variables, "[[", "name"))
  plotDat <- data.frame(theEffect$x, theEffect$prob, theEffect$upper.prob, theEffect$lower.prob)
  
  # organize data.frame for ggplot2
  colCount <- (ncol(plotDat) - length(theTerms)) / 3
  fit <- melt(plotDat[, 1:(length(theTerms) + colCount)], id.vars = theTerms, variable.name = theEffect$response, value.name = "prob")
  fit[, theEffect$response] <- as.numeric(fit[, theEffect$response])
  upper <- melt(plotDat[, c(theTerms, grep("U.", names(plotDat), value = TRUE, fixed = TRUE))], id.vars = theTerms, variable.name = theEffect$response, value.name = "prob")
  lower <- melt(plotDat[, c(theTerms, grep("L.", names(plotDat), value = TRUE, fixed = TRUE))], id.vars = theTerms, variable.name = theEffect$response, value.name = "prob")
  plotDat <- data.frame(fit, upper = upper$prob, lower = lower$prob)
  fit[, theEffect$response] <- factor(fit[, theEffect$response])
  
  head(plotDat)
  
  # unscale variables if requested (detect from data)
  if (unscale) {
    
    for (term in theTerms) {
      
      if ( is.element("scaled:center", names(attributes(theData[, term]))) ) {
        message("Reverse scoring variable: ", term)
        unscaled <- append(unscaled, term)
        plotDat[, term] <- round(eval(parse(text=paste("plotDat$", term, sep=""))) * attr(theData[, term], "scaled:scale") + attr(theData[, term], "scaled:center"), 2)
      }
      
    }
  }
  
  # recode terms as VarX, VarY, etc. for plotting
  #for (i in 1:length(theTerms)) {
  #  names(plotDat)[i] <- paste0("Var", i)
  #  plotDat[, i] <- factor(plotDat[, i])
  #}
  
  # factor variables for plotting
  for (term in theTerms)
    plotDat[, term] <- factor(plotDat[, term])
  plotDat[, theEffect$response] <- factor(plotDat[, theEffect$response])
  
  # add grouping variable for plotting (note: currently limited to two-way interaction [FIX ME])
  if (length(theTerms) > 1) {
    plotDat <- within(plotDat, Group <- paste(plotDat[, 1], plotDat[, 2], sep = " | "))
    plotDat$Group <- factor(plotDat$Group)
  }
  
 head(plotDat)
 
 return(list(data = plotDat,
             response = theEffect$response,
             terms = theTerms,
             unscaled = unlist(unscaled)))
    
}

# build ordinal plot
BuildOrdinalPlot <- function (PlotDat, render = FALSE) {
  
  pdCall <- expression(
    pd <- position_dodge(0.25)
  )
  
  pd <- eval(pdCall)
  
  if (length(PlotDat$terms) > 1)
    plotCall <- paste0(
    'ggplot(PlotDat$data, aes(x = ', PlotDat$response, ', y = prob, group = ', PlotDat$terms[2], ')) + 
      geom_point(aes(shape = ', PlotDat$terms[2], ', color = ', PlotDat$terms[2], '), position = pd, size = 3) + 
      geom_line(size = .75, position = pd, alpha = 0.5, aes(linetype = ', PlotDat$terms[2], ', color = ', PlotDat$terms[2], ')) + 
      geom_errorbar(aes(ymin = lower, ymax = upper, color = ', PlotDat$terms[2], '), width = .5, position = pd, size = .75) + 
      facet_wrap(~ ', PlotDat$terms[1], ') + 
      labs(y = "Probability") + 
      theme_bw(base_size = 15)'
      ) else
    plotCall <- paste0(
    'ggplot(PlotDat$data, aes(x = ', PlotDat$response, ', y = prob, group = ', PlotDat$terms[1], ')) + 
      geom_point(aes(shape = ', PlotDat$terms[1], ', color = ', PlotDat$terms[1], '), position = pd, size = 3) + 
      geom_line(size = .75, position = pd, alpha = 0.5, aes(linetype = ', PlotDat$terms[1], ', color = ', PlotDat$terms[1], ')) + 
      geom_errorbar(aes(ymin = lower, ymax = upper, color = ', PlotDat$terms[1], '), width = .5, position = pd, size = .75) + 
      labs(y = "Probability") + 
      theme_bw(base_size = 15)'
      )
      
  message(paste0(as.character(pdCall), '\n', as.character(plotCall)))
  
  thePlot <- eval(parse(text = plotCall))
  
  if (render)
    print(thePlot)
  
  return(thePlot)
  
}

# wrapper function to generate a plot
OrdinalPlot <- function(theEffect, theModel, theData, unscale = TRUE, render = TRUE) {
  
  plotDat <- BuildOrdinalPlotData(theEffect, theModel, theData, unscale = TRUE)
  message("\nPlot Code:\n")
  thePlot <- BuildOrdinalPlot(plotDat, render = render)
  
  return(thePlot)
  
}