# UNLEASH MACHINE LEARNING PAGE 151
library(gridExtra)
library(pastecs)
library(ggplot2)
library(gmodels)
#Summary Statisitics
get.numeric.variable.stats <- function(indep.var, detailed=FALSE){
  options(scipen=100)
  options(digits=2)
  if (detailed){
    var.stats <- stat.desc(indep.var)
  }else{
    var.stats <- summary(indep.var)
  }
  df <- data.frame(round(as.numeric(var.stats),2))
  colnames(df) <- deparse(substitute(indep.var))
  rownames(df) <- names(var.stats)
  if (names(dev.cur()) != "null device"){
    dev.off()
  }
  grid.table(t(df))
}
# Visualizations
# histograms\density
visualize.distribution <- function(indep.var){
  pl1 <- qplot(indep.var, geom="histogram",
               fill=I('gray'), binwidth=5,
               col=I('black'))+ theme_bw()
  pl2 <- qplot(age, geom="density",
               fill=I('gray'), binwidth=5,
               col=I('black'))+ theme_bw()
  grid.arrange(pl1,pl2, ncol=2)
}
#box plots
visualize.boxplot <- function(indep.var, dep.var){
  pl1 <- qplot(factor(0),indep.var, geom="boxplot",
               xlab = deparse(substitute(indep.var)),
               ylab="values") + theme_bw()
  pl2 <- qplot(dep.var,indep.var,geom="boxplot",
               xlab = deparse(substitute(dep.var)),
               ylab = deparse(substitute(indep.var))) + theme_bw()
  grid.arrange(pl1,pl2, ncol=2)
}

# Summary statistics Categorical
get.categorical.variable.stats <- function(indep.var){
  feature.name = deparse(substitute(indep.var))
  df1 <- data.frame(table(indep.var))
  colnames(df1) <- c(feature.name, "Frequency")
  df2 <- data.frame(prop.table(table(indep.var)))
  colnames(df2) <- c(feature.name, "Proportion")
  df <- merge(
    df1, df2, by = feature.name
  )
  ndf <- df[order(-df$Frequency),]
  if (names(dev.cur()) != "null device"){
    dev.off()
  }
  grid.table(ndf)
}
# generate contingency table
get.contingency.table <- function(dep.var, indep.var,
                                  stat.tests=F){
  if(stat.tests == F){
    CrossTable(dep.var, indep.var, digits=1,
               prop.r=F, prop.t=F, prop.chisq=F)
  }else{
    CrossTable(dep.var, indep.var, digits=1,
               prop.r=F, prop.t=F, prop.chisq=F,
               chisq=T, fisher=T)
  }
}
# Visualizations
# barcharts
visualize.barchart <- function(indep.var){
  qplot(indep.var, geom="bar",
        fill=I('gray'), col=I('black'),
        xlab = deparse(substitute(indep.var))) + theme_bw()
}
#mosaic plots
visualize.contingency.table <- function(dep.var, indep.var){
  if (names(dev.cur()) != "null device"){
    dev.off()
  }
  mosaicplot(dep.var ~ indep.var, color=T,
             main = "Contingency table plot")
}