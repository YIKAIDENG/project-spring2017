#################################
#  Programming Assessment in R  #
#   LIS590DV - Component 3      #
#        by Hanlin Zhang        #
#################################

################################################
#            Customize some funtions           #
################################################

#require library, @author Mohammed 25-12-2016
checkInstallLoad <- function(libName) 
{
  if(!require(libName, character.only=TRUE)) 
  {
    install.packages(libName)
    require(libName, character.only=TRUE)
  }
}

#multiplot viz, @author unknown
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {  
  library(grid)  
  
  # Make a list from the ... arguments and plotlist  
  plots <- c(list(...), plotlist)  
  
  numPlots = length(plots)  
  
  # If layout is NULL, then use 'cols' to determine layout  
  if (is.null(layout)) {  
    # Make the panel  
    # ncol: Number of columns of plots  
    # nrow: Number of rows needed, calculated from # of cols  
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),  
                     ncol = cols, nrow = ceiling(numPlots/cols))  
  }  
  
  if (numPlots==1) {  
    print(plots[[1]])  
    
  } else {  
    # Set up the page  
    grid.newpage()  
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))  
    
    # Make each plot, in the correct location  
    for (i in 1:numPlots) {  
      # Get the i,j matrix positions of the regions that contain this subplot  
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))  
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,  
                                      layout.pos.col = matchidx$col))  
    }  
  }  
}  


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

################################################
#                 Environment set              #
################################################
getwd()
setwd("D:/Projects/lis590dv/")

checkInstallLoad("pairsD3")
checkInstallLoad("ggplot2")
checkInstallLoad("Hmisc")
checkInstallLoad("car")
checkInstallLoad("psych")
checkInstallLoad("rgl")


#setup ggplot environment
theme_update(plot.title = element_text(hjust = 0.5))

################################################
#          load data from online file          #
################################################

df = read.table("sample_flat.csv", sep=",",header=T, fill=FALSE,strip.white=T)

################################################
#              frature enginering              #
################################################

df.sel = subset(df, select = c(categorical, quant1, quant2, quant3))

pairs.panels(df.sel, col="red")

df.sel$quant1 = 10 ^ (df.sel$quant1)

#==========Step.1 understanding dataset========#

fit = lm(quant3 ~ categorical + quant1 + quant2, data = df.sel)

#summary stats 

scatter3d(x= df.sel$quant1, y=df.sel$quant2, z = df.sel$quant3 )
