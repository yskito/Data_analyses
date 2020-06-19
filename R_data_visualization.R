#========================================
# Setting
#========================================
rm(list=ls(all=TRUE))
dirname(rstudioapi::getSourceEditorContext()$path)

# Load required packages.
library(psych)
library(GGally)
library(ggplot2)

#========================================
# Visualize data 
#========================================
pdf_name = "data_visualization.pdf"
data = iris

pdf(pdf_name, height=10, width=10)

# 1. pairs
pairs.panels(data, hist.col="#99CCFF")

# 2. histgram
if( length(data) %%2 == 0){ ncol=(length(data)/2)}
if( length(data) %%2 == 1){ ncol=(length(data)/2)+1}
par( mfrow=c(2,ncol ) )
for (i in 1:length(data)) {
   if( is.numeric(data[,i]) == TRUE ){
      hist(data[,i], freq=FALSE, col="#99CCFF", border="#0066FF", main = paste(names(data)[i], sep=""), xlab="",cex.main=1.5, cex.lab=1, cex.axis=1, font.lab=2, font.axis=2 )
   }
}
# 3. ggpairs
ggpairs( data ) + theme_bw();

dev.off()

print( summary( data ) )
   



