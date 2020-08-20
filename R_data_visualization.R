#========================================
# Setting
#========================================
rm(list=ls(all=TRUE))
dirname(rstudioapi::getSourceEditorContext()$path) #or setwd()

# Load required packages.
library( needs ); needs( psych, GGally, skimr, visdat, doParallel, snowfall, patchwork );

cl <- makeCluster(detectCores())
registerDoParallel(cl)

#========================================
# read data 
#========================================
data = iris;

#========================================
# summarize data 
#========================================
dim = dim( data ); print( dim );
head( data, 10 )
summary( data )
skim(data)

#appendix
#vis_miss(data)
#vis_dat(data)

#========================================
# visualize data 
#========================================
height=960; width=960;

# check whether there is "factor"-typed attribute.
is_facotr = sapply( data[,1:length(data)], is.factor);
obj_list=which( is_facotr == TRUE); obj_list = as.vector( obj_list );

# 1. ggpairs
foreach(i=obj_list, .packages ="GGally") %dopar%{
   png_name = paste( "ggpairs_obj=", names(data)[i] ,".png", sep="" );
   png(png_name, height=height, width=width);
   print( ggpairs( data, mapping = aes_string(col=names(data)[i], alpha=0.8) ) + theme_bw() )
   dev.off()
}


# 2. pairs
# png_name = "pairspanels.png"
# png(png_name, height=height, width=width)
# pairs.panels(data, hist.col="#99CCFF")
# dev.off()

# 3. histgram (正確な分布の確認)
# png_name = "hist.png"
# png(png_name, height=height, width=width)
# if( length(data) %%2 == 0){ ncol=(length(data)/2)}
# if( length(data) %%2 == 1){ ncol=(length(data)/2)+1}
# 
# par( mfrow=c(2,ncol ) )
# for (i in 1:length(data)) {
#    if( is.numeric(data[,i]) ){
#       hist(data[,i], freq=FALSE, col="#99CCFF", border="#0066FF", main = paste(names(data)[i], sep=""), xlab="",cex.main=1.5, cex.lab=1, cex.axis=1, font.lab=2, font.axis=2 )
#    }
# }
# dev.off()



#========================================
# easy to plot: pathcwork
#========================================
# see t.ly/Il0C










