#============================
# 1. setting
#============================
setting <- function(){
   
   # delete all object 
   rm(list=ls(all=TRUE))
   
   # set derectory
   dirname(rstudioapi::getSourceEditorContext()$path) # or use setwd()
   
   # load library
   library( needs ); needs( skimr, GGally, "e1071", snowfall, doParallel, psych ); 
   
   # set cores
   cores = detectCores();
   sfInit(parallel = TRUE, cpus = cores)
}

#============================
# 2. summarize data_frame
#============================
df_smrz<- function( data ){
   if(!is.data.frame(data)) data = as.data.frame( data );
   
   dim = dim( data ); print( dim );
   print( head( data, 10 ) )
   print( summary( data ) )
   print( skim(data) )
}

#============================
# 3. visualize data_frame
#============================
df_vis<- function( data ){
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
}

#============================
# 4. arrange data_frame
#============================
df_arrange<- function( data ){
   
}

#============================
# X. output image file
#============================
output_image<- function(  ){
   
}

#============================
# X. End data analyses
#============================
END <- function(){
   rm(list=ls(all=TRUE))
   sfStop()
}
