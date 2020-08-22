#============================
# 1. setting
#============================
setting <- function(){
   basic_setting <- function(){
      rm(list=ls(all=TRUE));
      dirname(rstudioapi::getSourceEditorContext()$path) # or use setwd()
      library( needs ); needs( skimr, GGally, "e1071", snowfall, doParallel, psych ); 
   }; basic_setting();
   
   PARALLEL_setting <- function(){
      cores <<- detectCores();
      sfInit(parallel = TRUE, cpus = cores)
   }; PARALLEL_setting();
}

#============================
# 2. set parameters
#============================
set_parms <- function(){
   seed <<- 123;
   train_rate <<- 0.8;
   object_col <<- 5;
}; 

#============================
# 3. summarize data_frame
#============================
df_smrz<- function( data ){
   if(!is.data.frame(data)) data <<- as.data.frame( data );
   
   print( skim(data) )
   # if u need, then use them.
   # dim = dim( data ); print( dim );
   # print( head( data, 10 ) )
   # print( summary( data ) )
}

#============================
# 4. visualize data_frame
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
# 5. divide dataset into train&test data
#============================
df_train_test<- function( data ){
   
   # STEP0: set seed & extract rows of train data
   set.seed(seed); 
   train_row <<- sample( nrow(data), nrow(data) * train_rate );
   
   # STEP1: train&test data  
   train_data <<- data[train_row,];  
   test_data  <<- data[-train_row,]; 
   
   # STEP2: objective variable is named as "y" (to facilitate you to use R packages (e.g., rpart, RF, lm and svm) )
   names(train_data)[object_col] <<- "y";
   names(test_data) [object_col] <<- "y";
   
   # STEP3: except objective variable
   train_feature <<- train_data[,-object_col]
   test_feature  <<- test_data [,-object_col]
   
   # STEP4: objective variable
   train_label   <<- as.factor( train_data[,object_col] )
   test_label    <<- as.factor( test_data [,object_col] )
}

#============================
# 5'. normalize train&test data
#============================
nrmlz_df_train_test<- function( data ){
   
   # STEP1: normalized dataset and train&test data
   nrmlz_data       <<-  cbind( as.data.frame( scale(       data[,-object_col] ) ), y=      data[,object_col] ) 
   nrmlz_train_data <<-  cbind( as.data.frame( scale( train_data[,-object_col] ) ), y=train_data[,object_col] ) 
   nrmlz_test_data  <<-  cbind( as.data.frame( scale( test_data [,-object_col] ) ), y=test_data [,object_col] ) 
   
   # STEP2: except objective variable
   nrmlz_train_feature <<- nrmlz_train_data[,-object_col]
   nrmlz_test_feature  <<- nrmlz_test_data [,-object_col]
}

#============================
# X. output image file
#============================
output_image<- function(  ){
   
}

#============================
# X. how correct/wrong prediction data are distributed in the dataset?
#============================
df_crrt_wrng<- function(  ){
   
}

#============================
# X. End data analyses
#============================
END <- function(){
   rm(list=ls(all=TRUE))
   sfStop()
}
