


## read .fcs files into a flowset.
library(flowCore)

f_read<-function(working_directory, pattern){
working_directory<-paste("C:/Users/aslia/Desktop/pisi/ETH/master project/FACS data/",
                         working_directory,sep="")
setwd(working_directory)
flowset<-read.flowSet(pattern=pattern)
return(flowset)
}

#---------------------------------------------------------------------------

## create one dataframe per plate, while adding doses.

f_df_list<-function(flowset,starting_well,wells_per_sample,doses,columns){
  
  df_list<-list()
  
  for(i in seq(starting_well,length(flowset),wells_per_sample)){
    
    strain<-data.frame()
    
    for(k in c(i:(i+wells_per_sample-1))){
      
    sample<-as.data.frame(flowset[[k]]@exprs[,columns])
    dose<-doses[[which(k==c(i:(i+wells_per_sample-1)))]]
    dose_column<-as.data.frame(rep(dose,nrow(sample)))
    sample<-cbind(sample,dose_column)
    colnames(sample)[4]<-"Dose"
    
    strain<-rbind(strain,sample)
    }
    
    index<-which(i==seq(starting_well,length(flowset),wells_per_sample))
    df_list[[index]]<-strain
    
  }
  
  return(df_list)
}

## create one dataframe per plate, for comparison purposes (no doses). subset for size
# if necessary

f_df_comparison<-function(flowset,columns, fsch_column, ssch_column, fsc_h_lower_limit,
               fsc_h_upper_limit,ssc_h_lower_limit, ssc_h_upper_limit){
  frame_list<-list()
  for(i in c(1:length(flowset))){
    frame<-as.data.frame(flowset[[i]]@exprs[,columns]) # pick FSC-H, SSC-H, 488(C)-H channels.
    if(size_subset==TRUE){
    frame<-subset(frame,frame[,fsch_column]>fsc_h_lower_limit) # for size subsetting 
    frame<-subset(frame,frame[,fsch_column]<fsc_h_upper_limit) # for size subsetting 
    frame<-subset(frame,frame[,ssch_column]>ssc_h_lower_limit) # for size subsetting 
    frame<-subset(frame,frame[,ssch_column]<ssc_h_upper_limit) # for size subsetting 
    frame_list[[i]]<-frame
    } else{
    frame_list[[i]]<-frame
    }
  }
  return(frame_list)
}


#----------------------------------------------------------------------------------

# to generate the df_list names
f_names_df_list<- function(strain_name, time_point){
  name<-paste(strain_name, time_point, sep = "")
  return(name)
}

#----------------------------------------------------------------------------------

## subsetting based on size (FSC-W, SSC-W)


f_size_subset<-function(data_frame,columns,col_fscw,limit_fscw,col_sscw,limit_sscw){
  data_frame<-as.data.frame(data_frame)
  subset(data_frame[,columns], 
         data_frame[,col_fscw]> limit_fscw & 
           data_frame[,col_sscw] < limit_sscw &
         data_frame[,col_fscw]> lower_limit_fscw & 
         data_frame[,col_sscw] > lower_limit_sscw )
  
}

#-----------------------------------------------------------------------------------

## saving plots

f_save<-function(plot,plot_name,output_folder,output_path, height,width){
  
  ggsave(paste(plot_name,".jpeg",sep=""),plot,path=output_folder, height=height,
         width=width)
  
}

## saving tables

f_save_table<-function(dataframe,file_name){
  setwd(output_path)
  write.csv(dataframe,file_name)
}


#-----------------------------------------------------------------------------------
## melting dataframe lists for making boxplots
library(reshape2)
f_melt<-function(sequence,df_list,label_list, subset_criterion){
  chosen_frames<-df_list[sequence]
  
  final_label_list<-c()
  for(i in c(1:length(label_list[sequence]))){
    letter<-letters[i]
    label<-paste(letter,label_list[sequence][[i]],sep = "_")
    final_label_list<-c(final_label_list,label)
  }
  
  names(chosen_frames)<-final_label_list
  melt_frames<-melt(chosen_frames)
  melt_frames_subset<-subset(melt_frames,melt_frames[,1]==subset_criterion) # to get only citrine data
  
  return(melt_frames_subset)
}

#-----------------------------------------------------------------------------------