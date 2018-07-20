

# generate summary statistics from a dataframe


# generate summary statistics from a dataframe

f_descriptives_media<-function(data_frame,dose_descriptives,column){
  final_data_frame<-data.frame()
  data_frame<-as.data.frame(data_frame)
  for (i in dose_descriptives){
    temporary<-data.frame()
    m1_median<-median(data_frame[which(data_frame$"Dose"==i),column])
    m2_mean<-mean(data_frame[which(data_frame$"Dose"==i),column])
    qunatile1<- as.numeric(quantile(data_frame[which(data_frame$"Dose"==i),column])[2])
    qunatile3<- as.numeric(quantile(data_frame[which(data_frame$"Dose"==i),column])[4])
    standard_dev<- sd(data_frame[which(data_frame$"Dose"==i),column])
    coef_of_dist<-(qunatile3-qunatile1)/m1_median
    coef_of_variation<-(standard_dev/m2_mean)
    temporary<-cbind(m1_median,m2_mean,qunatile1,qunatile3,standard_dev,coef_of_dist,
                     coef_of_variation,i)
    final_data_frame<-rbind(final_data_frame,temporary)
  }
  return(final_data_frame)
}



f_descriptives<-function(data_frame,column){
  final_data_frame<-data.frame()
  data_frame<-as.data.frame(data_frame)
  for (i in dose_descriptives){
    temporary<-data.frame()
    m1_median<-median(data_frame[which(data_frame$"Dose"==i),column])
    m2_mean<-mean(data_frame[which(data_frame$"Dose"==i),column])
    qunatile1<- as.numeric(quantile(data_frame[which(data_frame$"Dose"==i),column])[2])
    qunatile3<- as.numeric(quantile(data_frame[which(data_frame$"Dose"==i),column])[4])
    standard_dev<- sd(data_frame[which(data_frame$"Dose"==i),column])
    coef_of_dist<-(qunatile3-qunatile1)/m1_median
    coef_of_variation<-(standard_dev/m2_mean)
    temporary<-cbind(m1_median,m2_mean,qunatile1,qunatile3,standard_dev,coef_of_dist,
                     coef_of_variation,i)
    final_data_frame<-rbind(final_data_frame,temporary)
  }
  return(final_data_frame)
}

## descriptive statistics for comparison script

f_descriptives_comp<-function(data_frame,column){
    m1_median<-median(data_frame[,column])
    m2_mean<-mean(data_frame[,column])
    qunatile1<- as.numeric(quantile(data_frame[,column])[2])
    qunatile3<- as.numeric(quantile(data_frame[,column])[4])
    standard_dev<- sd(data_frame[,column])
    coef_of_dist<-(qunatile3-qunatile1)/m1_median
    coef_of_variation<-(standard_dev/m2_mean)
    temporary<-cbind(m1_median,m2_mean,qunatile1,qunatile3,standard_dev,coef_of_dist,
                     coef_of_variation)
  return(temporary)
}

#-----------------------------------------------------------------------------------------

## getting size from fsc/ssc

f_row_size<-function(frame){
  row_sum<-sum(frame[[1]]^2,frame[[2]]^2)
  row_sqrt<-sqrt(row_sum)
  return(row_sqrt)
}


f_size<-function(dataframe){
  new_df<-as.data.frame(dataframe)[,1:2]
  size<-as.data.frame(apply(new_df,1,f_row_size))
  
  dataframe<-cbind(dataframe,size)
  
}

#-----------------------------------------------------------------------------------------

## creating a fold change table


f_fold_change_table<-function(descriptive_list,stat_to_use){
  sequence<-vector()
  
  # get the medians/means of every sample into a list
  for(i in c(1:length(descriptive_list))){
    stat<-descriptive_list[[i]][,stat_to_use]
    sequence<-c(sequence,stat)
  } 
  
  
  list_of_rows<-list()
  for(j in c(1:length(sequence))){ # take every item on the list and
    row_name<-sequence[[j]]  
    row<-vector()
    for(k in sequence){
      fold<-row_name/k # divide it by every other item on the list
      row<-c(row,fold) # this makes up one row of the final table
    }
    list_of_rows[[j]]<-row # put all rows in a list
  }
  
  table<-as.data.frame(do.call("rbind",list_of_rows)) # turn the list into a dataframe
  rownames(table)<-names(descriptive_list) # name the rows and
  colnames(table)<-names(descriptive_list) # the columns
  return(table)
}
#-----------------------------------------------------------------------------------------

