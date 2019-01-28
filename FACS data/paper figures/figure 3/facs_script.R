

# read fcs files
t0p1<-f_read(t0p1_file,pattern_read)
t1p1<-f_read(t1p1_file,pattern_read)
t2p1<-f_read(t2p1_file,pattern_read)

t0p1c<-f_read(t0p1c_file,pattern_read)
t1p1c<-f_read(t1p1c_file,pattern_read)
t2p1c<-f_read(t2p1c_file,pattern_read)

proline<-f_read(proline_file,pattern_read)
media<-f_read(media_file,pattern_read)

#----------------------------------------------------------------------------------
# create the list of dataframes that contain all the data
df_list<-c(f_df_list(t0p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t0p1c,starting_well,2,c(0,200),columns_to_include),
           f_df_list(t1p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t1p1c,starting_well,2,c(0,200),columns_to_include),
           f_df_list(t2p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t2p1c,starting_well,2,c(0,200),columns_to_include),
           f_df_list(proline,starting_well,wells_per_sample,experiment_doses,columns_to_include))


#----------------------------------------------------------------------------------
##create names for the dataframe list

df_list_names<-vector()
for (i in time_points){
  name<-lapply(strain_names,f_names_df_list,i)
  df_list_names<-c(df_list_names, name)
}

names(df_list)<-df_list_names

#----------------------------------------------------------------------------------
# size subsetting, if wanted

if(size_subset==TRUE){
size_subset_df<-lapply(df_list,f_size_subset,columns=columns, 
                       col_fscw=col_fscw,col_sscw=col_sscw,limit_fscw=limit_fscw,
                       limit_sscw=limit_sscw, lower_limit_fscw=lower_limit_fscw,
                       lower_limit_sscw=lower_limit_sscw)

df_list<-size_subset_df
}

#----------------------------------------------------------------------------------

# descriptive stats

descriptives<-c(lapply(df_list,f_descriptives,column=cols_descriptives))

#----------------------------------------------------------------------------------

# getting a value for size
df_with_size<-lapply(df_list,f_size)

#----------------------------------------------------------------------------------

setwd("C:/repos/promoter/FACS data/paper figures/figure 3")
mapply("write.csv", descriptives[c(2,3,4,8,9,10)], paste(label_list[c(2,3,4,8,9,10)], ".csv", sep = ""), SIMPLIFY = F)
