

# read fcs files
t0p1<-f_read(t0p1_file,pattern_read)
t1p1<-f_read(t1p1_file,pattern_read)
t2p1<-f_read(t2p1_file,pattern_read)
t3p1<-f_read(t3p1_file,pattern_read)
t4p1<-f_read(t4p1_file,pattern_read)
t5p1<-f_read(t5p1_file,pattern_read)
t6p1<-f_read(t6p1_file,pattern_read)
t7p1<-f_read(t7p1_file,pattern_read)
t8p1<-f_read(t8p1_file,pattern_read)
t9p1<-f_read(t9p1_file,pattern_read)
#----------------------------------------------------------------------------------
# create the list of dataframes that contain all the data
df_list<-c(f_df_list(t0p1,starting_well,wells_per_sample,ypethanol_doses,columns_to_include),
           f_df_list(t3p1,starting_well,2,c(0,400),columns_to_include),
           f_df_list(t1p1,starting_well,wells_per_sample,glycerol_doses,columns_to_include),
           f_df_list(t4p1,starting_well,2,c(0,200),columns_to_include),
           f_df_list(t2p1,starting_well,24,proline_doses,columns_to_include),
           f_df_list(t5p1,starting_well,2,c(0,200),columns_to_include),
           f_df_list(t6p1,starting_well,24,sd_doses,columns_to_include),
           f_df_list(t7p1,starting_well,2,c(0,400),columns_to_include),
           f_df_list(t8p1,starting_well,24,ypd_doses,columns_to_include),
           f_df_list(t9p1,starting_well,2,c(0,600),columns_to_include)
           )
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

descriptives<-c(mapply(f_descriptives,df_list,experiment_doses_desc,
                       MoreArgs = list(column=3),SIMPLIFY = F))

#----------------------------------------------------------------------------------

# getting a value for size
df_with_size<-lapply(df_list,f_size)

