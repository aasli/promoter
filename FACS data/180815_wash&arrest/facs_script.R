

# read fcs files
t0p1<-f_read(t0p1_file,pattern_read)
t0p2<-f_read(t0p2_file,pattern_read)
t0p3<-f_read(t0p3_file,pattern_read)
t0p4<-f_read(t0p4_file,pattern_read)

#----------------------------------------------------------------------------------
# create the list of dataframes that contain all the data
df_list<-c(f_df_list(t0p1,starting_well,6,time_points,columns_to_include),
           f_df_list(t0p2,starting_well,3,c("0","360min","21h"),columns_to_include),
           f_df_list(t0p3,starting_well,3,c("0","360min","21h"),columns_to_include),
           f_df_list(t0p4,starting_well,1,"0",columns_to_include)
           )


#----------------------------------------------------------------------------------
##create names for the dataframe list

df_list_names<-c("2772","2772_atc","2769","2769Nstaved","2781")
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

