

# read fcs files
t0p1<-f_read(t0p1_file,pattern_read)
t0p2<-f_read(t0p2_file,pattern_read)
t0p3<-f_read(t0p3_file,pattern_read)

#----------------------------------------------------------------------------------


# create the list of dataframes that contain all the data
df_list<-c(f_df_list(t0p1,starting_well,2,c("parent","ipl1"),columns_to_include),
           f_df_list(t0p2,starting_well,2,c("parent","ipl1"),columns_to_include),
           f_df_list(t0p3,starting_well,5,c("parent","ipl1_5","ipl1_30","ipl1_60","ipl1_100"),columns_to_include)
           )


#----------------------------------------------------------------------------------
##create names for the dataframe list

df_list_names<-c("161018",
                 "171018",
                 "181018")
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

