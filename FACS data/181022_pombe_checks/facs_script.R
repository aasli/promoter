

# read fcs files
t0p1<-f_read(t0p1_file,pattern_read)

#----------------------------------------------------------------------------------


# create the list of dataframes that contain all the data
df_list<-c(f_df_list(t0p1,starting_well,2,c("1","2","3","4","8-1","8-2"),columns_to_include)
           )


#----------------------------------------------------------------------------------
##create names for the dataframe list

df_list_names<-c("p2_2377-1","p2_2377-2","p2_2377-3","p2_2377-4","p2_2378-1","p2_2378-2")
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

