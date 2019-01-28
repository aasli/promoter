

# read fcs files
t0p1<-f_read(t0p1_file,pattern_read)
t0p2<-f_read(t0p2_file,pattern_read)
t0p3<-f_read(t0p3_file,pattern_read)
t0p4<-f_read(t0p4_file,pattern_read)
t0p5<-f_read(t0p5_file,pattern_read)
t0p6<-f_read(t0p6_file,pattern_read)

#----------------------------------------------------------------------------------
# create the list of dataframes that contain all the data
df_list<-c(f_df_list(t0p1,starting_well,2,c(0,600),columns_to_include),
           f_df_list(t0p2,starting_well,6,c(0,1,5,8,15,60),columns_to_include),
           f_df_list(t0p3,starting_well,6,c(0,3,5,8,15,600),columns_to_include),
           f_df_list(t0p4,starting_well,6,c(0,3,5,8,15,600),columns_to_include),
           f_df_list(t0p5,starting_well,6,c(0,3,5,8,20,600),columns_to_include),
           f_df_list(t0p6,starting_well,6,c(0,10,20,50,100,600),columns_to_include)
           )


#----------------------------------------------------------------------------------
##create names for the dataframe list

df_list_names<-c("2769","2773","2828_sd","2828_ypd","2790","2791")
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


# getting a value for size
df_with_size<-lapply(df_list,f_size)
