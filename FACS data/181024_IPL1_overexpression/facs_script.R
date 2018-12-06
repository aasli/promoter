

# read fcs files
t0p1<-f_read(t0p1_file,pattern_read)
t0p2<-f_read(t0p2_file,pattern_read)
t0p3<-f_read(t0p3_file,pattern_read)
t0p4<-f_read(t0p4_file,pattern_read)
t0p5<-f_read(t0p5_file,pattern_read)
t0p6<-f_read(t0p6_file,pattern_read)
t0p7<-f_read(t0p7_file,pattern_read)
t0p8<-f_read(t0p8_file,pattern_read)
t0p9<-f_read(t0p9_file,pattern_read)
t0p10<-f_read(t0p10_file,pattern_read)

#----------------------------------------------------------------------------------
# create the list of dataframes that contain all the data
df_list<-c(f_df_list(t0p1,starting_well,5,c("t0","t1h","t2.5h","r3h","r17h"),columns_to_include),
           f_df_list(t0p2,starting_well,2,c("t0","t2.5h"),columns_to_include),
           f_df_list(t0p3,starting_well,3,c("r1h","r2h","r17h"),columns_to_include),
           f_df_list(t0p4,starting_well,4,c("r1h","r2h","r3h","r17h"),columns_to_include),
           f_df_list(t0p5,starting_well,4,c("r1h","r2h","r3h","r17h"),columns_to_include),
           f_df_list(t0p6,starting_well,2,c("r1h","r17h"),columns_to_include),
           f_df_list(t0p7,starting_well,3,c("r1h","r2h","r17h"),columns_to_include),
           f_df_list(t0p8,starting_well,3,c("r1h","r2h","r17h"),columns_to_include),
           f_df_list(t0p9,starting_well,2,c("r3h","r17h"),columns_to_include),
           f_df_list(t0p10,starting_well,2,c("r3h","r17h"),columns_to_include)
           )


#----------------------------------------------------------------------------------
##create names for the dataframe list

df_list_names<-c("arrest_2789","arrest_2769","growth_2789","growth_2769","from_arrest_0","from_arrest_10","from_arrest_400",
                 "from_growth_h0","from_growth_10", "from_growth_400","control_arrest",
                 "control_growth")
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

