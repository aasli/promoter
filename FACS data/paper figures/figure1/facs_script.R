

# read fcs files
t0p1<-f_read(t0p1_file,pattern_read)
t1p1<-f_read(t1p1_file,pattern_read)
t2p1<-f_read(t2p1_file,pattern_read)
t3p1<-f_read(t3p1_file,pattern_read)
t4p1<-f_read(t4p1_file,pattern_read)
t5p1<-f_read(t5p1_file,pattern_read)
t6p1<-f_read(t6p1_file,pattern_read)
t7p1<-f_read(t7p1_file,pattern_read)
#----------------------------------------------------------------------------------
# create the list of dataframes that contain all the data
df_list<-c(f_df_list(t0p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t3p1,starting_well,2,c(0,200),columns_to_include),
           f_df_list(t1p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t4p1,starting_well,2,c(0,200),columns_to_include),
           f_df_list(t2p1,starting_well,24,experiment_doses,columns_to_include),
           f_df_list(t5p1,starting_well,2,c(0,200),columns_to_include),
           f_df_list(t6p1,starting_well,24,experiment_doses,columns_to_include),
           f_df_list(t7p1,starting_well,2,c(0,200),columns_to_include)
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

descriptives<-c(lapply(df_list,f_descriptives,column=cols_descriptives))

#----------------------------------------------------------------------------------

#sigmoid curve fitting

library("minpack.lm")
#define x axis doses and the labels for plotting
doses_experiment<-frame_list[[1]][,8]
labels_x_axis<-c(0,frame_list[[1]][c(2:nrow(frame_list[[1]])),8])

# pick which strains/time_points you want to plot, and their labels
frame_list<-descriptives[descriptives_to_use]
label_list_sigmoid<-label_list[labels_to_use]


# replace 0 values in the doses with 0.1
for(i in c(1:length(frame_list))){
  frame_list[[i]][1,8]<-0.3
}

# generate log distributed x values for smoother line fitting. 
library(emdbook)
x_values<-lseq(min(frame_list[[1]][,8]),
               max(frame_list[[1]][,8]),1000)


# apply the fitting to each frame with its own individual starting points.
sigmoid_fit_descriptives<-mapply(function_curve_fitting,frame_list,list_of_starting_points, 
                                 SIMPLIFY = FALSE)

# plot the fitted lines and the individual data points
sigmoid_plot<-f_plot_sigmoid_curves(sigmoid_fit_descriptives,frame_list)

f_save(sigmoid_plot,paste("sigmoid_fit_weighted.jpeg",time_point_sigmoid,sep = "_"),
       output_folder=output_path,output_path="", 
       height=height_sigmoid, width=width_sigmoid)





