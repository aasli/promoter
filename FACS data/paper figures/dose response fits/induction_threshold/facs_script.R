

# read fcs files
t0p1<-f_read(t0p1_file,pattern_read)
t0p2<-f_read(t0p2_file,pattern_read)

t1p1<-f_read(t1p1_file,pattern_read)
t1p2<-f_read(t1p2_file,pattern_read)

t2p1<-f_read(t2p1_file,pattern_read)
t2p2<-f_read(t2p2_file,pattern_read)

t3p1<-f_read(t3p1_file,pattern_read)
t3p2<-f_read(t3p2_file,pattern_read)

t4p1<-f_read(t4p1_file,pattern_read)
t4p2<-f_read(t4p2_file,pattern_read)

t5p1<-f_read(t5p1_file,pattern_read)
t5p2<-f_read(t5p2_file,pattern_read)

t6p1<-f_read(t6p1_file,pattern_read)
t6p2<-f_read(t6p2_file,pattern_read)

t7p1<-f_read(t7p1_file,pattern_read)
t7p2<-f_read(t7p2_file,pattern_read)

t8p1<-f_read(t8p1_file,pattern_read)
t8p2<-f_read(t8p2_file,pattern_read)

t9p1<-f_read(t9p1_file,pattern_read)
t9p2<-f_read(t9p2_file,pattern_read)

t10p1<-f_read(t10p1_file,pattern_read)
t10p2<-f_read(t10p2_file,pattern_read)

#----------------------------------------------------------------------------------
# create the list of dataframes that contain all the data
df_list<-c(f_df_list(t0p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t0p2,starting_well,2,c(0,600),columns_to_include),
           f_df_list(t1p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t1p2,starting_well,2,c(0,600),columns_to_include),
           f_df_list(t2p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t2p2,starting_well,2,c(0,600),columns_to_include),
           f_df_list(t3p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t3p2,starting_well,2,c(0,600),columns_to_include),
           f_df_list(t4p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t4p2,starting_well,2,c(0,600),columns_to_include),
           f_df_list(t5p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t5p2,starting_well,2,c(0,600),columns_to_include),
           f_df_list(t6p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t6p2,starting_well,2,c(0,600),columns_to_include),
           f_df_list(t7p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t7p2,starting_well,2,c(0,600),columns_to_include),
           f_df_list(t8p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t8p2,starting_well,2,c(0,600),columns_to_include),
           f_df_list(t9p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t9p2,starting_well,2,c(0,600),columns_to_include),
           f_df_list(t10p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t10p2,starting_well,2,c(0,600),columns_to_include)
          
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
names(descriptives)<-df_list_names

#----------------------------------------------------------------------------------

# getting a value for size
df_with_size<-lapply(df_list,f_size)


#----------------------------------------------------------------------------------

#sigmoid curve fitting

high_color<-"darkblue"
low_color<-"lightblue"
library("minpack.lm")
library(ggplot2)
# pick which strains/time_points you want to plot, and their labels
frame_list<-descriptives[descriptives_to_use_sigmoid]
label_list_sigmoid<-labels_to_use


# define the controls for putting on the plot
control_list<-descriptives[controls_to_use_sigmoid]
control_list_sigmoid<-label_list[controls_to_use_sigmoid]




# replace 0 values in the doses with 0.1
for(i in c(1:length(frame_list))){
  frame_list[[i]][1,8]<-0.19
}



# generate log distributed x values for smoother line fitting. 
library(emdbook)
x_values<-c(0.19,lseq(min(frame_list[[1]][,8]),
               max(frame_list[[1]][,8]),1000))

x_values_plotting<-c(min(frame_list[[1]][,8]),x_values[c(2:length(x_values))])




#define x axis doses and the labels for plotting
doses_experiment<-frame_list[[1]][,8]
breaks_sigmoid<-c(0.19,1,5,20,60,200,600)
labels_x_axis<-c(0,1,5,20,60,200,600)

# apply the fitting to each frame with its own individual starting points.

parameters<-mapply(function_curve_fitting,frame_list,list_of_starting_points,c(1:length(list_of_starting_points)), 
                                 SIMPLIFY = FALSE)

sigmoid_fit_descriptives<-lapply(parameters,f_sigmoid_fit,x_values)

ec_list_descriptives<-lapply(parameters,f_ecs)

# plot the fitted lines and the individual data points
sigmoid_plot<-f_plot_sigmoid_curves(sigmoid_fit_descriptives,frame_list, control_list,
                                    control_list_sigmoid, ec_list_descriptives)


sigmoid_plot
sigmoid_plot +xlim(0,5) + ylim(0,2000)


f_save(sigmoid_plot,paste("sigmoid_fit_fplogistic.jpeg",time_point_sigmoid,sep = "_"),
       output_folder=output_path,output_path="", 
       height=height_sigmoid, width=width_sigmoid)

f_save(sigmoid_plot+xlim(0,5) + ylim(0,2000)+theme(legend.position = "none"),paste("sigmoid_fit_fplogistic_zoom.jpeg",time_point_sigmoid,sep = "_"),
       output_folder=output_path,output_path="", 
       height=height_sigmoid, width=width_sigmoid)

f_save_pdf(sigmoid_plot,paste("sigmoid_fit",time_point_sigmoid,sep = "_"),
       output_folder=output_path,output_path="", 
       height=height_sigmoid, width=width_sigmoid)

#----------------------------------------------------------------------------------
# plot derivative of the sigmoid_fit

x_values_d<-seq(0.19,
                max(frame_list[[1]][,8]),length.out=10000) # for the derivative estimation

axis_break1<-0.19

sigmoid_d_fit<-lapply(parameters,f_d_sigmoid_fit,x_values_d)

derivative_plot<-f_plot_sigmoid_d(x_values_d, sigmoid_d_fit, label_list_derivative)

derivative_plot

f_save(derivative_plot,paste("derivative_sigmoid_fplogistic.jpeg",time_point_sigmoid,sep = "_"),
       output_folder=output_path,output_path="", 
       height=height_sigmoid, width=width_sigmoid)

f_save_pdf(derivative_plot,paste("derivative_sigmoid",time_point_sigmoid,sep = "_"),
       output_folder=output_path,output_path="", 
       height=height_sigmoid, width=width_sigmoid)



#-----------------------------------------------------------------------------------------------

## check for correlation between size and induction. 
spearman_dfs<-c(41,43,44)
label_list_spearman<-c("420")
control_label_list<-c("pTDH3","fry70")

spearman_list<-df_with_size[spearman_dfs]


# replace 0 values in the doses with 0.19
for(i in c(1:length(spearman_list))){
  for(k in c(1:nrow(spearman_list[[i]]))){
    if(spearman_list[[i]][k,4]== 0){
      spearman_list[[i]][k,4]<-0.19
    }
  }
}

spearman_doses<-unique(frame_list[[1]][,8])

variances<-vector("list",length(spearman_list))

for(k in c(1:length(spearman_list))){
  variance<-vector()
  for(i in spearman_doses){
    dataframe<-spearman_list[[k]][which(spearman_list[[k]][,4]==i),]
    var<-cor(dataframe[,3],dataframe[,5], method = "spearman")
    variance<-c(variance,var)
    
  }
  variances[[k]]<-variance
}

variances_data<-variances[[1]]
variances_control<-variances[c(2,3)]





spearman<-f_spearman_plot(list(variances_data),label_list_spearman, variances_control,labels_x_axis,
                          control_label_list)


f_save(spearman,"spearman.jpeg",output_path, "",6,9.04)
f_save_pdf(spearman,"spearman.pdf",output_path, "",6,9.04)

