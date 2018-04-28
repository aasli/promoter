

# read fcs files
t0p1<-f_read(t0p1_file,pattern_read)
t1p1<-f_read(t1p1_file,pattern_read)

#----------------------------------------------------------------------------------
# create the list of dataframes that contain all the data
df_list<-c(f_df_list(t0p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t1p1,starting_well,2,c(0,200),columns_to_include))


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

# smoothed dose response

descriptives_to_use<-descriptives[descriptive_sequence]
descriptives_labels<-label_list_smooth

smooth_dose_response<-f_smooth(descriptives_to_use, descriptives_labels,statistic_to_use,
                               legend_title_smooth, ncol_legend_smooth,
                               scale_x_labels_smooth,scale_x_labels_smooth,plot_title_smooth,
                               xlab_smooth, ylab_smooth,legend_direction_smooth,
                               legend_position_smooth,additional_arguments_smooth)

f_save(smooth_dose_response,paste("smooth_dose_response_0.1_l.jpeg",time_point_smooth,sep = "_"),
       output_folder=output_path,output_path="", 
       height=height_smooth, width=width_smooth)

#----------------------------------------------------------------------------------

## size plots

size_plots<- lapply(df_list,f_size_plot,fsc_column=fscw_column_size,ssc_column=sscw_column_size,
                    xlab_title=xlab_size,ylab_title=ylab_size,xlims=xlimits_size,
                    ylims=ylimits_size)

name_list_size<- strain_names # should contain one name per strain that you want to 
#create a grid for.

label_list_size<- label_list # should contain one label per grid, and the order of the labels must 
#correspond to the order in which the strains appear in name_list.
names(label_list_size)<-name_list_size

# apply the grid function to create a new list with the grids, and name these using the name_list so 
#that you can later save them using the names. 
grid_plots_size<- lapply(name_list_size,f_sizeplots_grid,plot_list=size_plots,
                         legend_label=label_list_size,labels_for_grid=labels_for_grid_size)
names(grid_plots_size)<-name_list_size


mapply(f_save,size_plots,strain_names,
       MoreArgs = list(output_folder=output_path,output_path="size",
                       height=height_size, width=width_size),SIMPLIFY = FALSE)

#----------------------------------------------------------------------------------

## histograms

histograms<-mapply(f_geom_histogram,df_list[dfs_to_histogram],control_sequence,
                   MoreArgs = list(dose_column,
                                   citrine_column,labels_histogram,
                                   doses_histogram,size_histogram,breaks_histogram,
                                   legend_title_histogram,legend_ncol_histogram,
                                   legend_position_histogram,xlimits_histogram,
                                   ylimits_histogram, df_list, label_list[c(3:5)]),
                   USE.NAMES = TRUE, SIMPLIFY = FALSE)

names(histograms)<-strain_names[dfs_to_histogram]
### making the grids


name_list_histogram<-strain_names[dfs_to_histogram] # should contain one name per strain that you want
#to create a grid for.
label_list_histogram<-label_list[dfs_to_histogram] # should contain one label per grid, 
#and the order of the labels must correspond to the order in which the strains appear in name_list.
names(label_list_histogram)<-name_list_histogram


# apply the grid function to create a new list with the grids, 
#and name these using the name_list so that you can later save them using the names. 
grid_plots_histogram<- lapply(name_list_histogram,f_histogram_grid,plot_list=histograms,
                              legend_label=label_list_histogram, grid_labels=grid_labels_histograms)

names(grid_plots_histogram)<-name_list_histogram

## saving

mapply(f_save,grid_plots_histogram,names(grid_plots_histogram),
       MoreArgs = list(output_folder=output_path,output_path="histograms",
                       height=height_histograms, width=width_histograms),SIMPLIFY = FALSE)



## each dose plotted on its own plot area. 

histogram_grid<-lapply(df_list[dfs_to_histogram], f_individual_histograms,x_limits_individual_histogram,
                       x_breaks_individual_histogram, y_breaks_individual_histogram, 
                       y_limits_individual_histogram)

mapply(f_save,histogram_grid, names_individual_histograms,
       MoreArgs = list(output_path, "", 10, 15))

#----------------------------------------------------------------------------------

## descriptive plots

all_plots<-f_descriptive_plotting()



grid_plots_descriptives_1<- lapply(strain_names[c(1,2)],f_descriptive_grid,plot_list=all_plots[[1]],
                                   legend_label=label_list,grid_labels=c("median","cd"))
names(grid_plots_descriptives_1)<-strain_names[c(1,2)]

grid_plots_descriptives_2<- lapply(strain_names,f_descriptive_grid,plot_list=all_plots[[2]],
                                   legend_label=label_list,grid_labels=c("mean","cv"))
names(grid_plots_descriptives_2)<-strain_names

mapply(f_save,grid_plots_descriptives_1,names(grid_plots_descriptives_1),
       MoreArgs = list(output_folder=output_path, output_path="descriptives1",
                       height=height_descriptives, width=width_descriptives),SIMPLIFY = FALSE)

mapply(f_save,grid_plots_descriptives_1,names(grid_plots_descriptives_1),
       MoreArgs = list(output_folder=output_path, output_path="descriptives", 
                       height=height_descriptives, width=width_descriptives),SIMPLIFY = FALSE)

#----------------------------------------------------------------------------------

# getting a value for size
df_with_size<-lapply(df_list,f_size)

#----------------------------------------------------------------------------------



density_plots<-lapply(df_with_size,f_density_plot, labels=labels_size_vs_induction, 
                      doses=doses_size_vs_induction, palette=palette_density, 
                      x_breaks=x_breaks_density,legend_title=legend_title_density, 
                      ncol_legend=ncol_legend_density, legend_position=legend_position_density, 
                      size_column=size_column, citrine_column=citrine_column)


name_list_density<- strain_names # should contain one name per strain 
#that you want to create a grid for.
label_list_density<-label_list # should contain one label per grid, 
#and the order of the labels must correspond to the order in which the strains appear in name_list.
names(label_list_density)<-name_list_density

# apply the grid function to create a new list with the grids, 
#and name these using the name_list so that you can later save them using the names. 
grid_plots_density<- lapply(name_list_density,f_density_grid,plot_list=density_plots,
                            legend_label=label_list_density, grid_labels=grid_labels_density)

names(grid_plots_density)<-name_list_density

mapply(f_save,density_plots,names(density_plots),
       MoreArgs = list(output_folder=output_path,output_path="density",
                       height=height_density, width=width_density),SIMPLIFY = FALSE)

#----------------------------------------------------------------------------------

#sigmoid curve fitting

library("minpack.lm")
# pick which strains/time_points you want to plot, and their labels
frame_list<-descriptives[descriptives_to_use_sigmoid]
label_list_sigmoid<-label_list[labels_to_use]


#define x axis doses and the labels for plotting
doses_experiment<-frame_list[[1]][,8]
breaks_sigmoid<-c(0.1,doses_experiment[c(2:length(doses_experiment))])
labels_x_axis<-c(0,frame_list[[1]][c(2:nrow(frame_list[[1]])),8])


# replace 0 values in the doses with 0.1
for(i in c(1:length(frame_list))){
  frame_list[[i]][1,8]<-0.1
}

# define the controls for putting on the plot
control_list<-descriptives[controls_to_use_sigmoid]
control_list_sigmoid<-label_list[controls_to_use_sigmoid]

# generate log distributed x values for smoother line fitting. 
library(emdbook)
x_values<-c(0,lseq(min(frame_list[[1]][,8]),
               max(frame_list[[1]][,8]),1000))

x_values_plotting<-c(min(frame_list[[1]][,8]),x_values[c(2:length(x_values))])

# apply the fitting to each frame with its own individual starting points.

parameters<-mapply(function_curve_fitting,frame_list,list_of_starting_points, 
                                 SIMPLIFY = FALSE)

sigmoid_fit_descriptives<-lapply(parameters,f_sigmoid_fit,x_values)

ec_list_descriptives<-lapply(parameters,f_ecs)

# plot the fitted lines and the individual data points
sigmoid_plot<-f_plot_sigmoid_curves(sigmoid_fit_descriptives,frame_list, control_list,
                                    control_list_sigmoid, ec_list_descriptives)

f_save(sigmoid_plot,paste("sigmoid_fit_size_gated.jpeg",time_point_sigmoid,sep = "_"),
       output_folder=output_path,output_path="", 
       height=height_sigmoid, width=width_sigmoid)

#----------------------------------------------------------------------------------
# plot derivative of the sigmoid_fit

x_values_d<-seq(0,
               max(frame_list[[1]][,8]),length.out=10000)

sigmoid_d_fit<-lapply(parameters,f_d_sigmoid_fit,x_values_d)

f_derivative_plot<-f_plot_sigmoid_d(x_values_d, sigmoid_d_fit, label_list_derivative)




f_save(f_derivative_plot,paste("derivative_sigmoid_size_gated.jpeg",time_point_sigmoid,sep = "_"),
       output_folder=output_path,output_path="", 
       height=height_sigmoid, width=width_sigmoid)

#----------------------------------------------------------------------------------

## QQplots
df_normalized_list<-lapply(df_list,f_normalize,column=3)
df_normalized_list<-lapply(df_normalized_list,f_normalize,column=4)

dataframelist_1<-mapply(f_dataframe_create,dataframelist1,dataframelist1_doses,
                        MoreArgs = list(dataframe_list=df_normalized_list, dose_column=5),
                        SIMPLIFY = FALSE)

dataframelist_2<-mapply(f_dataframe_create,dataframelist2,dataframelist2_doses,
                        MoreArgs = list(dataframe_list=df_normalized_list, dose_column=5),
                        SIMPLIFY = FALSE)

qqplots<-mapply(f_qqplot,dataframelist_1,dataframelist_2,dataframelist1_columns,
                dataframelist2_columns,xlims_qqplot,ylims_qqplot,titles_qqplot,
                dataframelist1,dataframelist2, SIMPLIFY = FALSE)

names(qqplots)<-titles_qqplot

mapply(f_save,qqplots,names(qqplots),
       MoreArgs = list(output_folder=output_path,output_path="qq",
                       height=height_qq, width=width_qq),SIMPLIFY = FALSE)


#-----------------------------------------------------------------------------------------------

## check for correlation between size and induction. 
spearman_dfs<-descriptive_sequence
label_list_spearman<-label_list_smooth

spearman_list<-df_with_size[spearman_dfs]

variances<-vector("list",length(spearman_list))
variances<-vector("list",length(spearman_list))

for(k in c(1:length(spearman_list))){
  variance<-vector()
  for(i in experiment_doses){
    dataframe<-spearman_list[[k]][which(spearman_list[[k]][,4]==i),]
    var<-cor(dataframe[,3],dataframe[,5], method = "spearman")
    variance<-c(variance,var)
    
  }
  variances[[k]]<-variance
}

spearman<-f_spearman_plot(variances,label_list_spearman)


f_save(spearman,"spearman.jpeg",output_path, "",10,15)

