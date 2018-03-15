
library(ggplot2)



#-------------------------------------------------------------------------------------------

## plotting dose response curves of multiple strains, smoothed

f_smooth<-function(descriptive_list,label_list,stat,legend_title,ncol_legend,scale_x_breaks,
                   scale_x_labels,plot_title,xlab_title,ylab_title,legend_direction,
                   legend_position,additional_arguments){
  
  final_plot<- ggplot() 
  
  for(i in c(1:length(descriptive_list))){
    single_layer<-geom_point(aes_(x=descriptive_list[[i]][,8],
                                  y=descriptives_to_use[[i]][,stat],
                                  colour=label_list[i]),size=3)
    final_plot<-final_plot+single_layer
  }
  
  for(i in c(1:length(descriptive_list))){    
    smooth_layer<- geom_smooth(data=descriptive_list[[i]],
                               aes_(x=descriptive_list[[i]][,8],
                                    y=descriptive_list[[i]][,stat],
                                    colour=label_list[i]),
                               se=FALSE,size=1.5)
    final_plot<-final_plot+smooth_layer
  }
  
  # -min(descriptive_list[[i]][,1]))  , 
  #                           colour=label_list[i])
  pretty_plot<-final_plot +
    scale_colour_brewer(type="qual", palette = "Dark2") +
    scale_x_log10(breaks = scale_x_breaks,
                  labels = scale_x_labels) +
    guides(colour=guide_legend(title = legend_title,ncol=ncol_legend)) +
    ggtitle(plot_title) +
    xlab(xlab_title) +
    ylab(ylab_title) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          legend.direction = legend_direction, legend.position = legend_position,
          legend.background = element_blank()) +
    additional_arguments
  
  return(pretty_plot)
  
}

#------------------------------------------------------------------------------------------

## size plots

# size plot per well

f_size_plot<-function(dataframe,fsc_column,ssc_column, xlab_title, ylab_title,
                      xlims,ylims){
  dataframe<-as.data.frame(dataframe)
  ggplot() +
    geom_point(aes(x=dataframe[,fsc_column],y=dataframe[,ssc_column]), size=0.5) +
    
    geom_segment(aes(x=25000,xend=100000, y=20000,yend=20000), colour="red")+
    geom_segment(aes(x=25000,xend=100000, y=100000,yend=100000), colour="red")+
    
    geom_segment(aes(x=25000,xend=25000, y=20000,yend=100000), colour="blue")+
    geom_segment(aes(x=100000,xend=100000, y=20000,yend=100000), colour="blue")+
    
    theme_bw() +
    xlab(xlab_title) +
    ylab(ylab_title) +
  xlim(xlims) +
  ylim(ylims) 
}


# with grid saving, 4 time points, 1 plot per time point, 1 grid per strain

f_sizeplots_grid<- function(plot_list,name,legend_label,labels_for_grid){
  string_match<-(grep(name,names(plot_list),value = FALSE))
  strain_label<-grep(name,names(legend_label),value = FALSE)
  cowplot::plot_grid(plot_list[[string_match[1]]] + 
                       ggtitle(legend_label[[strain_label[1]]])+
                       theme(title = element_text(size = 8)),
                     #plot_list[[string_match[4]]],
                     labels = labels_for_grid)
}


#------------------------------------------------------------------------------------------

## histograms

f_geom_histogram <- function(dataframe,control_sequence, dose_column, citrine_column, labels_histogram, 
                             doses_histogram,size,breaks,legend_title,legend_ncol,
                             legend_position,xlimits,ylimits,control_list, labels_controls) {
  
  dataframe<-as.data.frame(dataframe)

  final_plot<-ggplot()
  
  for(i in doses_histogram) {
    single_layer<- geom_density(
      aes_(x=dataframe[which(dataframe[,dose_column]==i),citrine_column],
           colour=labels_histogram[which(doses_histogram==i)]),size=size) 
    
    final_plot<- final_plot + single_layer
    
  }
  
  
  for(i in control_sequence) {
    control<-as.data.frame(control_list[i])
    single_layer<- geom_density(
      aes_(x=control[,citrine_column],
           colour=labels_controls[i]),size=size) 
    
    final_plot<- final_plot + single_layer
    
  }
  
  
  pretty_plot <- final_plot + 
  
    theme_bw() +
    scale_colour_manual(values = plot_palette,
      breaks = breaks) +
    guides(colour=guide_legend(title = legend_title,ncol = legend_ncol)) +
    theme(panel.grid = element_blank(), legend.position = legend_position ,
          legend.title = element_text(size=7,face="bold"),
          legend.text = element_text(size=6),
          legend.key.size = unit(0.3,"cm"),
          legend.background = element_blank(),
          plot.title = element_text(size=8,face="bold")) +
    xlim(xlimits) +
    ylim(ylimits) +
    xlab("Fluorescence") +
    ylab("Count")
  
  
  return(pretty_plot)
  
  
}

# histogram grids

f_histogram_grid<- function(plot_list,name,legend_label,grid_labels){
  
  string_match<-(grep(name,names(plot_list),value = FALSE))
  strain_label<-grep(name,names(legend_label),value = FALSE)
 
  cowplot::plot_grid(plot_list[[string_match[1]]] + 
                       ggtitle(legend_label[[strain_label[1]]]) +
                       theme(title = element_text(size=6)),
                     labels = grid_labels)
}

#------------------------------------------------------------------------------------------
## histograms for comparison script

f_histogram_comparison<-function(title,
                      sequence,xlimits,
                      df_list,ylimits,
                      legend_position, 
                      size_value,
                      
                      
                      guide_title,label_list,
                      ncol_legend,
                      column_to_plot){
  
  
  final_plot<-ggplot()
  for(frame in sequence){
    
    single_layer<-
      geom_density(aes_(x=as.data.frame(df_list[frame])[,column_to_plot],
                        colour=(label_list[frame])),size=size_value)
    final_plot<-final_plot + single_layer
  }
  
  
  pretty_plot<-final_plot +
    theme_bw() +
    guides(colour=guide_legend(title=guide_title, ncol=ncol_legend)) +
    theme(legend.position = legend_position, 
          legend.background = element_blank(), 
          legend.title = element_text(size=14), 
          legend.text = element_text(size=12), 
          legend.key.size = unit(0.5, "cm"), 
          plot.title = element_text(size=12, face="bold"))+
    scale_colour_manual(values = mypalette) +
    ggtitle(paste(title)) +
    ylim(ylimits) +
    xlim(xlimits) +
    xlab("Fluorescence (a.u.)") 
  
  
  return(pretty_plot)
}


## control histograms

f_histogram_comparison_controls<-function(control_list, sequence,
                                          column_to_plot, label_list,size_value,
                                          guide_title,ncol_legend,legend_position,title,
                                          xlimits_controls,ylimits_controls, mypalette){
  
  
  final_plot<-ggplot()
  for(i in c(sequence)){
    frame<-as.data.frame(control_list[[i]])
    single_layer<-
      geom_density(aes_(x=frame[,column_to_plot],
                        colour=(label_list[i])),size=size_value)
    final_plot<-final_plot + single_layer
  }
  
  
  pretty_plot<-final_plot +
    theme_bw() +
    guides(colour=guide_legend(title=guide_title, ncol=ncol_legend)) +
    theme(legend.position = legend_position, 
          legend.background = element_blank(), 
          legend.title = element_text(size=14), 
          legend.text = element_text(size=12), 
          legend.key.size = unit(0.5, "cm"), 
          plot.title = element_text(size=12, face="bold"))+
    scale_colour_manual(values = mypalette) +
    ggtitle(paste(title)) +
    ylim(ylimits_controls) +
    xlim(xlimits_controls) +
    xlab("Fluorescence (a.u.)") 
  
  
  return(pretty_plot)
}
#----------------------------------------------------------------------------------------

## descriptive plots

f_point_plots<- function(name,dose_column,data_list,stat_column,palette,
                         colour_labels,scale_x_breaks,xlab_title,ylab_title,
                         legend_title){
  
 
  frame <- (grep(name, names(data_list), value= FALSE))
  
  plot<-ggplot() +
    geom_point(aes(x=data_list[[frame[1]]][,dose_column],
                   y=data_list[[frame[1]]][,stat_column], colour=colour_labels[[1]]),
               size=size_value_descriptives) +
    geom_point(aes(x=data_list[[frame[2]]][,dose_column],
                   y=data_list[[frame[2]]][,stat_column], colour=colour_labels[[2]]),
               size=size_value_descriptives) +
    geom_point(aes(x=data_list[[frame[3]]][,dose_column],
                   y=data_list[[frame[3]]][,stat_column], colour=colour_labels[[3]]),
               size=size_value_descriptives) +
    # geom_point(aes(x=data_list[[frame[4]]][,dose_column],
    #                y=data_list[[frame[4]]][,stat_column], colour=colour_labels[[4]]),
    #            size=size_value_descriptives) +
    scale_colour_manual(values = palette) +
    theme_bw() +
    theme(panel.grid = element_blank(), axis.text = element_text(size = 8),
          axis.title = element_text(size=8),plot.title = element_text(hjust=0.5,size=12)) +
    scale_x_log10(breaks=scale_x_breaks) +
    xlab(xlab_title) +
    ylab(ylab_title) +
    guides(colour=guide_legend(title=legend_title)) 
  
  
  return(plot)
}


# grids
f_descriptive_grid<- function(plot_list,name,legend_label,grid_labels){
 
  string_match<-(grep(name,names(plot_list),value = FALSE))
  strain_label<-grep(name,names(legend_label),value = FALSE)
  cowplot::plot_grid(plot_list[[string_match[1]]] +
                       ggtitle(legend_label[[strain_label[1]]]) +
                       theme(title = element_text(size=6)),
                     plot_list[[string_match[2]]] + theme(legend.position = "none"),
                     labels = grid_labels, nrow=2)
}

f_descriptive_plotting<-function(){
  all_plots_1<-list()
  all_plots_2<-list()
  
  for(i in stat_columns_1){
    
    plot_list<-lapply(strain_names,f_point_plots,
                      data_list = descriptives, dose_column = 8, stat_column = i,
                      palette=palette,colour_labels=colour_labels,scale_x_breaks=scale_x_breaks,
                      xlab_title=xlab_title,ylab_title=ylab_title,legend_title=legend_title)
    names(plot_list) <- strain_names
    
    all_plots_1<-c(all_plots_1,plot_list)
  }
  
  for(i in stat_columns_2){
    plot_list<-lapply(strain_names,f_point_plots,
                      data_list = descriptives, dose_column = 8, stat_column = i,
                      palette=palette,colour_labels=colour_labels,scale_x_breaks=scale_x_breaks,
                      xlab_title=xlab_title,ylab_title=ylab_title,legend_title=legend_title)
    names(plot_list) <- strain_names
    
    all_plots_2<-c(all_plots_2,plot_list)
  }
  
  all_plots<-list(all_plots_1,all_plots_2)
  
  return(all_plots)
}



#-------------------------------------------------------------------------------------

## density plots, size vs. fluorescence 

f_density_plot<-function(dataframe,labels,doses,size_column, citrine_column, 
                         legend_position,palette, x_breaks, legend_title, ncol_legend){
  
  final_plot<-ggplot()  
  dataframe<-as.data.frame(dataframe)
  for(i in 0){
    single_layer<- geom_density_2d(
      aes_(x=dataframe[which(dataframe[,dose_column]==i),size_column],
           y=dataframe[which(dataframe[,dose_column]==i),citrine_column],
           colour=labels[which(doses==i)]), 
      size=1) 
    
    final_plot<- final_plot + single_layer
  }
  
  
  pretty_plot<- final_plot + 
    theme_bw() + 
    theme(panel.grid = element_blank(), legend.position = legend_position,
          legend.title = element_text(size=7,face="bold"),
          legend.text = element_text(size=6),
          legend.key.size = unit(0.3,"cm"),
          legend.background = element_blank(),
          plot.title = element_text(size=8,face="bold")) +
    xlab("Size") + ylab("Fluorescence (a.u.)") +
    scale_colour_manual(values = palette,
                        breaks = x_breaks) +
    guides(colour=guide_legend(title = legend_title, ncol = ncol_legend)) 
  
  
  return(pretty_plot)
}

# density grids

f_density_grid<- function(plot_list,name,legend_label, grid_labels){
  string_match<-(grep(name,names(plot_list),value = FALSE))
  strain_label<-grep(name,names(legend_label),value = FALSE)
  cowplot::plot_grid(plot_list[[string_match[1]]] + 
                       ggtitle(legend_label[[strain_label[1]]])+
                       theme(title = element_text(size=6)),
                     plot_list[[string_match[2]]] + theme(legend.position = "none"),
                     plot_list[[string_match[3]]] + theme(legend.position = "none"),
                     plot_list[[string_match[4]]] + theme(legend.position = "none"),
                     labels = grid_labels)
}
#----------------------------------------------------------------------------------------

## size vs fluorescence plots for comparison script

f_size_vs_fluorescence_comp<-function(sequence,title,df_list,size_column,size_value,
                                      fluorescence_column,label_list,
                                      legend_position,legend_title,xlimits,ylimits,
                                      ncol_legend){
  final_plot<-ggplot()
  
  for(frame in sequence){
    single_layer_uninduced<-
      geom_density2d(aes_(x=as.data.frame(df_list[[frame]])[,size_column],
                          y=as.data.frame(df_list[[frame]])[,fluorescence_column],
                          colour=paste(label_list[frame])),size=size_value
      )
    final_plot<-final_plot + single_layer_uninduced
  }
  
  pretty_plot<-final_plot +
    theme_bw() +
    theme(legend.position =legend_position, 
          legend.background = element_blank(),
          legend.text = element_text(size=12),
          legend.key.size = unit(0.5,"cm"),
          legend.title = element_text(size=14,face="bold"),
          plot.title = element_text(size=12, face="bold"),
          axis.text = element_text(size=10),
          axis.title = element_text(size=10))+
    ggtitle(title) +
    guides(colour=guide_legend(title=legend_title, ncol = ncol_legend)) +
    scale_color_manual(values = mypalette) +
    xlab("Size")+
    ylab("Fluorescence (a.u.)") +
    ylim(ylimits) +
    xlim(xlimits)
  
  return(pretty_plot)
}

#----------------------------------------------------------------------------------------

##boxplots for comparison

f_boxplot<-function(frames,ylimits){
  plot<-ggplot(frames) +
    geom_boxplot(aes(x=reorder(L1,value,FUN="median"),y=value))+
    coord_flip() +
    ylim(ylimits) +
    theme_bw() +
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour="black"),
          axis.text = element_text(size=14),
          axis.title = element_text(size=14)
    )+
    ylab("Fluorescence (a.u.)") +
    xlab("")
  return(plot)
  
}


#----------------------------------------------------------------------------------------

## sigmoid curve fitting

## plotting

function_curve_fitting<-
  # this function fits a sigmoid curve to the datapoints. 
  # a is the upper asymptote, b is the steepest slope, c is the x axis value at b, d is the lower 
  # asymptote. you can define these individually for every curve.
  # a negative b gives you an inverse sigmoid, a positive one gives you a sigmoid. 
  # the function creates a list of fitted values, one for each "frame" input. 
  function(frame,list_of_starting_points){
    f_sigmoid <- function(params, x) {
      ( (params[1] / (1 + exp(params[2] * (x - params[3])))))
    }
    
    x = frame[,8]
    y = frame[,1]
    
    a<-list_of_starting_points[[1]]
    b<-list_of_starting_points[[2]]
    c<-list_of_starting_points[[3]]
    
    # fitting
    fitmodel <- nlsLM(y ~  (a/(1 + exp(b * (x-c)))), start=list(a=a,b=b,c=c), 
                      weights = (1/frame[,5]))
    
    # get the coefficients 
    params=coef(fitmodel)
    
    sigmoid_fit <- f_sigmoid(params,x_values)
    
    return(sigmoid_fit)
  }


## plotting the sigmoid curves.

f_plot_sigmoid_curves<-
  # plot the fitted line and the individual data points. 
  function(fit_list,frame_list){
    
    final_plot<-ggplot() 
    # plot the lines
    for (i in c(1:length(fit_list))){
      single_layer_line<- geom_line(aes_(x=x_values, y=fit_list[[i]],
                                         colour=label_list_sigmoid[[i]]),size=1)
      final_plot<-final_plot+single_layer_line
    }
    
    # plot the individual data points
    for (j in c(1:length(frame_list))){
      single_layer_point<- geom_point(aes_(x=frame_list[[j]][,8],y=frame_list[[j]][,1],
                                           colour=label_list_sigmoid[[j]]))   
      final_plot<-final_plot+single_layer_point
    }
    
    
    pretty_plot<-final_plot +
      scale_colour_manual(values = colour_palette) +
      scale_x_log10(breaks = breaks_sigmoid,
                    labels = labels_x_axis) +
      scale_y_log10() +
      guides(colour=guide_legend(title = "Strains",nrow=2)) +
      ggtitle(plot_title) +
      xlab(xlabel) +
      ylab("Fluorescence (a.u.)") +
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            legend.direction = "horizontal", legend.position = "bottom") +
      expand_limits(x=0.3)
    
    return(pretty_plot)
  }


#----------------------------------------------------------------------------------------

## QQplots

f_qqplot<-function(dataframe1, dataframe2,column1,column2, xlims, ylims, title, label_1, 
                   label_2){
plot_values<-as.data.frame(qqplot(x=dataframe1[,column1], 
                                  y=dataframe2[,column2], plot.it=FALSE))

qqplot<-ggplot(plot_values) +
  geom_point(aes(x=x,y=y)) +
  xlab(colnames(dataframe1)[column1]) +
  ylab(colnames(dataframe1)[column2]) +
  geom_abline(aes(slope=1,intercept=0)) +
  
  theme_bw() +
  xlim(xlims) +
  ylim(ylims) +
  ggtitle(title)

return(qqplot)

}
