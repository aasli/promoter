
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
  for(i in doses){
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
  # this function fits a sigmoid curve to the datapoints, using a 5 parameter logistic function. 
  # the input is a dataframe, which should include the datapoints in one column, and the doses in 
  # the other. There should also be a list of starting points, which includes one starting point
  # per parameter to give the poor algorithm somewhere to start. These you have to find by trial 
  # and error. 
  # the function finds the best fit and returns the 5 parameters giving this best fit. 
  function(frame,list_of_starting_points, dose_column, median_column){
    
    
    x = frame[,dose_column]
    y = frame[,median_column]
    
    a<-list_of_starting_points[[1]]
    b<-list_of_starting_points[[2]]
    c<-list_of_starting_points[[3]]
    d<-list_of_starting_points[[4]]
    e<-list_of_starting_points[[5]]
    
    # fitting
    fitmodel <- nlsLM(y ~  ( e+ (a-e)/((1 + ((x/c)^b))^(d))), start=list(a=a,b=b,c=c, d=d,e=e), 
                      weights = (1/(frame[,5])), control = list(maxiter=1000))
    
    
    
    # get the coefficients 
    params=coef(fitmodel)
    return(params)
    
  }


f_ecs<-function(params){
  # calculate ec50, ec90 and ec10
  ec50<-params[3]*(((2^(1/params[4]))-1))^(1/params[2])
  ec90<-params[3]*((((100/90)^(1/params[4]))-1))^(1/params[2])
  ec10<-params[3]*((((10)^(1/params[4]))-1))^(1/params[2])
  
  ec_list<-c(ec10,ec50,ec90)
  print(ec_list)
  return(ec_list)
}


f_sigmoid <- function(params, x) {
  
  ( params[5] + ((params[1]-params[5]) / ((1 + ((x/params[3])^params[2]))^
                                            (params[4]))))
}

f_sigmoid_fit<-function(params,x_values){ 
  # takes the parameters for the best fit, any number of x values, and returns a list of y values
  # for plotting. 
  sigmoid_fit <- f_sigmoid(params,x_values)
  print(params)
  return(sigmoid_fit)
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

#----------------------------------------------------------------------------------------
f_spearman_plot<-function(variance_list,label_list){
  
  final_plot<- ggplot() 
  
  for(i in c(1:length(variance_list))){ #variance list calculated in the facs script.
    print(label_list[[i]])
    single_layer<-
      geom_point(aes_(x=experiment_doses, y=variances[[i]], colour=label_list[i])) 
    final_plot<-final_plot+single_layer
  }
  
  pretty_plot<-final_plot+
    theme_bw() +
    scale_x_log10() +
    ylab("Spearman Correlation") +
    xlab("[aTc] (ng/mL)")
  
}

#----------------------------------------------------------------------------------------
## geom_flat_violin function taken frmo the internet for plotting split violin plots. 
library(ggplot2)
library(dplyr)


"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )
