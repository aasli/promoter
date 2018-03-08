
library(ggplot2)

## colour scale
plot_palette<-c(
  "0"="#a2f1f6",
  "1"="#80e8a1",
  "3"="#76e170",
  "5"="#94da61",
  "10"="#b8d353",
  "20"="#cdb745",
  "30"="#c67b38",
  "60"="#bf3b2c",
  "100"="#b82149",
  "400"="#b11679",
  "1000"="#a90caa",
  "3000"="#6703a3",
  "no repressor"="orange")

#-------------------------------------------------------------------------------------------

## plotting dose response curves of multiple strains, smoothed

f_smooth<-function(descriptive_list,label_list,stat,legend_title,ncol_legend,scale_x_breaks,
                   scale_x_labels,plot_title,xlab_title,ylab_title,legend_direction,
                   legend_position,additional_arguments){
  
  final_plot<- ggplot() 
  
  for(i in c(1:length(descriptive_list))){
    single_layer<-geom_point(aes_(x=descriptive_list[[i]][,8],
                                  y=descriptives_to_use[[i]][,stat],
                                  colour=label_list[i]),size=1)
    final_plot<-final_plot+single_layer
  }
  
  for(i in c(1:length(descriptive_list))){    
    smooth_layer<- geom_smooth(data=descriptive_list[[i]],
                               aes_(x=descriptive_list[[i]][,8],
                                    y=descriptive_list[[i]][,stat],
                                    colour=label_list[i]),
                               se=FALSE,size=1)
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
                     plot_list[[string_match[2]]],
                     plot_list[[string_match[3]]],
                     plot_list[[string_match[4]]],
                     labels = labels_for_grid)
}


#------------------------------------------------------------------------------------------

## histograms

f_geom_histogram <- function(dataframe, dose_column, citrine_column, labels_histogram, 
                             doses_histogram,size,breaks,legend_title,legend_ncol,
                             legend_position,xlimits) {
  
  dataframe<-as.data.frame(dataframe)
  
  final_plot<-ggplot()
  
  for(i in doses_histogram) {
    single_layer<- geom_density(
      aes_(x=dataframe[which(dataframe[,dose_column]==i),citrine_column],
           colour=labels_histogram[which(doses_histogram==i)]),size=size) 
    
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
    xlab("Fluorescence") +
    ylab("Count")
  
  
  return(pretty_plot)
  
  
}

# histogram grids

f_histogram_grid<- function(plot_list,name,legend_label,grid_labels){
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

#----------------------------------------------------------------------------------------

## descriptive plots

f_point_plots<- function(name,dose_column,data_list,stat_column,palette,
                         colour_labels,scale_x_breaks,xlab_title,ylab_title,
                         legend_title){
  
  frame <- (grep(name, names(data_list), value= FALSE))
  
  plot<-ggplot() +
    geom_point(aes(x=data_list[[frame[1]]][,dose_column],
                   y=data_list[[frame[1]]][,stat_column], colour=colour_labels[[1]])) +
    geom_point(aes(x=data_list[[frame[2]]][,dose_column],
                   y=data_list[[frame[2]]][,stat_column], colour=colour_labels[[2]])) +
    geom_point(aes(x=data_list[[frame[3]]][,dose_column],
                   y=data_list[[frame[3]]][,stat_column], colour=colour_labels[[3]])) +
    geom_point(aes(x=data_list[[frame[4]]][,dose_column],
                   y=data_list[[frame[4]]][,stat_column], colour=colour_labels[[4]])) +
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
                       ggtitle(legend_label[[strain_label[1]]])+
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

f_stat_summary<-function(dataframe){
  labels<-unique(dataframe$L1)
  frame<-t(as.data.frame(c(1,1,1)))
  
  for(i in labels){
    data<-dataframe[which(dataframe$L1==i),2]
    median<-median(data)
    quantile_1<-quantile(data,0.25)
    quantile_3<-quantile(data,0.75)
    row<-cbind(median,quantile_1,quantile_3)
    rownames(row)<-i
    frame<-rbind(frame,row)
  }
  
  frame<-as.data.frame(frame[-1,])
  frame[,4]<-rownames(frame)
  colnames(frame)<-c("median","Q1","Q3","label")
  
  return(frame)
}

f_boxplot<-function(frames,ylimits, width){
  summary_stats<-f_stat_summary(frames)
  
  plot<-ggplot(frames) +
    geom_violin(aes(x=L1,y=value),scale="width", width=width,
                size=2) +
    
    geom_errorbar(data=summary_stats,aes(x=label, ymin=Q1, ymax=Q3),
                 width=0.2, size=1.5) +
    geom_point(data=summary_stats,aes(x=label,y=median), size=5) +
    
    coord_flip() +
    ylim(ylimits) +
    theme_bw() +
    # theme(panel.background = element_blank(),
    #       panel.grid = element_blank(),
    #       panel.border = element_blank(),
    #       axis.line = element_line(colour="black"),
    #       axis.text = element_text(size=28, face="bold"),
    #       axis.title = element_text(size=28, face="bold"),
    #       axis.line.x = element_line(size=1),
    #       axis.line.y = element_blank(),
    #       axis.text.y = element_blank(), 
    #       aspect.ratio = 1,
    #       axis.ticks.y = element_blank()
          
    
    ylab("Fluorescence (a.u.)") +
    xlab("")
  return(plot)
  
}


ggplot() +
  geom_violin(aes(x=1,y=c(1:10))) +
  stat_summary(geom="errorbar",aes(x=1,y=c(1:10)), ymin=1,ymax=10,
               fun.y="median",
               fun.ymin="quantile",
               fun.ymax="quantile",
               fun.args = list(probs = c(0.25,0.75)))
