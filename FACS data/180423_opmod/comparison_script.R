

## read fcs files as a flowset
t0p1<-f_read(t0p1_file,pattern_read)

#-----------------------------------------------------------------------------------------------

## create the dataframe list
df_list<-f_df_comparison(t0p1,columns,fsch_column, ssch_column,lower_limit_fscw,
                         limit_fscw,lower_limit_sscw, limit_sscw)

names(df_list)<-strain_names

#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------

# descriptive stats

descriptives<-lapply(df_list,f_descriptives_comp,column=3)
names(descriptives)<-label_list

#----------------------------------------------------------------------------------

## size plots

size_plots<- lapply(df_list,f_size_plot,fsc_column=fscw_column_size,ssc_column=sscw_column_size,
                    xlab_title=xlab_size,ylab_title=ylab_size,xlims=xlimits_size,
                    ylims=ylimits_size)

names(size_plots)<-strain_names

mapply(f_save,size_plots,names(size_plots),
       MoreArgs = list(output_folder=output_path,output_path="size",
                       height=height_size, width=width_size),SIMPLIFY = FALSE)

#-----------------------------------------------------------------------------------------------
## histograms

histogram_list<-mapply(f_histogram_comparison,title_of_sequences,list_of_sequences,xlimits_his,
                       MoreArgs = list(df_list,ylimits_his,legend_position_his, 
                       size_value_his,guide_title_his,label_list,ncol_legend_his,
                       column_to_plot_his),SIMPLIFY = FALSE)

names(histogram_list)<-title_of_sequences

mapply(f_save,histogram_list,names(histogram_list),
       MoreArgs = list(output_folder=output_path,output_path="histograms",height=height_his,
                       width=width_his),SIMPLIFY = FALSE)
                       
#----------------------------------------------------------------------------------------------- 

## boxplots

#first melt dataframes you want to put on the plot into one dataframe

boxplot_frames<-lapply(list_of_sequences,f_melt, df_list, label_list,"488 [C]-H" )

boxplots<-mapply(f_boxplot, boxplot_frames, ylimits_box, widths, SIMPLIFY = FALSE) # create boxplots
names(boxplots)<-title_of_sequences


# then save
mapply(f_save,boxplots,names(boxplots),
       MoreArgs = list(output_folder=output_path,output_path="boxplots",height=height_box,
                       width=width_box),SIMPLIFY = FALSE)
#----------------------------------------------------------------------------------

# getting a value for size
df_with_size<-lapply(df_list,f_size)

#----------------------------------------------------------------------------------

## size vs fluorescence plots

svf_plots<-mapply(f_size_vs_fluorescence_comp,list_of_sequences,title_of_sequences,
                  MoreArgs = list(df_with_size,size_column_svf,size_value_svf,
                                  citrine_column_svf,label_list,legend_position_svf,legend_title_svf,
                                  xlimits_svf,ylimits_svf,ncol_legend_svf),SIMPLIFY = FALSE)
names(svf_plots)<-title_of_sequences

mapply(f_save,svf_plots,names(svf_plots),
       MoreArgs = list(output_folder=output_path,output_path="svf",height=height_svf,
                       width=width_svf),SIMPLIFY = FALSE)

#----------------------------------------------------------------------------------
## fold change table

fold_change_table<-f_fold_change_table(descriptives,stat_to_use_fold_change)
f_save_table(fold_change_table,"fold_change.csv")

#----------------------------------------------------------------------------------

## QQplots

qqplots<-mapply(f_qqplot,dataframe1_list,dataframe2_list,qqplot_column,xlims_qqplot,
                ylims_qqplot,titles_qqplot, SIMPLIFY = FALSE)

names(qqplots)<-titles_qqplot
  
mapply(f_save,qqplots,names(qqplots),
       MoreArgs = list(output_folder=output_path,output_path="qq",height=height_qq,
                       width=width_qq),SIMPLIFY = FALSE)
