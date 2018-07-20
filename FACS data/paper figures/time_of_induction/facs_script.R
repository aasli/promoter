

# read fcs files
t0p1<-f_read(t0p1_file,pattern_read)
t1p1<-f_read(t1p1_file,pattern_read)
t2p1<-f_read(t2p1_file,pattern_read)
t3p1<-f_read(t3p1_file,pattern_read)
t4p1<-f_read(t4p1_file,pattern_read)
t5p1<-f_read(t5p1_file,pattern_read)
t6p1<-f_read(t6p1_file,pattern_read)
t7p1<-f_read(t7p1_file,pattern_read)
t7p2<-f_read(t7p2_file,pattern_read)

#----------------------------------------------------------------------------------
# create the list of dataframes that contain all the data
df_list<-c(f_df_list(t0p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t1p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t2p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t3p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t4p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t5p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t6p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t7p1,starting_well,wells_per_sample,experiment_doses,columns_to_include),
           f_df_list(t7p2,starting_well,wells_per_sample,experiment_doses,columns_to_include)
           )

df_list_control<-c(f_df_list(t7p2,starting_well,wells_per_sample,experiment_doses,columns_to_include))



#----------------------------------------------------------------------------------
##create names for the dataframe list

df_list_names<-vector()

for (i in time_points){
 
  name<-lapply(strain_names,f_names_df_list,i)
  df_list_names<-c(df_list_names, name)
  
}

df_list_names<-c(df_list_names,"70 360min","2661 360min","2683 360min")
names(df_list)<-df_list_names


boxplot_frames<-f_melt(c(1:length(df_list)),df_list,df_list_names[c(1:length(df_list_names))],"488 [C]-H")
new_colnames<-c("strain","time")
newcols<-colsplit(boxplot_frames$L1," ",new_colnames)
boxplot_frames<-cbind(boxplot_frames,newcols)

boxplots<-f_boxplot(boxplot_frames,c(0,60000))

f_save(boxplots,"boxplot_grid.jpeg",output_path,"",20,30)
#----------------------------------------------------------------------------------
time_frames<-subset(boxplot_frames, boxplot_frames$Dose==1)
time_frames$time<-as.numeric(gsub("min","",time_frames$time))

time_descriptives<-aggregate(time_frames$value,by=list(time_frames$strain,time_frames$time),median)
colnames(time_descriptives)<-c("strain","time","median")

strains_to_plot<-c("2674","2759","2663","2669","2689","2667")

plot_frame<-subset(time_descriptives,time_descriptives$strain %in% strains_to_plot)

time_plot_medians<- ggplot() +
  geom_point(data=plot_frame,aes(y=median,x=time, colour=as.character(strain)),
              width=6) +
  geom_smooth(data=plot_frame,aes(y=median,x=time, colour=as.character(strain)),se=F) +
  geom_line(aes(y=time_descriptives[which(time_descriptives$strain=="2661"),3],
                x=unique(time_descriptives[,2])))+
  theme_bw() +
  scale_colour_manual(values=c("2674"="red",
                               "2759"="darkred",
                               "2663"="lightblue",
                               "2669"="blue",
                               "2689"="orange",
                               "2667"="purple")) +
  theme(legend.position = "none")


time_plot_medians
#----------------------------------------------------------------------------------



hist_strains_to_plot<-c("2674","2759","2663","2669","2675","2676","2717",
                       "2689","2688","2664","2665","2666","2667")

hist_plot_frames<-subset(time_frames[,c(1:ncol(time_frames))],time_frames$strain %in% hist_strains_to_plot)
hist_plot_frames$strain<-as.character(hist_plot_frames$strain)
hist_plot_frames$time<-as.factor(hist_plot_frames$time)

time_plot_hist<-ggplot(hist_plot_frames) +
  geom_density(aes(x=value, colour=time)) +
  facet_wrap(~strain) +
  #scale_colour_gradient() +
  theme_bw() +
  theme(legend.position = "none",
        strip.text = element_text(size=6)) +
  ylim(0,0.0005) +
  xlim(0,60000)

time_plot_hist

f_save(time_plot_hist,"time_plot_histograms.jpeg",output_path,"",6.5,9.5)
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

dfs_nf<-df_list[c(grep("2674", names(df_list)))]
dfs_tup1<-df_list[c(grep("2717", names(df_list)))]


f_normalize_medians<-function(dataframes){
  
  medians<-c()
  q1s<-c()
  q2s<-c()
  
  
  for(i in c(1:length(dataframes))){
    max_median<-median(dataframes[[length(dataframes)]][,3])
    
    median<-median(dataframes[[i]][,3])
    q1<-quantile(dataframes[[i]][,3], probs=0.25)
    q2<-quantile(dataframes[[i]][,3], probs=0.75)
    
    medians<-c(medians, (median/max_median))
    q1s<-c(q1s, (q1/max_median))
    q2s<-c(q2s, (q2/max_median))
    
  }
   
  normalized_medians<-cbind(medians,q1s,q2s,as.numeric(gsub("min","",time_points)))
  
  return(normalized_medians)
  
}

normalize_medians<-lapply(list(dfs_nf,dfs_tup1), f_normalize_medians)

label_list_induction_time<-c("NF", "TUP1")

f_plot_induction_time<-function(median_list,label_list){
  
  final_plot<-ggplot()
  
  for(i in c(1:length(median_list))){
    
    single_layer<- geom_point(aes_(x=median_list[[i]][,4], y=median_list[[i]][,1],colour=label_list[[i]]))
    error_bars<- geom_errorbar(aes_(x=median_list[[i]][,4], ymin=median_list[[i]][,2], 
                                     ymax=median_list[[i]][,3], colour=label_list[[i]]))
    
    final_plot<- final_plot+single_layer+error_bars
  }
  
  pretty_plot<-final_plot +
    theme(panel.grid.minor = element_blank(),
          #legend.direction = "horizontal",
          legend.position = c(0.15,0.8), 
          legend.title = element_blank(),
          legend.key.height = unit(0.35,"cm"),
          legend.key = element_blank(),
          legend.box.background = element_blank(),
          legend.margin = margin(t=0.04,r=0.1,b=0.1,l=0.1,unit="cm"),
          panel.border = element_blank(), 
          axis.line = element_line(),
          # axis.text = element_text(size=24),
          # axis.title = element_text(size=24),
          axis.title = element_text(hjust = 1),
          panel.background = element_blank()
    )  +
    xlab("Time (min)") +
    ylab("Normalized Fluorescence") +
    scale_x_continuous(limits = c(-20,400), breaks = seq(0,360,length.out = 5)) +
    scale_y_continuous(limits = c(0,1.5), breaks = seq(0,1.5,length.out = 4))
  
  return(pretty_plot)
  
}


induction_time_plot<-f_plot_induction_time(normalize_medians,label_list_induction_time)

ggsave(induction_time_plot,path = output_path, filename = "inductiontime.jpeg", width = 9.04, height=6, units= "cm")
ggsave(induction_time_plot,path = output_path, filename = "inductiontime.pdf", width = 9.04, height=6, units= "cm")
#----------------------------------------------------------------------------------