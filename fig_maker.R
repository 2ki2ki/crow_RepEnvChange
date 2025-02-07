ggplot()+
  geom_point(data = collective_data_glmm,
             aes(x = session, y = consumption_duration, color = factor(block),shape = factor(block)),
             position = position_jitter(height = 0, width = 0.15))+
  geom_line(data = AIC_min_model_cd_session,size = 1.0,
            aes(x = x, y = predicted))+
  geom_ribbon(data = AIC_min_model_cd_session,
              aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  scale_shape_manual(values = c(1,2,3,4,5,6),name = "Block")+
  scale_color_hue(name = "Block")+
  scale_x_continuous(breaks=seq(0,10,1))+
  scale_y_continuous(breaks=seq(0,100,20),limits = c(0,NA))+
  labs(x = "Session", y= "Foraging duration (sec)",color = "Block")+
  theme_minimal()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 20, vjust = 0),
        axis.title.y = element_text(size = 20, vjust = 1.8),
        legend.title = element_text(size = 15),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20),
       legend.position = "none"
        
  )

ggplot()+
  geom_point(data = collective_data_glmm,
             aes(x = block, y = consumption_duration, color = factor(session),shape = factor(session)),
             position = position_jitter(height = 0, width = 0.15))+
  geom_line(data = AIC_min_model_cd_block,
            aes(x = x, y = predicted))+
  geom_ribbon(data = AIC_min_model_cd_block,
              aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  scale_color_hue(name = "Session")+
  scale_shape_manual(values = c(15,16,17,18,7,8,9,10,11,12),name = "Session")+
  scale_x_continuous(breaks=seq(0,6,1))+
  scale_y_continuous(breaks=seq(0,100,20),limits = c(0,NA))+
  labs(x ="Block", y ="Foraging duration (sec)" , color = "Session")+
  theme_minimal()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 20, vjust = 0),
        axis.title.y = element_text(size = 20, vjust = 1.8),
        legend.title = element_text(size = 15),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        #legend.position = "none"
        
  )

ggplot()+
  geom_point(data = collective_data_glmm,
             aes(x = session, y = search_duration,color = factor(block),shape = factor(block)),
             position = position_jitter(height = 0, width = 0.15))+
  geom_line(data = AIC_min_model_sd_predict_session,
            aes(x = x, y = predicted,color = group,linetype = group))+
  geom_ribbon(data = AIC_min_model_sd_predict_session,
              aes(x = x, ymin = conf.low, ymax = conf.high,fill = group), alpha = 0.1)+
  scale_x_continuous(breaks=seq(0,10,1))+
  scale_y_continuous(breaks=seq(0,180,20),limits = c(0,NA))+
  coord_cartesian(ylim = c(0,180))+
  labs(x ="Session", y ="Exploring duration (sec)" )+
  scale_shape_manual(values = c(1,2,3,4,5,6),name = "Block")+
  scale_color_hue(name ="Block")+
  scale_fill_hue(name = "Block")+
  scale_linetype(name = "Block")+
  theme_minimal()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 20, vjust = 0),
        axis.title.y = element_text(size = 20, vjust = 1.8),
        legend.title = element_text(size = 15),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20),
       legend.position = "none"
        
  )

ggplot()+
  geom_point(data = collective_data_glmm,
             aes(x = block, y = num_sub_consumption,color = factor(session),shape = factor(session)),
             position = position_jitter(height = 0, width = 0.15))+
  geom_line(data = AIC_min_model_nsc_predict_block,
            aes(x = x, y = predicted))+
  geom_ribbon(data = AIC_min_model_nsc_predict_block,
              aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  scale_x_continuous(breaks=seq(0,6,1))+
  scale_y_continuous(breaks=seq(0,10,1),limits = c(0,10))+  
  labs(x ="Block", y ="Number of successful foragers")+
  scale_shape_manual(values = c(15,16,17,18,7,8,9,10,11,12),name = "Session")+
  scale_color_hue(name ="Session")+
  theme_minimal()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 20, vjust = 0),
        axis.title.y = element_text(size = 20, vjust = 1.8),
        legend.title = element_text(size = 15),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.position = "none"
        
  )

ggplot()+
  geom_point(data = collective_data_glmm,
             aes(x = session, y = gini_coef,color = factor(block),shape = factor(block)),
             position = position_jitter(height = 0, width = 0.15))+
  geom_line(data = AIC_min_model_gini_predict_session_by_block,
            aes(x = x, y = predicted,color = group,linetype = group))+
  geom_ribbon(data = AIC_min_model_gini_predict_session_by_block,
              aes(x = x, ymin = conf.low, ymax = conf.high,fill = group), alpha = 0.1)+
  scale_x_continuous(breaks=seq(0,10,1))+
  scale_y_continuous(breaks=seq(0,1,0.1),limits = c(0,NA))+
  labs(x ="Session", y ="Gini coefficient")+
  scale_shape_manual(values = c(1,2,3,4,5,6),name = "Block")+
  scale_color_hue(name = "Block")+
  scale_fill_hue(name = "Block")+
  scale_linetype(name = "Block")+
  theme_minimal()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 20, vjust = 0),
        axis.title.y = element_text(size = 20, vjust = 1.8),
        legend.title = element_text(size = 15),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.position = "none"
        
  )

ggplot()+
  geom_point(data = subject_data,aes(x = block, y = num_consumption,color = factor(sex)),
             size = 1.0,
             position = position_jitter(height = 0.2, width = 0.2))+
  geom_line(data = AIC_min_model_sex_prediction,
            aes(x = x, y= predicted,color = group),
            size = 1.0)+
  geom_ribbon(data = AIC_min_model_sex_prediction,
              aes(x = x, ymin = conf.low, ymax = conf.high,fill = group), alpha = 0.2)+
  scale_x_continuous(breaks=seq(0,6,1))+
  scale_y_continuous(breaks=seq(0,10,2))+
  scale_color_hue(name ="Sex")+
  scale_fill_hue(name = "Sex")+
  labs(x ="Block", y ="Number of foods gained by each subject")+
  theme_minimal()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 20, vjust = 0),
        axis.title.y = element_text(size = 20, vjust = 1.8),
        legend.title = element_text(size = 15),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        # legend.position = "none"
        
  )

ggplot(data = env_change_data, 
       aes(x = session, y = consumption_duration,fill = session))+
  geom_violin()+
  geom_boxplot(width = 0.075)+  
  geom_point(size = 1.25,position = position_jitter(height = 0, width = 0.15))+
  scale_x_discrete(limit = c("pre", "post", "post+1"),
                   labels = c("Pre","Post","Post + 1"))+
  scale_y_continuous(breaks=seq(0,180,20),limits = c(0,85))+
  labs(x ="Session", y ="Foraging duration (sec)",fill = "Session" )+
  geom_text(x = 1.5, y= 81, label = "***",size = 7)+
  geom_segment(x = 1, xend = 1, y= 80,yend = 79, linewidth = 1)+
  geom_segment(x = 1, xend = 2, y= 80,yend = 80, linewidth = 1)+
  geom_segment(x = 2, xend = 2, y= 80,yend = 79, linewidth = 1)+
  geom_text(x = 2, y= 84, label = "*",size = 7)+
  geom_segment(x = 1, xend = 1, y= 83.5,yend = 82, linewidth = 1)+
  geom_segment(x = 1, xend = 3, y= 83.5,yend = 83.5, linewidth = 1)+
  geom_segment(x = 3, xend = 3, y= 83.5,yend = 82, linewidth = 1)+
  theme_minimal()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 20, vjust = 0),
        axis.title.y = element_text(size = 20, vjust = 1.8),
        legend.title = element_text(size = 15),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.position = "none"
        
  )


ggplot(data = env_change_data, 
       aes(x = session, y = search_duration,fill = session))+
  geom_violin()+
  geom_boxplot(width = 0.075)+  
  geom_point(size = 1.25,position = position_jitter(height = 0, width = 0.15))+
  scale_x_discrete(limit = c("pre", "post", "post+1"),
                   labels = c("Pre","Post","Post + 1"))+
  scale_y_continuous(breaks=seq(0,180,20),limits = c(0,190))+
  labs(x ="Session", y ="Exploring duration (sec)",fill = "Session" )+
  geom_text(x = 1.5, y= 184, label = "***",size = 7)+
  geom_segment(x = 1, xend = 1, y= 183,yend = 180, linewidth = 1)+
  geom_segment(x = 1, xend = 2, y= 183,yend = 183, linewidth = 1)+
  geom_segment(x = 2, xend = 2, y= 183,yend = 180, linewidth = 1)+
  geom_text(x = 2, y= 191, label = "***",size = 7)+
  geom_segment(x = 1, xend = 1, y= 190,yend = 187, linewidth = 1)+
  geom_segment(x = 1, xend = 3, y= 190,yend = 190, linewidth = 1)+
  geom_segment(x = 3, xend = 3, y= 190,yend = 187, linewidth = 1)+
  theme_minimal()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 20, vjust = 0),
        axis.title.y = element_text(size = 20, vjust = 1.8),
        legend.title = element_text(size = 15),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.position = "none"
        
  )

ggplot(data = env_change_data, 
       aes(x = session, y = num_sub_consumption,fill = session))+
  geom_violin()+
  geom_boxplot(width = 0.075)+  
  geom_point(size = 1.25,position = position_jitter(height = 0, width = 0.15))+
  scale_x_discrete(limit = c("pre", "post", "post+1"),
                   labels = c("Pre","Post","Post + 1"))+
  scale_y_continuous(breaks=seq(0,10,1),limits = c(0,10))+
  labs(x ="Session", y ="Number of successful foragers",fill = "Session" )+
  theme_minimal()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 20, vjust = 0),
        axis.title.y = element_text(size = 20, vjust = 1.8),
        legend.title = element_text(size = 15),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        legend.position = "none"
        
  )

ggplot(data = env_change_data, 
       aes(x = session, y = gini_coef,fill = session))+
  geom_violin()+
  geom_boxplot(width = 0.075)+  
  geom_point(size = 1.25,position = position_jitter(height = 0, width = 0.15))+
  scale_x_discrete(limit = c("pre", "post", "post+1"),
                   labels = c("Pre","Post","Post + 1"))+
  scale_y_continuous(breaks=seq(0,1,0.1),limits = c(0,0.7))+
  scale_fill_discrete(labels = c("Pre","Post","Post + 1"))+
  labs(x ="Session", y ="Gini coefficient",fill = "Session" )+
  geom_text(x = 1.5, y= 0.61, label = "***",size = 7)+
  geom_segment(x = 1, xend = 1, y= 0.6,yend = 0.59, linewidth = 1)+
  geom_segment(x = 1, xend = 2, y= 0.6,yend = 0.6, linewidth = 1)+
  geom_segment(x = 2, xend = 2, y= 0.6,yend = 0.59, linewidth = 1)+
  geom_text(x = 2, y= 0.64, label = "*",size = 7)+
  geom_segment(x = 1, xend = 1, y= 0.63,yend = 0.62, linewidth = 1)+
  geom_segment(x = 1, xend = 3, y= 0.63,yend = 0.63, linewidth = 1)+
  geom_segment(x = 3, xend = 3, y= 0.63,yend = 0.62, linewidth = 1)+
  theme_minimal()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 20, vjust = 0),
        axis.title.y = element_text(size = 20, vjust = 1.8),
        legend.title = element_text(size = 15),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        #legend.position = "none"
        
  )


