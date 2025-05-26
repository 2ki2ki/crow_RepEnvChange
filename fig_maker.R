#######
fig_2a = ggplot(data = short_term_data, 
          aes(x = session, y = gini_coef,fill = session))+
            geom_violin()+
            geom_boxplot(width = 0.075)+  
            geom_point(size = 1.25,position = position_jitter(height = 0, width = 0.15))+
            scale_x_discrete(limit = c("pre", "post", "post+1"),
                             labels = c("Pre","Post","Post + 1"))+
            scale_y_continuous(breaks=seq(0,1,0.1),limits = c(0,0.87))+
            scale_fill_discrete(labels = c("Pre","Post","Post + 1"))+
            labs(x ="Session", y ="Gini coefficient",fill = "Session" )+
            geom_text(x = 1.5, y= 0.81, label = "***",size = 7)+
            geom_segment(x = 1, xend = 1, y= 0.8,yend = 0.79, linewidth = 1)+
            geom_segment(x = 1, xend = 2, y= 0.8,yend = 0.8, linewidth = 1)+
            geom_segment(x = 2, xend = 2, y= 0.8,yend = 0.79, linewidth = 1)+
            geom_text(x = 2, y= 0.86, label = "*",size = 7)+
            geom_segment(x = 1, xend = 1, y= 0.85,yend = 0.84, linewidth = 1)+
            geom_segment(x = 1, xend = 3, y= 0.85,yend = 0.85, linewidth = 1)+
            geom_segment(x = 3, xend = 3, y= 0.85,yend = 0.84, linewidth = 1)+
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

fig_2b = ggplot(data = short_term_data, 
                aes(x = session, y = num_successful_forager,fill = session))+
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

fig_2c = ggplot(data = short_term_data, 
                aes(x = session, y = foraging_duration,fill = session))+
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
  # geom_text(x = 2, y= 84, label = "n.s.",size = 7)+
  # geom_segment(x = 1, xend = 1, y= 83.5,yend = 82, linewidth = 1)+
  # geom_segment(x = 1, xend = 3, y= 83.5,yend = 83.5, linewidth = 1)+
  # geom_segment(x = 3, xend = 3, y= 83.5,yend = 82, linewidth = 1)+
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


fig_2d = ggplot(data = short_term_data, 
                aes(x = session, y = exploring_duration,fill = session))+
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
        #legend.position = "none"
        
  )


layout <- "
ABCD
"
wrap_plots(
  A = fig_2a,
  B = fig_2b,
  C = fig_2c,
  D = fig_2d,
  design = layout
) +
  plot_annotation(
    tag_levels = "A",
    tag_prefix = "(",
    tag_suffix = ")"
  )




####
fig_3a = ggplot()+
            geom_point(data = group_data,
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
                  #legend.position = "none"
                  
            )

fig_3b =   ggplot()+
              geom_point(data = group_data,
                         aes(x = block, y = num_successful_forager,color = factor(session),shape = factor(session)),
                         position = position_jitter(height = 0, width = 0.15))+
              geom_line(data = AIC_min_model_nsf_predict_block,
                        aes(x = x, y = predicted))+
              geom_ribbon(data = AIC_min_model_nsf_predict_block,
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
                    #legend.position = "none"
                    
              )


fig_3c = ggplot()+
          geom_point(data = group_data,
                     aes(x = block, y = foraging_duration, color = factor(session),shape = factor(session)),
                     position = position_jitter(height = 0, width = 0.15))+
          geom_line(data = AIC_min_model_fd_block,
                    aes(x = x, y = predicted))+
          geom_ribbon(data = AIC_min_model_fd_block,
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

fig_3d = ggplot()+
            geom_point(data = group_data,
                       aes(x = session, y = foraging_duration, color = factor(block),shape = factor(block)),
                       position = position_jitter(height = 0, width = 0.15))+
            geom_line(data = AIC_min_model_fd_session,size = 1.0,
                      aes(x = x, y = predicted))+
            geom_ribbon(data = AIC_min_model_fd_session,
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
                  #legend.position = "none"
                  
            )


fig_3e = ggplot()+
            geom_point(data = group_data,
                       aes(x = session, y = exploring_duration,color = factor(block),shape = factor(block)),
                       position = position_jitter(height = 0, width = 0.15))+
            geom_line(data = AIC_min_model_ed_predict_session,
                      aes(x = x, y = predicted,color = group,linetype = group))+
            geom_ribbon(data = AIC_min_model_ed_predict_session,
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
                 #legend.position = "none"
                  
            )

layout <- "
AB.
CDE
"
wrap_plots(
  A = fig_3a,
  B = fig_3b,
  C = fig_3c,
  D = fig_3d,
  E = fig_3e,
  design = layout
) +
  plot_annotation(
    tag_levels = "A",
    tag_prefix = "(",
    tag_suffix = ")"
  )


######fig4
ggplot()+
  geom_point(data = individual_data,aes(x = block, y = num_food_gain,color = sex),
             size = 1.0,
             position = position_jitter(height = 0.2, width = 0.2))+
  geom_line(data = AIC_min_model_sex_prediction,
            aes(x = x, y= predicted,color = group),
            size = 1.0)+
  geom_ribbon(data = AIC_min_model_sex_prediction,
              aes(x = x, ymin = conf.low, ymax = conf.high,fill = group), alpha = 0.2)+
  scale_x_continuous(breaks=seq(0,6,1))+
  scale_y_continuous(breaks=seq(0,10,2))+
  scale_color_hue(name ="Sex",direction = -1)+
  scale_fill_hue(name = "Sex",direction = -1)+
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



session_summary = individual_data %>% 
  mutate(total_session = (block-1)*10 + session) %>%
  group_by(subject,rank,total_session) %>%
  summarize(
    num_food_gain_by_session = sum(num_food_gain)
  )

individual_gain_by_session = session_summary %>% 
  group_by(total_session) %>%
  mutate(
    total_amount_of_food_gained = sum(num_food_gain_by_session)
  ) %>%
  ungroup()%>%
  mutate(rate_of_food_gain_by_session = (num_food_gain_by_session / total_amount_of_food_gained) *100) %>%
  as.data.frame()

order_color <- c("#1f2f6a","#3f78ca","#539ada","#85beea","#7f1927",
                 "#c1ddf8","#ac243c","#d22f54","#f06c95","#f9ccde")

ggplot(individual_gain_by_session,
       aes(x =total_session, y = rate_of_food_gain_by_session)) +
  annotate("rect",xmin = 10,xmax = 20, ymin = -Inf, ymax = Inf,alpha = 0.2, fill ="black")+
  annotate("rect",xmin = 20,xmax = 30, ymin = -Inf, ymax = Inf,alpha = 0.4, fill ="black")+
  annotate("rect",xmin = 40,xmax = 50, ymin = -Inf, ymax = Inf,alpha = 0.2, fill ="black")+
  annotate("rect",xmin = 50,xmax = 56.5, ymin = -Inf, ymax = Inf,alpha = 0.4, fill ="black")+
  geom_vline(xintercept = 10, size = 1.5,linetype = "dashed", color = "red",alpha = 0.5)+
  geom_vline(xintercept = 20, size = 1.5,linetype = "dashed", color = "red",alpha = 0.5)+
  geom_vline(xintercept = 30, size = 1.5,linetype = "dashed", color = "red",alpha = 0.5)+
  geom_vline(xintercept = 40, size = 1.5,linetype = "dashed", color = "red",alpha = 0.5)+
  geom_vline(xintercept = 50, size = 1.5,linetype = "dashed", color = "red",alpha = 0.5)+
  geom_bar(stat = "identity", aes(fill = factor(rank)))+
  scale_fill_manual(values = order_color)+  
  labs(x ="Session", y ="Food gain of each subject (%)",fill = "Rank" )+
  scale_x_continuous(breaks=seq(0,60,5),expand = c(0.01,0.015))+
  theme_minimal()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20),
        #legend.position = "none"
  )
