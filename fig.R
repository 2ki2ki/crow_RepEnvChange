library(patchwork)

# color palette for color vision diversity
oi_color_palette = palette.colors(palette = "Okabe-Ito")

phase_cols <- c(
  "pre"    = oi_color_palette[4],  # bluishgreen 
  "post"   = oi_color_palette[7],  # vermillion
  "post+1" = oi_color_palette[6]   # blue
)


change_event_cols <- c(
  "1" = oi_color_palette[2],  # orange
  "2" = oi_color_palette[3],  # sky blue
  "3" = oi_color_palette[4],  # bluish green
  "4" = oi_color_palette[6],  # blue
  "5" = oi_color_palette[8]   # reddish purple
)

change_event_cols_c <- c(
  "-2" = oi_color_palette[2],
  "-1" = oi_color_palette[3],
  "0"  = oi_color_palette[4],
  "1"  = oi_color_palette[6],
  "2"  = oi_color_palette[8]
)


block_cols <- c(
  "1" = oi_color_palette[2], # orange
  "2" = oi_color_palette[3], # sky blue
  "3" = oi_color_palette[4], # bluish green
  "4" = oi_color_palette[6], # blue
  "5" = oi_color_palette[7], # vermillion
  "6" = oi_color_palette[8]  # reddish purple
)

sex_cols <- c(
  "female" = oi_color_palette[7],  # vermillion
  "male"   = oi_color_palette[3]   # blue
)


# fig2
# A: gini
fig_2a =
ggplot() +
  geom_point(
    data = short_term_data,
    aes(x = session, y = gini_coef),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.5,
    color = "#999999"
  ) +
  geom_line(
    data = short_term_gini_model_predict,
    aes(
      x = x,
      y = predicted,
      colour = factor(group),
      group = factor(group)
    ),
    position = position_dodge(width = 0.35),
    linetype = "dotted",
    linewidth = 1,
    alpha = 0.8
  ) +
  geom_errorbar(
    data = short_term_gini_model_predict,
    aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, colour = factor(group)),
    position = position_dodge(width = 0.35),
    linewidth = 0.7
  ) +
  geom_point(
    data = short_term_gini_model_predict,
    aes(x = x, y = predicted,colour = factor(group), shape = factor(group)), 
    size = 2.5,
    position = position_dodge(width = 0.35),
  )+
  labs(x = "Phase", y = "Gini coefficient",  colour = "Change event",shape = "Change event") +
  scale_colour_manual(values = change_event_cols_c,
                      name = "Change event",
                      labels = function(x) as.numeric(x) + 3) +
  scale_shape_manual(
    name = "Change event",
    values = c(16, 17, 15, 3, 4),
    labels = function(x) as.numeric(as.character(x)) + 3
  )+
  theme_minimal()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 18),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 1.2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20)
  )

## 2B: Number of successful foragers
fig_2b =
  ggplot(data = short_term_data, 
         aes(x = phase, y = num_successful_forager,fill = phase))+
  geom_point(size = 1.25,position = position_jitter(height = 0, width = 0.15)
             ,alpha = 0.5)+
  geom_violin(alpha = 0.7)+
  geom_boxplot(width = 0.075, alpha = 0.7)+  
  scale_x_discrete(limit = c("pre", "post", "post+1"))+
  scale_fill_manual(values = phase_cols)+
  scale_y_continuous(breaks=seq(0,10,1),limits = c(0,10))+
  labs(x = "Phase", y = "Number of successful foragers",  fill = "Phase") +
  theme_minimal()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 18),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 1.2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20)
  )



## 2c foraging duration

fig_2c =
ggplot() +
  geom_point(
    data = short_term_data,
    aes(x = phase, y = foraging_duration),
    position = position_jitter(width = 0.1, height = 0),
    alpha = 0.5,
    color = "#999999"
  ) +
  geom_line(
    data = short_term_fd_model_predict,
    aes(x = x, y = predicted, group = factor(group)),
    #position = position_nudge(x = 0.15),
    linetype = "dotted",
    linewidth = 1,
    alpha = 0.8
  ) +
  geom_errorbar(
    data = short_term_fd_model_predict,
    aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high,colour = x),
    #position = position_nudge(x = 0.15),
    linewidth = 0.8,
    width = 0.1
  ) +
  geom_point(
    data = short_term_fd_model_predict,
    aes(x = x, y = predicted, colour = x),
    #position = position_nudge(x = 0.15),
    size = 3
  )+
  scale_colour_manual(values = phase_cols) +
  scale_y_continuous(breaks=seq(0,55,10),limits = c(0,55))+
  labs(x = "Phase", y = "Foraging duration (sec)", colour = "Phase") +
  theme_minimal()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 18),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 1.2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20)
  )


layout <- "
ABC
"
wrap_plots(
  A = fig_2a,
  B = fig_2b,
  C = fig_2c,
  design = layout
) +
  plot_annotation(
    tag_levels = "A",
    tag_prefix = "(",
    tag_suffix = ")"
  )

# Fig3

## A: gini
fig_3a =
  ggplot()+
  geom_point(data = group_data,
             aes(x = session, y = gini_coef,color = factor(block),shape = factor(block)),
             position = position_jitter(height = 0, width = 0.15))+
  geom_line(data = AIC_min_model_gini_predict_session_by_block,
            aes(x = x, y = predicted,color = group,linetype = group),
            linewidth = 1)+
  geom_ribbon(data = AIC_min_model_gini_predict_session_by_block,
              aes(x = x, ymin = conf.low, ymax = conf.high,fill = group), alpha = 0.15)+
  scale_x_continuous(breaks=seq(0,10,1))+
  scale_y_continuous(breaks=seq(0,1,0.1),limits = c(0,NA))+
  labs(x ="Session", y ="Gini coefficient")+
  scale_shape_manual(values = c(1,2,3,4,5,6),name = "Block")+
  scale_color_manual(values = block_cols,name = "Block")+
  scale_fill_manual(values = block_cols,name = "Block")+
  scale_linetype(name = "Block")+
  theme_minimal()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 20, vjust = 0),
        axis.title.y = element_text(size = 20, vjust = 1.8),
        legend.title = element_text(size = 15),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 1.2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        #legend.position = "none"
        
  )


## B: num sub successful forager
fig_3b =
  ggplot()+
  geom_point(data = group_data,
             aes(x = block, y = num_successful_forager),
             position = position_jitter(height = 0, width = 0.15),
             alpha = 0.5,
             color = "#999999")+
  geom_line(data = AIC_min_model_nsf_predict_block,
            aes(x = x, y = predicted),
            linewidth = 1)+
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
        axis.ticks = element_line(size = 1.2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        #legend.position = "none"
        
  )

# foraging duration
fig_3c =
  ggplot()+
  geom_point(data = group_data,
             aes(x = session, y = foraging_duration),
             position = position_jitter(height = 0, width = 0.15),
             alpha = 0.5,
             color = "#999999")+
  geom_line(data = AIC_min_model_fd_session,
            aes(x = x, y = predicted),
            linewidth = 1.1)+
  geom_ribbon(data = AIC_min_model_fd_session,
              aes(x = x, ymin = conf.low, ymax = conf.high), 
              alpha = 0.2)+
  scale_x_continuous(breaks=seq(0,10,1))+
  scale_y_continuous(breaks=seq(0,100,20),limits = c(0,NA))+
  labs(x = "Session", y= "Foraing duration (sec)")+
  theme_minimal()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 20, vjust = 0),
        axis.title.y = element_text(size = 20, vjust = 1.8),
        legend.title = element_text(size = 15),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 1.2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        #legend.position = "none"
        
  )

fig_3d =
  ggplot()+
  geom_point(data = group_data,
             aes(x = block, y = foraging_duration ),
             position = position_jitter(height = 0, width = 0.15),
             alpha = 0.5,
             color = "#999999")+
  geom_line(data = AIC_min_model_fd_block,
            aes(x = x, y = predicted),
            linewidth = 1.1)+
  geom_ribbon(data = AIC_min_model_fd_block,
              aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2)+
  scale_x_continuous(breaks=seq(0,6,1))+
  scale_y_continuous(breaks=seq(0,100,20),limits = c(0,NA))+
  labs(x ="Block", y ="Foraging duration (sec)")+
  theme_minimal()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 20, vjust = 0),
        axis.title.y = element_text(size = 20, vjust = 1.8),
        legend.title = element_text(size = 15),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 1.2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        #legend.position = "none"
        
  )


layout <- "
AB
CD
"
wrap_plots(
  A = fig_3a,
  B = fig_3b,
  C = fig_3c,
  D = fig_3d,
  design = layout
) +
  plot_annotation(
    tag_levels = "A",
    tag_prefix = "(",
    tag_suffix = ")"
  )


######## fig4

#fig_4 =
  ggplot()+
  
  geom_point(data = individual_data,
             aes(x = block, y = num_food_gain,color = sex,shape = sex),
             position = position_jitter(height = 0.1, width = 0.2),
             size = 1.0,
             alpha = 0.5)+
  geom_line(data = pred_sex,
            aes(x = block, y= predicted,color = group, linetype = group),
            linewidth = 1.2)+
  scale_x_continuous(breaks=seq(0,6,1))+
  scale_y_continuous(breaks=seq(0,10,2))+
  scale_colour_manual(values = sex_cols) +
  scale_shape_manual(values = c(16,17),name = "sex")+
  scale_linetype_manual(values=c("solid", "twodash"),name = "sex")+
  labs(x ="Block", y ="Number of acquired food")+
  theme_minimal()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 20, vjust = 0),
        axis.title.y = element_text(size = 20, vjust = 1.8),
        legend.title = element_text(size = 15),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 1.2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20),
        #legend.position = "none"
        
  )


#######S

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
  labs(x ="Session", y ="Food gain (%)",fill = "Rank" )+
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
