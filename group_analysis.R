library(needs)
needs(tidyverse,MASS,ggeffects,performance,MuMIn)
options(na.action = "na.fail")

group_data = read.csv("./group_data.csv")
################################################################

##long-term

#################num of successful forager#######################
ggplot(group_data,aes(x = num_successful_forager))+
  geom_bar()

group_data$num_successful_forager %>% summary
group_data$num_successful_forager %>% se

full_model_nsf <- glm(num_successful_forager ~ session * block,
                      data = group_data, 
                      family = poisson(link = "log"))

full_model_nsf %>% check_overdispersion()

AIC_min_nsf <- dredge(full_model_nsf,rank = "AIC")

AIC_min_nsf

AIC_min_model_nsf <- get.models(AIC_min_nsf, subset = 1)[[1]]

AIC_min_model_nsf %>% summary()

##############
AIC_null_model_nsf <-  glm(num_successful_forager ~ 1,
                             data = group_data,family = poisson(link = "log"))

AIC(AIC_min_model_nsf,AIC_null_model_nsf)

###############

AIC_min_model_nsf_predict_block <- ggpredict(AIC_min_model_nsf, terms = "block",
                                             type = "re",interval = "confidence")

ggplot()+
  geom_point(data = group_data,
             aes(x = block, y = num_successful_forager,color = factor(session)),
             position = position_jitter(height = 0, width = 0.15))+
  geom_line(data = AIC_min_model_nsf_predict_block,
            aes(x = x, y = predicted))+
  geom_ribbon(data = AIC_min_model_nsf_predict_block,
              aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  scale_x_continuous(breaks=seq(0,6,1))+
  scale_y_continuous(breaks=seq(0,10,1),limits = c(0,10))+  
  labs(x ="Block", y ="Number of subjects participating in foraging")+
  scale_color_hue(name ="Session")+
  theme_classic()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 18),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20),
        #legend.position = "none"
  )



###############foraging duration#############################

ggplot(group_data,aes(x = foraging_duration))+
  geom_density()

group_data$foraging_duration %>% summary
group_data$foraging_duration %>% se

full_model_fd <- glm(foraging_duration ~ session * block,
                     data = group_data,
                     family = Gamma(link = "log"),
                    )

AIC_min_fd <- dredge(full_model_fd,rank = "AIC")

AIC_min_fd

AIC_min_model_fd <- get.models(AIC_min_fd, subset = 1)[[1]]

AIC_min_model_fd %>% summary

###############
AIC_null_model_fd <-  glm(foraging_duration ~1,
                          data = group_data,
                          family = Gamma(link = "log"))

AIC(AIC_min_model_fd,AIC_null_model_fd)

#############

AIC_min_model_fd_session <- ggpredict(AIC_min_model_fd,
                                      terms = "session",
                                      type = "re",
                                      interval = "confidence")

ggplot()+
  geom_point(data = group_data,
             aes(x = session, y = foraging_duration, color =factor(block)),
             position = position_jitter(height = 0, width = 0.15))+
  geom_line(data = AIC_min_model_fd_session,size = 1.0,
            aes(x = x, y = predicted))+
  geom_ribbon(data = AIC_min_model_fd_session,
              aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  scale_x_continuous(breaks=seq(0,10,1))+
  scale_y_continuous(breaks=seq(0,100,20),limits = c(0,NA))+
  labs(x = "Session", y= "Foraing duration (sec)",color = "Block")+
  theme_classic()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 18),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20)

  )

AIC_min_model_fd_block <- ggpredict(AIC_min_model_fd,
                                    terms = "block",
                                    type = "re",
                                    interval = "confidence")

ggplot()+
  geom_point(data = group_data,
             aes(x = block, y = foraging_duration, color = factor(session)),
             position = position_jitter(height = 0, width = 0.15))+
  geom_line(data = AIC_min_model_fd_block,
            aes(x = x, y = predicted))+
  geom_ribbon(data = AIC_min_model_fd_block,
              aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  scale_x_continuous(breaks=seq(0,6,1))+
  scale_y_continuous(breaks=seq(0,100,20),limits = c(0,NA))+
  labs(x ="Block", y ="Foraging duration (sec)" , color = "Session")+
  theme_classic()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 18),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20),
        #legend.position = "none"
  )



########################exploring duration#####################################
density_exploring_duration <- ggplot(group_data,
                                  aes(x = exploring_duration))+
                                geom_density()

density_exploring_duration

full_model_ed <-  glm(exploring_duration ~ session * block,
                      data = group_data,
                      family = Gamma(link = "log"))

AIC_min_ed <- dredge(full_model_ed,rank = "AIC")

AIC_min_ed

AIC_min_model_ed <- get.models(AIC_min_ed, subset = 1)[[1]]

AIC_min_model_ed %>% summary


################
AIC_null_model_ed <-  glm(exploring_duration ~ 1,
                            data = group_data,
                          family = Gamma(link = "log"))

AIC(AIC_min_model_ed,AIC_null_model_ed)
################

AIC_min_model_ed_predict_session <- ggpredict(AIC_min_model_ed,
                                              terms = c("session[all]","block"))

ggplot()+
  geom_point(data = group_data,
             aes(x = session, y = exploring_duration,color = factor(block)),
             position = position_jitter(height = 0, width = 0.15))+
  geom_line(data = AIC_min_model_ed_predict_session,
            aes(x = x, y = predicted,color = group))+
  geom_ribbon(data = AIC_min_model_ed_predict_session,
              aes(x = x, ymin = conf.low, ymax = conf.high,fill = group), alpha = 0.1)+
  scale_x_continuous(breaks=seq(0,10,1))+
  scale_y_continuous(breaks=seq(0,180,20),limits = c(0,NA))+
  coord_cartesian(ylim = c(0,180))+
  labs(x ="Session", y ="Exploration duration (sec)" )+
  scale_color_hue(name ="Block")+
  scale_fill_hue(name = "Block")+
  theme_classic()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 18),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20),
        #legend.position = "none"
  )


AIC_min_model_ed_predict <- ggpredict(AIC_min_model_ed, terms = c("block[all]","session[all]"))

ggplot()+
  geom_point(data = group_data,
             aes(x = block, y = exploring_duration,color =factor(session)),
             position = position_jitter(height = 0, width = 0.15))+
  geom_line(data = AIC_min_model_ed_predict,
            aes(x = x, y = predicted,color = group))+
  geom_ribbon(data = AIC_min_model_ed_predict,
              aes(x = x, ymin = conf.low, ymax = conf.high,fill = group), alpha = 0.1)+
  scale_x_continuous(breaks=seq(0,10,1))+
  scale_y_continuous(breaks=seq(0,180,20),limits = c(0,NA))+
  coord_cartesian(ylim = c(0,180))+
  labs(x ="Block", y ="Exploration duration (sec)" )+
  scale_color_hue(name ="Session")+
  scale_fill_hue(name = "Session")+
  theme_classic()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 18),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20),
        #legend.position = "none"
  )



#########################################
ggplot(group_data,aes(x = gini_coef))+
  geom_density()

group_data$gini_coef %>% summary()
group_data$gini_coef %>% se

full_model_gini <- glm(gini_coef ~ session * block,
                       data = group_data,
                       family = gaussian("identity"))

AIC_min_gini<- dredge(full_model_gini,rank = "AIC")

AIC_min_gini

AIC_min_model_gini <- get.models(AIC_min_gini, subset = 1)[[1]]

AIC_min_model_gini %>% summary()


##############
AIC_null_model_gini <-  glm(gini_coef ~ 1,
                            data = group_data,
                            family = gaussian("identity"))

AIC(AIC_min_model_gini,AIC_null_model_gini)

################

AIC_min_model_gini_predict_session <- ggpredict(AIC_min_model_gini,
                                                terms = c("session"))

ggplot()+
  geom_point(data = group_data,
             aes(x = session, y = gini_coef,color = factor(block)),
             position = position_jitter(height = 0, width = 0.15))+
  geom_line(data = AIC_min_model_gini_predict_session,
            aes(x = x, y = predicted))+
  geom_ribbon(data = AIC_min_model_gini_predict_session,
              aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  scale_x_continuous(breaks=seq(0,10,1))+
  scale_y_continuous(breaks=seq(0,1,0.1),limits = c(0.12,NA))+
  labs(x ="Session", y ="Gini coefficient")+
  scale_color_hue(name = "Block")+
  theme_classic()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 18),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20),
        #legend.position = "none"
  )

AIC_min_model_gini_predict_session_by_block <- ggpredict(AIC_min_model_gini, terms = c("session[all]","block[all]"))

ggplot()+
  geom_point(data = group_data,
             aes(x = session, y = gini_coef,color = factor(block)),
             position = position_jitter(height = 0, width = 0.15))+
  geom_line(data = AIC_min_model_gini_predict_session_by_block,
            aes(x = x, y = predicted,color = group))+
  geom_ribbon(data = AIC_min_model_gini_predict_session_by_block,
              aes(x = x, ymin = conf.low, ymax = conf.high,fill = group), alpha = 0.1)+
  scale_x_continuous(breaks=seq(0,10,1))+
  scale_y_continuous(breaks=seq(0,1,0.1),limits = c(0.12,NA))+
  labs(x ="Session", y ="Gini coefficient")+
  scale_color_hue(name = "Block")+
  scale_fill_hue(name = "Block")+
  theme_classic()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 18),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20),
        #legend.position = "none"
  )

AIC_min_model_gini_predict_block <- ggpredict(AIC_min_model_gini, terms = c("block"))

ggplot()+
  geom_point(data = group_data,
             aes(x = block, y = gini_coef,color = factor(session)),
             position = position_jitter(height = 0, width = 0.15))+
  geom_line(data = AIC_min_model_gini_predict_block,
            aes(x = x, y = predicted))+
  geom_ribbon(data = AIC_min_model_gini_predict_block,
              aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.1)+
  scale_x_continuous(breaks=seq(0,10,1))+
  scale_y_continuous(breaks=seq(0,1,0.1),limits = c(0.12,NA))+
  labs(x ="Block", y ="Gini coefficient")+
  scale_color_hue(name = "Session")+
  theme_classic()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 18),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20),
        #legend.position = "none"
  )

AIC_min_model_gini_predict_block_by_session <- ggpredict(AIC_min_model_gini, terms = c("block[all]","session[all]"))

ggplot()+
  geom_point(data = group_data,
             aes(x = block, y = gini_coef,color = factor(session)),
             position = position_jitter(height = 0, width = 0.15))+
  geom_line(data = AIC_min_model_gini_predict_block_by_session,
            aes(x = x, y = predicted,color = group))+
  geom_ribbon(data = AIC_min_model_gini_predict_block_by_session,
              aes(x = x, ymin = conf.low, ymax = conf.high,fill = group), alpha = 0.1)+
  scale_x_continuous(breaks=seq(0,10,1))+
  scale_y_continuous(breaks=seq(0,1,0.1),limits = c(0.12,NA))+
  labs(x ="Block", y ="Gini coefficient")+
  scale_color_hue(name = "Session")+
  scale_fill_hue(name = "Session")+
  theme_classic()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 18),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20),
        #legend.position = "none"
  )

###########################short-term############################################################

short_term_data <- group_data %>% filter(session >= 10 | session <= 2)

short_term_data$session <- replace(short_term_data$session,(short_term_data$session ==10),"pre")
short_term_data$session <- replace(short_term_data$session,(short_term_data$session ==1),"post")
short_term_data$session <- replace(short_term_data$session,(short_term_data$session ==2),"post+1")
short_term_data <- short_term_data %>% mutate(session = factor(session, levels = c("pre","post","post+1")))

short_term_data_pre <- short_term_data %>% filter(session == "pre")
short_term_data_post <- short_term_data %>% filter(session == "post")
short_term_data_post_plus <- short_term_data %>% filter(session == "post+1")



############num successful forager##################
ggplot(short_term_data,
       aes(x = num_successful_forager))+
  geom_bar()

short_term_data_pre$num_successful_forager %>% mean
short_term_data_pre$num_successful_forager %>% se

short_term_data_post$num_successful_forager %>% mean()
short_term_data_post$num_successful_forager %>% se

short_term_data_post_plus$num_successful_forager %>% mean()
short_term_data_post_plus$num_successful_forager %>% se


ggplot(data = short_term_data, 
       aes(x = session, y = num_successful_forager,fill = session))+
  geom_violin()+
  geom_boxplot(width = 0.075)+  
  geom_point(size = 1.25,position = position_jitter(height = 0, width = 0.15))+
  scale_x_discrete(limit = c("pre", "post", "post+1"))+
  scale_y_continuous(breaks=seq(0,10,1),limits = c(0,10))+
  labs(x ="Session", y ="Number of subjects participating in foraging",fill = "Session" )+
  theme_classic()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 20),        
        legend.title = element_text(size = 18),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20)
  )


short_term_nsf_model_null <- glm(num_successful_forager ~ 1,
                                   data = short_term_data,
                                   family = poisson(link = "log")) 

short_term_nsf_model <- glm(num_successful_forager ~ session,
                              data = short_term_data,
                              family = poisson(link = "log")) 

AIC(short_term_nsf_model_null, short_term_nsf_model)
check_overdispersion(short_term_nsf_model_null)
short_term_nsf_model_null %>% summary

###foraging_duration
short_term_data_pre$foraging_duration %>% mean
short_term_data_pre$foraging_duration %>% se

short_term_data_post$foraging_duration %>% mean
short_term_data_post$foraging_duration %>% se

short_term_data_post_plus$foraging_duration %>% mean
short_term_data_post_plus$foraging_duration %>% se

ggplot(short_term_data,
       aes(x = foraging_duration))+
      geom_density()



short_term_fd_model_null <- glm(foraging_duration ~ 1,
                             data = short_term_data,
                             family = Gamma(link = "log")) 

short_term_fd_model <- glm(foraging_duration ~ session,
                               data = short_term_data,
                               family = Gamma(link = "log")) 



AIC(short_term_fd_model_null,short_term_fd_model)
short_term_fd_model %>% summary



short_term_fd_model_predict <- ggpredict(short_term_fd_model, terms = "session",type = "re",interval = "confidence")

ggplot(data = short_term_data, 
       aes(x = session, y = foraging_duration,fill = session))+
  geom_violin()+
  geom_boxplot(width = 0.075)+  
  geom_point(size = 1.25,position = position_jitter(height = 0, width = 0.15))+
  scale_x_discrete(limit = c("pre", "post", "post+1"))+
  scale_y_continuous(breaks=seq(0,180,20),limits = c(0,NA))+
  labs(x ="Session", y ="Foraging duration(sec)",fill = "Session" )+
  theme_classic()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 18),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20),
        #legend.position = "none"
  )


##########################################
ggplot(short_term_data,
       aes(x = exploring_duration))+
  geom_density()

short_term_data_pre$exploring_duration %>% mean
short_term_data_pre$exploring_duration %>% se

short_term_data_post$exploring_duration %>% mean()
short_term_data_post$exploring_duration %>% se

short_term_data_post_plus$exploring_duration %>% mean()
short_term_data_post_plus$exploring_duration %>% se

short_term_ed_model_null <- glm(exploring_duration ~ 1,
                                  data = short_term_data,
                                  family = Gamma(link = "log")) 

short_term_ed_model <- glm(exploring_duration ~ session,
                             data = short_term_data,
                             family = Gamma(link = "log")) 

AIC(short_term_ed_model_null,short_term_ed_model)

short_term_ed_model %>% summary


ggplot(data = short_term_data, 
       aes(x = session, y = exploring_duration,fill = session))+
  geom_violin()+
  geom_boxplot(width = 0.075)+  
  geom_point(size = 1.25,position = position_jitter(height = 0, width = 0.15))+
  scale_x_discrete(limit = c("pre", "post", "post+1"))+
  scale_y_continuous(breaks=seq(0,180,20),limits = c(0,NA))+
  labs(x ="Session", y ="Exploration duration(sec)",fill = "Session" )+
  theme_classic()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 18),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20)
  )


#########################
ggplot(short_term_data,
       aes(x = gini_coef))+
  geom_density()


short_term_data_pre$gini_coef %>% mean
short_term_data_pre$gini_coef %>% se

short_term_data_post$gini_coef %>% mean()
short_term_data_post$gini_coef %>% se

short_term_data_post_plus$gini_coef %>% mean()
short_term_data_post_plus$gini_coef %>% se

ggplot(data = short_term_data, 
       aes(x = session, y = gini_coef,fill = session))+
  geom_violin()+
  geom_boxplot(width = 0.075)+  
  geom_point(size = 1.25,position = position_jitter(height = 0, width = 0.15))+
  scale_x_discrete(limit = c("pre", "post", "post+1"))+
  scale_y_continuous(breaks=seq(0,1,0.1),limits = c(0,1.0))+
  labs(x ="Session", y ="Gini coefficient",fill = "Session" )+
  theme_classic()+
  theme(axis.line = element_line(color = "black",size = 2, lineend="square"),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 18),
        text = element_text(size = 20),
        axis.ticks = element_line(size = 2),
        axis.ticks.length = unit(3, "mm"),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20)
  )

short_term_gini_model_null <- glm(gini_coef ~ 1,
                                  data = short_term_data,
                                  family = gaussian("identity")) 
  short_term_gini_model <- glm(gini_coef ~ session,
                                data = short_term_data,
                                family = gaussian("identity")) 


AIC(short_term_gini_model_null, short_term_gini_model)

short_term_gini_model %>% summary

###########################################################################################

