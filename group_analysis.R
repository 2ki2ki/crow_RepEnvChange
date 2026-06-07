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
full_model_nsf%>% check_collinearity()

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
                                             interval = "confidence")

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

full_model_fd%>% check_collinearity()

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
