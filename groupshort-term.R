library(needs)
needs(tidyverse,MASS,ggeffects,performance,MuMIn, ggdist)
options(na.action = "na.fail")

group_data = read.csv("./group_data.csv")
#####

short_term_data <- group_data %>% filter(session >= 10 | session <= 2)

short_term_data$session <- replace(short_term_data$session,(short_term_data$session ==10),"pre")
short_term_data$session <- replace(short_term_data$session,(short_term_data$session ==1),"post")
short_term_data$session <- replace(short_term_data$session,(short_term_data$session ==2),"post+1")

short_term_data = short_term_data %>%
        mutate(
          phase = factor(session, levels = c("pre", "post", "post+1")),
          change_event = case_when(
            session == "pre"                  ~ block,
            session %in% c("post", "post+1") ~ block - 1,
            TRUE                              ~ NA_real_
          ),
          change_event_c = as.numeric(change_event - 3)
        ) %>%
        filter(change_event != 0)

short_term_data_pre <- short_term_data %>% filter(phase == "pre")
short_term_data_post <- short_term_data %>% filter(phase == "post")
short_term_data_post_plus <- short_term_data %>% filter(phase == "post+1")




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
       aes(x = phase, y = num_successful_forager,fill = phase))+
  geom_violin()+
  geom_boxplot(width = 0.075)+  
  geom_point(size = 1.25,position = position_jitter(height = 0, width = 0.15))+
  scale_x_discrete(limit = c("pre", "post", "post+1"))+
  scale_y_continuous(breaks=seq(0,10,1),limits = c(0,10))+
  labs(x ="Phase", y ="Number of subjects participating in foraging",fill = "Phase" )+
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


full_model_nsf_shotr_term <- glm(num_successful_forager ~ phase * change_event_c,
                                 data = short_term_data, 
                                 family = poisson(link = "log"))

full_model_nsf_shotr_term %>% check_overdispersion()
full_model_nsf_shotr_term%>% check_collinearity()

AIC_min_nsf_short_term <- dredge(full_model_nsf_shotr_term,rank = "AIC")

AIC_min_nsf_short_term

AIC_min_model_nsf_short_term <- get.models(AIC_min_nsf_short_term, subset = 1)[[1]]

AIC_min_model_nsf_short_term %>% summary()

AIC_min_model_nsf_short_term %>% check_overdispersion()

AIC_min_model_nsf_short_term%>% check_collinearity()

AIC(AIC_min_model_nsf_short_term)

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


full_model_fd_short_term <- glm(foraging_duration ~ phase * change_event_c,
                                data = short_term_data,
                                family = Gamma(link = "log"),
)

full_model_fd_short_term%>% check_collinearity()

AIC_min_fd_short_term <- dredge(full_model_fd_short_term,rank = "AIC")

AIC_min_fd_short_term

AIC_min_model_fd_short_term <- get.models(AIC_min_fd_short_term, subset = 1)[[1]]

AIC_min_model_fd_short_term %>% summary
AIC_min_model_fd_short_term %>% check_collinearity()

AIC_null_model_fd_short_term <- glm(foraging_duration ~ 1,
                                   data = short_term_data,
                                   family = Gamma(link = "log"),)

AIC(AIC_min_model_fd_short_term,AIC_null_model_fd_short_term)

short_term_fd_model_predict <-  ggpredict(AIC_min_model_fd_short_term, terms = c( "phase"),interval = "confidence")

ggplot() +
  geom_point(
    data = short_term_data,
    aes(x = session, y = foraging_duration ),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.5
  ) +
  geom_errorbar(
    data = short_term_fd_model_predict,
    aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, colour = group),
    position = position_dodge(width = 0.35),
    linewidth = 0.4
  ) +
  geom_point(
    data = short_term_fd_model_predict,
    aes(x = x, y = predicted,colour = group),
    position = position_dodge(width = 0.35),
  )+
  labs(x = "Session", y = "Foraging duration (sec)", colour = "Block") +
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


ggplot(data = short_term_data, 
       aes(x = session, y = foraging_duration,fill = session))+
  geom_violin()+
  geom_boxplot(width = 0.075)+  
  geom_point(size = 0.8,position = position_jitter(height = 0, width = 0.15))+
  geom_errorbar(
    data = short_term_fd_model_predict,
    aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high,),
    position = position_nudge(x = 0.4),
    width = 0.08,
    inherit.aes = FALSE
  )+
  geom_point(
    data = short_term_fd_model_predict,
    aes(x = x, y = predicted,),
    position = position_nudge(x = 0.4),
    inherit.aes = FALSE
  )+
  scale_x_discrete(limits = c("pre", "post", "post+1"))+
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


full_model_short_term_gini_model <- glm(gini_coef ~ phase * change_event_c,
                                        data = short_term_data) 

full_model_short_term_gini_model%>% check_collinearity()

AIC_min_gini_short_term <- dredge(full_model_short_term_gini_model,rank = "AIC")

AIC_min_gini_short_term

AIC_min_model_gini_short_term <- get.models(AIC_min_gini_short_term, subset = 1)[[1]]

AIC_min_model_gini_short_term %>% summary
AIC_min_model_gini_short_term%>% check_collinearity()

AIC_null_model_gini_short_term <- glm(gini_coef ~ 1,
                                      data = short_term_data) 

AIC(AIC_min_model_gini_short_term,AIC_null_model_gini_short_term)

short_term_gini_model_predict <- ggpredict(AIC_min_model_gini_short_term, terms = c( "phase", "change_event_c[all]"),interval = "confidence")

ggplot() +
  geom_point(
    data = short_term_data,
    aes(x = session, y = gini_coef  ),
    position = position_jitter(width = 0.08, height = 0),
    alpha = 0.5
  ) +
  geom_errorbar(
    data = short_term_gini_model_predict,
    aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, colour = group),
    position = position_dodge(width = 0.35),
    linewidth = 0.4
  ) +
  geom_point(
    data = short_term_gini_model_predict,
    aes(x = x, y = predicted,colour = group),
    position = position_dodge(width = 0.35),
  )+
  labs(x = "Session", y = "Gini Coef.", colour = "Change Event") +
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
