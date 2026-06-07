library(needs)
needs(tidyverse,lme4,ggeffects,performance,MuMIn,parameters)
options(na.action = "na.fail")

individual_data = read.csv("./individual_data.csv")

################################################################
rank_center <- 5.5
rank_scale  <- sd(1:10)

block_center <- 3.5
block_scale  <- sd(1:6)

session_center <- 5.5
session_scale  <- sd(1:10)

individual_data$rank_c <- 
  (individual_data$rank - rank_center) / rank_scale

individual_data$block_c <- 
  (individual_data$block - block_center) / block_scale

individual_data$session_c <- 
  (individual_data$session - session_center) / session_scale

individual_data$sex <- relevel(factor(individual_data$sex), ref = "female")

################################################################
ggplot(individual_data,
       aes(x = num_food_gain))+
  geom_bar()


####rank

full_model_rank <- glmer(num_food_gain ~ rank_c * block_c * session_c +  (1|subject),
                         data =individual_data,
                         family = poisson("log")) 

full_model_rank %>% check_overdispersion()
full_model_rank%>% check_collinearity()

AIC_min_rank<- dredge(full_model_rank,rank = "AIC")

AIC_min_rank

AIC_min_model_rank <- get.models(AIC_min_rank,subset = 1)[[1]]

AIC_min_model_rank %>% summary

AIC(full_model_rank, AIC_min_model_rank)

###sex
full_model_sex <- glmer(num_food_gain ~ sex * block_c * session_c + (1|subject),
                        data =individual_data,
                        family = poisson("log")) 

full_model_sex %>% check_overdispersion()
full_model_sex%>% check_collinearity()

AIC_min_sex<- dredge(full_model_sex,rank = "AIC")

AIC_min_sex

AIC_min_model_sex <- get.models(AIC_min_sex,subset = 1)[[1]]

AIC_min_model_sex %>% summary


AIC_null_model_sex <- glmer(num_food_gain ~ (1|subject),
                            data =individual_data,
                            family = poisson("log")) 

AIC(AIC_null_model_sex,AIC_min_model_sex,full_model_sex)


AIC_min_model_sex_prediction <- ggpredict(AIC_min_model_sex,
                                          terms = c("block_c[all]","sex"),
                                          bias_correction = TRUE)

pred_sex <- as.data.frame(AIC_min_model_sex_prediction) %>%
  mutate(
    block = x * block_scale + block_center,
    sex = group
  )



ggplot()+

  geom_point(data = individual_data,
             aes(x = block, y = num_food_gain,color = sex),
             position = position_jitter(height = 0.2, width = 0.2),
             size = 1.0)+
  geom_line(data = pred_sex,
            aes(x = block, y= predicted,color = group),
            size = 1.0)+
  scale_x_continuous(breaks=seq(0,6,1))+
  scale_y_continuous(breaks=seq(0,10,2))+
  scale_color_hue(name ="Sex",direction = 1)+
  scale_fill_hue(name = "Sex",direction = 1)+
  labs(x ="Block", y ="Number of acquired food")+
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



##########Stratified GLMM sex-block##############

male_data <- individual_data %>% filter(sex == "male")

male_model_block <- glmer(num_food_gain ~ block + (1|subject),
                           data = male_data,
                          family = poisson("log"))

male_model_null <- glmer(num_food_gain ~ 1+ (1|subject),
                          data = male_data,
                         family = poisson("log"))

AIC(male_model_null,male_model_block)

male_model_block %>% summary
##
female_data <- individual_data %>% filter(sex == "female")

female_model_block <- glmer(num_food_gain ~ block + (1|subject),
                            data = female_data,
                            family = poisson("log"))

female_model_null <- glmer(num_food_gain ~ 1+ (1|subject),
                           data = female_data,
                           family = poisson("log"))

AIC(female_model_null,female_model_block)

female_model_block %>% summary

######################
leave_data = individual_data %>% filter(subject != "HN")

# rank
leave_model_full_rank <- glmer(num_food_gain ~ rank_c * block_c * session_c + (1|subject),
                              data =leave_data ,
                              family = poisson("log")) 

leave_model_full_rank %>% check_overdispersion()
leave_model_full_rank%>% check_collinearity()


leave_model_full_rank <- glmer.nb(num_food_gain ~ rank_c * block_c * session_c + (1|subject),
                                 data =leave_data ) 

leave_model_AIC_min_rank <- dredge(leave_model_full_rank,rank = "AIC")

leave_model_AIC_min_rank

leave_model_AIC_min_model_rank <- get.models(leave_model_AIC_min_rank,subset = 1)[[1]]

leave_model_AIC_min_model_rank %>% summary


# sex
leave_model_full_sex <- glmer(num_food_gain ~ sex * block_c * session_c + (1|subject),
                        data =leave_data ,
                        family = poisson("log")) 

leave_model_full_sex %>% check_overdispersion()
leave_model_full_sex%>% check_collinearity()

leave_model_full_sex <- glmer.nb(num_food_gain ~ sex * block_c * session_c + (1|subject),
                              data =leave_data ) 

leave_model_AIC_min_sex<- dredge(leave_model_full_sex,rank = "AIC")

leave_model_AIC_min_sex

leave_model_AIC_min_model_sex <- get.models(leave_model_AIC_min_sex,subset = 1)[[1]]

leave_model_AIC_min_model_sex %>% summary


leave_model_AIC_null_model_sex <- glmer(num_food_gain ~ (1|subject),
                            data =leave_data,
                            family = poisson("log")) 

AIC(leave_model_AIC_null_model_sex,leave_model_AIC_min_model_sex)


leave_model_AIC_min_model_sex_prediction <- ggpredict(leave_model_AIC_min_model_sex,
                                          terms = c("block_c[all]","sex"),
                                          bias_correction = TRUE)

pred_sex <- as.data.frame(leave_model_AIC_min_model_sex_prediction) %>%
  mutate(
    block = x * block_scale + block_center,
    sex = group
  )



ggplot()+
  
  geom_point(data = individual_data,
             aes(x = block, y = num_food_gain,color = sex),
             position = position_jitter(height = 0.2, width = 0.2),
             size = 1.0)+
  geom_line(data = pred_sex,
            aes(x = block, y= predicted,color = group),
            size = 1.0)+
  scale_x_continuous(breaks=seq(0,6,1))+
  scale_y_continuous(breaks=seq(0,10,2))+
  scale_color_hue(name ="Sex",direction = 1)+
  scale_fill_hue(name = "Sex",direction = 1)+
  labs(x ="Block", y ="Number of acquired food")+
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

# versions
## R
R.version.string

## performancce
packageVersion("performance")
citation("performance")

##  lme4
packageVersion("lme4")
citation("lme4")

## MuMIn
packageVersion("MuMIn")
citation("MuMIn")

## parameters
packageVersion("parameters")
citation("parameters")

## ggeffects
packageVersion("ggeffects")
citation("ggeffects")

## ineq
packageVersion("ineq")
citation("ineq")

