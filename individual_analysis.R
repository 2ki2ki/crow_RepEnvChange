library(needs)
needs(tidyverse,MASS,ggeffects,performance,MuMIn)
options(na.action = "na.fail")

individual_data = read.csv("./individual_data.csv")

################################################################

ggplot(individual_data,
       aes(x = num_food_gain))+
  geom_bar()

glm(num_food_gain ~ sex * rank,
    data =individual_data,
    family = poisson("log")) %>% check_overdispersion()

glm.nb(num_food_gain ~ sex + rank + block + session,
    data =individual_data) %>% check_collinearity()

####rank

full_model_rank <- glm.nb(num_food_gain ~  rank * block * session,
                            data = individual_data)

AIC_min_rank<- dredge(full_model_rank,rank = "AIC")

AIC_min_rank

AIC_min_model_rank <- get.models(AIC_min_rank,subset = 1)[[1]]

AIC_min_model_rank %>% summary


###sex
full_model_sex <- glm.nb(num_food_gain ~  sex * block * session,
                            data = individual_data)

AIC_min_sex<- dredge(full_model_sex,rank = "AIC")

AIC_min_sex

AIC_min_model_sex <- get.models(AIC_min_sex,subset = 1)[[1]]

AIC_min_model_sex %>% summary


AIC_null_model_sex <- glm.nb(num_food_gain ~  1,
                                data = individual_data)

AIC(AIC_null_model_sex,AIC_min_model_sex)


AIC_min_model_sex_prediction <- ggpredict(AIC_min_model_sex,terms = c("block[all]","sex"))

ggplot()+
  geom_point(data = individual_data,
             aes(x = block, y = num_food_gain,color = sex),
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
  labs(x ="Block", y ="Number of foods acquired by each subject")+
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


##########Stratified GLM sex-block##############

male_data <- individual_data %>% filter(sex == "male")

male_model_block <- glm.nb(num_food_gain ~ block,
                       data = male_data)

male_model_null <- glm.nb(num_food_gain ~ 1,
                          data = male_data)

AIC(male_model_null,male_model_block)



##
female_data <- individual_data %>% filter(sex == "female")

female_model_block <- glm.nb(num_food_gain ~ block,
                          data = female_data)

female_model_null <- glm.nb(num_food_gain ~ 1,
                            data = female_data)

AIC(female_model_null,female_model_block)

female_model_block %>% summary

######################