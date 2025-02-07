#######################################
setwd("path")

library(needs)
needs(tidyverse,ineq)
prioritize(dplyr)
source("./src/functions.r")

all_trial_data <- read.csv("./data.csv")

########################################
subject_menber <- c("ZN","MN","AN","PN","kn","HN","sn","bn","yn","wn")

order_color <- c("#1f2f6a","#3f78ca","#539ada","#85beea","#7f1927",
                 "#c1ddf8","#ac243c","#d22f54","#f06c95","#f9ccde")

rank_order <- 1:10

sub_sex <- c("male","male","male","male","female",
             "male","female","female","female","female")

sub_color_corresp <- data.frame(subject_menber,order_color,rank_order,sub_sex)

######################################
trial_time_limit <- 180
trial_per_session <- 4
######################################

group_data <- NULL
individual_data <- NULL
stay_info_sub <- NULL
total_trial <- 0
pre_session <- 1
total_session <- 1


for (trial in all_trial_data$Observation.id %>% unique()){
  
  #shape infomation of trials
  total_trial <- total_trial + 1
  
  print(trial)
  data <- all_trial_data %>% filter(Observation.id == trial)
  
  info_observation <- data$Observation.id[[1]] %>% strsplit(.,split = "-")
  
  block <- info_observation[[1]][1] %>% as.numeric()
  
  data$block <- block
  
  session <- info_observation[[1]][2] %>% as.numeric()
  
  data$session <- session
  
  if(pre_session != session){
    
    total_session <- total_session + 1
    
  }
  pre_session <- session
  
  data$total_session <- total_session 
  
  
  data$trial <- info_observation[[1]][3] %>% as.numeric()
  
  data$total_trial <- total_trial
  
  data <- data[,-1] %>% 
    dplyr::select(.,block,
                  session,total_session,
                  trial,
                  total_trial,
                  Time,
                  everything())
  
  experiment_info <- data[1,c(1:5)]
  
  time_tiral_start <- (data %>% filter(Subject == "experimenter" & 
                                        Behavior == "trial_start"))$Time
  
  data$Time <- data$Time - time_tiral_start

  ##group##
  ###Opening cup###
  #########################
  
  opening_cup_data <- data %>% filter(Behavior == "A_peck_open_newly"|
                                    Behavior == "B_peck_open_newly"|
                                    Behavior == "C_peck_open_newly") %>% filter(Time <= 180)
  
  if(block %% 3 == 1){
    
    opening_hit <- opening_cup_data %>% filter(Behavior == "B_peck_open_newly"|
                                                Behavior == "C_peck_open_newly")
    
  }else if(block %% 3 == 2){ 
    
    opening_hit <- opening_cup_data %>% filter(Behavior == "A_peck_open_newly"|
                                                Behavior == "B_peck_open_newly")
    
  }else if(block %% 3 == 0){   
    
    opening_hit <- opening_cup_data %>% filter(Behavior == "C_peck_open_newly"|
                                                Behavior == "A_peck_open_newly")
    
  }
  
  num_cup_open <- opening_cup_data %>% nrow
  num_sub_open_cup <- opening_cup_data$Subject %>% unique %>% length()
  time_first_open <- (opening_cup_data %>% head(1))$Time
  time_last_open <- (opening_cup_data %>% tail(1))$Time
  time_start_foraging <- (opening_hit %>% head(1))$Time
  
  
  ###food gain###    
  #########################  
  
  food_gaining_temp <- data %>% filter(Behavior == "get_food")
  
  num_food_gained <- nrow(food_gaining_temp)
  time_first_gain <- food_gaining_temp$Time[1]
  time_last_gain <- food_gaining_temp$Time[num_food_gained]
  
  
  ###stay box###    
  #########################   
  
  stay_data <- data %>% filter(Behavior == "stay_A"|
                               Behavior == "stay_B"|
                               Behavior == "stay_C")
  
  onset_stay <- stay_data %>% filter(Behavior.type == "START")
  offset_stay <- stay_data %>% filter(Behavior.type == "STOP")
  
  sub_stay <- onset_stay$Subject %>% unique
  
  stay_on_off_data <- data.frame()
  stay_date_by_sub <- data.frame()
  
  for (sub_temp in sub_stay){
    
    stay_on_off_data_temp <- onset_stay %>% filter(Subject == sub_temp)
    stay_on_off_data_temp$time_off <- (offset_stay %>% filter(Subject == sub_temp))$Time
    stay_on_off_data_temp <- stay_on_off_data_temp %>% rename(time_on =Time)
    stay_on_off_data_temp <- stay_on_off_data_temp %>% filter(time_on <= trial_time_limit)
    stay_on_off_data_temp 
    
    stay_on_off_data <- rbind(stay_on_off_data,stay_on_off_data_temp)
    
  }
  
  
  stay_on_off_data <-  stay_on_off_data %>%
    filter(time_on <= trial_time_limit)%>%
    arrange(.,time_on)
  
  stay_on_off_data$time_off <- stay_on_off_data$time_off %>% 
    sapply(.,time_limmiter,trial_time_limit)
  
  #num_sub_stay <- stay_on_off_data$Subject %>% unique %>% length()
  
  time_start_stay <- stay_on_off_data$time_on[1]
  
  #stay_duration_df <- stay_on_off_data %>% stay_duration_df_maker
  
  ###index###
  #########################   
  num_successful_forager <- food_gaining_temp$Subject %>% unique %>% length()
  foraging_duration <- time_last_gain - time_start_foraging
  exploring_duration <- time_last_open - time_start_stay
  
  group_data_temp <- data.frame(experiment_info,
                                num_successful_forager,
                                foraging_duration,
                                exploring_duration
                                )
  
  
  
  group_data <- rbind(group_data,group_data_temp)
  
  ##individual##
  ###############################
  
  individual_data_temp <- data.frame(matrix(rep(NA,1),nrow =1))
  
  colnames(individual_data_temp) <- "subject"
  individual_data_temp <- cbind(experiment_info,individual_data_temp)
  
  for (subject in subject_menber){
    
    individual_data_temp$subject <- subject
    
    individual_data_temp$num_food_gain <- food_gaining_temp %>%filter(Subject == subject) %>% nrow
    
    individual_data <- rbind(individual_data, individual_data_temp)
    
  }
  
}

individual_data$subject <- factor(individual_data$subject,levels = sub_color_corresp[,"subject_menber"])

sub_rank_make <- (individual_data$subject %>% as.vector)
sub_sex_make <- sub_rank_make

for(rank in rank_order){
  
  sub_rank_make[sub_rank_make == sub_color_corresp[rank,"subject_menber"]] <- rank
  sub_sex_make[sub_sex_make == sub_color_corresp[rank,"subject_menber"]] <- sub_color_corresp[rank,"sub_sex"] 
}
individual_data$rank <- sub_rank_make %>% as.numeric()
individual_data$sex <- sub_sex_make

individual_data <- individual_data[c("block",
                                     "session",
                                     "trial",
                                     "total_trial",
                                     "subject",
                                     "rank",
                                     "sex",
                                     "num_food_gain")]


###gini###
gini_coef <- NULL

for(i in 1:max(individual_data$total_trial)){
  food_distribution <- individual_data %>% filter(total_trial == i) %>%
    group_by(subject)%>%
    summarise(sum = sum(num_food_gain))
  
    gini_coef <-  c(gini_coef,Gini(food_distribution$sum,TRUE))
}

group_data$gini_coef <- gini_coef



write_csv(group_data,"./group_data.csv")
write_csv(individual_data,"./individual_data.csv")
