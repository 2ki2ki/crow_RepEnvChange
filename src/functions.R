
#####functions#####

se <- function(d){
  
  return(sd(d)/sqrt(length(d)))
  
}



time_limmiter <- function(time_off,limit){
  
  if(time_off > limit){
    time_off <- limit
  }
  
  return(time_off)
  
}


stay_duration_timer <- function(stay_on_off_data,limit){
  
  stay_on_off_data <- stay_on_off_data %>% filter(time_on <=  limit)
  
  if(nrow(stay_on_off_data) <=0){
    
    return(NA)
    
  }
  
  time_off <- stay_on_off_data$time_off %>% sapply(.,time_limmiter,limit)
  
  later_stay_off <- time_off[1]
  duration_stay <-max(time_off) - min(stay_on_off_data$time_on)
  
  for (i in (nrow(stay_on_off_data) -1) %>% seq_len){
    
    if(time_off[i+1] > time_off[i]){
      
      if(stay_on_off_data[i+1,]$time_on > later_stay_off){
        
        no_saty_duration <- stay_on_off_data[i+1,]$time_on - later_stay_off
        
        duration_stay <- duration_stay - no_saty_duration

      }
      
      later_stay_off <- time_off[i+1]
      
    }
    
  }
  
  return(duration_stay)
}


stay_duration_df_maker <- function(data){
  
  stay_duration <- stay_duration_timer(data,trial_time_limit)
  stay_duration_by_last_consumption <- stay_duration_timer(data,time_last_consumption)
  
  return(data.frame(stay_duration,stay_duration_by_last_consumption))
  
}

