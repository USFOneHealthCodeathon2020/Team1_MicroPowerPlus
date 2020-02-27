library(tidyverse)
df_sim_data<-read_csv("human_gut_power_simulation_results.csv")


calculate_effect_size_model_for_sample_size<-function(df,sample_size){
  ###Calculate a model to predict the effect size given power
  bp_model<-df %>% filter(Sample_Size==sample_size)
  #bp_model <- subset(bp, power < 0.95 & power > 0.2)
  bp_model <- data.frame(log_omega2=log10(bp_model$simulated_omega2),log_power=log10(bp_model$power))
  bp_model <- subset(bp_model, log_omega2>-Inf)
  View(bp_model)
  bp_lm <- lm(log_omega2 ~ log_power, data=bp_model)

  return(bp_lm)
}

get_effect_size_from_power<-function(model,power){
  effect_size <- 10^predict(model, newdata=data.frame(log_power=log10(power)))
  return(effect_size)
}


get_effect_size_from_sample_size_and_power<-function(df_sim_data,sample_size,power){
  model<-calculate_effect_size_model_for_sample_size(df_sim_data,sample_size)
  effect_size<-get_effect_size_from_power(model,power)
  return(effect_size)
}

effect_size<-get_effect_size_from_sample_size_and_power(df_sim_data,5,.90)




####Stuff that isn't working#####
# calculate_power_model_for_sample_size<-function(df,sample_size){
#   ####Calculate a model to predict power from effect size
#   bp_model<-df %>% filter(Sample_Size==sample_size)
#   #bp_model <- subset(bp, power < 0.95 & power > 0.2)
#   bp_model <- data.frame(log_omega2=log10(bp_model$simulated_omega2),log_power=log10(bp_model$power))
#   bp_model <- subset(bp_model, log_omega2>-Inf)
#   View(bp_model)
#   bp_lm <- lm(log_power ~log_omega2, data=bp_model)
#   
#   return(bp_lm)
# }

# get_power_from_effect_size<-function(model,effect_size){
#   power <- approx(x = model$fitted, y = model$x, xout = effect_size)$y
#   #power <- 10^predict(model, newdata=data.frame(log_omega2=log10(effect_size)))
#   return(power)
# }


#bp5_power_lm<-calculate_power_model_for_sample_size(df_sim_data,5)


