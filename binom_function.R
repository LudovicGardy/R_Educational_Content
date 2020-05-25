library(lmtest)
library(lme4)

# Binomial ata simulation
#--------------------------
simu_data = function(nb_subjects, nb_measures, balanced_groups) {
  
  df = data.frame(matrix(NA,(nb_subjects * nb_measures), 3))
  colnames(df) = c("Subject", "Group", "Measure")
  
  count_begin = 1
  count_end = nb_measures
  
  for(i in 1:nb_subjects) {
    df[count_begin:count_end,1] = paste("Subject_", i, sep = "")
    
    if(runif(1) > 0.5) {
      df[count_begin:count_end,2] = "Group_A"
    } else {
      df[count_begin:count_end,2] = "Group_B"
    }
    
    count_begin = count_end + 1
    count_end = count_end + nb_measures
  }

  if(balanced_groups == F) {    
    for(i in 1:length(df[,1])) {
      if(df[i,2] == "Group_A") {
        if(runif(1) > 0.3) {
          df[i,3] = 1
        } else {
          df[i,3] = 0
        } 
      } else {
        if(runif(1) > 0.7) {
          df[i,3] = 1
        } else {
          df[i,3] = 0
        }       
      }
    }
  } 
  
  if(balanced_groups == T) {    
    for(i in 1:length(df[,1])) {
      if(df[i,2] == "Group_A") {
        if(runif(1) > 0.5) {
          df[i,3] = 1
        } else {
          df[i,3] = 0
        } 
      } else {
        if(runif(1) > 0.5) {
          df[i,3] = 1
        } else {
          df[i,3] = 0
        }       
      }
    }
  }    
  return(df)
  
}

# Binomial model simulation
#----------------------------
simu_model = function(n_simu) {
  res_simu = NA
  
  for(i in 1:n_simu) {
    
    data = simu_data(nb_subjects = 21, nb_measures = 4, balanced_groups = F)
    
    model <- glmer(data$Measure ~ data$Group + (1|data$Subject), family = binomial)
    model_res = summary(model)
    res_simu[i] = model_res$coefficients[2,4]
    
  }
  
  return(res_simu)
  
}