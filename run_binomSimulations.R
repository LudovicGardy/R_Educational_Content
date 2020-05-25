source("C:/Users/GARDy/Desktop/simubinomial_function.R")

#------------------------------------------
# Options: You can change these parameters
#------------------------------------------

nb_subjects = 21
nb_measures = 4
balanced_groups = TRUE

n_simu = 10

#------------------------------------------------------------------#
#------------------------------------------------------------------#
#------------------------------------------------------------------#
#------------------------------------------------------------------#

#-------------------------------------------------------------
# Run simulations: You should not change the following lines
#-------------------------------------------------------------

# Data simulation
#-----------------
data = simu_data(nb_subjects, nb_measures, balanced_groups)

# glm with Subjects ID as a random effect
model <- glmer(data$Measure ~ data$Group + (1|data$Subject), family = binomial)
summary(model)

# Repeat data and model simulations "n_simu" times
#--------------------------------------------------
res_simu = simu_model(n_simu)

# Display results
#-----------------
good = paste((length(which(res_simu < 0.05)) / n_simu * 100), " % significant")
wrong = paste((length(which(res_simu > 0.05)) / n_simu * 100), " % not significant")

for(printres in c(good,wrong)) {
  print(printres)
}