# By Ludovic G
data(iris) # For the example.

#----------------------------
# Parameters
#----------------------------
data = iris # Replace this by your own full data frame.
Independant_Variable_1 = as.factor(data$Species) # Replace this by the Independant variable within your database. You can create several Independant variables in several variables if necessary.
Dependant_Variables = data[1:4] # This subset of your initial dataframe should only contain the dependant variables that you want to analyse.

#----------------------------
# Run the script
#----------------------------
### Define initial variables
x_data = Independant_Variable_1 # Independant variable (here we have only one but it could be more).

### Put all the Dependant Variables (of intereset) into a matrix by adding columns to the first vector y_data.
count = 1
for(i in Dependant_Variables[1:(length(Dependant_Variables))]) {
  if(count == 1) {
    y_data = cbind(i)
    colnames(y_data)[count] = colnames(Dependant_Variables)[count]
  } else {
    y_data = cbind(y_data, i)
    colnames(y_data)[count] = colnames(Dependant_Variables)[count]
  }
  count = count + 1
}

### Compute the MANOVA
res.man = manova(y_data ~ x_data)

### Check the normality of residuals
hist(res.man$residuals, col = "lightblue")

### Ask for the summary
my_summary = summary.aov(res.man)
#print(my_summary) # Full summary.
#print(my_summary[1]) # Summary for 1 dependant variable.

### Ask significance for each dependant variable
count = 1
for(value in my_summary) {
  if(is.na(value$`Pr(>F)`[1] == T)) {
    print("Missing value")
  } else if(value$`Pr(>F)`[1] < 0.05) {
      print(paste(names(my_summary)[count],": ",value$`Pr(>F)`[1], "***"))
      count = count + 1
  } else {
      print(paste(names(my_summary)[count],"> 0.05"))
  }
}