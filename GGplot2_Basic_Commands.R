# By Ludovic G. (with help from https://datacamp.com)

#								#======================#
#								# GGPlot 2 : Commandes #
#								#======================#

rm(list = ls())
clc = function() {cat(rep("\n",100))}
clc() 

# Note : aes = aesthetics

#--------#
# Part 1 #
#--------#

# Load the ggplot2 package
library(ggplot2)

# Explore the mtcars data frame with str()
str(mtcars)

# Execute the following command : Quantitative Vs Quantitative
ggplot(mtcars, aes(x = cyl, y = mpg)) +
  geom_point()
  
# Change the command below so that cyl is treated as factor
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_point()
  
# A scatter plot has been made for you : Quantitative Vs Quantitative
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()
  
# Change the COLOR of the scatter plot
ggplot(mtcars, aes(x = wt, y = mpg, color = "red")) +
  geom_point() # or
ggplot(mtcars, aes(x = wt, y = mpg, color = disp)) +
  geom_point()
  
# Change the SIZE of the scatter plot (function of disp) : 3* Quantitative
ggplot(mtcars, aes(x = wt, y = mpg, size = disp)) +
  geom_point()

# Change the SHAPE of the scatter plot (only available for categorical data)
ggplot(mtcars, aes(x = wt, y = mpg, shape = disp)) +
  geom_point()
  
#--------#
# Part 2 #
#--------#
# Explore the diamonds data frame with str()
str(diamonds)

# Without geom_point
ggplot(diamonds, aes(x = carat, y = price))

# With geom_point
ggplot(diamonds, aes(x = carat, y = price)) + geom_point()

# With geom_smooth
ggplot(diamonds, aes(x = carat, y = price)) + geom_point() + geom_smooth()

# Copy the above command but show only the smooth line
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_smooth()
  
# Copy the above command and assign the correct value to col in aes()
ggplot(diamonds, aes(x = carat, y = price, col = clarity)) +
  geom_smooth()
  
# Keep the color settings from previous command. Plot only the points with argument alpha (transparence)
ggplot(diamonds, aes(x = carat, y = price, col = clarity)) +
  geom_point(alpha = 0.4)

#-----------------------#
# Part 2.2 : raccourcis #
#-----------------------#

# Create the object containing the data and aes layers: dia_plot
dia_plot <- ggplot(diamonds, aes(x = carat, y = price))

# Add a geom layer with + and geom_point()
dia_plot + geom_point()

# Add the same geom layer, but with aes() inside
dia_plot + geom_point(aes(color = clarity))

#---------------------------------#
# Part 2.3 : avec ou sans std.err #
#---------------------------------#

# 1 - The dia_plot object has been created for you
dia_plot <- ggplot(diamonds, aes(x = carat, y = price))

# 2 - Expand dia_plot by adding geom_point() with alpha set to 0.2
dia_plot <- dia_plot + geom_point(alpha = 0.2)

# 3 - Plot dia_plot with additional geom_smooth() with se set to FALSE
dia_plot + geom_smooth(se = F)

# 4 - Copy the command from above and add aes() with the correct mapping to geom_smooth()
dia_plot + geom_smooth(se = F, aes(col = clarity))

#--------#
# Part 3 #
#--------#
# Plot the correct variables of mtcars
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl)

# Change cyl inside mtcars to a factor
mtcars$fcyl <- as.factor(mtcars$cyl)

# Make the same plot as in the first instruction
plot(mtcars$wt, mtcars$mpg, col = mtcars$fcyl)

#---------------------------------------#
# Part 3.1 : Linear model SANS GGplot 2 #
#---------------------------------------#
# Use lm() to calculate a linear model and save it as carModel
carModel <- lm(mpg ~ wt, data = mtcars)

# Basic plot
mtcars$cyl <- as.factor(mtcars$cyl)
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl)

# Call abline() with carModel as first argument and set lty to 2
abline(carModel, lty = 2)

# Plot each subset efficiently with lapply
# You don't have to edit this code
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl)
lapply(mtcars$cyl, function(x) {
  abline(lm(mpg ~ wt, mtcars, subset = (cyl == x)), col = x)
  })

# This code will draw the legend of the plot
legend(x = 5, y = 33, legend = levels(mtcars$cyl),
       col = 1:3, pch = 1, bty = "n")

#---------------------------------------#
# Part 3.1 : Linear model AVEC GGplot 2 #
#---------------------------------------#
# Convert cyl to factor (don't need to change)
mtcars$cyl <- as.factor(mtcars$cyl)

# Example from base R (don't need to change)
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl)
abline(lm(mpg ~ wt, data = mtcars), lty = 2)
lapply(mtcars$cyl, function(x) {
  abline(lm(mpg ~ wt, mtcars, subset = (cyl == x)), col = x)
  })
legend(x = 5, y = 33, legend = levels(mtcars$cyl),
       col = 1:3, pch = 1, bty = "n")

# Plot 1: add geom_point() to this command to create a scatter plot
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point()  # Fill in using instructions Plot 1

# Plot 2: include the lines of the linear models, per cyl
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point() + # Copy from Plot 1
  geom_smooth(method = "lm", se = F)   # Fill in using instructions Plot 2

# Plot 3: include a lm for the entire dataset in its whole
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, linetype = 2)
  
#----------------------------------------------#
# Part 4 : Forme que doivent avoir les donnees #
#----------------------------------------------#

#-----------------#
# Part 4.1 : wide #
#-----------------#

# Option 1 : DON'T DO THAT
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  geom_point(aes(x = Petal.Length, y = Petal.Width), col = "red")

# Option 2 : DO THIS
ggplot(iris.wide, aes(x = Length, y = Width, col = Part)) +
  geom_point()
 
#----------------------------------#
# Part 4.2 : format tidy pour iris #
#----------------------------------#
#====\ \=====#
# Creation de iris.tidy et iris.wide : je transforme les donnees ce qui est fait automatiquement dans data camp

# "A dataset is called tidy when every row is an observation and every column is a variable."
# ! IRIS.TIDY ! #
str(iris)
my_names = c("Species","Part","Measure","Value")
iris.tidy = data.frame(matrix(NA,600,4))
colnames(iris.tidy) = my_names
iris.tidy$Value = c(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width )
iris.tidy$Part = c(rep("Sepal", 300), rep("Petal", 300))
iris.tidy$Measure = c(rep("Length", 150), rep("Width", 150), rep("Length", 150), rep("Width", 150))
iris.tidy$Species = rep(iris$Species, 4)

# ! IRIS.WIDE ! #
str(iris)
my_names = c("Species","Part","Length","Width")
iris.wide = data.frame(matrix(NA,300,4))
colnames(iris.wide) = my_names
iris.wide$Species = rep(iris$Species,2) 
iris.wide$Length = c(iris$Sepal.Length, iris$Petal.Length)
iris.wide$Width = c(iris$Sepal.Width, iris$Petal.Width)
iris.wide$Part = c(rep("Sepal",150),rep("Petal",150))

# !OU DE MANIERE PLUS SIMPLE...! #
library(tidyr)
# " The gather() function moves information from the columns to the rows. It takes multiple columns and gathers them into a single column by adding rows. The separate() function splits one column into two or more columns according to a pattern you define. Lastly, the %>% (or "pipe") operator passes the result of the left-hand side as the first argument of the function on the right-hand side. "

# ! IRIS.TIDY ! #
iris.tidy <- iris %>%
  gather(key, Value, -Species) %>%
  separate(key, c("Part", "Measure"), "\\.")

# ! IRIS.WIDE ! #
iris$Flower <- 1:nrow(iris)
iris.wide <- iris %>%
  gather(key, value, -Species, -Flower) %>%
  separate(key, c("Part", "Measure"), "\\.") %>%
  spread(Measure, value)
#====\ \=====#

# Consider the structure of iris, iris.wide and iris.tidy (in that order)
str(iris)
str(iris.tidy)

# Think about which dataset you would use to get the plot shown right
# Fill in the ___ to produce the plot given to the right
ggplot(iris.tidy, aes(x = Species, y = Value, col = Part)) +
  geom_jitter() +
  facet_grid(. ~ Measure)

# The 3 data frames (iris, iris.wide and iris.tidy) are available in your environment
# Execute head() on iris, iris.wide and iris.tidy (in that order)
head(iris)
head(iris.wide)
head(iris.tidy)

# Think about which dataset you would use to get the plot shown right
# Fill in the ___ to produce the plot given to the right
ggplot(iris.wide, aes(x = Length, y = Width, color = Part)) +
  geom_jitter() +
  facet_grid(. ~ Species)

#=====================#
# Part 5 : Aesthetics #
#=====================#
# 1 - Map mpg to x and cyl to y
ggplot(mtcars, aes(mpg, cyl)) +
  geom_point()
  
# 2 - Reverse: Map cyl to x and mpg to y
ggplot(mtcars, aes(cyl, mpg)) +
  geom_point()

# 3 - Map wt to x, mpg to y and cyl to col
ggplot(mtcars, aes(wt, mpg, col = cyl)) +
  geom_point()

# 4 - Change shape and size of the points in the above plot
ggplot(mtcars, aes(wt, mpg, col = cyl)) +
  geom_point(size = 4, shape = 1)
  
#----------------------------------------------#
# Part 5.2 : Remplire les points avec couleurs #
#----------------------------------------------#
# "The color aesthetic typically changes the outside outline of an object and the fill aesthetic is typically the inside shading. However, as you saw in the last exercise, geom_point() is an exception. Here you use color, instead of fill for the inside of the point. But it's a bit subtler than that."

#Which shape to use? The default geom_point() uses shape = 19 (a solid circle with an outline the same colour as the inside). Good alternatives are shape = 1 (hollow) and shape = 16 (solid, no outline). These all use the col aesthetic (don't forget to set alpha for solid points). 
#A really nice alternative is shape = 21 which allows you to use both fill for the inside and col for the outline! This is a great little trick for when you want to map two aesthetics to a dot.

# am and cyl are factors, wt is numeric
class(mtcars$am)
class(mtcars$cyl)
class(mtcars$wt)

# Given from the previous exercise
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point(shape = 1, size = 4)

# 1 - Map cyl to fill
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
  geom_point(shape = 1, size = 4)

# 2 - Change shape and alpha of the points in the above plot
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
  geom_point(shape = 21, size = 4, alpha = 0.6)
  
# 3 - Map am to col in the above plot
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl, col = am)) +
  geom_point(shape = 21, size = 4, alpha = 0.6)
  
# Map cyl to size
ggplot(mtcars, aes(x = wt, y = mpg, size = cyl)) +
  geom_point()

# Map cyl to alpha
ggplot(mtcars, aes(x = wt, y = mpg, alpha = cyl)) +
  geom_point()

# Map cyl to shape 
ggplot(mtcars, aes(x = wt, y = mpg, shape = cyl)) +
  geom_point()

# Map cyl to labels
ggplot(mtcars, aes(x = wt, y = mpg, label = cyl)) +
  geom_text()  
  
#=====================#
# Part 5 : Attributes #
#=====================# 
# In the video you saw that you can use all the aesthetics as attributes. Let's see how this works with the aesthetics you used in the previous exercises: x, y, color, fill, size, alpha, label and shape.
# This time you'll use these arguments to set attributes of the plot, not aesthetics. However, there are some pitfalls you'll have to watch out for: these attributes can overwrite the aesthetics of your plot!
# A word about shapes: In the exercise "All about aesthetics, part 2", you saw that shape = 21 results in a point that has a fill and an outline. Shapes in R can have a value from 1-25. Shapes 1-20 can only accept a color aesthetic, but shapes 21-25 have both a color and a fill aesthetic. See the pch argument in par() for further discussion.
# A word about hexadecimal colours: Hexadecimal, literally "related to 16", is a base-16 alphanumeric counting system. Individual values come from the ranges 0-9 and A-F. This means there are 256 possible two-digit values (i.e. 00 - FF). Hexadecimal colours use this system to specify a six-digit code for Red, Green and Blue values ("#RRGGBB") of a colour (i.e. Pure blue: "#0000FF", black: "#000000", white: "#FFFFFF"). R can accept hex codes as valid colours.

# Define a hexadecimal color
my_color <- "#4ABEFF"

# 1 - First scatter plot, with col aesthetic:
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
geom_point()


# 2 - Plot 1, but set col attributes in geom layer:
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
geom_point(col = my_color)


# 3 - Plot 2, with fill instead of col aesthetic, plut shape and size attributes in geom layer.
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
geom_point(size = 10, shape = 23, col = my_color)

#===============================================#
# 5.1 : Ajout texte au lieu de points sur graph #
#===============================================#

# Expand to draw points with alpha 0.5
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
  geom_point(alpha = 0.5)

# Expand to draw points with shape 24 and color yellow
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
  geom_point(shape = 24, color = 'yellow')

# Expand to draw text with label rownames(mtcars) and color red
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) +
  geom_text(label = rownames(mtcars), color = 'red')

#=========================================#
# 5.2 Conclusion Aesthetics et Attributes #
#=========================================#
#Note: In this chapter you saw aesthetics and attributes. Variables in a data frame are mapped to aesthetics in aes(). (e.g. aes(col = cyl)) within ggplot(). Visual elements are set by attributes in specific geom layers (geom_point(col = "red")). Don't confuse these two things - here you're focusing on aesthetic mappings.

# Map mpg onto x, qsec onto y and factor(cyl) onto col
ggplot(mtcars, aes(x = mpg, y = qsec, col = factor(cyl))) +
geom_point()


# Add mapping: factor(am) onto shape
ggplot(mtcars, aes(x = mpg, y = qsec, col = factor(cyl), shape = factor(am) )) +
geom_point()


# Add mapping: (hp/wt) onto size
ggplot(mtcars, aes(x = mpg, y = qsec, col = factor(cyl), shape = factor(am), size = (hp/wt) )) +
geom_point()

# QUESTION : Many of the aesthetics can be mapped onto continuous or categorical variables, but some are restricted to categorical data. Which aesthetics are they?
# ANSWER : label & shape

#===============================#
# Part 6 : Modifying aesthetics #
#===============================# 
# Barplots : You saw how jittering worked in the video, but bar plots suffer from their own issues of overplotting, as you'll see here. Use the "stack", "fill" and "dodge" positions to reproduce the plot in the viewer. 
# The ggplot2 base layers (data and aesthetics) have already been coded; they're stored in a variable cyl.am. It looks like this:
cyl.am <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(am)))

# The base layer, cyl.am, is available for you
# Add geom (position = "stack" by default)
cyl.am + 
  geom_bar()

# Fill - show proportion (Entre 0 et 1)
cyl.am + 
  geom_bar(position = "fill")  

# Dodging - principles of similarity and proximity (cote a cote)
cyl.am +
  geom_bar(position = "dodge")
  
# Clean up the axes with scale_ functions
val = c("#E41A1C", "#377EB8")
lab = c("Manual", "Automatic")
cyl.am +
  geom_bar(position = "dodge") +
  scale_x_discrete("Cylinders") + 
  scale_y_continuous("Number") +
  scale_fill_manual("Transmission", 
                    values = val,
                    labels = lab)
                    
#===================================================================#
# 6.2 : les stripchart (c'est a dire uniquement axe des x et pas y) #
#===================================================================#
# 1 - Create jittered plot of mtcars, mpg onto x, 0 onto y
ggplot(mtcars, aes(x = mpg, y = 0)) +
  geom_jitter()

# 2 - Add function to change y axis limits
ggplot(mtcars, aes(x = mpg, y = 0)) +
  geom_jitter() +
  scale_y_continuous(limits = c(-2,2))

#===========================#
# 6.3 : Joue avec le design #
#===========================# 

#****************#
#** Les points **#
#****************#

# Basic scatter plot: wt on x-axis and mpg on y-axis; map cyl to col
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
geom_point(size = 4)


# Hollow circles - an improvement
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
geom_point(size = 4,shape = 1)


# Add transparency - very nice
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
geom_point(size = 4,shape = 1, alpha= 0.6)

# Scatter plot: carat (x), price (y), clarity (color)
ggplot(diamonds, aes(x = carat, y = price, col = clarity)) +
geom_point()

# Adjust for overplotting
ggplot(diamonds, aes(x = carat, y = price, col = clarity)) +
geom_point(alpha = 0.5)


# Scatter plot: clarity (x), carat (y), price (color)
ggplot(diamonds, aes(x = clarity, y = carat, col = price)) +
geom_point(alpha = 0.5)


# Dot plot with jittering
ggplot(diamonds, aes(x = clarity, y = carat, col = price)) +
geom_point(alpha = 0.5, position = "jitter")

# Shown in the viewer:
ggplot(mtcars, aes(x = cyl, y = wt)) +
  geom_point()

# Solutions:
# 1 - With geom_jitter()
ggplot(mtcars, aes(x = cyl, y = wt)) +
  geom_jitter()

# 2 - Set width in geom_jitter()
ggplot(mtcars, aes(x = cyl, y = wt)) +
  geom_jitter(width = 0.1)

# 3 - Set position = position_jitter() in geom_point() ()
ggplot(mtcars, aes(x = cyl, y = wt)) +
  geom_point(position = position_jitter(0.1))

# Examine the structure of Vocab
str(Vocab)

# Basic scatter plot of vocabulary (y) against education (x). Use geom_point()
ggplot(Vocab, aes(x = education, y = vocabulary)) +
geom_point()


# Use geom_jitter() instead of geom_point()
ggplot(Vocab, aes(x = education, y = vocabulary)) +
geom_jitter()

  
# Using the above plotting command, set alpha to a very low 0.2
ggplot(Vocab, aes(x = education, y = vocabulary)) +
geom_jitter(alpha = 0.2)

  
# Using the above plotting command, set the shape to 1
ggplot(Vocab, aes(x = education, y = vocabulary)) +
geom_jitter(alpha = 0.2, shape = 1)

#**********************************#
#** Les barplots et histogrammes **#
#**********************************#
# Histograms are one of the most common and intuitive ways of showing distributions. In this exercise you'll use the mtcars data frame to explore typical variations of simple histograms. But first, some background:
# The x axis/aesthetic: The documentation for geom_histogram() states the argument stat = "bin" as a default. Recall that histograms cut up a continuous variable into discrete bins - thats what the stat "bin" is doing. You always get 30 evenly-sized bins by default, which is specified with the default argument binwidth = range/30. This is a pretty good starting point if you don't know anything about the variable being ploted and want to start exploring.
# The y axis/aesthetic: geom_histogram() only requires one aesthetic: x. But there is clearly a y axis on your plot, so where does it come from? Actually, there is a variable mapped to the y aesthetic, it's called ..count... When geom_histogram() executed the binning statistic (see above), it not only cut up the data into discrete bins, but it also counted how many values are in each bin. So there is an internal data frame where this information is stored. The .. calls the variable count from this internal data frame. This is what appears on the y aesthetic. But it gets better! The density has also been calculated. This is the proportional frequency of this bin in relation to the whole data set. You use ..density.. to access this information.

# 1 - Make a univariate histogram
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram()

# 2 - Plot 1, plus set binwidth to 1 in the geom layer
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 1)


# 3 - Plot 2, plus MAP ..density.. to the y aesthetic (i.e. in a second aes() function)
ggplot(mtcars, aes(x = mpg, y = ..density..)) +
  geom_histogram(binwidth = 1, aes(y = ..density..))


# 4 - plot 3, plus SET the fill attribute to "#377EB8"
ggplot(mtcars, aes(x = mpg, y = ..density..)) +
  geom_histogram(binwidth = 1, aes(y = ..density..), fill = "#377EB8")

# In the previous chapter you saw that there are lots of ways to position scatter plots. Likewise, the geom_bar() and geom_histogram() geoms also have a position argument, which you can use to specify how to draw the bars of the plot.
# Three position arguments will be introduced here:
# stack: place the bars on top of each other. Counts are used. This is the default position.
# fill: place the bars on top of each other, but this time use proportions.
# dodge: place the bars next to each other. Counts are used.
# In this exercise you'll draw the total count of cars having a given number of cylinders (cyl), according to manual or automatic transmission type (am) - as shown in the viewer.
# Since, in the built-in mtcars data set, cyl and am are integers, they have already been converted to factor variables for you.

# Draw a bar plot of cyl, filled according to am
ggplot(mtcars, aes(x = cyl, fill = factor(am))) +
  geom_bar()

# Change the position argument to stack
ggplot(mtcars, aes(x = cyl, fill = factor(am))) +
  geom_bar()


# Change the position argument to fill
ggplot(mtcars, aes(x = cyl, fill = factor(am))) +
  geom_bar(position = "fill")


# Change the position argument to dodge
ggplot(mtcars, aes(x = cyl, fill = factor(am))) +
  geom_bar(position = "dodge")

# 1 - The last plot form the previous exercise
ggplot(mtcars, aes(x = cyl, fill = factor(am))) + 
  geom_bar(position = "dodge")

# 2 - Define posn_d with position_dodge()
posn_d <- position_dodge(width = 0.2)

# 3 - Change the position argument to posn_d
ggplot(mtcars, aes(x = cyl, fill = factor(am))) + 
  geom_bar(position = posn_d)


# 4 - Use posn_d as position and adjust alpha to 0.6
ggplot(mtcars, aes(x = cyl, fill = factor(am))) + 
  geom_bar(position = posn_d, alpha = 0.6)
  
# Overlapping histograms pose similar problems to overlapping bar plots, but there is a unique solution here: a frequency polygon.
# This is a geom specific to binned data that draws a line connecting the value of each bin. Like geom_histogram(), it takes a binwidth argument and by default stat = "bin" and position = "identity"

# A basic histogram, add coloring defined by cyl 
ggplot(mtcars, aes(mpg, fill = cyl)) +
  geom_histogram(binwidth = 1)

# Change position to identity 
ggplot(mtcars, aes(mpg, fill = cyl)) +
  geom_histogram(binwidth = 1, position = "identity")

# Change geom to freqpoly (position is identity by default) 
ggplot(mtcars, aes(mpg, col = cyl)) +
  geom_freqpoly(binwidth = 1)

#= ! Palettes de couleurs ! =#

# Example of how to use a brewed color palette
ggplot(mtcars, aes(x = cyl, fill = factor(am))) +
  geom_bar() +
  scale_fill_brewer(palette = "Set1")

# Use str() on Vocab to check out the structure
str(Vocab)

# Plot education on x and vocabulary on fill
# Use the default brewed color palette (INCOMPLETE GRAPH)
ggplot(Vocab, aes(x = education, fill = factor(vocabulary))) +
geom_bar(position = "fill") + 
scale_fill_brewer()

# In the previous exercise, you ended up with an incomplete bar plot. This was because for continuous data, the default RColorBrewer palette that scale_fill_brewer() calls is "Blues". There are only 9 colours in the palette, and since you have 11 categories, your plot looked strange.
# In this exercise, you'll manually create a color palette that can generate all the colours you need. To do this you'll use a function called colorRampPalette().
# The input is a character vector of 2 or more colour values, e.g. "#FFFFFF" (white) and "#0000FF" (pure blue). (See this exercise for a discussion on hexadecimal codes).
# The output is itself a function! So when you assign it to an object, that object should be used as a function. To see what we mean, execute the following three lines in the console:

new_col <- colorRampPalette(c("#FFFFFF", "#0000FF"))
new_col(4)
munsell::plot_hex(new_col(4)) # Quick and dirty plot

# Final plot of last exercise
ggplot(Vocab, aes(x = education, fill = vocabulary)) +
  geom_bar(position = "fill") +
  scale_fill_brewer()
  
# Definition of a set of blue colors
blues <- brewer.pal(9, "Blues")

# Make a color range using colorRampPalette() and the set of blues
blue_range <- colorRampPalette(blues)

# Use blue_range to adjust the color of the bars, use scale_fill_manual()
ggplot(Vocab, aes(x = education, fill = vocabulary)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = blue_range(11))

# 1 - Basic histogram plot command
ggplot(mtcars, aes(mpg)) + 
  geom_histogram(binwidth = 1)

# 2 - Plot 1, Expand aesthetics: am onto fill
ggplot(mtcars, aes(mpg, fill = am)) + 
  geom_histogram(binwidth = 1)


# 3 - Plot 2, change position = "dodge"

ggplot(mtcars, aes(mpg, fill = am)) + 
  geom_histogram(binwidth = 1, position = "dodge")


# 4 - Plot 3, change position = "fill"
ggplot(mtcars, aes(mpg, fill = am)) + 
  geom_histogram(binwidth = 1, position = "fill")


# 5 - Plot 4, plus change position = "identity" and alpha = 0.4
ggplot(mtcars, aes(mpg, fill = am)) + 
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.4)


# 6 - Plot 5, plus change mapping: cyl onto fill
ggplot(mtcars, aes(mpg, fill = cyl)) + 
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.4)

#**********************************#
#**   Line plots : time series   **#
#**********************************#
# Print out head of economics
head(economics)

# Plot unemploy as a function of date using a line plot
ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line()
  
# Adjust plot to represent the fraction of total population that is unemployed
ggplot(economics, aes(x = date, y = unemploy/pop)) +
  geom_line()
  
# Basic line plot
ggplot(economics, aes(x = date, y = unemploy/pop)) +
  geom_line()

# Expand the following command with geom_rect() to draw the recess periods
recess = data.frame(matrix(,6,2))
colnames(recess) = c("begin","end")
recess[,1] = as.Date(c("1969-12-01", "1973-11-01", "1980-01-01", "1981-07-01", "1990-07-01", "2001-03-01"))
recess[,2] = as.Date(c("1970-11-01", "1975-03-01", "1980-07-01", "1982-11-01", "1991-03-01","2001-11-01"))


ggplot(economics, aes(x = date, y = unemploy/pop)) +
  geom_rect(data = recess, 
            aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf), 
            inherit.aes = FALSE, fill = "red", alpha = 0.2) +
  geom_line()
  
# Loading fish.species data
library(Rcmdr)
fish.species <- readXL("/Users/ludovicgardy/Desktop/fish_species.xlsx",
                       rownames=FALSE, header=TRUE, na="", sheet="Feuil1", 
                       stringsAsFactors=TRUE)

# Check the structure as a starting point
str(fish.species)

# Use gather to go from fish.species to fish.tidy
fish.tidy <- gather(fish.species, Species, Capture, -Year)

# Recreate the plot shown on the right
ggplot(fish.tidy, aes(x = Year, y = Capture, colour = Species)) + geom_line()
  
#*****************#
#**   Qplots    **#
#*****************#
# The old way (shown)
plot(mpg ~ wt, data = mtcars) # formula notation
with(mtcars, plot(wt, mpg)) # x, y notation

# Using ggplot:
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

# Using qplot:
qplot(wt, mpg, data = mtcars) 

# basic qplot scatter plot:
qplot(wt, mpg, data = mtcars)

# Categorical variable mapped onto size:
# cyl
qplot(wt, mpg, data = mtcars, size = factor(cyl))

# gear
qplot(wt, mpg, data = mtcars, size = factor(gear))

# Continuous variable mapped onto col:
# hp
qplot(wt, mpg, data = mtcars, color = hp)

# qsec
qplot(wt, mpg, data = mtcars, color = qsec)

# qplot() with x only
qplot(factor(cyl),data = mtcars)

# qplot() with x and y
qplot(x = factor(cyl), y = factor(vs),data = mtcars)

# qplot() with geom set to jitter manually
qplot(x = factor(cyl), y = factor(vs),data = mtcars, geom = "jitter")

# cyl and am are factors, wt is numeric
class(mtcars$cyl)
class(mtcars$am)
class(mtcars$wt)

# "Basic" dot plot, with geom_point():
ggplot(mtcars, aes(cyl, wt, col = am)) +
  geom_point(position = position_jitter(0.2, 0))

# 1 - "True" dot plot, with geom_dotplot():
ggplot(mtcars, aes(cyl, wt, fill = am)) +
  geom_dotplot(stackdir = "center", binaxis = "y")

# 2 - qplot with geom "dotplot", binaxis = "y" and stackdir = "center"
qplot(
  cyl, wt, 
  data = mtcars, 
  fill = am, 
  geom = "dotplot", 
  binaxis = "y", 
  stackdir = "center"
)


