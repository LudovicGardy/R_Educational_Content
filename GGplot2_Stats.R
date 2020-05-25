# By Ludovic G. (with help from https://datacamp.com)

#=========================#
# GGplot datacamp, Part 3 #
#=========================#

#----------------------#
# Exercice 1 : rappels #
#----------------------#

# Create movies_small
#install.packages("ggplot2movies")
library(ggplot2movies)
library(ggplot2)
set.seed(123)
movies_small <- movies[sample(nrow(movies), 1000), ]
movies_small$rating <- factor(round(movies_small$rating))

# Explore movies_small with str()
str(movies_small)

# Build a scatter plot with mean and 95% CI
ggplot(movies_small, aes(x = rating, y = votes)) +
  geom_point() +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "crossbar",
               width = 0.2,
               col = "red") +
  scale_y_log10()
  
# Reproduce the plot
data(diamonds)
attach(diamonds)

ggplot(diamonds, aes(x = carat, y = price, col = color)) +
  geom_point(alpha = 0.5, size = 0.5, shape = 16) +
  scale_x_log10(expression(log[10](Carat)), limits = c(0.1,10)) +
  scale_y_log10(expression(log[10](Price)), limits = c(100,100000)) +
  scale_color_brewer(palette = "YlOrRd") +
  coord_equal() +
  theme_classic()
  
# Add smooth layer and facet the plot
ggplot(diamonds, aes(x = carat, y = price, col = color)) +
  scale_x_log10(expression(log[10](Carat)), limits = c(0.1,10)) +
  scale_y_log10(expression(log[10](Price)), limits = c(100,100000)) +
  scale_color_brewer(palette = "YlOrRd") +
  coord_equal() +
  theme_classic() +
  stat_smooth(method = "lm")
  
#--------------------------#
# Exercice 2 : stats plots #
#--------------------------#

# movies_small is available

# Add a boxplot geom
d <- ggplot(movies_small, aes(x = rating, y = votes)) +
  geom_point() +
  geom_boxplot() +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "crossbar",
               width = 0.2,
               col = "red")

# Untransformed plot
d

# Transform the scale (the transformation happens BEFORE calculating the statistics)
d + scale_y_log10()

# Transform the coordinates (the transformation happens AFTER calculating the statistics)
d + coord_trans(y = "log10")

#--------------------------#
# Exercice 3 : Cut it up ! #
#--------------------------#
#If you only have continuous variables, you can convert them into ordinal variables using any of the following functions:
#cut_interval(x, n) makes n groups from vector x with equal range.
#cut_number(x, n) makes n groups from vector x with (approximately) equal numbers of observations.
#cut_width(x, width) makes groups of width width from vector x.
#This is useful when you want to summarize a complex scatter plot like the one shown in the viewer. By applying these functions to the carat variable and mapping that onto the group aesthetic, you can convert the scatter plot in the viewer into a series of box plots on the fly.

# Plot object p
p <- ggplot(diamonds, aes(x = carat, y = price))

# Use cut_interval
p + geom_boxplot(aes(group = cut_interval(carat, n = 10)))

# Use cut_number
p + geom_boxplot(aes(group = cut_number(carat, n = 10)))

# Use cut_width
p + geom_boxplot(aes(group = cut_width(carat, width = 0.25)))
