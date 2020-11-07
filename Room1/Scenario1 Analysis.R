# Analysis room 1 extension
# Packages required
library(ggplot2)
library(plyr)
# Required functions

# Source: http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
# Summarizes data.
# Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
#   data: a data frame.
#   measurevar: the name of a column that contains the variable to be summariezed
#   groupvars: a vector containing names of columns that contain grouping variables
#   na.rm: a boolean that indicates whether to ignore NA's
#   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE){ 
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
# Loading all the data
r1_d1pxl = read.table("room1_st1pxl.txt") # Individuals with a radius of 1 pixel
r1_d12pxl = read.table("room1_st12pxl.txt") # Individuals with a radius of 12 pixels
r1_d15pxl = read.table("room1_st15pxl.txt") # Individuals with a radius of 15 pixels
r1_drpxl = read.table("room1_rpxl.txt") # Individuals with a random uniform radius between 9 and 15

# The first dataset
# Now we will make a graph with confidence intervals of the evacuation times per person
names(r1_d1pxl) =  c("exit_pp", "exit_group", "rank", "nr_collisions")
y1 = r1_d1pxl[-c(seq(73, 747+75, by = 75), seq(74, 748+75, by = 75), seq(75, 749+75, by = 75)), ]
y1 = y1[y1[,1]>0, ]
y1 = summarySE(y1, measurevar = "exit_pp", groupvars=c("rank"))
y1 = cbind(y1, cn = rep(15, 72))

# The second dataset
# Now we will make a graph with confidence intervals of the evacuation times per person
names(r1_d12pxl) =  c("exit_pp", "exit_group", "rank", "nr_collisions")
y2 = r1_d12pxl[-c(seq(73, 747+75, by = 75), seq(74, 748+75, by = 75), seq(75, 749+75, by = 75)), ]
y2 = y2[y2[,1]>0, ]
y2 = summarySE(y2, measurevar = "exit_pp", groupvars=c("rank"))
y2 = cbind(y2, cn = rep(16, 72))

# The third dataset
# Now we will make a graph with confidence intervals of the evacuation times per person
names(r1_d15pxl) =  c("exit_pp", "exit_group", "rank", "nr_collisions")
y3 = r1_d15pxl[-c(seq(73, 747+75, by = 75), seq(74, 748+75, by = 75), seq(75, 749+75, by = 75)), ]
y3 = y3[y3[,1]>0, ]
y3 = summarySE(y3, measurevar = "exit_pp", groupvars=c("rank"))
y3 = cbind(y3, cn = rep(17, 72))

# The fourth dataset
# Now we will make a graph with confidence intervals of the evacuation times per person
names(r1_drpxl) =  c("exit_pp", "exit_group", "rank", "nr_collisions")
y4 = r1_drpxl[-c(seq(73, 747+75, by = 75), seq(74, 748+75, by = 75), seq(75, 749+75, by = 75)), ]
y4 = y4[y4[,1]>0, ]
y4 = summarySE(y4, measurevar = "exit_pp", groupvars=c("rank"))
y4 = cbind(y4, cn = rep(7, 72))

# Merging all dataframes to one
fdata = rbind(y1,y2,y3,y4)
fdata$cn = as.factor(fdata$cn)

# Creating the plot
ggplot(data = fdata, mapping = aes(x=rank, y=exit_pp, group = cn, ymin=exit_pp-se, ymax=exit_pp+se)) + geom_line() + 
  geom_point(aes(shape = cn)) + geom_errorbar(width=0) +
  theme(legend.position = c(0.4,0.8)) + 
  labs(x = "Rank of Person", y = "Evacuation Time") + 
  ylim(0,70)+xlim(0,72)+ggtitle("Outflow without an obstacle") + 
  scale_shape_manual("Distribution sets and corresponding lines", values = c(7,15,16,17), labels=c("Random uniform radius between 0.1875 and 0.3125 meters","No-collision case", "Radius of 0.2500 meters", "Radius of 0.3125 meters"))

# Compare the lines in this figure
library(BSDA)
mean(y1$exit_pp[72])-mean(y3$exit_pp[72])
SIGN.test(unique(r1_d1pxl[[2]]), unique(r1_d15pxl[[2]]))
mean(r1_d1pxl$exit_group)-mean(r1_d12pxl$exit_group)
SIGN.test(unique(r1_d1pxl[[2]]), unique(r1_d12pxl[[2]]))
mean(r1_d1pxl$exit_group)-mean(r1_drpxl$exit_group)
SIGN.test(unique(r1_d1pxl[[2]]), unique(r1_drpxl[[2]]))
# With no-collision case

# With eachother
mean(y2$exit_pp[72])-mean(y3$exit_pp[72])
SIGN.test(unique(r1_d12pxl[[2]]), unique(r1_drpxl[[2]]))
mean(y2$exit_pp[72])-mean(y4$exit_pp[72])
SIGN.test(unique(r1_d12pxl[[2]]), unique(r1_d15pxl[[2]]))
mean(y3$exit_pp[72])-mean(y4$exit_pp[72])
SIGN.test(unique(r1_drpxl[[2]]), unique(r1_d15pxl[[2]]))
