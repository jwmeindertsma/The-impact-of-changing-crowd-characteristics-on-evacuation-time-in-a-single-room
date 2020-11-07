# Outflow Comparison for the two scenarios under multiple rules
# In this document the two scenarios will be discussed under multiple rules
# We will compare the evacuation times per person and for the crowd as a whole under all rules
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
# Required packages
library(ggplot2)
library(plyr)
library(BSDA)
# First the data will be loaded
# Loading all the data
r1_d1pxl = as.data.frame(read.table("room1_st1pxl.txt")) # Individuals with a radius of 1 pixel
names(r1_d1pxl) = c("exit_pp", "exit_group", "rank", "nr_collisions")
r1_d12pxl = as.data.frame(read.table("room1_st12pxl.txt")) # Individuals with a radius of 12 pixels
names(r1_d12pxl) = c("exit_pp", "exit_group", "rank", "nr_collisions")
r1_d15pxl = as.data.frame(read.table("room1_st15pxl.txt")) # Individuals with a radius of 15 pixels
names(r1_d15pxl) = c("exit_pp", "exit_group", "rank", "nr_collisions")
r1_drpxl = as.data.frame(read.table("room1_rpxl.txt")) # Individuals with a random uniform radius between 9 and 15
names(r1_drpxl) = c("exit_pp", "exit_group", "rank", "nr_collisions")
r2_d1pxl = as.data.frame(read.table("room2_st1pxl466")) # Individuals with a radius of 1 pixel
names(r2_d1pxl) = c("exit_pp", "exit_group", "rank", "nr_collisions")
r2_d12pxl = as.data.frame(read.table("room2_st12pxl466")) # Individuals with a radius of 12 pixels
names(r2_d12pxl) = c("exit_pp", "exit_group", "rank", "nr_collisions")
r2_d15pxl = as.data.frame(read.table("room2_st15pxl466")) # Individuals with a radius of 15 pixels
names(r2_d15pxl) = c("exit_pp", "exit_group", "rank", "nr_collisions")
r2_drpxl = as.data.frame(read.table("room2_rpxl466")) # Individuals with a random uniform radius between 9 and 15
names(r2_drpxl) = c("exit_pp", "exit_group", "rank", "nr_collisions")
# Note: The 1 pixel case will be skipped in comparisons as it is unreasonable to assume individuals will not collide.
# First for the other rules we will make a graph of the evacuation times of the crowd as a whole.
# We will draw a general conlusion about the distribution of the evacuation times and use this conclusion to determine
# which tests will be used in further analysis.

# Rule: Individuals with a radius of 12 pixels
# Scenario 1
ggplot(r1_d12pxl, mapping = aes(exit_group))+geom_density()
# Scenario 2
ggplot(r2_d12pxl, mapping = aes(exit_group))+geom_density()

# Rule: Individuals with a radius of 15 pixels
# Scenario 1
ggplot(r1_d15pxl, mapping = aes(exit_group))+geom_density()
# Scenario 2
ggplot(r2_d15pxl, mapping = aes(exit_group))+geom_density()

# Rule: Individuals with a random uniform radius between 9 and 15
# Scenario 1
ggplot(r1_drpxl, mapping = aes(exit_group))+geom_density()
# Scenario 2
ggplot(r2_drpxl, mapping = aes(exit_group))+geom_density()
# We conclude that the densities suggest we should use non-parametric tests.

# The non-paramteric tests used in this analysis are:
# - Sign Test
# - Mann-Whitney Test(Wilcoxon Rank Sum Test)
# - Wilcoxon Signed Rank Test

# Now the two scenarios will be compared using their individual outflow perfmance as well a outflow performance
# of the crowd as a whole

# Rule: Individuals with a radius of 12 pixels
# Now we will make a graph with confidence intervals of the evacuation times per person
names(r1_d12pxl) =  c("exit_pp", "exit_group", "rank", "nr_collisions")
y1 = r1_d12pxl[-c(seq(74, 748+75, by = 75), seq(75, 749+75, by = 75)), ]
y1 = y1[y1[,1]>0, ]
y1 = summarySE(y1, measurevar = "exit_pp", groupvars=c("rank"))
y1 = cbind(y1, cn = rep(1,72))
names(r2_d12pxl) =  c("exit_pp", "exit_group", "rank", "nr_collisions")
y2 = r2_d12pxl[-c(seq(74, 748+75, by = 75), seq(75, 749+75, by = 75)), ]
y2 = y2[y2[,1]>0, ]
y2 = summarySE(y2, measurevar = "exit_pp", groupvars=c("rank"))
y2 = cbind(y2, cn = rep(2,72))
# Merging data into one data-set
fdata = rbind(y1,y2)
fdata$cn = as.factor(fdata$cn)
mean(y1$exit_pp[72])-mean(y2$exit_pp[72])
# The plot is then 
ggplot(data = fdata, mapping = aes(x=rank, y=exit_pp, group = cn, ymin=exit_pp-se, ymax=exit_pp+se)) + geom_line() + 
  geom_point(aes(shape = cn)) + geom_errorbar(width=0) +
  ggtitle("Outflow comparison for individuals with a radius of 0.2500 meters") + theme(legend.position = c(0.3,0.8)) + 
  labs(x = "Rank of Person", y = "Evacuation Time" ,color = "Scenarios and corresponiding lines") + ylim(0,40) + 
  scale_shape_manual("Scenarios and corresponding lines", values = c(15,16), labels=c("Room without obstacle", "Room with obstacle"))

# Now we test whether the mean evacuation time for the crowd as whole is lower in case of the triangle (Scenario 2):
# Sign-test
SIGN.test(unique(r1_d12pxl[[2]]), unique(r2_d12pxl[[2]]))
# Mann-Whitney Test(Wilcoxon Rank Sum Test) 
wilcox.test(unique(r1_d12pxl[[2]]), unique(r2_d12pxl[[2]]))
# Wilcoxon Signed Rank Test
wilcox.test(unique(r1_d12pxl[[2]]), unique(r2_d12pxl[[2]]), paired = TRUE)

# Rule: Individuals with a radius of 15 pixels
# Now we will make a graph with confidence intervals of the evacuation times per person
names(r1_d15pxl) =  c("exit_pp1", "exit_group1", "rank1", "nr_collisions")
y1 = r1_d15pxl[-c(seq(74, 748+75, by = 75), seq(75, 749+75, by = 75)), ]
y1 = y1[y1[,1]>0, ]
y1 = summarySE(y1, measurevar = "exit_pp1", groupvars=c("rank1"))
names(r2_d15pxl) =  c("exit_pp2", "exit_group2", "rank2", "nr_collisions")
y2 = r2_d15pxl[-c(seq(74, 748+75, by = 75), seq(75, 749+75, by = 75)), ]
y2 = y2[y2[,1]>0, ]
y2 = summarySE(y2, measurevar = "exit_pp2", groupvars=c("rank2"))
# Merging data into one data-set
fdata = cbind(y1,y2)
names(fdata) = c("rank1", "N1", "exit_pp1", "sd1", "se1", "ci1", "rank2", "N2", "exit_pp2", "sd2", "se2", "ci2")
mean(y1$exit_pp[72])-mean(y2$exit_pp[72])
# The plot is then 
ggplot(data = fdata) + geom_line(mapping = aes(x=rank1, y=exit_pp1, color = "Room without obstacle")) + 
  geom_point(mapping = aes(x=rank1, y=exit_pp1, color = "Room without obstacle")) + 
  geom_errorbar(mapping = aes(ymin=exit_pp1-se1, ymax=exit_pp1+se1, x=rank1, y=exit_pp1, color = "Room without obstacle"), width=0.1) +
  geom_line(mapping = aes(x=rank2, y=exit_pp2, color = "Room with obstacle")) + 
  geom_point(mapping = aes(x=rank2, y=exit_pp2, color = "Room with obstacle")) + 
  geom_errorbar(mapping = aes(ymin=exit_pp2-se2, ymax=exit_pp2+se2, x=rank2, y=exit_pp2, color = "Room with obstacle"), width=.1) +
  ggtitle("Outflow comparison for individuals with a radius of 0.3125 meters") + theme(legend.position = c(0.2,0.8)) + 
  labs(x = "Rank of Person", y = "Evacuation Time" ,color = "Scenarios and corresponiding lines") + 
  scale_colour_manual(values = c("green", "red")) + ylim(0,110)

# Now we test whether the mean evacuation time for the crowd as whole is lower in case of the triangle (Scenario 2):
# Sign-test
SIGN.test(unique(r1_d15pxl[[2]]), unique(r2_d15pxl[[2]]))
# Mann-Whitney Test(Wilcoxon Rank Sum Test) 
wilcox.test(unique(r1_d15pxl[[2]]), unique(r2_d15pxl[[2]]))
# Wilcoxon Signed Rank Test
wilcox.test(unique(r1_d15pxl[[2]]), unique(r2_d15pxl[[2]]), paired = TRUE)

# Rule: Individuals with a radius that is RU distributed
# Now we will make a graph with confidence intervals of the evacuation times per person
names(r1_drpxl) =  c("exit_pp1", "exit_group1", "rank1", "nr_collisions")
y1 = r1_drpxl[-c(seq(74, 748+75, by = 75), seq(75, 749+75, by = 75)), ]
y1 = y1[y1[,1]>0, ]
y1 = summarySE(y1, measurevar = "exit_pp1", groupvars=c("rank1"))
names(r2_drpxl) =  c("exit_pp2", "exit_group2", "rank2", "nr_collisions")
y2 = r2_drpxl[-c(seq(74, 748+75, by = 75), seq(75, 749+75, by = 75)), ]
y2 = y2[y2[,1]>0, ]
y2 = summarySE(y2, measurevar = "exit_pp2", groupvars=c("rank2"))
# Merging data into one data-set
fdata = cbind(y1,y2)
names(fdata) = c("rank1", "N1", "exit_pp1", "sd1", "se1", "ci1", "rank2", "N2", "exit_pp2", "sd2", "se2", "ci2")
mean(y1$exit_pp[72])-mean(y2$exit_pp[72])
# The plot is then 
ggplot(data = fdata) + geom_line(mapping = aes(x=rank1, y=exit_pp1, color = "Room without obstacle")) + 
  geom_point(mapping = aes(x=rank1, y=exit_pp1, color = "Room without obstacle")) + 
  geom_errorbar(mapping = aes(ymin=exit_pp1-se1, ymax=exit_pp1+se1, x=rank1, y=exit_pp1, color = "Room without obstacle"), width=0.1) +
  geom_line(mapping = aes(x=rank2, y=exit_pp2, color = "Room with obstacle")) + 
  geom_point(mapping = aes(x=rank2, y=exit_pp2, color = "Room with obstacle")) + 
  geom_errorbar(mapping = aes(ymin=exit_pp2-se2, ymax=exit_pp2+se2, x=rank2, y=exit_pp2, color = "Room with obstacle"), width=.1) +
  ggtitle("Outflow comparison for individuals with a RU distributed radius") + theme(legend.position = c(0.4,0.8)) + 
  labs(x = "Rank of Person", y = "Evacuation Time" ,color = "Scenarios and corresponiding lines") + 
  scale_colour_manual(values = c("green", "red")) + ylim(0,100)

# Now we test whether the mean evacuation time for the crowd as whole is lower in case of the triangle (Scenario 2):
# Sign-test
SIGN.test(unique(r1_drpxl[[2]]), unique(r2_drpxl[[2]]))
# Mann-Whitney Test(Wilcoxon Rank Sum Test) 
wilcox.test(unique(r1_drpxl[[2]]), unique(r2_drpxl[[2]]))
# Wilcoxon Signed Rank Test
wilcox.test(unique(r1_drpxl[[2]]), unique(r2_drpxl[[2]]), paired = TRUE)

# Comparing evacuation times
