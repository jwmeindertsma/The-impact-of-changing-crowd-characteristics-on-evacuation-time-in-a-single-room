# Room 1 Analysis
library(dplyr)
# Reading data into R
room1st = as.data.frame(read.table("room1_st"))
room1r = as.data.frame(read.table("room1_r"))
room1twod = as.data.frame(read.table("room1_twodoors"))
room1vis = as.data.frame(read.table("room1_vis"))

# Means in all cases
mean(room1st[,2])
mean(room1r[,2])
mean(room1vis[,2])
mean(room1twod[,2])

# Now I will make a plot of the data for all different cases
# Finding average exit time for person i, i = 0,..,72. Room1
# Deleting observations that are 0 for last 2 persons
names(room1st) =  c("exit_pp", "exit_group", "rank", "nr_collisions")
y1 = room1st[-c(seq(74, 748+75, by = 75), seq(75, 749+75, by = 75)), ]
y1 = y1[y1[,1]>0, ]
y1 = summarise_at(group_by(y1, rank), vars(exit_pp), mean)
y1 = y1$exit_pp
x1 = 0:72
# Plotting both curves in one figure
plot(x1, y1, col = "Green", type = "l", ylim = c(0,100), xlab =  "Rank", ylab = "Exit time for person",
     main = "Outflow Representation Standard Case")

# Now I will make a plot of the data for all different cases
# Finding average exit time for person i, i = 0,..,72. Room1
# Deleting observations that are 0 for last 2 persons
names(room1r) =  c("exit_pp", "exit_group", "rank", "nr_collisions")
y1 = room1r[-c(seq(74, 748+75, by = 75), seq(75, 749+75, by = 75)), ]
y1 = y1[y1[,1]>0, ]
y1 = summarise_at(group_by(y1, rank), vars(exit_pp), mean)
y1 = y1$exit_pp
x1 = 0:72
# Plotting both curves in one figure
plot(x1, y1, col = "Green", type = "l", ylim = c(0,100), xlab =  "Rank", ylab = "Exit time for person",
     main = "Outflow Representation Randomized Case")

# Now I will make a plot of the data for all different cases
# Finding average exit time for person i, i = 0,..,72. Room1
# Deleting observations that are 0 for last 2 persons
names(room1twod) =  c("exit_pp", "exit_group", "rank", "nr_collisions")
y1 = room1twod[-c(seq(74, 748+75, by = 75), seq(75, 749+75, by = 75)), ]
y1 = y1[y1[,1]>0, ]
y1 = summarise_at(group_by(y1, rank), vars(exit_pp), mean)
y1 = y1$exit_pp
x1 = 0:72
# Plotting both curves in one figure
plot(x1, y1, col = "Green", type = "l", ylim = c(0,100), xlab =  "Rank", ylab = "Exit time for person",
     main = "Outflow Representation Two Doors Case")

# Now I will make a plot of the data for all different cases
# Finding average exit time for person i, i = 0,..,72. Room1
# Deleting observations that are 0 for last 2 persons
names(room1vis) =  c("exit_pp", "exit_group", "rank", "nr_collisions")
y1 = room1vis[-c(seq(74, 748+75, by = 75), seq(75, 749+75, by = 75)), ]
y1 = y1[y1[,1]>0, ]
y1 = summarise_at(group_by(y1, rank), vars(exit_pp), mean)
y1 = y1$exit_pp
x1 = 0:72
# Plotting both curves in one figure
plot(x1, y1, col = "Green", type = "l", ylim = c(0,100), xlab =  "Rank", ylab = "Exit time for person",
     main = "Outflow Representation Bad Visibility")


