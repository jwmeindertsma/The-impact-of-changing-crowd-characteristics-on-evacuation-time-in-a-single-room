# In this document I compare the means of the experiments with differenten places of the triangle
# I try to draw a conclusion in which place is the most optimal place given the data
# I used 10 experiments per case and made the customers start on the same location in experiment i for 
# the different places of the triangle. i = 1,2,3,4,5
# Since my sample is quite small and non-normal (too many experiments takes to much time) is will use a non-paramteric tests 
# the triangle start at position 464, I shifted it to the door untill 470 (using my scourse of Frank and Dorso)
# First I load the data
d464 = read.table("room2_st12pxl464")
d466 = read.table("room2_st12pxl466")
d468 = read.table("room2_st12pxl468")
d470 = read.table("room2_st12pxl470")

# Now I compute the mean exit time in every case
mean(d464[,2])
mean(d466[,2])
mean(d468[,2])
mean(d470[,2])

# Now since I suspect the 2th case presents the optimal case I test if it has a smaller mean than the other means
library(BSDA)
SIGN.test(unique(d466[,2]), unique(d464[,2]), alternative = "less")
SIGN.test(unique(d466[,2]), unique(d468[,2]), alternative =  "less")
SIGN.test(unique(d466[,2]), unique(d470[,2]), alternative = "less")
# Conclusion: the vertical wall of the triangle should be at x-coordinate 666.