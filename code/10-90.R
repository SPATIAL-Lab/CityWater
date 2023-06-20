# Let's recalculate stuff from 10-90th percentile instead of SD

#note, diff calculates difference
diff(c(10,1))

quantile(tapData$d18O, c(0.1, 0.9)) 

diff(quantile(tapData$d18O, c(0.1, 0.9)))

# okay but I don't need the '90%' report. That's where names = F comes in. And it can be absolute just in case. 
abs(diff(quantile(tapData$d18O, c(0.1, 0.9), names = F)))

# datasummary.csv needs to be changed in CW4 before any other changes can be made