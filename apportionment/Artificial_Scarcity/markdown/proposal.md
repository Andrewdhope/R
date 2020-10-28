### Proposal  
#### Interpretation of Article I  
##### The Typo  
From Madison, to the House, to the Senate, to the final version.  
Only the final version uses *more*.  
Using more sets up a mathematical condradiction.  
They were busy, and probably didn't totally get the algorithm.  
Good quote from Melancton Smith.  
[^[2]](#references)
##### Extending the Algorithm  
*Article the first* is an apportionment algorithm. It describes an iterative process to increase the ratio of constituents to representatives. Beginning with 30,000 constituents per representative, the ratio increases to 40,000:1 and 50,000:1 as the membership of the House crosses 100-member thresholds. It is a clever approach that expands the House more gradually than a strictly proportional relationship.  

The algorithm is explicitly written to address the case of up to 50,000 constituents per representative and no further. But the pattern established in the text of Article I is fairly clear, and it can be applied to larger and larger ratios.  

The analysis presented below relies on an extrapolation of the original algorithm. After every 100 member threshold, the ratio increases by 10,000 constituents. In this extrapolated version, the pattern continues without a maximum size for the House.  


	uncappedApportionment <- function(population, initial = 30000, stepval = 100) {  
		seat_limit <- 0  		
		  while (TRUE) {
			# if your population requires more than stepval representatives...
			if ((population/initial)>=stepval) {
				# add the stepval to the overall seat_limit
				seat_limit <- seat_limit+stepval
				# reduce the population by the amount of people 'handled' by this step of the apportionment
				population <- population-(initial*stepval)
			}
			else {
				# we have crossed the last stepval threshold, just need to add the remainder
				seat_limit <- seat_limit + ceiling(population/initial)
				break # exit while loop
			}
			# increase initial ratio before next iteration
			initial <- initial+10000
		  }
		seat_limit
	}

