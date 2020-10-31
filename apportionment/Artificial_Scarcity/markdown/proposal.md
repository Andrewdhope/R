### Proposal  
#### Interpretation of Article I  
##### The Error  
The final line of Article I, approved by Congress in September 1789, contains a critical error. Here is the entirety of change as described by the Journal of the House of Representatives (1789):

> That the first article be amended by striking out the word "less," in the last place of the said first article, and inserting in lieu therreof the word "more."  

This single-word change in the final clause of Article I causes major problems. It is inconsistent with the pattern of growth laid out in the rest of the text, which aims to contiunally expand the House in response to changes in population. It is also at odds with the versions of Article I drafted sepeartely by the House and the Senate.  

Historical research published at thirty-thousand.org[^[2]](#references) chronicles the evolution of Article I, from James Madison's notebooks, to the version initially authored by the House in August 1789, to the version revised by the Senate in September 1789, and the version approved by Congress later in September 1789. The only version of Article I that limits the ratio of constituents to representatives is the final version produced by the 1789 House-Senate Conference Committee[^[4]](#references).  

The change from *less* to *more* was ordered during the House and Senate Conference Committee held from September 21 through September 24, 1789. During this committee, the Congress debated, ammended, and resolved a wide range of differences present throughout the entire Bill of Rights. They had a mere four days. This key change was ordered, but likely not fully understood.  

Possibly the most direct indication that the modified wording was indeed made in error, is that it establishes a mathematical contradiction. The final version of Article I requires that the House contain more than 200 members, **and** less than one representative per 50,000 constituents. But with certain population levels (around 8 million), that select ratio results in a number that is less than 200. The membership of the House cannot be both over 200, and under X, where X is less than 200. This contradictory state only effects the House in a relatively small set of all cases, but it does lend support to the case that the shift to *more* was not fully thought through.  

There is a strong case that this late revision was indeed an error, made without a thorough-enough analysis of the effects. The remainder of this analysis interprets the Article I algorithm with *less* in the final clause.  

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

