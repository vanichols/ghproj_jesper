# use the data jesper uses in the paper
# make it so it will be easy to play with

library(tidyverse)

# inputs:


# 1. replicates --------------------------------------------------------------
# start with 1
j_rep <- 1


# 2. intensities -------------------------------------------------------------
# 9 intensities (make so can remove some, change range?)
j_int <- seq(0, 4, by = 0.5)
# not sure how to subset this, these values aren't actually used


# 3. weed density at each intensity (% of potential) -------------------------------------------
# - jesper's values of weed density as proportion of natural weed density
# - in reality this will be measured, he modeled it
# - so this would have replicates with variation between them
#--not sure how much the relationship would vary or what determines that
#--I guess the weed community and how it responds to harrowing
j_wd_d <- 1.88 #-no idea what this represents
j_wd <- exp(-j_wd_d*(log(j_int+1))) * 100


# 4. crop soil cover at each intensity (% of no cover) -----------------------------------------
# - jesper's values of crop soil cover at each intensity
# - in reality this would be estimated in the field, he modeled it
# - the way he models it is weird, it is circular and includes unnecessary steps
# - but I guess I'll just do it his way
# - leaf cover = L0 * exp(-b*I) 
# - 1 (low selectivity) means it is a floppy crop, 2 (high) means it is an upright crop

#--a low selectivity crop
j_cr_b1 <- 0.28 #-a crop resist param
j_cr_L01 <- 0.8

#--a high selectivity crop
j_cr_b2 <- 0.1 #-resis param
j_cr_L02 <- 0.55

j_cr1 <- (j_cr_L01 - (j_cr_L01 * exp(-j_cr_b1 * j_int)))/j_cr_L01 * 100
j_cr2 <- (j_cr_L02 - (j_cr_L02 * exp(-j_cr_b2 * j_int)))/j_cr_L02 * 100

#--then he calculates a ratio for some reason, but ignores the 0/0 issue
j_cr_ratio <- c(0, (j_cr2/j_cr1)[-1])

# - this would have replicates with variation between them
# - He again uses an exponential relationship, assuming two crop selectivities
# - where is this used again?
# - oh it isn't, he uses intensity


# 5. intensity-driven crop yield loss (%) ------------------------------------
# - Weed free crop yield loss (%) at each soil cover 
# - He determined this using an equation
# - In actuality there would be measurement error of yields and variation btwn reps
# - Not sure how much the equation parameters would vary
j_wfyl_c <- 0.025 #-not sure what this represents

j_wfyl <- 100 - exp(-j_wfyl_c * j_int)*100


# 6. weed-driven yield loss (%) ----------------------------------------------
# estimated using Cousens 
# here it is as interpreted by Jesper 
# he says yield as a % of max at a given weed density is:
# = (1 - (I*W1)/(100*(1+(I*W1)/A)) where W1 is the weed density
# - the weed density input would have variation
# - The assumed parameters would vary by weed community
jc_A <- 55 #-not sure what this represents or how much it would vary
jc_I1 <- 0.2 #-not sure what this represents, this is for a highly competitive community
jc_I2 <- 0.04 #-a low competitive weed community

j_wyl1 <- ( (jc_I1 * j_wd) / (100*(1 + ( (jc_I1*j_wd)/jc_A ))) ) * 100
j_wyl2 <- ( (jc_I2 * j_wd) / (100*(1 + ( (jc_I2*j_wd)/jc_A ))) ) * 100

#

# total yield losses at each intensity (%) --------------------------------

j_ytot <- 100 - j_wfyl - j_wyl

#--take all of these and calc optimum soil cover using
#  a polynomial fit or something? unclear from his paper
#  versus ANOVA to compare each of the intensities

tibble(Harrow_inten = j_int,
       yield_PctOfTot = j_ytot) %>% 
  ggplot



#--cousens curve note
#--jesper uses a calculation that looks a little 
# different compared to what he presents