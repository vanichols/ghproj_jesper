# use the data jesper uses in the paper
# make it so it will be easy to play with

library(tidyverse)

rm(list = ls())

# inputs:

# 0. max yield possible ---------------------------------------------------

max_yield <- 6


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
j_wctl <- 100 - j_wd


# 4. crop soil cover at each intensity (% of no cover) -----------------------------------------
# - jesper's values of crop soil cover at each intensity
# - in reality this would be estimated in the field, he modeled it
# - the way he models it is weird, it is circular and includes unnecessary steps
# - but I guess I'll just do it his way
# - leaf cover = L0 * exp(-b*I) 
# - 1 (low selectivity) means it is a floppy crop, 2 (high) means it is an upright crop

#--a low selectivity crop
j_cr_bL <- 0.28 #-a crop sensitivity param (jesper calls it resistance, but higher means more leaf coverage)
j_cr_L0L <- 0.8

#--a high selectivity crop
j_cr_bH <- 0.1 #-crop sensitivity parameter (see note above, that paper seems like garbage)
j_cr_L0H <- 0.55

j_crL <- (j_cr_L0L - (j_cr_L0L * exp(-j_cr_bL * j_int)))/j_cr_L0L * 100
j_crH <- (j_cr_L0H - (j_cr_L0H * exp(-j_cr_bH * j_int)))/j_cr_L0H * 100

#--then he calculates a ratio for some reason, but ignores the 0/0 issue
j_cr_ratio <- c(0, (j_crH/j_crL)[-1])

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

#--he sets this as the yield loss at high selectivity
j_wfylH <- 100 - exp(-j_wfyl_c * j_int)*100
j_wfylH_abs <- j_wfylH/100 * max_yield

#--he multiplies by the ratio of the crop soil coverages at a given intensity
#--this doesn't make sense to me, at low selectivity the crop damage yield losses should be higher than at high select
j_wfylL <- j_wfylH * j_cr_ratio
j_wfylL_abs <- j_wfylL/100 * max_yield

#--shouldn't this depend on how much the crop was covered by the soil?
#--that isn't part of his equation
#--I guess indirectly through crop soil cover's relationship with intensity

# 6. weed-driven yield loss (%) ----------------------------------------------
# estimated using Cousens 
# here it is as interpreted by Jesper 
# he says yield as a % of max at a given weed density is:
# = (1 - (I*W1)/(100*(1+(I*W1)/A)) where W1 is the weed density
# - the weed density input would have variation
# - The assumed parameters would vary by weed community
jc_A <- 55 #-not sure what this represents or how much it would vary
jc_IA <- 0.2 #-not sure what this represents, this is for a highly competitive community
jc_IG <- 0.04 #-a low competitive weed community

j_wyA_abs <- max_yield * (1 - ( (jc_IA * j_wd) / (100*(1 + ( (jc_IA*j_wd)/jc_A ))) ))
j_wyG_abs <- max_yield * (1 - ( (jc_IG * j_wd) / (100*(1 + ( (jc_IG*j_wd)/jc_A ))) ))

j_wylA <- ( (jc_IA * j_wd) / (100*(1 + ( (jc_IA*j_wd)/jc_A ))) ) * 100
j_wylG <- ( (jc_IG * j_wd) / (100*(1 + ( (jc_IG*j_wd)/jc_A ))) ) * 100

#

# total yield losses at each intensity (%) --------------------------------

#--highly selective crop
j_ytot_HA <- 100 - j_wfylH - j_wylA
j_ytot_HG <- 100 - j_wfylH - j_wylG

j_ytot_HA_abs <- j_wyA_abs - j_wfylH_abs
j_ytot_HG_abs <- j_wyG_abs - j_wfylH_abs


#--low selective crop
j_ytot_LA <- 100 - j_wfylL - j_wylA
j_ytot_LG <- 100 - j_wfylL - j_wylG

j_ytot_LA_abs <- j_wyA_abs - j_wfylL_abs
j_ytot_LG_abs <- j_wyG_abs - j_wfylL_abs

#--take all of these and calc optimum soil cover using
#  a polynomial fit or something? unclear from his paper
#  versus ANOVA to compare each of the intensities

d_H <- 
  tibble(Harrow_inten = j_int,
         weedden = j_wd,
       cropsoilcover = j_crH,
       agg_weeds = j_ytot_HA_abs,
       gen_weeds = j_ytot_HG_abs) %>% 
  mutate(cropselectivity = "high")

d_L <- 
  tibble(Harrow_inten = j_int,
         weedden = j_wd,
         cropsoilcover = j_crL,
         agg_weeds = j_ytot_LA_abs,
         gen_weeds = j_ytot_LG_abs) %>% 
  mutate(cropselectivity = "low")


#--grouped by crop selectiveness       
d_H %>% 
  bind_rows(d_L) %>%
  pivot_longer(agg_weeds:gen_weeds, names_to = "weed_aggressiveness", values_to = "yield")  %>%
  group_by(cropselectivity, weed_aggressiveness) %>% 
  mutate(maxval = max(yield),
         maxval = ifelse(maxval == yield, maxval, NA)) %>%
  ggplot(aes(cropsoilcover, yield)) + 
  geom_point(aes(y = maxval, color = weed_aggressiveness), pch = 16) + 
  geom_line(aes(color = weed_aggressiveness)) + 
  facet_grid(.~cropselectivity) + 
  scale_y_continuous(limits = c(5, 6))

#--grouped by weed aggressiveness, which is what he does bc the crop soil cover varies in his x axis
d_H %>% 
  bind_rows(d_L) %>%
  pivot_longer(agg_weeds:gen_weeds, names_to = "weed_aggressiveness", values_to = "yield")  %>%
  group_by(cropselectivity, weed_aggressiveness) %>% 
  mutate(maxval = max(yield),
         maxval = ifelse(maxval == yield, maxval, NA)) %>%
  ggplot(aes(cropsoilcover, yield)) + 
  geom_point(aes(y = maxval, color = cropselectivity), pch = 16) + 
  geom_line(aes(color = cropselectivity)) + 
  facet_grid(.~weed_aggressiveness) + 
  scale_y_continuous(limits = c(5, 6)) +
  theme(legend.position = "bottom")


