# use the data jesper uses in the paper
# make it so it will be easy to play with

library(tidyverse)

rm(list = ls())


# replicates --------------------------------------------------------------
# start with 1
n_reps <- 2


# intensities -------------------------------------------------------------
# 9 intensities (make so can remove some, change range?)
range_int <- seq(0, 4, by = 0.5)
# these values aren't actually used in analyses
# they are just to assure you get a range in crop soil coverages, which will be a measured value

# 1. crop soil cover at each intensity (% of no cover) -----------------------------------------
#--in theory this follows an exponential relationship
#--however, intensity is not a standardized unit
# - leaf cover = L0 * exp(-b*I) 
#-L0 is leaf cover in untreated plots
#-b is 'crop resistance parameter', which doesn't actually line up
#-the term 'resistance seems like a misnomer. Higher resistance corresponds to less leaf cover at a given intensity
#--crop sensitivity seems like a better term
#-L is leaf cover at intensity I
#-b seems arbitrary, since the intensity units are arbitrary
#--is the entire paper just garbage?

#--assign a variation
csc_sd <- 2

#--use intensity range and two sensitivity values to get theoretical crop soil cover
d1a <- expand_grid(tibble(int = range_int,
       L0 = 0.5),
       tibble(cropsens = c(0.25, 0.75))) %>% 
  mutate(L = L0 * exp(-cropsens * int),
         cropsoilcov = (L0-L)/L0*100) 

#--look at it
d1a %>% 
  ggplot(aes(int, L, group = cropsens)) + 
  geom_line(aes(color = as.factor(cropsens)))

#--the less sensitive crop has less coverate at the same intensity
d1a %>% 
  ggplot(aes(int, cropsoilcov, group = cropsens)) + 
  geom_line(aes(color = as.factor(cropsens)))

d1b <- 
  d1a %>%
  mutate(cropsens = ifelse(cropsens == 0.25, "Low (0.25)", "High (0.75)")) %>% 
  select(cropsens, int, cropsoilcov) %>% 
  arrange(cropsens, int)

#use number of replicates to create a dataset
d1 <- NULL

for (i in 1:n_reps){
  di <- 
    d1b %>% 
    mutate(rep = i,
           cropsoilcov_meas = cropsoilcov + rnorm(1, 0, sd = csc_sd),
           cropsoilcov_meas = ifelse(cropsoilcov == 0, 0, cropsoilcov_meas))
    
  d1 <- bind_rows(d1, di)       
  
}

d1 %>% 
  ggplot() + 
  geom_line(aes(int, cropsoilcov, group = rep), color = "black") + 
  geom_line(aes(int, cropsoilcov_meas, group = rep), color = "red") + 
  facet_grid(.~cropsens)


# 2. weed density at each intensity (% of potential) -------------------------------------------
# - jesper's values of weed density as proportion of natural weed density
# - in reality this will be measured, he modeled it
# W = W0 * exp (-d * ln(int + 1))
# - no idea what d represnts
#--looks like higher value means more weed ctl
#--so it is kind of 'weed sensitivity', and we hope it is higher than the crop sensitivity
#--weed control should never go above 100% though

#--assign a variation, it should be higher than the crops because weeds vary more
wctl_sd <- csc_sd * 2

d2a <- expand_grid(tibble(int = range_int,
                          W0 = 100), #--100 weeds per unit area
                   tibble(weedval = c(1, 1.88, 5))) %>% 
  mutate(W = W0 * exp(-weedval * log(int + 1)),
         weedctl = (W0-W)/W0*100) 

d2a %>% 
  ggplot(aes(int, weedctl, group = weedval)) + 
  geom_line(aes(color = as.factor(weedval)))

d2b <- 
  d2a %>% 
  filter(weedval == 1.88) %>%  
  select(int, weedctl)

#use number of replicates to create a dataset
d2 <- NULL

for (i in 1:n_reps){
  di <- 
    d2b %>% 
    mutate(rep = i,
           weedctl_meas = weedctl + rnorm(1, 0, sd = wctl_sd),
           weedctl_meas = ifelse(weedctl == 0, 0, weedctl_meas))
  
  d2 <- bind_rows(d2, di)       
  
}


d2 %>% 
  ggplot() + 
  geom_line(aes(int, weedctl, group = rep), color = "black") + 
  geom_line(aes(int, weedctl_meas, group = rep), color = "red") 

# 3. intensity-driven crop yield loss (%) ------------------------------------
# - Weed free crop yield loss (%) at each soil cover 
# - He determined this using an equation
# - Y = Y0 * exp(-c * int)
#- c is the crop response parameter, 0.025 is jesper's value
#--I think the weed free yield loss equation should be a function of the crop soil cover
#--let's replace 'int' with Eqn 1;
# - int = b + ln(1-CSC)

#--assign a variation, let's say yields can only be measured within 2.5% accuracy
wfyld_sd <- 2.5

d3a <- 
  d1 %>% 
  mutate(Y0 = 6,
         #yldres = 0.0125,#--this can just be changed if using cropsoilcov_meas instead of 'int', 5.71 is lowest
         yldres = 0.00075) %>% 
  mutate(Y = Y0 * exp(-yldres * cropsoilcov_meas),
         #Y2 = Y0 * exp(-yldres * int))
         wfyieldloss = (Y0-Y)/Y0*100) 


d3a %>% 
  ggplot(aes(cropsoilcov_meas, wfyieldloss, group = cropsens)) + 
  geom_line(aes(color = as.factor(cropsens)))

d3b <- 
  d3a %>% 
  select(cropsens, int, cropsoilcov, rep, cropsoilcov_meas, wfyieldloss)

#use number of replicates to create a dataset
d3 <- NULL

for (i in 1:n_reps){
  di <- 
    d3b %>% 
    mutate(rep = i,
           wfyieldloss_meas = wfyieldloss + rnorm(1, 0, sd = wfyld_sd),
           wfyieldloss_meas = ifelse(wfyieldloss == 0, 0, wfyieldloss_meas))
  
  d3 <- bind_rows(d3, di)       
  
}

d3

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


