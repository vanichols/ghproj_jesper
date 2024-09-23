# use the data jesper uses in the paper
# make it so it will be easy to play with

library(tidyverse)

rm(list = ls())


# replicates --------------------------------------------------------------
# start with 1 or 2
n_reps <- 2

yield_max <- 6 #--seems arbitrary but we can find out
weeds_max <- 100 #--weeds per unit area (again, arbitrary)

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

#--assign a variation in yield msmts
csc_sd <- 0.1*yield_max

#--use intensity range and two sensitivity values to get theoretical crop soil cover
d1a <- expand_grid(tibble(int = range_int,
       L0 = 0.5),
       tibble(cropsens = c(0.25, 0.75))) %>% 
  mutate(L = L0 * exp(-cropsens * int),
         cropcov_pct = (L0-L)/L0*100) 

#--look at it
d1a %>% 
  ggplot(aes(int, L, group = cropsens)) + 
  geom_line(aes(color = as.factor(cropsens)))

#--the less sensitive crop has less coverate at the same intensity
d1a %>% 
  ggplot(aes(int, cropcov_pct, group = cropsens)) + 
  geom_line(aes(color = as.factor(cropsens)))

d1b <- 
  d1a %>%
  mutate(cropsens = ifelse(cropsens == 0.25, "Low crop sens to harr (0.25)", "High crop sens to harr (0.75)")) %>% 
  select(cropsens, int, cropcov_pct) %>% 
  arrange(cropsens, int)

#use number of replicates to create a dataset
d1 <- NULL

for (i in 1:n_reps){
  di <- 
    d1b %>% 
    mutate(rep = i) %>% 
    rowwise() %>% 
    mutate(msmt_error = rnorm(1, 0, sd = csc_sd)) %>%
    mutate(cropcov_pct_meas = cropcov_pct + msmt_error, 
           cropcov_pct_meas = ifelse(cropcov_pct == 0, 0, cropcov_pct_meas))
    
  d1 <- bind_rows(d1, di)       
  
}

d1 %>% 
  ggplot() + 
  geom_line(aes(int, cropcov_pct, group = rep), color = "black") + 
  geom_point(aes(int, cropcov_pct_meas, group = rep, color = as.factor(rep))) + 
  facet_grid(.~cropsens)


# 2. weed density at each intensity (% of potential) -------------------------------------------
# - jesper's values of weed density as proportion of natural weed density
# - in reality this will be measured, he modeled it
# W = W0 * exp (-d * ln(int + 1))
# - no idea what d represnts
#--looks like higher value means more weed ctl
#--so it is kind of 'weed sensitivity', and we hope it is higher than the crop sensitivity
#--weed control should never go above 100% though

#--assign a variation, weeds are pretty variable, I'm not sure what a fair number here is
wden_sd <- weeds_max * 0.1

d2a <- expand_grid(tibble(int = range_int,
                          W0 = weeds_max), #--100 weeds per unit area
                   tibble(weedval = c(1, 1.88, 5))) %>% 
  mutate(W = W0 * exp(-weedval * log(int + 1)),
         weedctl_pct = (W0-W)/W0*100) 

d2a %>% 
  ggplot(aes(int, weedctl_pct, group = weedval)) + 
  geom_line(aes(color = as.factor(weedval)))

d2b <- 
  d2a %>% 
  filter(weedval == 1.88)

#use number of replicates to create a dataset
d2 <- NULL

for (i in 1:n_reps){
  di <- 
    d2b %>% 
    mutate(rep = i) %>% 
    rowwise() %>% 
    mutate(msmt_error = rnorm(1, 0, sd = wden_sd)) %>%
    mutate(W_meas = W + msmt_error)
  
  d2 <- bind_rows(d2, di)       
  
}


d2 %>% 
  ggplot() + 
  geom_line(aes(int, W, group = rep), color = "black") + 
  geom_point(aes(int, W_meas, group = rep, color = as.factor(rep))) 

# 3. intensity-driven crop yield loss (%) ------------------------------------
# - Weed free crop yield loss (%) at each soil cover 
# - He determined this using an equation
# - Y = Y0 * exp(-c * int)
#- c is the crop response parameter, 0.025 is jesper's value
#--I think the weed free yield loss equation should be a function of the crop soil cover
#--let's replace 'int' with Eqn 1;
# - int = b + ln(1-CSC)

#--assign a variation, let's say yields can only be measured within 2.5% accuracy
wfyld_sd <- 0.001*yield_max

d3a <- 
  d1 %>% 
  mutate(Y0 = yield_max,
         #yldres = 0.0125,#--this can just be changed if using cropsoilcov_meas instead of 'int', 5.71 is lowest
         yldres = 0.00175) %>% 
  mutate(wfY = Y0 * exp(-yldres * cropcov_pct_meas),
         #Y2 = Y0 * exp(-yldres * int))
         wfyieldloss_pct = (Y0-wfY)/Y0*100) 


#--it is such a small range it looks linear, I guess
d3a %>% 
  ggplot(aes(cropcov_pct_meas, wfY, group = cropsens)) + 
  geom_point(aes(color = as.factor(cropsens)))

d3b <- 
  d3a 

#use number of replicates to create a dataset
d3 <- NULL


for (i in 1:n_reps){
  di <- 
    d3b %>% 
    mutate(rep = i) %>% 
    rowwise() %>% 
    mutate(msmt_error = rnorm(1, 0, sd = wfyld_sd)*Y0) %>%
    mutate(wfY_meas = wfY + msmt_error)
    
    d3 <- bind_rows(d3, di)       
  
}

#--this seems realistic
d3 %>% 
  ggplot(aes(cropcov_pct_meas)) + 
  geom_line(aes(y = wfY), color = "black") + 
  geom_point(aes(y = wfY_meas, color = as.factor(rep)), size = 2) + 
  facet_grid(cropsens~yldres)

# 4. weed-driven yield loss (%) ----------------------------------------------
# estimated using Cousens 
# here it is as interpreted by Jesper 
# he says yield as a % of max at a given weed density is:
# Y = Y0 * (1 - (I*W1)/(100*(1+(I*W1)/A)) where W1 is the weed density
# - the weed density input would have variation
# - The assumed parameters would vary by weed community

d4a <- 
  expand_grid(tibble(int = range_int, Y0 = yield_max), tibble(cropweedsens = c(0.2, 0.05))) %>%
  expand_grid(., tibble(Aval = c(25, 55, 100))) %>% #--this seems to change the 0 intercept
  left_join(d2, relationship = "many-to-many") %>%
  arrange(rep, cropweedsens, Aval) %>% 
  mutate(wY = Y0*(1 - (cropweedsens * W)/(100*(1+(cropweedsens * W)/Aval))),
         wY_meas = Y0*(1 - (cropweedsens * W_meas)/(100*(1+(cropweedsens * W_meas)/Aval)))) 

#--get a feel for the constants
d4a %>% 
  ggplot(aes(int, wY)) + 
  geom_line(aes(group = interaction(Aval, cropweedsens), color = Aval, size = cropweedsens))

d4 <-
  d4a %>% 
  mutate(cropweedsens = ifelse(cropweedsens == 0.2, "Agg weeds (0.2)", "Nice weeds (0.05)")) %>% 
  filter(Aval == 55)


# 5. total yield losses --------------------------------

#--there is something wrong here...it shouldn't be a many-to-many
d5a <- 
  d4 %>% 
  select(int, Y0, cropweedsens, rep, wY, wY_meas) %>% 
  left_join(d3 %>%
              select(int, Y0, cropsens, rep, cropcov_pct, cropcov_pct_meas, wfY, wfY_meas),
            by = c("int", "Y0", "rep"), relationship = "many-to-many") %>% 
  mutate(totyld = Y0 - (Y0 - wY) - (Y0 - wfY),
         totyld_meas = Y0 - (Y0 - wY_meas) - (Y0 - wfY_meas)) 

d5a %>% 
  ggplot() + 
  geom_point(aes(cropcov_pct, totyld, group = rep), color = "black") + 
  geom_point(aes(cropcov_pct_meas, totyld_meas, group = rep), color = "red") + 
  facet_grid(cropweedsens ~ cropsens)


# 6. fit a quadratic ------------------------------------------------------

d6 <- 
  d5a %>% 
  filter(grepl("Agg", cropweedsens),
         grepl("Low", cropsens)) %>% 
  select(cropcov_pct, totyld) %>% 
  mutate(cropcov_pct2 = cropcov_pct*cropcov_pct)

d6

data$hours2 <- data$hours^2

#fit quadratic regression model
quadraticModel <- lm(totyld ~ cropcov_pct + cropcov_pct2, data=d6)

#view model summary
summary(quadraticModel)

cropcovVals <- seq(0, 70, 0.1)
totyldPredict <- predict(quadraticModel,
                            list(cropcov_pct = cropcovVals, 
                                 cropcov_pct2 = cropcovVals*cropcovVals))

tibble(cropcov_pct = cropcovVals,
       totyldPred = totyldPredict) %>% 
  left_join(d6 %>% mutate(cropcov_pct = round(cropcov_pct, 1))) %>% 
  ggplot() +
  geom_point(aes(cropcov_pct, totyld)) +
  geom_line(aes(cropcov_pct, totyldPred))
