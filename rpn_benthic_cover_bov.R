require(tidyverse)
require(plyr)
require(betareg)
require(lmtest)
require(glmmTMB)
require(boot)
require(emmeans)
require(brms)
require(mvtnorm)
require(nlme)
require(rstan)
require(VGAM)
require(Rmisc)

bov <- read.csv('bov2.csv')

rpn_2016_main2 <- read.csv("rpn_2016_main2.csv")

rpn_2016_main.cover_bov <-
rpn_2016_main2 %>%
mutate(
  bov = case_when(
    location == "ana" & depth == "sh" ~ "0.9623988", 
    location == "ana" & depth == "md" ~ "0.5947166",
    location == "ana" & depth == "dp" ~ "0.3737202",
    location == "mtt" & depth == "sh" ~ "1.0448322",
    location == "mtt" & depth == "md" ~ "0.7100925",
    location == "mtt" & depth == "dp" ~ "0.5009428",
    location == "man" & depth == "sh" ~ "1.0448322",
    location == "man" & depth == "md" ~ "0.7100925",
    location == "man" & depth == "dp" ~ "0.5009428",
    location == "se"  & depth == "sh" ~ "1.3414435",
    location == "se"  & depth == "md" ~ "0.9116692", 
    location == "se"  & depth == "dp" ~ "0.6415056"
  ) 
    ) %>%
    mutate_at(vars(bov), as.numeric)

write_csv(rpn_2016_main.cover_bov, "rpn_2016_main.cover_bov.csv")
rpn_2016_main.cover_bov <- read.csv("rpn_2016_main.cover_bov.csv")

rpn_2016_main.sum1 <- summarySE(data = rpn_2016_main.cover_bov, measurevar = "cover",
                                groupvars = c("group", "location", "depth")
                                )

write_csv(rpn_2016_main.sum1, "rpn_2016_main.sum1.csv")

rpn_2016_main.sum1 <- read.csv("rpn_2016_main.sum1.csv") %>%
  as_tibble() %>%
  mutate_at(vars(group, location, depth), factor) %>%
  select(group, location, depth, cover) %>%
  pivot_wider(names_from = group, values_from = c(cover)) %>%
  ungroup() %>%
  add_column(site = c(
    "ana_dp",
    "ana_md",
    "ana_sh",
    "man_dp",
    "man_md",
    "man_sh",
    "mtt_dp",
    "mtt_md",
    "mtt_sh",
    "se_dp",
    "se_md",
    "se_sh"
    )
  ) 

#%>%
#  column_to_rownames(var = "site")

write_csv(rpn_2016_main.sum1, "rpn_2016_main.sum1.csv")


#this is using the betareg function within the package betareg in R

plob.bm4 <- betareg(plob.scaled ~ bov, data = rpn_2016_main.plob_scaled, 
                    link = c("logit"), link.phi = NULL,
                   type = c("ML"))

summary(plob.bm4)

# allowing for the phi parameter to among years

plob.bm5  <- betareg(plob.scaled ~ bov | bov, data = rpn_2016_main.plob_scaled, 
                     link = c("logit"), link.phi = NULL,
                   type = c("ML"))

# For comparison this applies a logit-transformation to the empirical proportions and
# then uses a standard linear regression model.

mod.lmlogit <- lm(Sago.logit ~ yrunit, data = dat.BDW)

#this is code for applying a naive linear model applied to the raw proportions as done
#in the simulation study

mod.lmraw <- lm(prop.STPE15.t ~ yrunit, data = dat.BDW)

#Demonstration on how to extract AIC from beta regression models
aic.betamodel1 <- AIC(plob.bm4)
aic.betamodel2 <- AIC(plob.bm5)

# plob

rpn_2016_main.plob <-
  rpn_2016_main.cover_bov %>%
  filter(group == "plob")

# linear regression

plob.lm <- lm(cover ~ bov, data = rpn_2016_main.plob)

par(mfrow = c(2, 2))
plot(plob.lm)

summary(plob.lm)

# Generalized linear model

plob.glm1 <- glm(cbind(successes, failures) ~ bov, 
                     family = binomial(link = "logit"), data = rpn_2016_main.plob)

par(mfrow = c(2, 2))
plot(plob.glm1)

summary(plob.glm1)

# `Anova` function from the *car* package
Anova(plob.glm1, type = "III") # Type III because...


plob.glm1_qu <- glm(cbind(successes, failures) ~ bov, 
                      family = quasibinomial(link = "logit"), rpn_2016_main.plob)

summary(plob.glm1_qu)

# `Anova` function from the *car* package
Anova(plob.glm1_qu, type = "III") # Type III because...

plob.nlme <- lme(cover ~ bov, random = ~1|photo,
                   data = rpn_2016_main.plob,
                   method = "REML")

summary(plob.nlme)

anova.lme(plob.nlme,
          type = "sequential",
          adjustSigma = FALSE)

# Test the significance of the random effect in the mixed effects model

# In order to the test the significance of the random effect from our model (photo), 
# we can fit a new model with only the fixed effects from the model.  
# For this we use the gls function in the nlme package.  
# We then compare the two models with the anova fuction.  
# Note that the p-value does not agree with p-value from the Handbook, 
# because the technique is different, though in this case the conclusion is the same.  
# As a general precaution, if your models are fit with “REML” 
# (restricted maximum likelihood) estimation, then you should compare only models 
# with the same fixed effects.  If you need to compare models with different fixed 
# effects, use “ML” as the estimation method for all models.

plob.model_fixed = gls(cover ~ bov,
                         data = rpn_2016_main.plob,
                         method = "REML")

anova(plob.nlme,
      plob.model_fixed)

# Checking assumptions of the model

hist(residuals(ana_sh.nlme2),
     col = "darkgray")

plot(fitted(plob.nlme),
     residuals(plob.nlme))

plot(plob.nlme)

# Using the aov function for a nested anova

# The aov function in the native stats package allows you to specify an error 
# component to the model.  When formulating this model in R, the correct error is Rat, not Tech/Rat (Rat within Tech) as used in the SAS example.  The SAS model will tolerate Rat or Rat(Tech).

# The summary of the aov will produce the correct test for Tech.  
# The test for Rat can be performed by manually calculating the p-value 
# for the F-test using the output for Error:Rat and Error:Within.

# See the rattlesnake example in the Two-way anova chapter for designating an 
# error term in a repeated-measures model.

plob.fit = aov(cover ~ bov + Error(photo), data = rpn_2016_main.plob)
summary(plob.fit)

# Using Mean Sq and Df values to get p-value for H = Tech and Error = Rat

pf(q = 0.07093/0.00000,
   df1 = 6,
   df2 = 53,
   lower.tail = FALSE)

### Note: This is same test as summary(ana_sh.fit)

# Using Mean Sq and Df values to get p-value for H = photo and Error = Within

pf(q = 0.00000/0.013,
   df1 = 53,
   df2 = 191,
   lower.tail = F)

# Post-hoc comparison of means with Tukey
# The aov function with an Error component produces an object of aovlist type, 
# which unfortunately isn’t handled by many post-hoc testing functions.  
# However, in the TukeyC package, you can specify a model and error term.  
# For unbalanced data, the dispersion parameter may need to be modified.

tuk = TukeyC(ana_sh6,
             model = 'cover  ~ group + Error(photo)',
             error = 'photo',
             which = 'group',
             fl1 = 1,
             sig.level = 0.05)

summary(tuk)

# A very similar analysis can be conducted using the library `nlme` for 
# mixed effects modeling.

plob.lme_1 <- lme(cover ~ bov, random = ~ 1 | photo, data = rpn_2016_main.plob)

anova(plob.lme_1)

# **Did I do this right?**

# plob_2016_lme.null <- lme(plob ~ 1, random = ~ 1 | tran, data = plob_2016)
# lrtest(plob_2016_lme.1, plob_2016_lme.2, plob_2016_lme.null)

## Beta regression with pooled observations

# We now turn to the beta regression model, that models the response variable as being 
# generated from a beta distribution (i.e. that is bounded at 0 and 1).

# Given that the observations of percent (coral) cover are based on replicate photos 
# (i.e., similar to quadrats) nested within depths (i.e., similar to patches), 
# we simplify the initial analysis by averaging the observations within each depth 
# (i.e., like a patch).

# We begin by attempting to fit a beta regression model, with `plob` as the response, 
# and `loc` as the categorical predictor.

plob.betareg_1 <- betareg(cover ~ bov, data = rpn_2016_main.plob)

summary(plob.betareg_1)

#As is evident from the error message, `betareg` will not accept values of 0 and 1 in the 
# response variable.

# There are two possible solutions here, rescaling the data to remove 0s and 1s, or fitting 
# zero-inflated models. We start here with the rescaling solution. A suggested rescaling 
# equation is:

#  $$ x^*_{i} = \frac{x_i(n-1)+0.5}{n} $$

# Where $x^*_i$ is the transformation of $x_i$ and $n$ is the total number of observations 
# in the dataset.

# For convenience we define this as a custom function `tranform01` and apply it to the 
# dataset:

transform01 <- function(x) {
  (x * (length(x) - 1) + 0.5) / (length(x))
}

plob.scaled <- transform01(rpn_2016_main.plob$cover)

rpn_2016_main.plob_scaled <-
  rpn_2016_main.plob %>%
  mutate(plob.scaled = transform01(cover))

# From Damgaard (2019) - supplementary material 0002.
# This paper assumed that recorded data represents the number of times a pin hit the 
# species of interest with a known number of pins that arise with...

# The `VGAM` package uses **rho** as the spatial aggregation parameter we refer to as
# $\delta$ . A logistic regression for binomial counts is an option, but a check for 
# over-dispersion should be done as empirical plant
# cover data are subject to spatial aggregation that may manifest in overdispersed 
# binomial counts (object named: mod.glm).
# Alternatively, a logit-transformed response can be used in linear regression options 
# (object named: mod.lm). For some
# situations, the assumptions for the linear regression models are violated and they 
# should be checked as we show below. Data
# may display non-constant variance, outliers, etc and just using a transformation is 
# insufficient.

plob.mod_betabinom <- vglm(cover ~ bov, betabinomial(zero = "rho"),
                             data = rpn_2016_main.plob, trace = TRUE)

# Error in eval(binomialff()@initialize) : 
# response values 'y' must be 0 or 1

# With this scaled data we can now successfully fit the model. 
# And test its significance relative to a null model that assumes no effect of the location 
# treatment. For reference we also fit a classical ANOVA model assuming normally distributed 
# errors using `lm`.

plob.bm1 <- betareg(plob.scaled ~ bov, data = rpn_2016_main.plob_scaled)
plob.bmnull <- betareg(plob.scaled  ~ 1, data = rpn_2016_main.plob_scaled)
plob.lm1 <- lm(plob.scaled ~ bov, data = rpn_2016_main.plob_scaled)

lrtest(plob.bm1, plob.bmnull, plob.lm1) #Error in (function (classes, fdef, mtable)  : 
# unable to find an inherited method for function ‘lrtest’ for signature ‘"betareg"’

lrtest(plob.bmnull, plob.lm1)

AIC(plob.bm1, plob.lm1, plob.bmnull)

# ana_sh.bmnull <- betareg(cover.scaled  ~ 1, data = ana_sh7)
# plob_2016.bmnull <- betareg(PLOB.scaled ~ 1, data = plob_2016)

# ana_sh.bm1 <- betareg(cover.scaled ~ group, data = ana_sh7)
# plob_2016.bm1 <- betareg(PLOB.scaled ~ loc + depth, data = plob_2016)
# plob_2016.bm2 <- betareg(PLOB.scaled ~ loc * depth, data = plob_2016)

# ana_sh.lm1 <- lm(cover.scaled  ~ group, data = ana_sh7)
# plob_2016.lm1 <- lm(PLOB.scaled ~ loc + depth, data = plob_2016)
# plob_2016.lm2 <- lm(PLOB.scaled ~ loc * depth, data = plob_2016)

summary(plob.bm1)
summary(plob.bmnull)
summary(plob.lm1)

# According to the likelihood-ratio test there is a significant difference between the 
# null model and the treatment model. 

# The AIC analysis supports this conclusion, but also highlights the improved model fit with 
# beta regression relative to normal ANOVA (`lm1`). 
# From this initial analysis we would tentatively conclude that using beta regression 
# improves our ability to model the cover.

# It is useful to plot the predictions derived from the model and compare them to the observed 
# data. 


# First we define two new functions to allow us to use the `dbeta` and `rbeta` 
# functions with the $\mu$ and $\phi$ parameterization.

dbeta2 <- function(X, mu, phi, ...) {
  dbeta(X, shape1 = mu * phi, shape2 = (1 - mu) * phi, ...)
}

rbeta2 <- function(N, mu, phi, ...) {
  rbeta(N, shape1 = mu * phi, shape2 = (1 - mu) * phi, ...)
}

# With this function we can plot the distributions corresponding to the MLE parameters for 
# each location (i.e., treatment) and depth :

# extract coefficients of beta regression model
plob_coefs.bm1 <- coef(plob.bm1)
plob_coefs.bm1

# create vector spanning the transformed 0-1 interval

plob_n.bm1 <- length(fitted(plob.bm1))
plob_x.range <- seq(0.5/plob_n.bm1 , 1 - 0.5/plob_n.bm1, length.out = 200)
plob_x.range.bt <- (plob_x.range * plob_n.bm1 - 0.5)/(plob_n.bm1-1)

# 
plot(plob_x.range.bt, dbeta2(plob_x.range, inv.logit(plob_coefs.bm1["(Intercept)"] 
                                                         + plob_coefs.bm1[2]), plob_coefs.bm1["(phi)"]),
     type = "l", 
     lty = 1,
     ylab = "Probability density", xlab = "Proportion cover",
     ylim = c(0, 10), 
     lwd = 2, 
     col = "red")

# northwest
# lines(plob_2016_x.range.bt, dbeta2(plob_2016_x.range, inv.logit(plob_2016_coefs.bm2["(Intercept)"] + plob_2016_coefs.bm2[3]), plob_2016_coefs.bm2["(phi)"]), col = "blue", lwd = 2)

# west
# lines(plob_2016_x.range.bt, dbeta2(plob_2016_x.range, inv.logit(plob_2016_coefs.bm2["(Intercept)"] + plob_2016_coefs.bm2[4]), plob_2016_coefs.bm2["(phi)"]), lwd = 2, col = "green")

# southeast
# lines(plob_2016_x.range.bt, dbeta2(plob_2016_x.range, inv.logit(coefs.bm2["(Intercept)"] + plob_2016_coefs.bm2[5]), plob_2016_coefs.bm2["(phi)"]), lwd = 2, col = "yellow")

rug(ana_sh7$plob[ana_sh7$group == "plob"], col = "red", lwd = 1.5, pos = 10)
rug(ana_sh7$poci[ana_sh7$group == "poci"], col = "blue", pos = 9.75, side = 3,lwd = 1.5)
#rug(plob_2016$PLOB[plob_2016$loc == "w"], col = "green", pos = 9.5, side = 3, lwd = 1.5)
#rug(plob_2016$PLOB[plob_2016$loc == "se"], pos = 9.25, col = "yellow", side = 3, lwd = 1.5)

legend("topright", lwd = 2, lty = c(2, 1, 1, 1), col = c("red", "blue", "green", "yellow"), legend = c("n", "nw", "w", "se"), bty = "n")

# We have added the original observations (patch-level means) as ticks of the appropriate color, 
# and back-transformed the densities to allow fair visual comparisons between the fitted distributions and the original data using:

# $$ x_{i} = \frac{x^*_in-0.5}{(n-1)} $$

# Note that the vertical positioning of the dots is merely to prevent overplotting.

# From this plot we can see that the model does a reasonable job of fitting beta distributions to each of the treatment levels, but that perhaps the variance of the [control group - not this study] is somewhat overestimated by the model. And that likewise, the variance of the [other three groups - not this study] appears to me somewhat underestimated. This can be confirmed with a residual plot, using residuals calculated relative to their predicted variance.

plot(resid(plob.bm1) ~ fitted(plob.bm1))

#The spread of the standardized residuals is strongly related to the fitted values, 
#suggesting that variance is not being adequately modeled. 
#This observation suggests the possible utility of allowing for the precision 
#parameter $\phi$ to vary between treatment groups. The following section will 
#show extension of the beta regression model to allow for this.

### Variable precision $\phi$

# We can repeat the above analysis using a model that allows $\phi$ to vary with predictors. 

# This is achieved by adding a second part to the right hand side of the formula, separated with the `|` symbol. 

# All covariates to the right of this `|` symbol will be used to model $\phi$. 

# Note that they do not have to be the same covariates used to model $\mu$ (specified to the left of the `|`).

plob.bm3 <- betareg(plob.scaled ~ bov | photo, data = rpn_2016_main.plob_scaled)
summary(plob.bm3)

#From the `Coefficients` table we see that the estimate for $\mu$ are the following: 
  
# `inv.logit(...)` = `...` = ...\% plob cover 

# Moreover, the estimates of $\mu$ for the other 3 treatments are each significantly higher.
# From the `Phi coefficients` table we can see that the maximum likelihood estimate [**Estimate**] 
# of the precision is highest at the **North** location (i.e., treatment) and is reduced 
# significantly relative to this baseline in each of other locations (i.e., treatments).

# In other words, the model fit confirms our impression from the previous two graphs that a fixed $\phi$ 
# model overestimates variance in the **North** location, and underestimates it in the other three locations ->
# **IS THIS TRUE?**.

# We can use likelihood-ratio tests to compare the new model to the fixed - $\phi$ and null models.

lrtest(plob.bm2, plob.bm3)
lrtest(plob.bmnull, plob.bm2, plob.bm3)

# The likelihood ratio tests indicate that the model with varying $\phi$ is significantly better than both the 
# previous fixed $\phi$ model, and the null model. 

# In this case the conclusion is that including the model for $\phi$ led to a better fitting model than both 
# the fixed $\phi$ model and the null model.

# It is possible to apply post-hoc tests to identify which pairwise contrasts of locations (i.e., treatments) 
# and depth levels are significant.

par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(4, 4, 0.2, 0.2))
plot(residuals(plob.bm3) ~ fitted(plob.bm3))

rpn_2016_main.plob <- read.csv("rpn_2016_main.plob.csv")

rpn_2016_main.plob <-
  rpn_2016_main.plob %>%
  select(-X)

rpn_2016_main.plob <-
  rpn_2016_main.plob %>%
  add_column(photo_id = "p") %>%
  rowid_to_column("id") 
  
rpn_2016_main.plob <-
  rpn_2016_main.plob %>%  
  unite("p_id", photo_id, id)

write.csv(rpn_2016_main.plob, "rpn_2016_main.plob.csv")

# Pocillopora

rpn_2016_main.poci <-
  rpn_2016_main.cover_bov %>%
  filter(group == "poci")

rpn_2016_main.poci <-
  rpn_2016_main.poci %>%
  add_column(photo_id = "p") %>%
  rowid_to_column("id") 

rpn_2016_main.poci <-
  rpn_2016_main.poci %>%  
  unite("p_id", photo_id, id)

write.csv(rpn_2016_main.poci, "rpn_2016_main.poci.csv")

# Macroalgae

rpn_2016_main.macro <-
  rpn_2016_main.cover_bov %>%
  filter(group == "macro")

rpn_2016_main.macro <-
  rpn_2016_main.macro %>%
  add_column(photo_id = "p") %>%
  rowid_to_column("id") 

rpn_2016_main.macro <-
  rpn_2016_main.macro %>%  
  unite("p_id", photo_id, id)

write.csv(rpn_2016_main.macro, "rpn_2016_main.macro.csv")

# Turf

rpn_2016_main.turf <-
  rpn_2016_main.cover_bov %>%
  filter(group == "turf")

rpn_2016_main.turf <-
  rpn_2016_main.turf %>%
  add_column(photo_id = "p") %>%
  rowid_to_column("id") 

rpn_2016_main.turf <-
  rpn_2016_main.turf %>%  
  unite("p_id", photo_id, id)

write.csv(rpn_2016_main.turf, "rpn_2016_main.turf.csv")

# Abiotic

rpn_2016_main.abiotic <-
  rpn_2016_main.cover_bov %>%
  filter(group == "abiotic")

rpn_2016_main.abiotic <-
  rpn_2016_main.abiotic %>%
  add_column(photo_id = "p") %>%
  rowid_to_column("id") 

rpn_2016_main.abiotic <-
  rpn_2016_main.abiotic %>%  
  unite("p_id", photo_id, id)

write.csv(rpn_2016_main.abiotic, "rpn_2016_main.abiotic.csv")

# CCA

rpn_2016_main.cca  <-
  rpn_2016_main.cover_bov %>%
  filter(group == "cca")

rpn_2016_main.cca <-
  rpn_2016_main.cca %>%
  add_column(photo_id = "p") %>%
  rowid_to_column("id") 

rpn_2016_main.cca <-
  rpn_2016_main.cca %>%  
  unite("p_id", photo_id, id)

write.csv(rpn_2016_main.cca, "rpn_2016_main.cca.csv")

library(vegan)
data(dune)
data(dune.env)
dune.env2 <- dune.env
dune.env2$Moisture = as.numeric(dune.env$Moisture)










