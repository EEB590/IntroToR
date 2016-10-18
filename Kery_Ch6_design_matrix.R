###############################################################
#        Chapter 6: Essentials of linear models               #
###############################################################

### 6.3. Deterministic part of linear models: Linear predictor and design matrices. Make a dataset (very simple- only 6 rows!)
mass <- c(6, 8, 5, 7, 9, 11)
pop <- factor(c(1,1,2,2,3,3))
region <- factor(c(1,1,1,1,2,2))
hab <- factor(c(1,2,3,1,2,3))
svl <- c(40, 45, 39, 50, 52, 57)

#just to show this as a dataframe- below, we'll use individual variables, not dataframe version. 
snake<-as.data.frame(cbind(mass, pop, region,hab,svl))

### 6.3.1. The model of the mean (null model)
lm(mass ~ 1)
summary(lm(mass~1))
mean(mass)

model.matrix(mass~1)

### 6.3.2. t-Test
#effects parameterization
mod1<-lm(mass ~ region) #test whether mass varies by region
model.matrix(mod1) # R default is to use treatment contrasts, which give you an "effects parameterization".  Region 1 is the reference level, and effect of region 2 shows how it differs from baseline (region 1).  
summary(mod1)

#means parameterization
mod2<-lm(mass~region-1) #suppress the intercept to get the "means parameterization". This 
model.matrix(mod2) 
summary(mod2)


### 6.3.3. Simple linear regression
#effects parameterization (R default)
mod1 <- lm(mass ~ svl)
model.matrix(mod1)
summary(mod1) #note that this suggests that a snake of 0 length has a mss of -5.6. This is a meaningless intercept

#means parameterization
mod2 <- lm(mass~svl-1)
model.matrix(mod2)
summary(mod2) #note that the summary tables are different because we changed the intercept to force it through 0 ("suppress the intercept")- this is a different model than model1. Think carefulyl about whether it makes sense to force the intercept through 0. 

### 6.3.4. One-way analysis of variance (one-way ANOVA)
#effects parameterization (R default)
mod1 <- lm(mass ~ pop)
model.matrix(mod1)
summary(mod1)

#means parameterization
mod2 <- lm(mass~pop-1)	
model.matrix(mod2)
summary(mod2)

### 6.3.5. Two-way analysis of variance (two-way ANOVA)
#effects parameterization
mod1 <- lm(mass ~ region + hab)
model.matrix(mod1)
summary(mod1a)

mod1a <- lm(mass ~ region * hab)
model.matrix(mod1a)
summary(mod1a)

#means parameterization
mod2 <- lm(mass ~ region * hab-1-region-hab)
model.matrix(mod2)
summary(mod2)

### 6.3.6. Analysis of covariance (ANCOVA)
mod1add <- lm(mass ~ pop + svl)			# Additive model
mod1int <- lm(mass ~ pop * svl)			# Interactive model
lm(mass ~ pop + svl + pop:svl) 	# Same, R’s way of specifying the interaction term

#Effects parameterization
model.matrix(mod1add)	# Additive model
model.matrix(mod1int)	# Interactive model

#Means parameterization
mod2add <- lm(mass ~ pop + svl-1)			# Additive model
mod2int <- lm(mass ~ pop * svl-1-svl)			# Interactive model

model.matrix(mod2add)	# Additive model
model.matrix(mod2int) # Interactive model

#graph results, showing actual data as circles, and model fit as lines, with a color for each population
fm <- lm(mass ~ pop + svl)		# Refit model
plot(svl, mass, col = c(rep("red", 2), rep("blue", 2), rep("green", 2)))
abline(fm$coef[1], fm$coef[4], col = "red")
abline(fm$coef[1]+ fm$coef[2], fm$coef[4], col = "blue")
abline(fm$coef[1]+ fm$coef[3], fm$coef[4], col = "green")

#same for interaction model
fm <- lm(mass ~ pop * svl)		# Refit model
plot(svl, mass, col = c(rep("red", 2), rep("blue", 2), rep("green", 2)))
abline(fm$coef[1], fm$coef[4], col = "red")
abline(fm$coef[1]+ fm$coef[2], fm$coef[4] + fm$coef[5], col = "blue")
abline(fm$coef[1]+ fm$coef[3], fm$coef[4] + fm$coef[6], col = "green")
