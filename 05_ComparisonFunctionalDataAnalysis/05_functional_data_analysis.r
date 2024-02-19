####################################################################################################
###
### File:    05_functional_data_analysis.r
### Purpose: Create a simple Functional data analysis model to compare treatments
### Authors: Gabriel Rodrigues Palma
### Date:    04/04/23
###
####################################################################################################
# Loading packages -----
source(here('00_source.r'))

# Learning how to use functional data analysis ----
# Load the package

library(refund)
        
# Load the data

data(gasoline)
             
# Create the functional data object

bbasis = create.bspline.basis(c(900, 1700), 40)
                                           
wavelengths = 2*450:850

nir <- t(gasoline$NIR)

gas.fd = smooth.basisPar(wavelengths, nir, bbasis)$fd

# Fit the model

gasmod = fpcr(gasoline$octane, fdobj = gas.fd, ncomp = 30)
              
# Plot the results
plot(gasmod, xlab="Wavelength")

gasmod$fitted.values

##### Using mixed models -----
data("growth")

# Fit a mixed model with random intercept and slope

# using the 'fpcr' function

model <- fpcr(height ~ age + sex + (1 + age | id), data = growth, nfold = 3)

# Print the model summary

summary(model)

## Another example -----
# Load the refund package
library(refund)
library(fda)

# Load the Berkeley Growth Study data
data(growth)

# Fit a functional regression model
fit <- fRegress(height ~ age, growth)

# Print the summary of the model
summary(fit)
