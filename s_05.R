
##=============================================================================
##
## This code was written in RStudio 2022.07.1+554 "Spotted Wakerobin"
## release. Functions and package compatibility may be subject to
## change.
##
##=============================================================================

## Libraries
library(foreign)
library(readr)
library(lavaan)
library(semPlot)
library(psych)
library(apaTables)
library(memisc)
library(car)
library(readxl)
library(knitr)
library(ggpubr)
library(e1071)
library(effsize)
library(ltm)
library(mice)
library(naniar)
library(finalfit)
library(writexl)
library(semTools)
library(MASS)
library(simsem)

Data <- read_excel("Dataset.xlsx", col_names = TRUE)
## Let's first check if there are missing data in the
## data set.
miss_var_summary(Data)
prop_miss(Data)
n_miss(Data)
prop_miss(Data)
Data %>% is.na() %>% colSums()

## It is recommended to handle missing data by; 
## 1) Remove all the incomplete respondents
## 2) Impute the missing values using MICE
## 3) Impute the missing values using machine
##    learning (DataWig). 
##
## For this analysis, the MICE (Multivariate Imputation by
## Chained Equations) was utilized to impute missing values.
## See Azur et al. (2011) "Multiple imputation by chained 
## equations: what is it and how does it work?" for more
## documentation. 
mcar_test(Data)
init  = mice(Data, maxit = 0)
meth  = init$method
predM = init$predictorMatrix
Data_imputed = mice(Data, 
                    method = meth, 
                    predictorMatrix = predM, 
                    m = 500, 
                    MaxNWts = 1000000)
Data_imputed = complete(Data_imputed)
write_xlsx(Data_imputed, "C:\\Users\\mjo101/Desktop/Dataset.xlsx")
## Reload the new data set to make sure everything is fine.
Data <- read_excel("Dataset.xlsx", col_names = TRUE)
percent(Data$s_1) ## Gender.
percent(Data$s_2) ## Age.

##=============================================================================
##
## Descriptive statistics
##
##=============================================================================

describe(Data$BeiAuSu)    
describe(Data$Competence) 
describe(Data$AutonomySup)
describe(Data$IntrMot)    
describe(Data$ExtrinsMot) 
describe(Data$Intentions) 

## Cronbach's alphas
AS_alpha <- data.frame(Data$s_33, Data$s_34, Data$s_35,
                       Data$s_36, Data$s_37, Data$s_38)
  cronbach.alpha(AS_alpha) 
CO_alpha <- data.frame(Data$s_67, Data$s_68, Data$s_69,
                       Data$s_70, Data$s_158)
  cronbach.alpha(CO_alpha) 
IM_alpha  <- data.frame(Data$s_169, Data$s_170, Data$s_171)
  cronbach.alpha(IM_alpha) 
EM_alpha  <- data.frame(Data$s_172, Data$s_173, Data$s_174)
  cronbach.alpha(EM_alpha) 
BAS_alpha <- data.frame(Data$s_44, Data$s_45, Data$s_48)
  cronbach.alpha(BAS_alpha) 
TI_alpha  <- data.frame(Data$s_146, Data$s_147, Data$s_148,
                        Data$s_160)
  cronbach.alpha(TI_alpha) 

## Correlations of the main variables
corr <- data.frame(Data$IntrMot, Data$ExtrinsMot, Data$Compe, Data$AutonomySup,
                   Data$BeiAuSu, Data$Intentions)
corr.test(corr)
apa.cor.table(corr, filename = "Table1.doc", table.number = 1,
              show.conf.interval = TRUE, landscape = TRUE)

##=============================================================================
##
## Confirmatory factor analysis
##
## We're using the cfa() function to verify
## the factor structure of the variables. 
##
##=============================================================================

CFA_full <- "
  AuSp =~ s_33 + s_34 + s_35 + s_36 + s_37 + s_38
  IM   =~ s_169 + s_170 + s_171 
  EM   =~ s_172 + s_173 + s_174
  INT  =~ s_146 + s_148 + s_149 + s_147 + s_160
  COMP =~ s_67 + s_68 + s_69 + s_158
  BAS  =~ s_44 + s_45 + s_48 
  "
fit_CFA_full <- cfa(model = CFA_full, data = Data, missing = "ML") 
summary(fit_CFA_full, standardized = TRUE, estimates = TRUE, header = TRUE, 
        ci = TRUE, fit.measures = TRUE)
fitmeasures(fit_CFA_full)

##=============================================================================
##
## Structural Equation Modeling
##
##=============================================================================

## Before we run a full SEM, let's assess the 
## null model.
model_0 <- '
  AuSp =~ s_33 + s_34 + s_35 + s_36 + s_37 + s_38 
  IM   =~ s_169 + s_170 + s_171 
  EM   =~ s_172 + s_173 + s_174
  INT  =~ s_146 + s_148 + s_149 + s_147 + s_160
  COMP =~ s_67 + s_68 + s_69 + s_158
  BAS  =~ s_44 + s_45 + s_48 
  '
fit_model_0 <- sem(model_0, data = Data)
fitmeasures(fit_model_0, c("cfi","tli", "rmsea", "srmr"))
summary(fit_model_0, fit.measures = TRUE, standardized = TRUE)
## CIs for SRMR
fit_model_sem_boot <- sem(model_0, data = Data, se = "bootstrap", 
                          bootstrap = 1000)
boot_srmr <- bootstrapLavaan(fit_model_sem, R = 1000, 
                             FUN = function(x) fitMeasures(x)["srmr"])
quantile(boot_srmr, probs = c(0.05, 0.95), na.rm = TRUE)
## Let's proceed with the full SEM model. 
model_sem <- '
  AuSp =~ s_33 + s_34 + s_35 + s_36 + s_37 + s_38
  IM   =~ s_169 + s_170 + s_171 
  EM   =~ s_172 + s_173 + s_174
  INT  =~ s_146 + s_148 + s_149 + s_147 + s_160
  COMP =~ s_67 + s_68 + s_69 + s_158
  BAS  =~ s_44 + s_45 + s_48 
  IM   ~  a*AuSp + g*COMP
  EM   ~  c*AuSp + e*COMP
  BAS  ~  b*IM + d*EM 
  INT  ~  h*IM + f*EM 
  indirect_effect_1 := a * b
  indirect_effect_2 := a * h
  indirect_effect_3 := c * d
  indirect_effect_4 := c * f
  indirect_effect_5 := g * b
  indirect_effect_6 := g * h
  indirect_effect_7 := e * d
  indirect_effect_8 := e * f
  '
fit_model_sem <- sem(model_sem, data = Data)
fitmeasures(fit_model_sem, c("cfi","tli", "rmsea", "srmr"))
summary(fit_model_sem, fit.measures = TRUE, standardized = TRUE)
## CIs for SRMR
fit_model_sem_boot <- sem(model_sem, data = Data, se = "bootstrap", 
                          bootstrap = 1000)
boot_srmr <- bootstrapLavaan(fit_model_sem, R = 1000, 
                             FUN = function(x) fitMeasures(x)["srmr"])
quantile(boot_srmr, probs = c(0.05, 0.95), na.rm = TRUE)

##=============================================================================
##
## Post hoc power analysis - Monte-Carlo
##
##=============================================================================

fit <- sem(model_sem, data = Data)
param_estimates <- parameterEstimates(fit)
sim_model <- model.lavaan(fit)
## Define the simulation parameters
power_analysis <- sim(nRep = 1000, model = sim_model, n = nrow(Data))
## View the summary of the power analysis
summary(power_analysis)

##=============================================================================
##
## END
##
##=============================================================================
