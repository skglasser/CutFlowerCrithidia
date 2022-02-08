########################################################################################

#### 2/6/22


require(here)
require(lme4)
library(performance)
library(dplyr)
library(car)

### third modeling of crithidia presences absencene and log10 counts 
### in relation to total cut, sf, nsfcut, border and asteraceae inflorescence. 
### this is data set with the estimates of border flower abundance for the farms where border
### flowers were only measured once. 


### Here are values for the different variables in the dataframe 
#### random factors (probably)

* "farm" = farm    
* "bee_ID"   = bee_ID  


#### response / dependent variables 

* "PA"  = presence/absent crithidia   
* "crit_cnt" = raw crith count    
* "log10_cnt"  = log10 crith count  


#### independent / explanatory 

* "sf"  = log10(sum all sunflower flr inflr)   
* "nsfcut"  =  log10(sum all non sunflower cut flr inflr)        
* "border" = log10(sum all border flr inflr)  
* "cut"  =  log10(sum all cut flr inflr)
* "log.sum_ast_inflr" = log10(sum all asteraceae flr inflr)


#### covariates / other explanatory variables 

* "sp" = bombus sp.
* "Wing_MM" = wing size  
* "j_date" = julian date
* "visit"   = visit 

crit_ast <- read.csv(here("data", "crit_ast_v1_brd_est.csv"))
crit_ast<-crit_ast[,-1] # get rid of that wierd number column
#visit is ordered factor
crit_ast$visit <- factor(crit_ast$visit, 
                         levels = c("1", "2", "3"))
crit_ast$Date.COLLECTED<-as.Date(crit_ast$Date.COLLECTED, format = "%m/%d/%Y")

# other variables are just normal factors
cols <- c("sp", "PA", "farm", "bee_ID")
crit_ast<-crit_ast %>%
  mutate_each_(funs(factor(.)),cols)

# dataframe w/ just impatiens
crit_imp<-crit_ast%>%
  filter(sp=="imp")%>%
  filter(PA!="NA")%>%
filter(Wing_MM!="NA")

colnames(crit_imp)
is.na(crit_imp)

# full model 

full_model <- glmer(PA ~ cut + 
                      log.sum_ast_inflr + Date.COLLECTED +
                      Wing_MM +
                        (1|farm), family = binomial, 
                        data = crit_imp, 
                      control =glmerControl(optimizer = "bobyqa", 
                                            optCtrl=list(maxfun=100000)))
summary(full_model)
Anova(full_model)
plot(full_model)



