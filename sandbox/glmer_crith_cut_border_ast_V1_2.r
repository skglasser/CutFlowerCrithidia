########################################################################################

#### 2/2/22


require(here)
require(lme4)
library(performance)
library(dplyr)
library(car)

### second modeling of crithidia presences absencene and log10 counts 
### in relation to total cut, sf, nsfcut, border and asteraceae infloresences. 


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

crit_ast <- read.csv(here("data", "crit_ast_v1.csv"))
crit_ast<-crit_ast[,-1] # get rid of that wierd number column
#visit is ordered factor
crit_ast$visit <- factor(crit_ast$visit, 
                         levels = c("1", "2", "3"))

#other variables are just normal factors
cols <- c("sp", "PA", "farm", "bee_ID")
crit_ast<-crit_ast %>%
  mutate_each_(funs(factor(.)),cols)

# dataframe w/ just impatiens
crit_imp<-crit_ast%>%
  filter(sp=="imp")

### These are the models that I used with Rachel

# #total count model, negative binomial #https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/
#model.nb <- glm.nb(Count~Farm*Species,data=b)
#Anova(model.nb) 
#summary(model.nb)#sig dif group variance based on main effect of farm and main effect of species but not their interaction.
#plot(model.nb)
#AIC(model.nb)#checking the fit model, does not actually look that great


### Here is the full model that Rosemary used in the Sunflower paper 

#crith.prev.full = glmer(crith.pa ~ site.visit*log.sf + site.visit*log.ast2 + 
#site.visit*log.fl.dens + max.H + body.size + 
# (1|site.id/col.id), family = binomial, 
#data = dat1.red, control =glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
#check_collinearity(crith.prev.full)
#summary(crith.prev.full)
#Anova(crith.prev.full)
#exp(fixef(crith.prev.full))
#AIC(crith.prev.full)
#plot(allEffects(crith.prev.full))

# I will try the glmer binomial model, where crithidia presence vs. absence is depedent on the site visit,
# cut flower, border flower and total asteraceae flowers. 

crith_PA_V1 = glmer(PA ~ visit*cut + 
                      visit*log.sum_ast_inflr + 
                      Wing_MM +
                      (1|farm), family = binomial, 
                    data = crit_imp, 
                    control =glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
summary(crith_PA_V1)
Anova(crith_PA_V1)
plot(crith_PA_V1)

crith_PA_V1_j = glmer(PA ~ j_date*cut + 
                      j_date*log.sum_ast_inflr + 
                      Wing_MM +
                      (1|farm), family = binomial, 
                    data = crit_imp, 
                    control =glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
summary(crith_PA_V1_j)
Anova(crith_PA_V1_j)
plot(crith_PA_V1_j)

#weird error message
crith_PA_V1_1 = glmer(PA ~ cut + 
                        (1|farm), family = binomial, 
                      data = crit_imp, 
                      control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))

summary(crith_PA_V1_1)
Anova(crith_PA_V1_1)
plot(crith_PA_V1_1)

crith_PA_V1_tot_ast = glmer(PA ~ log.sum_ast_inflr + 
                        (1|farm), family = binomial, 
                      data = crit_imp, 
                      control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))

summary(crith_PA_V1_tot_ast)
Anova(crith_PA_V1_tot_ast)
plot(crith_PA_V1_tot_ast)

crith_PA_V1_farm = glmer(PA ~ farm + (1|bee_ID)
                        , family = binomial, 
                      data = crit_imp, 
                      control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))

summary(crith_PA_V1_farm)
Anova(crith_PA_V1_farm)
plot(crith_PA_V1_farm)

crith_PA_V1_farm = glmer(PA ~ cut*j_date + (1|farm)
                         , family = binomial, 
                         data = crit_imp, 
                         control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))

summary(crith_PA_V1_farm)
Anova(crith_PA_V1_farm)
plot(crith_PA_V1_farm)