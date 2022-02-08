###############################################################################

### 1/20/2022

require(here)
require(lme4)
library(performance)
library(dplyr)
library(car)

### first modeling of crithidia presences absencene and log10 counts 
### in relation to total cut, border and asteraceae infloresences. 


### Here are values for the different variables in the dataframe 
# "farm" = farm     "visit"   = visit      "bee_ID"   = bee_ID      "sp" = bombus sp.
# "PA"  = presence/absent crithidia    "crit_cnt" = raw crith count    
# "log10_cnt"  = log10 crith count  "Wing_MM" = wing size     
#"border"  = log10(sum all border flr inflr)   "cut"  =  log10(sum all cut flr inflr)        
# "log.sum_ast_inflr" = log10(sum all asteraceae flr inflr)

o <- read.csv(here("data", "crit_ast_v1.csv"))
o<-o[,-1] # get rid of that wierd number column
str(o)
class(o$PA)
o$PA  <- as.factor(o$PA)

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

crith_PA_V1 = glmer(PA ~ visit*cut + visit*border + 
                          visit*log.sum_ast_inflr + 
                          Wing_MM +
                          (1|farm), family = binomial, 
                          data = o, 
                          control =glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))
summary(crith_PA_V1)
Anova(crith_PA_V1)
plot(crith_PA_V1)

#weird error message

crith_PA_V1_1 = glmer(PA ~ cut + 
                      (1|farm), family = binomial, 
                    data = o, 
                    control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)))

summary(crith_PA_V1_1)
