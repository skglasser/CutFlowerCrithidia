########################################################################################

### 2/4/2022 
### I have copied and pasted crith_ast_transpose.r to now transpose the 
### log_sum_inflr_brd_est_0values that included the estimates I made for the border 
### flowers in the farms that we did not measure over the 3 visits with the crith2022.csv

## In this file I will transpose the log.tot_ast_flwr_0values.csv data to the crith2022.csv

## this script results in a dataframe which I have named "crit_ast_v1.csv" and I will start some  preliminary models. 
## I have made a combined data set with the following values: 

# "farm" =farm     "visit"   =visit      "bee_ID"   =bee_ID      "sp" = bombus sp.
# "PA"  = presence/absent crithidia  "crit_cnt" = raw crith count     "log10_cnt"  = log10 crith count  
# "Wing_MM" = wing size      "border"  = log10(sum all border flr inflr)  
#"sf"= log10(sum all sunflower flr inflr)
#"nsfcut" = log10(sum all non sunflower cut flr inflr)
#"cut"  =  log10(sum all cut flr inflr)        
# "log.sum_ast_inflr" = log10(sum all asteraceae flr inflr)



require(here)
library(dplyr)
library(tidyverse)
library(magrittr) # change from character to factor w/ dplyr

ast <- read.csv(here("data", "log.tot_ast_flwr_brdr_est_0values.csv"))
crit <- read.csv(here("raw_data", "crith2022.csv"))

# create dataframe with totals of sunflower, non sunflower cut, cut, border and all ast. flowers,
# not each individual flower genus
colnames(ast)
df_all_ast<-ast%>%
  select(farm, visit, sf_nsfcut_border, cut_border,log.sum_sf_nsf_border_inflr, 
         log.sum_cutborder_inflr,log.sum_ast_inflr)%>%
  unique()#pair down to just the unique totals. 

# need to spread the column values sf, nsfcut cut and border so that they are in different
#columns. I will connect this with the totals columnn and create another csv. 
# for data analysis / exploration. 

#first I start with the sunflower related columns. 

df_sf<-df_all_ast%>%
  select(-log.sum_ast_inflr, -log.sum_cutborder_inflr, -cut_border)%>%
  spread(key = "sf_nsfcut_border", value = "log.sum_sf_nsf_border_inflr")

#I will do the same for the "cut_border" column.  

df_cutborder<-df_all_ast%>%
  select(-log.sum_ast_inflr,-log.sum_sf_nsf_border_inflr,-sf_nsfcut_border)%>%
  unique()%>%
  spread(key = "cut_border", value = "log.sum_cutborder_inflr")


#pull apart df_tot_ast to only have the totals 

df_tot_ast <- df_all_ast %>%
  select(farm,visit,log.sum_ast_inflr)

#combine dataframes, this is the correct format, but it double the rows.

#sf_nsfcut_border + tot_ast

df_sf_tot<-left_join(df_sf, df_tot_ast)

sf_tot_u <- df_sf_tot %>% # only select unique rows...undouble
  unique()

#cut_border + tot_ast

df_cut_border_tot<-left_join(df_cutborder, df_tot_ast)

cut_border_tot_u <- df_cut_border_tot %>% # only select unique rows...undouble
  unique()

# I want to grab just the cut column from the cut_border_tot_u and add it to the sf_tot_u 

cut_only<-cut_border_tot_u%>%
  select(farm,visit,cut)

ast_final<-left_join(sf_tot_u,cut_only)%>%
  select(farm,visit,cut,border,sf,nsfcut,log.sum_ast_inflr)#this just reorders the columns. 


#select relevant collumns from the crit2022
colnames(crit)
crit_select<-crit%>%
  select(farm, visit, Date.COLLECTED,j_date, bee_ID, sp, PA, crit_cnt, log10_cnt, Wing_MM)

#combine final cirthidia and asteraceae dataframes
crit_ast_v1<-left_join(crit_select,ast_final)

#make variables factors
#visit is ordered factor
crit_ast_v1$visit <- factor(crit_ast_v1$visit, levels = c("1", "2", "3"))

#other variables are just normal factors
cols <- c("sp", "PA", "farm", "bee_ID")
crit_ast_v1 %<>%
  mutate_each_(funs(factor(.)),cols)
#check that everything looks good
summary(crit_ast_v1)
#i am realizing that there should be some 0 instead of NAs for some of the flower data. 
#sf, border have NAs. 
crit_ast_v1 <- mutate_at(crit_ast_v1, 
                         c("sf","border"), 
                         ~replace(., is.na(.), 0))

#final dataframe
write.csv(crit_ast_v1, here("data", "crit_ast_v1_brd_est.csv"))


#########################################################################################
#this it script from when I made 2 dataframes instead of one with all the variables together. 


#this is the sf_nsfcut_border + tot_ast + crithdia 

#crit_ast_sf_v1<-left_join(crit_select,sf_tot_u)
#colnames(crit_ast_sf_v1)

#summary(crit_ast_sf_v1)
#change variables, visit, sp, PA, bee_ID and farm to factors 
#make visit an ordered factor 
crit_ast_sf_v1$visit <- factor(crit_ast_sf_v1$visit, levels = c("1", "2", "3"))
cols <- c("sp", "PA", "farm", "bee_ID")
crit_ast_sf_v1 %<>%
  mutate_each_(funs(factor(.)),cols)

#make visit an ordered factor 
o$visit <- factor(o$visit, levels = c("1", "2", "3"))


write.csv(crit_ast_sf_v1, here("data", "crit_ast_sf_v1.csv"))

#this worked... this is version 1  of the cut_border + tot_ast + crithidia

crit_ast_v1<-left_join(crit_select,cut_border_tot_u)
colnames(crit_ast_v1)

write.csv(crit_ast_v1, here("data", "crit_ast_v1.csv"))

#### wait! I think I can combine the two dataframes. 

crit_ast_v1_combine<-crit_ast_v1%>%
  select(farm,visit,bee_ID,cut)
crit_ast_v1_combine<-left_join(crit_ast_v1_combine,crit_ast_sf_v1)
