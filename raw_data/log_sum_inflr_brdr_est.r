######################################################################################333

### 2/4/2022 - 2/6/2022 (just figured out that I needed to do the same for the sunflowers at SIMP)
### I have copied and pasted log_sum_inflr.r to now transform the 
### flwr.resource.row_cover_brdr_estimate that included the estimates I made for the border 
### flowers in the farms that we did not measure over the 3 visits. 

### In this script I create a new data frame that has the
### total inflorescence grouped by genus, site and visit.
### I add an additional columns that have the total cut flowers vs. border flowers
### and a column that just has total asteraceae flowers per site and visit.  
### I then make a log10 transformed column for all three summed columns and 
### save it as a csv file in the data sub folder. 
### I will be trying to transpose this data frame in order to add it to the crithidia data
### the final csv is stored in the data sub folder. 


library(vegan)#general eco stats functions
library(MASS)#glm.nb model comes from this package.
library(ggplot2)#this package has pretty plots #cheatsheets, print
library(car)#Anova () 
library(dplyr)#piping code for data visualiation and organization #cheatsheets, print
library(emmeans)#emmeans aka ls-means, #https://aosmith.rbind.io/2019/03/25/
require(here)

#calling in data
data <- read.csv(here("raw_data", "flr.resource.row_cover_brdr_estimate.csv"))
#looking at column names
colnames(data)
#summary of data
summary(data)

unique(data$genus)
#I need to filter out the non-asteraceae flowers and the "" (empty columns) from the data frame 

data<-data%>%
  filter(genus!="cucumis", 
         genus!="clover", 
         genus!="", 
         genus!="citrillus", 
         genus!="citrullus", 
         genus!="galinsoga")

##aggregated data, I added all the inflorescens/flower heads together and grouped
## by farm, visit and genus. I do not think I would like to keep some of the smaller border flowers
## I wonder how we are going to account for flower head size differences... 

tot_site_visit_flower<- data %>%
  group_by(farm, date, visit, genus, row_cover, sf_nsfcut_border, cut_border)%>%
  summarise(sum_inflr = sum(tot_inflr, na.rm = TRUE)) #%>%
filter(sum_inflr != 0 )

##log10 column of tot inflr summed by genus, site, visit
log.tot_site_visit_flower<-tot_site_visit_flower%>%
  mutate(log.sum_genus_inflr = log10(sum_inflr))

##aggregated tot inflr based on site, visit, and sf vs. nsfcut vs. border (the sun flower data)
tot_site_visit_sf<-tot_site_visit_flower%>%
  group_by(farm, visit, sf_nsfcut_border)%>%
  summarise(tot_sf_nsfcut_border_inflr = sum(sum_inflr))

##log10 column of tot inflr summed by sf_nsfcut_border, site, visit
log.tot_site_visit_sf<-tot_site_visit_sf%>%
  mutate(log.sum_sf_nsf_border_inflr = log10(tot_sf_nsfcut_border_inflr))


##aggregated tot inflr based on site, visit, and cut vs. border 
tot_site_visit_cut_border<-tot_site_visit_flower%>%
  group_by(farm, visit, cut_border)%>%
  summarise(tot_cut_or_border_inflr = sum(sum_inflr))

##log10 column of tot inflr summed by cut_border, site, visit
log.tot_site_visit_cut_border<-tot_site_visit_cut_border%>%
  mutate(log.sum_cutborder_inflr = log10(tot_cut_or_border_inflr))

## aggregated all asteraceae flowers at each sites 
tot_site_visit_ast<-tot_site_visit_flower%>%
  group_by(farm, visit)%>%
  summarise(tot_ast_inflr = sum(sum_inflr))

##log10 column of tot inflr summed site, visit
log.tot_site_visit_ast<-tot_site_visit_ast%>%
  mutate(log.sum_ast_inflr = log10(tot_ast_inflr))

#joining dataframes log.tot_site_visit_flower,log.tot_site_visit_sf

m1<-left_join(log.tot_site_visit_flower, log.tot_site_visit_sf)#, 

#joining dataframes m1,log.tot_site_visit_cut_border

m2<-left_join(m1, log.tot_site_visit_cut_border)

#joining dataframes m2,log.tot_site_visit_ast

m3<-left_join(m2,log.tot_site_visit_ast) 

colnames(m3)

#add rcon "0" asteraceae rows for, eliminate numbered column, cannot figure out.   

class(m3)
#m3<-m2%>%add_row(farm="RCON",date="7/15/21",
#  visit=1, genus=NA, row_cover=NA, cut_border="cut",
#sum_inflr=0,log.sum_genus_inflr=0, 
#tot_cut_border_inflr=0,log.sum_cutborder_inflr=0,
#tot_ast_inflr=0, log.sum_ast_inflr=0)

#this did not work going to add rcon rows by hand

# write csv and stored in the data dub folder

write.csv(m3, here("data", "log.tot_ast_flwr_brdr_est.csv"))####added by hand the rcon "0" asteraceae row 


#############################################################################################33

### Emelia list of flowers 

emelia_list<-m3%>%
  ungroup() %>%
  select(genus, cut_border) %>%
  unique()
write.csv(emelia_list, here("raw_data", "emelia_list.csv"))
