library(dplyr)
library(tidyr)
library(stringr)
fg <- read.csv("guides-export-default.csv")
head(fg)
fg$category<-as.character(fg$category)
unique(fg$category)
#change column names
names(fg)<-c("guide_title", "guide_no", "page_no", "country", "state","category","authors", "date_created", "year_publisehd", "language")

########################################
#Creating a new column, Category
########################################

#plants - if "category" contains a word "plants", then stamp with "plants" in the new category 
fg2<-fg%>%mutate(plant_cat = ifelse(grepl("Plants", category), "Plants", "")) %>%
  mutate(bird_cat = ifelse(grepl("Birds", category), "Birds", ""))%>%
  mutate(mammal_cat = ifelse(grepl("Mammals", category), "Mammals", ""))%>%      
  mutate(insect_cat = ifelse(grepl("Insects", category), "Insects", ""))%>%
  mutate(fish_cat = ifelse(grepl("Fishes", category), "Fishes", ""))%>%
  mutate(herp_cat = ifelse(grepl("Amphibians|Reptiles", category), "Herp",
                          ifelse(grepl("Plants|Birds|Mammals|Insects|Fishes|Amphibians|Reptiles", category),"", "Other")))
  
#gather category columns
fg3<-fg2 %>% gather(key = "category_type", value="Category",plant_cat, bird_cat, mammal_cat, insect_cat, fish_cat, herp_cat)%>%
   filter(Category!="")#delete when "Category" is blank
nrow(fg3)#N = 1027  
nrow(fg)#N = 1000     

#Double check to make sure that the gathering of hte columns worked proprly 
count<- rowSums((fg2[-(1:10)]!=""))#count the numbers of non-empty rows in columns betwen plant_cat and herp_cat
sum(count)#N=1027 - the same value as above! 

########################################
#Creating a new column, Country2
########################################
#trim white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
fg3$country<-trim(fg3$country)

#plants - if "category" contains a word "Brazil", then stamp with "Brazil" in the new country column
fg4<-fg3%>%mutate(Brazil_country = ifelse(grepl("Brazil", country), "Brazil", "")) %>%
  mutate(Ecuador_country = ifelse(grepl("Ecuador", country), "Ecuador", ""))%>%
  mutate(Colombia_country = ifelse(grepl("Colombia", country), "Colombia", ""))%>%      
  mutate(Peru_country = ifelse(grepl("Peru", country), "Peru", ""))%>%
  mutate(Lao_country = ifelse(grepl("Lao People's Democratic Republic", country), "Lao People's Democratic Republic", ""))%>%
  mutate(Venezuela_country = ifelse(grepl("Venezuela", country), "Venezuela",""))%>%
  mutate(SriLanka_country = ifelse(grepl("Sri Lanka", country), "Sri Lanka", ""))%>%
  mutate(Palau_country = ifelse(grepl("Republic of Palau", country), "Republic of Palau", ""))%>%
  mutate(Mozambique_country = ifelse(grepl("Mozambique", country), "Mozambique", ""))%>%
  mutate(Bolivia_country = ifelse(grepl("Bolivia", country), "Bolivia", ""))%>%
  mutate(Belize_country = ifelse(grepl("Belize", country), "Belize", ""))%>%
  mutate(Guyana_country = ifelse(grepl("Guyana", country), "Guyana", ""))%>%
  mutate(Argentina_country = ifelse(grepl("Argentina", country), "Argentina", ""))%>%
  mutate(USA_country = ifelse(grepl("USA", country), "USA", ""))%>%
  mutate(Gabon_country = ifelse(grepl("Gabon", country), "Gabon", ""))%>%
  mutate(Puerto_country = ifelse(grepl("Puerto Rico", country), "Puerto Rico", ""))%>%
  mutate(Panama_country = ifelse(grepl("Panama", country), "Panama", ""))%>%
  mutate(Costa_country = ifelse(grepl("Costa Rica", country), "Costa Rica", ""))%>%
  mutate(Mexico_country = ifelse(grepl("Mexic", country), "Mexic", ""))%>%
  mutate(Madagascar_country = ifelse(grepl("Madagascar", country), "Madagascar", ""))%>%
  mutate(Paraguay_country = ifelse(grepl("Paraguay", country), "Paraguay", ""))%>%
  mutate(Thailand_country = ifelse(grepl("Thailand", country), "Thailand", ""))%>%
  mutate(Greenland_country = ifelse(grepl("Greenland", country), "Greenland", ""))%>%
  mutate(Guatemala_country = ifelse(grepl("Guatemala", country), "Guatemala", ""))%>%
  mutate(India_country = ifelse(grepl("India", country), "India", ""))%>%
  mutate(Honduras_country = ifelse(grepl("Honduras", country), "Honduras", ""))%>%
  mutate(Suriname_country = ifelse(grepl("Suriname", country), "Suriname", ""))%>%
  mutate(FrGuina_country = ifelse(grepl("French Guiana", country), "French Guiana", ""))%>%
  mutate(Nicaragua_country = ifelse(grepl("Nicaragua", country), "Nicaragua", ""))%>%
  mutate(ElSalvador_country = ifelse(grepl("El Salvado", country), "El Salvado", ""))%>%
  mutate(Tanzania_country = ifelse(grepl("Tanzania", country), "Tanzania", ""))%>%
  mutate(Indonesia_country = ifelse(grepl("Indonesia", country), "Indonesia", ""))%>%
  mutate(China_country = ifelse(grepl("China", country), "China", ""))%>%
  mutate(Philippines_country = ifelse(grepl("Philippines", country), "Philippines", ""))%>%
  mutate(Namibia_country = ifelse(grepl("Namibia", country), "Namibia", ""))%>%
  mutate(Botswana_country = ifelse(grepl("Botswana", country), "Botswana", ""))%>%
  mutate(Kenya_country = ifelse(grepl("Kenya", country), "Kenya", ""))%>%
  mutate(Ethiopia_country = ifelse(grepl("Ethiopia", country), "Ethiopia", ""))%>%
  mutate(Malaysia_country = ifelse(grepl("Malaysia", country), "Malaysia", ""))%>%
  mutate(Dominica_country = ifelse(grepl("Dominica", country), "Dominica", ""))%>%
  mutate(Uzbekistan_country = ifelse(grepl("Uzbekistan", country), "Uzbekistan",
    ifelse(grepl("Brazil|Ecuador|Colombia|Peru|Lao People's Democratic Republic|Venezuela|Sri Lanka|epublic of Palau|Mozambique|Bolivia|elize|Guyana|Argentina|USA|Gabon|Argentina|Puerto Rico|Panama|Costa Rica|Mexico|Madagascar|Paraguay|Thailand|Greenland|Guatemala|India|Hoduras|Tanzania|Suriname|French Guiana|Nicaragua|Belize|El Salvador|Honduras|Costa Rica|Indonesia|Chile|Cuba|Costa Rica|China|Philippines|Namibia|Botswana|Kenya|Ethiopia|Dominica|Malaysia|Uzbekistan", country),"", "Other")))
  

#gather country columns
fg5<-fg4 %>% gather(key = "countries", value="Countries",Brazil_country, Ecuador_country, Colombia_country, Peru_country, Lao_country,
                    Venezuela_country,SriLanka_country,Palau_country,Mozambique_country, Bolivia_country,Belize_country,Guyana_country,
                    Argentina_country,USA_country,Gabon_country,Puerto_country,Panama_country,Costa_country,Mexico_country,Madagascar_country,
                    Paraguay_country,Thailand_country,Greenland_country,Guatemala_country,India_country,Honduras_country, Suriname_country,
                    FrGuina_country,Nicaragua_country,ElSalvador_country,Tanzania_country,Indonesia_country,China_country,Philippines_country,Namibia_country,
                    Botswana_country,Kenya_country, Ethiopia_country,Malaysia_country,Dominica_country,Uzbekistan_country)%>%
  filter(Countries!="")%>%#delete when "Category" is blank
  select(-category_type, -countries)

nrow(fg5)#N = 1070  




#write.csv(fg5, "FG_Category_Counry_cleaned_3.12.2019.csv")
