Yx2510 data tidy
================
Yi Xiao
25/11/2018

``` r
library(haven)
library(dplyr)
library(tidyverse)
library(stringi)
library(ggplot2)
library(ggmap)
library(rgeos)
library(maptools)
library(geojsonio)
library(viridis)
library(plotly)
library(ggpubr)
```

1. data import
==============

``` r
#import death data from 2004 to 2014
name = list.files(path = "./data", full.names = TRUE, pattern = "*.sas7bdat") 
cd_data =  map_df(name, read_sas)  %>%
  janitor::clean_names()
year = as.data.frame(rep(2000:2014, each = 59)) 

# add a column "year"
cd_data = cbind(year, cd_data) 
colnames(cd_data)[1] = "year"

community_district = unique(cd_data$community_district)
cd_number = c(101:112, 201:212, 301: 318, 401:414, 501:503)
community_district = as.tibble(cbind(community_district, cd_number))

cd_data = merge(cd_data, community_district , by = "community_district")



# import population data
cd_number2 = c(201:212, 301: 318, 101:112, 401:414, 501:503)
pop_data2 = read_csv("./data/New_York_City_Population_By_Community_Districts 16.09.03.csv") %>%
 janitor::clean_names() 
pop_data2 = cbind(cd_number2, pop_data2) %>%
  select(borough, cd_number2, cd_name, x2000_population, x2010_population) %>%
  rename("cd_number" = "cd_number2")
  # standardize the cd_number

  


  
# merge the two datasets
my_data = merge(pop_data2, cd_data, by = "cd_number") 
 
my_tidy_data = my_data %>% # we'll use 2000 year population data for 2000 - 2009, and 2010 year data for 2010 -2014 
  mutate(population = ifelse(year < 2010, my_data$x2000_population, my_data$x2010_population)) %>%
  select(-c(x2000_population, x2010_population)) %>% 
  mutate(cd_name = as.factor(cd_name)
         ) %>%
  select(c(cd_number:cd_name, year:population))
```

The generated dataset consists of 885 observations and 1315 columns. Each observation records information on population and death statistics in one of 59 community districts in New York City. Variables in the original dataset include sex, age, ethnicity and cause of death and their cross variable with each other. Since we are interested in cause of death in this project, we only kept cause of death and its cross information with other demographic variables. Noteworthily, the presence of a large proportion of NAs could be an issue in later steps.

Cause of death is denoted with c1 to c22, which represent Deaths due to: 1. septicemia, 2. HIV, 3. malignant neoplasms (cancer), 4. cancer of the colon, rectum, and anus, 5. cancer of the pancreas, 6. trachea, bronchus, and lung, 7. cancer of the breast (female), 8. cancer of the prostate, 9. diabetes mellitus, 10. use of or poisoning by psychoactive substance excluding alcohol and tobacco, 11. Alzheimer's disease, 12. diseases of the heart, 13. essential hypertension and hypertensive renal disease, 14. cerebrovascular diseases, 15. influenza and pneumonia, 16. chronic lower respiratory diseases, 17. chronic liver disease and cirrhosis, 18. Nephritis, Nephrotic Syndrome and Nephrosis, 19. accident except drug poisoning, 20. intentional self‐harm (suicide), 21. assault (homicide), respectively. 22. others

Race/ethnicity is denoted with 1 to 5, standing for: 1. Hispanic, 3. Asian Non‐Hispanic, 4. White Non‐Hispanic, 5. Black Non‐Hispanic,

2. tidy the data
================

#### 2.0 dealing with the missing value

``` r
missing_value = 
  my_tidy_data %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(term, num_na, everything()) %>% 
  mutate(percent = num_na/nrow(my_tidy_data)) %>% 
  filter(percent > 0.2) 
```

We filtered those variables with more than 20% of missing value and found `nrow(missing_value)` fell into this category. However, it is quite reasonable that no people from a certain age or gender group died of a certain disease in a specific year and neighbourhood and thus resulted in absence of recording. Missing data could suggest 0 death and removing them will lead to loss of valuable information. Therefore, we kept those missing value in our dataset.

#### 2.1 dataset without demographic characteristics

``` r
# we are first dealing with death infomation without accounting for other demographic characteristics
total_death_data = my_tidy_data %>%
  select(cd_number:year, x1: x22, population, total) %>%
  gather(key = "cause_of_death", value = number, x1:x22) 
```

2.2 cause of death crossed with gender
--------------------------------------

``` r
# cause of death crossed with gender
gender_death_data = my_tidy_data %>%
   select(cd_number : year, c1male :c22female) %>%
   gather (key = "cause_of_death", value = "number", c1male : c22female)   

# split "death cause/gender" into gender
  gender_death_data = gender_death_data %>% 
   mutate(gender = ifelse(str_detect(gender_death_data$cause_of_death, "female"), "female", "male"))

  #split "death cause/gender" into cause of death
gender_death_data =gender_death_data %>%
 mutate(cause_of_death = 
       substr(gender_death_data$cause_of_death,1,str_locate(gender_death_data$cause_of_death, "[0-9][a-zA-Z]")[,1])
 )  

# aggregate cd-level data into borough-level
gender_death_data2 = gender_death_data %>%
  group_by(borough, year, gender, cause_of_death) %>%
  summarise(number_cs_boro_gdr = sum(number, na.rm = TRUE))
```

2.3 dataset crossed with age
----------------------------

``` r
# cause of death crossed with age 
# gather the age and death info 
age_death_data = my_tidy_data %>% 
select(cd_number: year, c1age_1_12months:c22age_85) %>%
  gather (key = "cause_of_death", value = "number", c1age_1_12months : c22age_85) 

# split the comb variable into age and cause of death
age_death_data = age_death_data %>% 
  mutate(
    age_group = 
      substr(age_death_data$cause_of_death, str_locate(age_death_data$cause_of_death, "age")[,1] + 4, nchar(age_death_data$cause_of_death))
    ) %>%
  mutate(cause_of_death =  
           substr(age_death_data$cause_of_death, 0, (str_locate(age_death_data$cause_of_death, "age")[,1] -1)))  


# aggregate cd-wise number into borough-wise
age_death_data2 = age_death_data %>%
  group_by(borough, year, age_group, cause_of_death) %>%
  summarise(borough_age_death = sum(number, na.rm = TRUE))

# aggregate age groups
age_death_data3 = age_death_data2 %>%
  ungroup %>%
  mutate(age_group = case_when(
    !(age_group  %in% c("1_12months", "28days", "65_69", "70_74", "75_79", "80_84", "85")) ~ "Premature Death",
    age_group %in% c("65_69", "70_74", "75_79", "80_84", "85") ~ ">65 age death",
    age_group == "28days" ~ "new born death",
    age_group == "1_12months" ~ "1_12months death"
  ) )
```

2.4 dataset crossed with race
-----------------------------

``` r
# cause of death crossed with race 
race_death_data = my_tidy_data %>%
   select(cd_number:year, c11:c225) %>%
     gather (key = "cause_of_death", value = "number", c11:c225) 

race_death_data = race_death_data %>%
  mutate(race = stri_sub(race_death_data$cause_of_death, -1)) %>%
  mutate(
    cause_of_death = 
      substr(cause_of_death, start = 1, stop = nchar(race_death_data$cause_of_death)-1)
                  ) 

# aggregate cd-wise number into borough-wise
race_death_data2 = race_death_data %>%
  group_by(borough, year, race, cause_of_death )%>%
  summarise(borough_race_death = sum(number, na.rm = TRUE))
```

Now I added specific cause of death to the dataset.

``` r
replace_cd = function(df){ 
 
  df$cause_of_death[df$cause_of_death == "x1" | df$cause_of_death == "c1"] = "septicemia" 
  df$cause_of_death[df$cause_of_death == "x2" | df$cause_of_death == "c2"] = "HIV"
  df$cause_of_death[df$cause_of_death == "x3" | df$cause_of_death == "c3"] = "malignant neoplasms (cancer)"
  df$cause_of_death[df$cause_of_death == "x4" | df$cause_of_death == "c4"] = "cancer of the colon, rectum, and anus"
  df$cause_of_death[df$cause_of_death == "x5" | df$cause_of_death == "c5"] = "cancer of the pancreas"
  df$cause_of_death[df$cause_of_death == "x6" | df$cause_of_death == "c6"] = "trachea, bronchus, and lung"
  df$cause_of_death[df$cause_of_death == "x7" | df$cause_of_death == "c7"] = "cancer of the breast (female)"
  df$cause_of_death[df$cause_of_death == "x8" | df$cause_of_death == "c8"] = "cancer of the prostate"
  df$cause_of_death[df$cause_of_death == "x9" | df$cause_of_death == "c9"] = " diabetes mellitus"
  df$cause_of_death[df$cause_of_death == "x10" | df$cause_of_death == "c10"] = "psychoactive substance"
  df$cause_of_death[df$cause_of_death == "x11"  | df$cause_of_death == "c11"] = "Alzheimer's disease"
  df$cause_of_death[df$cause_of_death == "x12" | df$cause_of_death == "c12"] = " diseases of the heart"
  df$cause_of_death[df$cause_of_death == "x13" | df$cause_of_death == "c13"] = "hypertension/hypertensive renal disease"
  df$cause_of_death[df$cause_of_death == "x14" | df$cause_of_death == "c14"] = "cerebrovascular diseases"
  df$cause_of_death[df$cause_of_death == "x15" | df$cause_of_death == "c15"] = "influenza and pneumonia"
  df$cause_of_death[df$cause_of_death == "x16" | df$cause_of_death == "c16"] = "chronic lower respiratory diseases"
  df$cause_of_death[df$cause_of_death == "x17" | df$cause_of_death == "c17"] = "chronic liver disease and cirrhosis"
  df$cause_of_death[df$cause_of_death == "x18" | df$cause_of_death == "c18"] = "Nephritis, Nephrotic Syndrome and Nephrosis"
  df$cause_of_death[df$cause_of_death == "x19" | df$cause_of_death == "c19"] = "accident except drug poisoning"
  df$cause_of_death[df$cause_of_death == "x20" | df$cause_of_death == "c20"] = "intentional self‐harm (suicide)"
  df$cause_of_death[df$cause_of_death == "x21" | df$cause_of_death == "c21"] = "assault (homicide)"
  df$cause_of_death[df$cause_of_death == "x22" | df$cause_of_death == "c22"] = "others"
  df
}

total_death_data = replace_cd(total_death_data)
gender_death_data2 = replace_cd(gender_death_data2)
race_death_data2 = replace_cd(race_death_data2)
age_death_data2 = replace_cd(age_death_data2)
age_death_data3 = replace_cd(age_death_data3)
```

3. data viasualization
======================

### 3.1 death data analysis by borough, community distribution and year

#### 3.1.1 crude mortality data for the sample population

``` r
# 14-year average crude mortality rate in each community district in New York City
cd_death_rate  = total_death_data %>%
  group_by(borough, cd_name, year) %>%
  summarise(total_cd_death = sum(number, na.rm = TRUE), population = mean(population), cd_number = mean(as.numeric(cd_number))) %>%
  mutate(motality_rate = total_cd_death/population) %>%
  group_by(cd_name, borough) %>%
  summarise(average_death_rate = mean(motality_rate), cd_number = mean(cd_number)) %>%
  arrange(desc(average_death_rate))


cd_death_rate %>%
   ggplot(aes(x = reorder(cd_name,average_death_rate), y = average_death_rate, fill = borough)) +
   geom_bar(stat = "identity") +
   labs(title = "10-year average crude mortality rate in each community district in New York City",
             x = "Community District",
             y = "Average Motality Rate") +
        theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  viridis::scale_fill_viridis(discrete = TRUE) 
```

![](death_data_yx2510_files/figure-markdown_github/crude%20mortality%20rate-1.png)

3.1.2 map: crude mortality data by community district
-----------------------------------------------------

``` r
URL <- "http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nycd/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson"
fil <- "nyc_community_districts.geojson"
if (!file.exists(fil)) download.file(URL, fil)

nyc_districts = geojson_read(fil, what="sp")


nyc_districts_map = fortify(nyc_districts, region="BoroCD") # add border line 

mids = cbind.data.frame(as.data.frame(gCentroid(nyc_districts, byid=TRUE)), 
                         id=nyc_districts$BoroCD)

ny_map = ggplot() %>% +  # draw NYC map
         geom_map(data=nyc_districts_map, map=nyc_districts_map,
                    aes(x=long, y=lat, map_id=id),
                    color="#2b2b2b", size=0.15, fill=NA) + 
        geom_text(data=mids, aes(x=x, y=y, label=id), size=2) +
        coord_map() + 
        ggthemes::theme_map()
```

    ## Warning: Ignoring unknown aesthetics: x, y

``` r
par(mfrow = c(2,1))
 nyc_districts@data =  merge(nyc_districts@data, cd_death_rate, by.x = "BoroCD", by.y = "cd_number")

choro = data.frame(district=nyc_districts@data$BoroCD,  
                    average_death_rate=nyc_districts@data$average_death_rate)

cd_death_map = nyc_districts_map %>% # add color to map 
ggplot()+
geom_map(map=nyc_districts_map,
                    aes(x=long, y=lat, map_id=id),
                    color="#2b2b2b", size=0.15, fill=NA) +
geom_map(data=choro, map=nyc_districts_map,
                    aes(fill=average_death_rate, map_id=district),
                    color="#2b2b2b", size=0.15) +
scale_fill_viridis(name="Average death rate") + 
coord_map() +
ggthemes::theme_map() +
theme(legend.position=c(0.1,0.5)) +
   labs(title = "average crude mortality rate in each community district in New York City from 2000 to 2014"
            )
```

    ## Warning: Ignoring unknown aesthetics: x, y

``` r
cd_death_map
```

![](death_data_yx2510_files/figure-markdown_github/add%20map-1.png)

3.1.3 cause-specific mortality rate
-----------------------------------

``` r
# cause-specific mortality rate in each borough
specific_death_rate = total_death_data %>%
  group_by(borough, year, cause_of_death) %>%
  summarise(cause_specific_death = sum(number, na.rm = TRUE), population = sum(population)) %>% # calculate the population  in each borough and number of people died of each cause 
  mutate(cause_specific_death_rate = cause_specific_death/population) %>% # calculate cause-specific mortality rate
  group_by(borough, cause_of_death) %>% # get average death rate
  summarise(mean_cs_death_rate = mean(cause_specific_death_rate)) %>%
  arrange(borough, mean_cs_death_rate)

specific_death_rate %>%
  ggplot(aes(x = reorder(cause_of_death, mean_cs_death_rate), y = mean_cs_death_rate)) +
  geom_bar(aes(fill = cause_of_death), stat = "identity") +
  facet_grid(. ~ borough)  +
   labs(title = "14-year average cause-specific crude mortality rate in each borough",
             x = "Cause of Death",
             y = "Cause-specific Mortality Rate",
              caption = "Source: vital statistics database: 2004-2014") +
        theme(axis.text.x = element_text(angle=90, size = 8, vjust=0.6)) +
        theme(legend.position="bottom", 
              legend.key.size = unit(.1, "in")) +
    coord_flip()  +
  viridis::scale_fill_viridis(discrete = TRUE)
```

![](death_data_yx2510_files/figure-markdown_github/unnamed-chunk-7-1.png)

#### 3.2 Death data analysis by year

##### 3.2.1 motality rate in New York City from 2000 to 2014

``` r
# annual mortality rate in New York City from 2000 to 2010
year_death_rate = total_death_data %>%
  group_by(borough, year) %>%
  summarise(total_borough_death = sum(number, na.rm = TRUE), population = sum(population)/22) %>%
  mutate(motality_rate = total_borough_death/population) 
  

year_death_rate %>%
  ggplot(aes(x = year, y = motality_rate, group = borough, color = borough)) +
  geom_line() + 
  geom_point() +
  labs(x = "Year", 
         y = "Crude motality rate", 
         title = "Crude motality rate in New York City across time") + 
  viridis::scale_color_viridis(discrete = TRUE)
```

![](death_data_yx2510_files/figure-markdown_github/unnamed-chunk-8-1.png)

##### 3.2.2 leading cause of death in selected years

``` r
year_cs_death_ny = total_death_data %>%
  group_by(year, cause_of_death) %>%
  summarise(specific_death_number = sum(number, na.rm = TRUE), total_population = sum(population)) %>%
  mutate(motality_rate = specific_death_number/total_population) %>%
filter(year %in% c("2000", "2006", "2012", "2014")) %>%
  group_by(year) %>%
  top_n(n = 10, wt = motality_rate) 
  
lead_cause = merge(year_cs_death_ny, total_death_data) %>% 
  group_by(year, cause_of_death) %>%
  summarise(specific_death_number = sum(number, na.rm = TRUE), total_population = sum(population)) %>%
  mutate(motality_rate = specific_death_number/total_population)

lead_cause %>%
  filter(year == 2000) %>%
  ggplot(aes(reorder(x = cause_of_death, motality_rate), y = motality_rate)) +
  geom_bar(stat = "identity", fill = "lightblue") + 
  theme_bw() +
   theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
   labs(x = "Cause of death", 
         y = "Crude mortality rate", 
         title = "leading cause of death in 2000") +
  coord_flip()
```

![](death_data_yx2510_files/figure-markdown_github/crude%20cause-specific%20mortality%20rate%20in%20different%20year-1.png)

``` r
lead_cause %>%
  filter(year == 2006) %>%
  ggplot(aes(reorder(x = cause_of_death, motality_rate), y = motality_rate)) +
  geom_bar(stat = "identity", fill = "deeppink1") + 
  theme_bw() +
   theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
   labs(x = "Cause of death", 
         y = "Crude mortality rate", 
         title = "leading cause of death in 2006") +
  coord_flip()
```

![](death_data_yx2510_files/figure-markdown_github/crude%20cause-specific%20mortality%20rate%20in%20different%20year-2.png)

``` r
 lead_cause %>%
  filter(year == 2012) %>%
  ggplot(aes(reorder(x = cause_of_death, motality_rate), y = motality_rate)) +
  geom_bar(stat = "identity", fill = "darkolivegreen1") + 
  theme_bw() +
   theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
   labs(x = "Cause of death", 
         y = "Crude mortality rate", 
         title = "leading cause of death in 2000") +
  coord_flip()
```

![](death_data_yx2510_files/figure-markdown_github/crude%20cause-specific%20mortality%20rate%20in%20different%20year-3.png)

``` r
 lead_cause %>%
  filter(year == 2014) %>%
  ggplot(aes(reorder(x = cause_of_death, motality_rate), y = motality_rate)) +
  geom_bar(stat = "identity", fill = "red") + 
  theme_bw() +
   theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
   labs(x = "Cause of death", 
         y = "Crude mortality rate", 
         title = "leading cause of death in 2014") +
  coord_flip()
```

![](death_data_yx2510_files/figure-markdown_github/crude%20cause-specific%20mortality%20rate%20in%20different%20year-4.png)

### by year and borough

``` r
# identify the top ten death by borough
lead_death3 = merge(lead_cause, total_death_data, by = c("year", "cause_of_death")) %>%
  group_by(borough, year, cause_of_death) %>%
  summarise(br_death_number = sum(number, na.rm = TRUE), br_population = sum(population)) %>%
  mutate(br_rate = br_death_number/br_population)
  
yr00 =   
lead_death3 %>% 

  filter(year == 2000) %>%
  ggplot(aes(reorder(x = cause_of_death, br_rate), y = br_rate, fill = borough)) +
  geom_bar(stat = "identity") + 
  theme_bw() +
   theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
   labs(x = "Cause of death", 
         y = "Crude mortality rate", 
         title = "leading cause of death in 2000") +
  coord_flip()  +
  viridis::scale_fill_viridis(discrete = TRUE) 

yr06 = 
lead_death3 %>% 
filter(year == 2006) %>%
  ggplot(aes(reorder(x = cause_of_death, br_rate), y = br_rate, fill = borough)) +
  geom_bar(stat = "identity") + 
  theme_bw() +
   theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
   labs(x = "Cause of death", 
         y = "Crude mortality rate", 
         title = "leading cause of death in 2006") +
  coord_flip()  +
  viridis::scale_fill_viridis(discrete = TRUE) 

yr12 = 
lead_death3 %>% 
filter(year == 2012) %>%
  ggplot(aes(reorder(x = cause_of_death, br_rate), y = br_rate, fill = borough)) +
  geom_bar(stat = "identity") + 
  theme_bw() +
   theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
   labs(x = "Cause of death", 
         y = "Crude mortality rate", 
         title = "leading cause of death in 2012") +
  coord_flip()  +
  viridis::scale_fill_viridis(discrete = TRUE) 

yr14=
  lead_death3 %>% 
filter(year == 2014) %>%
  ggplot(aes(reorder(x = cause_of_death, br_rate), y = br_rate, fill = borough)) +
  geom_bar(stat = "identity") + 
  theme_bw() +
   theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
   labs(x = "Cause of death", 
         y = "Crude mortality rate", 
         title = "leading cause of death in 2014") +
  coord_flip()  +
  viridis::scale_fill_viridis(discrete = TRUE) 

yr00
```

![](death_data_yx2510_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
yr06
```

![](death_data_yx2510_files/figure-markdown_github/unnamed-chunk-9-2.png)

``` r
yr12
```

![](death_data_yx2510_files/figure-markdown_github/unnamed-chunk-9-3.png)

``` r
yr14
```

![](death_data_yx2510_files/figure-markdown_github/unnamed-chunk-9-4.png)

#### 3.3 by gender

``` r
# death by gender in each borough by year
gender_death_cause = gender_death_data2 %>%
  group_by(gender, cause_of_death) %>%
  summarise(average_death = sum(number_cs_boro_gdr)/11) %>%
  top_n(n = 10, wt = average_death) %>%
  arrange(gender, desc(average_death))


female = gender_death_cause %>% 
  filter(gender == "female") %>%
  ggplot(aes(x = reorder(cause_of_death, average_death), y = average_death)) +
  geom_bar(stat = "identity", fill = "lightblue") +

  labs(x = "Cause of death", 
         y = "Number of death", 
         title = "leading cause of death for female") + 
theme_bw()+
   theme(axis.text.x = element_text(angle=90, vjust=0.6))

male = gender_death_cause %>% 
  filter(gender == "male") %>%
  ggplot(aes(x = reorder(cause_of_death, average_death), y = average_death)) +
  geom_bar(stat = "identity", fill = "red" ) +
  ylim(c(0, 10000)) +
  
theme_bw() +
   theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
   labs(x = "Cause of death", 
         y = "Number of death", 
         title = "leading cause of death for male")
  

ggarrange(male, female, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
```

    ## Warning: Removed 1 rows containing missing values (position_stack).

    ## Warning: Removed 1 rows containing missing values (position_stack).

![](death_data_yx2510_files/figure-markdown_github/unnamed-chunk-10-1.png)

##### by gender and borough

``` r
gender_death_cause2 = gender_death_data2 %>%
  group_by(gender, cause_of_death, borough) %>%
  summarise(average_death_br = sum(number_cs_boro_gdr)/11) %>%
  top_n(n = 10, wt = average_death_br) %>%
  arrange(gender, desc(average_death_br))

female1 = gender_death_cause2 %>% 
  filter(gender == "female") %>%
  ggplot(aes(x = reorder(cause_of_death, average_death_br), y = average_death_br, fill = borough)) +
  geom_bar(stat = "identity") +

  labs(x = "Cause of death", 
         y = "Number of death", 
         title = "leading cause of death for female") + 
theme_bw()+
   theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  
  viridis::scale_fill_viridis(discrete = TRUE) 

male1 = gender_death_cause2 %>% 
  filter(gender == "male") %>%
  ggplot(aes(x = reorder(cause_of_death, average_death_br), y = average_death_br, fill = borough)) +
  geom_bar(stat = "identity" ) +
  ylim(c(0, 10000)) +
  
theme_bw()+
   theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
   labs(x = "Cause of death", 
         y = "Number of death", 
         title = "leading cause of death for male") +
  viridis::scale_fill_viridis(discrete = TRUE) 

ggarrange(male1, female1, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
```

    ## Warning: Removed 1 rows containing missing values (geom_bar).

    ## Warning: Removed 1 rows containing missing values (geom_bar).

![](death_data_yx2510_files/figure-markdown_github/unnamed-chunk-11-1.png) \#\#\#\# by time

``` r
gender_death_data2 %>%
   group_by(year, gender) %>%
  summarise(total_death = sum(number_cs_boro_gdr)) %>%
  ggplot(aes(x = year, y = total_death, color = gender)) +
  geom_point() +
  geom_line() + 
   labs(title = "Number of death in different gender group in New York City",
             x = "Year",
             y = "Number of death")  + 
  theme_bw()+
  viridis::scale_color_viridis(discrete = TRUE) +
   theme(axis.text.x = element_text(angle=90, vjust=0.6))
```

![](death_data_yx2510_files/figure-markdown_github/unnamed-chunk-12-1.png)

#### 3.4 by race

``` r
race_death_data2 = race_death_data2 %>%
  ungroup() %>%
  mutate(race = case_when(
    race == 1 ~ "Hispanic",
    race == 3 ~ "Asian Non-Hispanic",
    race == 4 ~ "White Non-Hispanic",
    race == 5 ~ "Black Non-hispanic"
  )) 

# identify the top leading cause in NYC


race_death_cause  = race_death_data2%>%
  group_by(race, cause_of_death) %>%
  summarise(cs_death_race = sum(borough_race_death)/11)  %>% # average yearly death from 2004 to
  top_n(n = 10, wt = cs_death_race) %>%
  arrange(race, desc(cs_death_race)) 

## plot for hispanic
race_death_cause %>% 
  filter(race == "Hispanic") %>%
  ggplot(aes(x = reorder(cause_of_death, cs_death_race), y = cs_death_race)) +
  geom_bar(stat = "identity", fill = "antiquewhite") +

  labs(x = "Cause of death", 
         y = "Number of death", 
         title = "leading cause of death for Hispanic") + 
theme_bw() +
   theme(axis.text.x = element_text(angle=90, vjust=0.6))
```

![](death_data_yx2510_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
# asian
asian = race_death_cause %>% 
  filter(race == "Asian Non-Hispanic") %>%
  ggplot(aes(x = reorder(cause_of_death, cs_death_race), y = cs_death_race)) +
  geom_bar(stat = "identity", fill = "aquamarine1" ) +
  
theme_bw() +
   theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
   labs(x = "Cause of death", 
         y = "Number of death", 
         title = "leading cause of death for Asian Non-hispanic") +
   theme(axis.text.x = element_text(angle=90, vjust=0.6))
  
  # white
white = 
  race_death_cause %>%
  filter(race == "White Non-Hispanic") %>%
  ggplot(aes(x = reorder(cause_of_death, cs_death_race) , y = cs_death_race)) +
  geom_bar(stat = "identity" , fill = "chartreuse2") + 
    theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
   labs(x = "Cause of death", 
         y = "Number of death", 
         title = "leading cause of death for white")  +
theme_bw() +
   theme(axis.text.x = element_text(angle=90, vjust=0.6))

# black
black = 
  race_death_cause %>%
  filter(race == "Black Non-hispanic") %>%
  ggplot(aes(x = reorder(cause_of_death, cs_death_race) , y = cs_death_race)) +
  geom_bar(stat = "identity", fill = "cyan2") + 
    theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
   labs(x = "Cause of death", 
         y = "Number of death", 
         title = "leading cause of death for the black") +
theme_bw() +
   theme(axis.text.x = element_text(angle=90, vjust=0.6))
```

##### by race and borough

``` r
race_death_cd = merge(race_death_data2, race_death_cause) %>%
  group_by(race, cause_of_death, borough) %>%
  summarise(race_ds_br = sum(borough_race_death)/10) 

race_death_cd %>% 
  filter(race == "Hispanic") %>%
  ggplot(aes(x = reorder(cause_of_death, race_ds_br), y = race_ds_br, fill = borough)) +
  geom_bar(stat = "identity") +

  labs(x = "Cause of death", 
         y = "Number of death", 
         title = "leading cause of death for Hispanic") + 
theme_bw() +
   theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  viridis::scale_fill_viridis(discrete = TRUE) 
```

![](death_data_yx2510_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
race_death_cd %>% 
  filter(race == "Asian Non-Hispanic") %>%
  ggplot(aes(x = reorder(cause_of_death, race_ds_br), y = race_ds_br, fill = borough)) +
  geom_bar(stat = "identity") +

  labs(x = "Cause of death", 
         y = "Number of death", 
         title = "leading cause of death for Asian") +
  theme_bw()+
   theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  viridis::scale_fill_viridis(discrete = TRUE) 
```

![](death_data_yx2510_files/figure-markdown_github/unnamed-chunk-14-2.png)

``` r
race_death_cd %>% 
  filter(race == "White Non-Hispanic") %>%
  ggplot(aes(x = reorder(cause_of_death, race_ds_br), y = race_ds_br, fill = borough)) +
  geom_bar(stat = "identity") +

  labs(x = "Cause of death", 
         y = "Number of death", 
         title = "leading cause of death for White") + 
theme_bw()+
   theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  viridis::scale_fill_viridis(discrete = TRUE) 
```

![](death_data_yx2510_files/figure-markdown_github/unnamed-chunk-14-3.png)

``` r
race_death_cd %>% 
  filter(race == "Black Non-hispanic") %>%
  ggplot(aes(x = reorder(cause_of_death, race_ds_br), y = race_ds_br, fill = borough)) +
  geom_bar(stat = "identity") +

  labs(x = "Cause of death", 
         y = "Number of death", 
         title = "leading cause of death for Black") + 
theme_bw() +
   theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  viridis::scale_fill_viridis(discrete = TRUE) 
```

![](death_data_yx2510_files/figure-markdown_github/unnamed-chunk-14-4.png)

#### by time

``` r
race_death_data2 %>%
  group_by(race, year) %>%
  summarise(total_death_yr_rc = sum(borough_race_death)) %>%
  ggplot(aes(x = year, y = total_death_yr_rc, color = race)) +
  geom_line() +
  geom_point() +
  theme_bw()
```

![](death_data_yx2510_files/figure-markdown_github/unnamed-chunk-15-1.png)

``` r
  labs(x = "Year", 
         y = "Number of death", 
         title = "Change in total number of death in each race group") + 
  viridis::scale_fill_viridis(discrete = TRUE)  
```

    ## NULL

#### 3.5 by age group

``` r
# leading cause of death in each age group
age_death_cause = age_death_data3 %>%
  group_by(age_group, cause_of_death) %>%
  summarise(av_death_age = sum(borough_age_death)/11 ) %>%
  top_n(n = 10, wt = av_death_age) %>%
  arrange(age_group, desc(av_death_age)) 

# not much useful info in the newborn and under 1 group 
age_death_cause = age_death_cause %>%
  filter(age_group == "Premature Death" | age_group == ">65 age death")

over_65 = age_death_cause %>%
  filter(age_group == ">65 age death") %>%
  ggplot(aes(x = reorder(cause_of_death, av_death_age), y = av_death_age)) +
  geom_bar(stat = "identity", fill = "antiquewhite1") + 
  labs(x = "Cause of death", 
         y = "Number of death", 
         title = "leading cause of death for over 65 year old group ") + 
theme_bw()+
   theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
coord_flip()


premature = age_death_cause %>%
  filter(age_group == "Premature Death") %>%
  ggplot(aes(x = reorder(cause_of_death, av_death_age), y = av_death_age)) +
  geom_bar(stat = "identity", fill = "cadetblue2") + 
  labs(x = "Cause of death", 
         y = "Number of death", 
         title = "leading cause of premature death ") + 
theme_bw()+
   theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  coord_flip()

ggarrange(over_65, premature, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
```

![](death_data_yx2510_files/figure-markdown_github/unnamed-chunk-16-1.png) We see that for 1-12 months age group, the leading cause of deaths are "others". For less than 28 days, the leading cause of deaths are also "others". For 65-69 age group the leading cause of death is malignant neoplasms (cancer). For &gt;85 years old, diseases of the heart is the leading cause.

#### age borough

``` r
age_death_data3 = merge(age_death_data3, age_death_cause) %>%
  group_by(cause_of_death, age_group, borough) %>%
  summarise(br_death_age = sum(av_death_age)/11) 

over_65_2 = age_death_data3 %>%
  filter(age_group == ">65 age death") %>%
  ggplot(aes(x = reorder(cause_of_death, br_death_age), y = br_death_age, fill = borough)) +
  geom_bar(stat = "identity") + 
  labs(x = "Cause of death", 
         y = "Number of death", 
         title = "leading cause of death for over 65 year old group ") + 
theme_bw() +
   theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  viridis::scale_fill_viridis(discrete = TRUE)  


premature2 = age_death_data3 %>%
  filter(age_group == "Premature Death") %>%
  ggplot(aes(x = reorder(cause_of_death, br_death_age), y = br_death_age, fill = borough)) +
  geom_bar(stat = "identity") + 
  labs(x = "Cause of death", 
         y = "Number of death", 
         title = "leading cause of death for over 65 year old group ") + 
theme_bw()+
   theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  viridis::scale_fill_viridis(discrete = TRUE) 


ggarrange(over_65_2, premature2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
```

![](death_data_yx2510_files/figure-markdown_github/unnamed-chunk-17-1.png)

#### change of death number by age group

``` r
#age_death_data3 %>%
 # group_by(age_group, year)  %>%
  #summarise(total_death = sum(borough_age_death)) %>%
  #ggplot(aes(x = year, y = total_death, color = age_group)) +
  #geom_line() +
  #geom_point() +
  # labs(title = "Number of death in different age group in New York City",
   #          x = "Year",
    #         y = "Number of death")  + 
  #viridis::scale_color_viridis(discrete = TRUE) +
   #theme(axis.text.x = element_text(angle=90, vjust=0.6))
```
