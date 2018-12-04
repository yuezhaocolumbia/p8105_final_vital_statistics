Birth\_data
================
Xue Yang
12/3/2018"

#### Load and clean the data from community\_district source

``` r
birth_data = 
  tibble(file_name = list.files(path = "./birth_data/community_district")) %>% 
  
  # iterate over file names and read in data for each subject
  mutate(output = purrr::map(str_c("./birth_data/community_district/", file_name), haven::read_sas)) %>%
  
  # unnest the dataframe
  unnest() %>% 
  separate(file_name, c("name", "year", "del"), sep = c(6, 8)) %>% 
  select(-name, -del) %>%
  mutate(year = as.factor(year))

# import cd code data
cd_code_data = 
  read_csv("./data/New_York_City_Population_By_Community_Districts2.csv") %>%
  janitor::clean_names() %>%
  select(borough, cd_number, cd_name) %>% 
  rename(cd = cd_number)
```

    ## Parsed with column specification:
    ## cols(
    ##   Borough = col_character(),
    ##   `CD Number` = col_integer(),
    ##   `CD Name` = col_character(),
    ##   `1970 Population` = col_integer(),
    ##   `1980 Population` = col_integer(),
    ##   `1990 Population` = col_integer(),
    ##   `2000 Population` = col_integer(),
    ##   `2010 Population` = col_integer()
    ## )

``` r
# join two data
birth_data_un = 
  birth_data %>% 
  unnest() %>% 
  left_join(cd_code_data, by = "cd") 
```

#### Deal with missing value

``` r
# check for the number of missing value
missing_value = 
  birth_data_un %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(term, num_na, everything()) %>% 
  mutate(percent = num_na/885) %>% 
  filter(percent > 0.2)
```

For the missing value, we count for the number of missing value for each column of the variables and then count for the proportion of them. Using the cutoff = 20%. If the proprtion of missing value is larger than 20%, we delete this column of variable(usually this means to delete a category of one specific variable). And if the proportion is less than 20%, we keep this catagory of the specific variable and valua the NA as the same value as it was in the last yeat of the same borough.

``` r
# for the data maternal age
age_na = 
  birth_data_un %>% 
  select(year, cd, birthtot, age1tot:age9tot) %>%
  gather(maternal_age, num, age1tot:age9tot) %>% 
  group_by(year, cd, birthtot) %>% 
  summarise(ttl = sum(num, na.rm = TRUE)) %>% 
  mutate(delta = birthtot - ttl) %>% 
  arrange(desc(delta))
```

So for the age variable, we delete the category "age1tot", "age9tot"

#### Tidy the data for each variables separately

**Individual Variables**

age, race, nativity, parity, maternal marital status, infant sex, infant birthweight, infant gestational age

``` r
# data for maternal age
maternal_age_data = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, age1tot:age9tot) %>%
  gather(maternal_age, num, age1tot:age9tot) %>% 
  group_by(year, borough, maternal_age) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = maternal_age, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for maternal race
maternal_race_data = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, eth_bl_tot:eth_ap_tot) %>%
  gather(maternal_race, num, eth_bl_tot:eth_ap_tot) %>% 
  group_by(year, borough, maternal_race) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = maternal_race, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for maternal nativity
born_demo_data = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, nat1tot:nat2tot) %>%
  gather(born_demo, num, nat1tot:nat2tot) %>% 
  group_by(year, borough, born_demo) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = born_demo, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for parity
parity_data = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, par1tot:par2tot) %>%
  gather(parity, num, par1tot:par2tot) %>% 
  group_by(year, borough, parity) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = parity, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for maternal marital status 
marry_data = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, mar1tot:mar2tot) %>%
  gather(marry_status, num, mar1tot:mar2tot) %>% 
  group_by(year, borough, marry_status) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = marry_status, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for infant sex
infant_sex_data = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, sex1tot:sex2tot) %>%
  gather(infant_sex, num, sex1tot:sex2tot) %>% 
  group_by(year, borough, infant_sex) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = infant_sex, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for infant birthweight
birth_weight_data =  
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, bwt1tot:bwt9tot) %>%
  gather(birth_weight, num, bwt1tot:bwt9tot) %>% 
  group_by(year, borough, birth_weight) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = birth_weight, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for infant gestational age
birth_ges = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, ga1tot:ga5tot) %>% 
  gather(key = gestational_age, value = num, ga1tot:ga5tot)  %>% 
  group_by(year, borough, gestational_age) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = gestational_age, value = number) %>% 
  knitr::kable(digits = 3)
```

**Crossed Variables** infant birthweight and maternal age, infant sex and maternal nativity, maternal nativity and maternal age, gestational and maternal age, infant sex and maternal age

``` r
# data for infant birthweight and maternal age
bwt_age_data =  
  birth_data_un %>%
  select(year, cd, cd_name, borough, birthtot, contains("ctot_a")) %>%
  select(year, cd, cd_name, borough, birthtot, starts_with("bwt")) %>% 
  gather(bwt_age, num, bwt1ctot_a1:bwt2ctot_a4) %>% 
  group_by(year, borough, bwt_age) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = bwt_age, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for infant sex and maternal nativity
sex_nat_data =  
  birth_data_un %>%
  select(year, cd, cd_name, borough, birthtot, contains("tot_n")) %>%
  select(year, cd, cd_name, borough, birthtot, starts_with("sex")) %>% 
  gather(sex_nat, num, sex1tot_n1:sex2tot_n2) %>% 
  group_by(year, borough, sex_nat) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = sex_nat, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for maternal nativity and maternal age
birth_nat_age = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, nat1tot_a1, nat2tot_a1, nat1tot_a2, nat2tot_a2, nat1tot_a3, nat2tot_a3, nat1tot_a4, nat2tot_a4) %>% 
  gather(key = nativity_age, value = num, nat1tot_a1:nat2tot_a4) %>% 
  group_by(year, borough, nativity_age) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = nativity_age, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for gestational and maternal age
birth_ges_age = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, ga1ctot_a1, ga2ctot_a1, ga1ctot_a2, ga2ctot_a2, ga1ctot_a3, ga2ctot_a3, ga1ctot_a4, ga2ctot_a4) %>% 
  gather(key = ges_age, value = num, ga1ctot_a1:ga2ctot_a4) %>% 
  group_by(year, borough, ges_age) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = ges_age, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for infant sex and maternal age
birth_sex_age = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, sex1tot_a1, sex2tot_a1, sex1tot_a2, sex2tot_a2, sex1tot_a3, sex2tot_a3, sex1tot_a4, sex2tot_a4) %>% 
  gather(key = sex_age, value = num, sex1tot_a1:sex2tot_a4) %>% 
  group_by(year, borough, sex_age) %>% 
  summarise(number = sum(num)) %>% 
  spread(key = sex_age, value = number) %>% 
  knitr::kable(digits = 3)
```
