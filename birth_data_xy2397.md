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

``` r
# check for the number of missing value
birth_data_un %>% 
  summarise_all(funs(sum(is.na(.)))) 
```

    ## # A tibble: 1 x 364
    ##    year    cd birthtot age1tot age2tot age3tot age4tot age5tot age6tot
    ##   <int> <int>    <int>   <int>   <int>   <int>   <int>   <int>   <int>
    ## 1     0     0        0     774      84      46       0       0       0
    ## # ... with 355 more variables: age7tot <int>, age8tot <int>,
    ## #   age9tot <int>, eth_bl_tot <int>, eth_wh_tot <int>, eth_hi_tot <int>,
    ## #   eth_ap_tot <int>, nat1tot <int>, nat2tot <int>, ancs1tot <int>,
    ## #   ancs2tot <int>, ancs3tot <int>, ancs4tot <int>, ancs5tot <int>,
    ## #   ancs6tot <int>, ancs7tot <int>, ancs8tot <int>, ancs9tot <int>,
    ## #   ancs10tot <int>, ancs11tot <int>, ancs12tot <int>, ancs13tot <int>,
    ## #   ancs14tot <int>, ancs15tot <int>, ancs_oth_tot <int>, bpl1tot <int>,
    ## #   bpl2tot <int>, bpl3tot <int>, bpl4tot <int>, bpl5tot <int>,
    ## #   bpl6tot <int>, bpl7tot <int>, bpl8tot <int>, bpl9tot <int>,
    ## #   bpl10tot <int>, bpl11tot <int>, bpl12tot <int>, bpl13tot <int>,
    ## #   bpl14tot <int>, bpl15tot <int>, bpl_oth_tot <int>, educ1tot <int>,
    ## #   educ2tot <int>, educ3tot <int>, par1tot <int>, par2tot <int>,
    ## #   prenat1tot <int>, prenat2tot <int>, prenat3tot <int>,
    ## #   prenat4tot <int>, pay1tot <int>, pay2tot <int>, pay3tot <int>,
    ## #   mar1tot <int>, mar2tot <int>, sex1tot <int>, sex2tot <int>,
    ## #   weight1tot <int>, weight2tot <int>, weight3tot <int>,
    ## #   weight4tot <int>, bwt1tot <int>, bwt2tot <int>, bwt3tot <int>,
    ## #   bwt4tot <int>, bwt5tot <int>, bwt6tot <int>, bwt7tot <int>,
    ## #   bwt8tot <int>, bwt9tot <int>, ga1tot <int>, ga2tot <int>,
    ## #   ga3tot <int>, ga4tot <int>, ga5tot <int>, apg1tot <int>,
    ## #   apg2tot <int>, apg3tot <int>, apg4tot <int>, apg5tot <int>,
    ## #   plur1tot <int>, plur2tot <int>, mth1tot <int>, mth2tot <int>,
    ## #   mth3tot <int>, mth4tot <int>, pob1tot <int>, pob2tot <int>,
    ## #   pob3tot <int>, pob4tot <int>, pob5tot <int>, educ1tot_a1 <int>,
    ## #   educ2tot_a1 <int>, educ3tot_a1 <int>, nat1tot_a1 <int>,
    ## #   nat2tot_a1 <int>, prenat1ctot_a1 <int>, prenat2ctot_a1 <int>,
    ## #   weight1tot_a1 <int>, weight2tot_a1 <int>, ...

#### Tidy the data for each variables separately

``` r
# data for maternal age
maternal_age_data = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, age1tot:age9tot) %>%
  gather(maternal_age, num, age1tot:age9tot) %>% 
  group_by(year, borough, maternal_age) %>% 
  summarise(number = n()) %>% 
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
  summarise(number = n()) %>% 
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
  summarise(number = n()) %>% 
  spread(key = born_demo, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for maternal ancestry
maternal_anc_data = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, ancs1tot:ancs15tot) %>%
  gather(maternal_anc, num, ancs1tot:ancs15tot) %>% 
  group_by(year, borough, maternal_anc) %>% 
  summarise(number = n()) %>% 
  spread(key = maternal_anc, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for maternal birthplace
maternal_birth_place_data = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, bpl1tot:bpl_oth_tot) %>%
  gather(maternal_birth_place, num, bpl1tot:bpl_oth_tot) %>% 
  group_by(year, borough, maternal_birth_place) %>% 
  summarise(number = n()) %>% 
  spread(key = maternal_birth_place, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for parity
parity_data = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, par1tot:par2tot) %>%
  gather(parity, num, par1tot:par2tot) %>% 
  group_by(year, borough, parity) %>% 
  summarise(number = n()) %>% 
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
  summarise(number = n()) %>% 
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
  summarise(number = n()) %>% 
  spread(key = infant_sex, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for maternal Pre-pregnancy Weight
birth_weight_data =  
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, bwt1tot:bwt9tot) %>%
  gather(birth_weight, num, bwt1tot:bwt9tot) %>% 
  group_by(year, borough, birth_weight) %>% 
  summarise(number = n()) %>% 
  spread(key = birth_weight, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for Five minute Apgar score counts
apg_count_data =  
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, apg1tot:apg5tot) %>%
  gather(apg_count, num, apg1tot:apg5tot) %>% 
  group_by(year, borough, apg_count) %>% 
  summarise(number = n()) %>% 
  spread(key = apg_count, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for Place of Birth Counts
birth_place_data =  
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, pob1tot:pob5tot) %>%
  gather(birth_place, num, pob1tot:pob5tot) %>% 
  group_by(year, borough, birth_place) %>% 
  summarise(number = n()) %>% 
  spread(key = birth_place, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for infant gestational age
birth_ges = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, ga1tot:ga5tot) %>% 
  gather(key = gestational_age, value = num, ga1tot:ga5tot)  %>% 
  group_by(year, borough, gestational_age) %>% 
  summarise(number = n()) %>% 
  spread(key = gestational_age, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for plurality
birth_plur = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, plur1tot:plur2tot) %>% 
  gather(key = plur, value = num, plur1tot:plur2tot) %>% 
  group_by(year, borough, plur) %>% 
  summarise(number = n()) %>% 
  spread(key = plur, value = number) %>% 
  knitr::kable(digits = 3)
```

**Cross data**

``` r
# data for infant birthweight and maternal age
bwt_age_data =  
  birth_data_un %>%
  select(year, cd, cd_name, borough, birthtot, contains("ctot_a")) %>%
  select(year, cd, cd_name, borough, birthtot, starts_with("bwt")) %>% 
  gather(bwt_age, num, bwt1ctot_a1:bwt2ctot_a4) %>% 
  group_by(year, borough, bwt_age) %>% 
  summarise(number = n()) %>% 
  spread(key = bwt_age, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for infant birthweight and maternal nativity
bwt_nat_data =  
  birth_data_un %>%
  select(year, cd, cd_name, borough, birthtot, contains("ctot_n")) %>%
  select(year, cd, cd_name, borough, birthtot, starts_with("bwt")) %>% 
  gather(bwt_nat, num, bwt1ctot_n1:bwt2ctot_n2) %>% 
  group_by(year, borough, bwt_nat) %>% 
  summarise(number = n()) %>% 
  spread(key = bwt_nat, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for infant gestational age and maternal nativity
ga_nat_data =  
  birth_data_un %>%
  select(year, cd, cd_name, borough, birthtot, contains("ctot_n")) %>%
  select(year, cd, cd_name, borough, birthtot, starts_with("ga")) %>% 
  gather(ga_nat, num, ga1ctot_n1:ga2ctot_n2) %>% 
  group_by(year, borough, ga_nat) %>% 
  summarise(number = n()) %>% 
  spread(key = ga_nat, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for Plurality and maternal age
plur_age_data =  
  birth_data_un %>%
  select(year, cd, cd_name, borough, birthtot, contains("tot_a")) %>%
  select(year, cd, cd_name, borough, birthtot, starts_with("plu")) %>% 
  gather(plur_age, num, plur1tot_a1:plur2tot_a4) %>% 
  group_by(year, borough, plur_age) %>% 
  summarise(number = n()) %>% 
  spread(key = plur_age, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for Plurality and maternal nativity
plur_nat_data = 
  birth_data_un %>%
  select(year, cd, cd_name, borough, birthtot, contains("tot_n")) %>%
  select(year, cd, cd_name, borough, birthtot, starts_with("plu")) %>% 
  gather(plur_nat, num, plur1tot_n1:plur2tot_n2) %>% 
  group_by(year, borough, plur_nat) %>% 
  summarise(number = n()) %>% 
  spread(key = plur_nat, value = number) %>% 
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
  summarise(number = n()) %>% 
  spread(key = sex_nat, value = number) %>% 
  knitr::kable(digits = 3)
```

``` r
# data for nativity and maternal age
birth_nat_age = 
  birth_data_un %>% 
  select(year, cd, cd_name, borough, birthtot, nat1tot_a1, nat2tot_a1, nat1tot_a2, nat2tot_a2, nat1tot_a3, nat2tot_a3, nat1tot_a4, nat2tot_a4) %>% 
  gather(key = nativity_age, value = num, nat1tot_a1:nat2tot_a4) %>% 
  group_by(year, borough, nativity_age) %>% 
  summarise(number = n()) %>% 
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
  summarise(number = n()) %>% 
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
  summarise(number = n()) %>% 
  spread(key = sex_age, value = number) %>% 
  knitr::kable(digits = 3)
```
