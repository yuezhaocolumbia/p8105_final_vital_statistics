Birth
================
Xue Yang
2018/11/07

Three different source of the data: community district, zip code and census tract

``` r
# community district
birth2000 = haven::read_sas("./birth_data/community_district/cdall_00.sas7bdat")
names(birth2000)
```

    ##   [1] "cd"             "birthtot"       "age1tot"        "age2tot"       
    ##   [5] "age3tot"        "age4tot"        "age5tot"        "age6tot"       
    ##   [9] "age7tot"        "age8tot"        "age9tot"        "eth_bl_tot"    
    ##  [13] "eth_wh_tot"     "eth_hi_tot"     "eth_ap_tot"     "nat1tot"       
    ##  [17] "nat2tot"        "ancs1tot"       "ancs2tot"       "ancs3tot"      
    ##  [21] "ancs4tot"       "ancs5tot"       "ancs6tot"       "ancs7tot"      
    ##  [25] "ancs8tot"       "ancs9tot"       "ancs10tot"      "ancs11tot"     
    ##  [29] "ancs12tot"      "ancs13tot"      "ancs14tot"      "ancs15tot"     
    ##  [33] "ancs_oth_tot"   "bpl1tot"        "bpl2tot"        "bpl3tot"       
    ##  [37] "bpl4tot"        "bpl5tot"        "bpl6tot"        "bpl7tot"       
    ##  [41] "bpl8tot"        "bpl9tot"        "bpl10tot"       "bpl11tot"      
    ##  [45] "bpl12tot"       "bpl13tot"       "bpl14tot"       "bpl15tot"      
    ##  [49] "bpl_oth_tot"    "educ1tot"       "educ2tot"       "educ3tot"      
    ##  [53] "par1tot"        "par2tot"        "prenat1tot"     "prenat2tot"    
    ##  [57] "prenat3tot"     "prenat4tot"     "pay1tot"        "pay2tot"       
    ##  [61] "pay3tot"        "mar1tot"        "mar2tot"        "sex1tot"       
    ##  [65] "sex2tot"        "weight1tot"     "weight2tot"     "weight3tot"    
    ##  [69] "weight4tot"     "bwt1tot"        "bwt2tot"        "bwt3tot"       
    ##  [73] "bwt4tot"        "bwt5tot"        "bwt6tot"        "bwt7tot"       
    ##  [77] "bwt8tot"        "bwt9tot"        "ga1tot"         "ga2tot"        
    ##  [81] "ga3tot"         "ga4tot"         "ga5tot"         "apg1tot"       
    ##  [85] "apg2tot"        "apg3tot"        "apg4tot"        "apg5tot"       
    ##  [89] "plur1tot"       "plur2tot"       "mth1tot"        "mth2tot"       
    ##  [93] "mth3tot"        "mth4tot"        "pob1tot"        "pob2tot"       
    ##  [97] "pob3tot"        "pob4tot"        "pob5tot"        "educ1tot_a1"   
    ## [101] "educ2tot_a1"    "educ3tot_a1"    "nat1tot_a1"     "nat2tot_a1"    
    ## [105] "prenat1ctot_a1" "prenat2ctot_a1" "weight1tot_a1"  "weight2tot_a1" 
    ## [109] "weight3tot_a1"  "weight4tot_a1"  "bwt1ctot_a1"    "bwt2ctot_a1"   
    ## [113] "ga1ctot_a1"     "ga2ctot_a1"     "plur1tot_a1"    "plur2tot_a1"   
    ## [117] "sex1tot_a1"     "sex2tot_a1"     "educ1tot_a2"    "educ2tot_a2"   
    ## [121] "educ3tot_a2"    "nat1tot_a2"     "nat2tot_a2"     "prenat1ctot_a2"
    ## [125] "prenat2ctot_a2" "weight1tot_a2"  "weight2tot_a2"  "weight3tot_a2" 
    ## [129] "weight4tot_a2"  "bwt1ctot_a2"    "bwt2ctot_a2"    "ga1ctot_a2"    
    ## [133] "ga2ctot_a2"     "plur1tot_a2"    "plur2tot_a2"    "sex1tot_a2"    
    ## [137] "sex2tot_a2"     "educ1tot_a3"    "educ2tot_a3"    "educ3tot_a3"   
    ## [141] "nat1tot_a3"     "nat2tot_a3"     "prenat1ctot_a3" "prenat2ctot_a3"
    ## [145] "weight1tot_a3"  "weight2tot_a3"  "weight3tot_a3"  "weight4tot_a3" 
    ## [149] "bwt1ctot_a3"    "bwt2ctot_a3"    "ga1ctot_a3"     "ga2ctot_a3"    
    ## [153] "plur1tot_a3"    "plur2tot_a3"    "sex1tot_a3"     "sex2tot_a3"    
    ## [157] "educ1tot_a4"    "educ2tot_a4"    "educ3tot_a4"    "nat1tot_a4"    
    ## [161] "nat2tot_a4"     "prenat1ctot_a4" "prenat2ctot_a4" "weight1tot_a4" 
    ## [165] "weight2tot_a4"  "weight3tot_a4"  "weight4tot_a4"  "bwt1ctot_a4"   
    ## [169] "bwt2ctot_a4"    "ga1ctot_a4"     "ga2ctot_a4"     "plur1tot_a4"   
    ## [173] "plur2tot_a4"    "sex1tot_a4"     "sex2tot_a4"     "educ1tot_n1"   
    ## [177] "educ2tot_n1"    "educ3tot_n1"    "prenat1ctot_n1" "prenat2ctot_n1"
    ## [181] "weight1tot_n1"  "weight2tot_n1"  "weight3tot_n1"  "weight4tot_n1" 
    ## [185] "bwt1ctot_n1"    "bwt2ctot_n1"    "ga1ctot_n1"     "ga2ctot_n1"    
    ## [189] "plur1tot_n1"    "plur2tot_n1"    "sex1tot_n1"     "sex2tot_n1"    
    ## [193] "educ1tot_n2"    "educ2tot_n2"    "educ3tot_n2"    "prenat1ctot_n2"
    ## [197] "prenat2ctot_n2" "weight1tot_n2"  "weight2tot_n2"  "weight3tot_n2" 
    ## [201] "weight4tot_n2"  "bwt1ctot_n2"    "bwt2ctot_n2"    "ga1ctot_n2"    
    ## [205] "ga2ctot_n2"     "plur1tot_n2"    "plur2tot_n2"    "sex1tot_n2"    
    ## [209] "sex2tot_n2"     "prenat1ctot_e1" "prenat2ctot_e1" "weight1tot_e1" 
    ## [213] "weight2tot_e1"  "weight3tot_e1"  "weight4tot_e1"  "bwt1ctot_e1"   
    ## [217] "bwt2ctot_e1"    "ga1ctot_e1"     "ga2ctot_e1"     "plur1tot_e1"   
    ## [221] "plur2tot_e1"    "sex1tot_e1"     "sex2tot_e1"     "prenat1ctot_e2"
    ## [225] "prenat2ctot_e2" "weight1tot_e2"  "weight2tot_e2"  "weight3tot_e2" 
    ## [229] "weight4tot_e2"  "bwt1ctot_e2"    "bwt2ctot_e2"    "ga1ctot_e2"    
    ## [233] "ga2ctot_e2"     "plur1tot_e2"    "plur2tot_e2"    "sex1tot_e2"    
    ## [237] "sex2tot_e2"     "prenat1ctot_e3" "prenat2ctot_e3" "weight1tot_e3" 
    ## [241] "weight2tot_e3"  "weight3tot_e3"  "weight4tot_e3"  "bwt1ctot_e3"   
    ## [245] "bwt2ctot_e3"    "ga1ctot_e3"     "ga2ctot_e3"     "plur1tot_e3"   
    ## [249] "plur2tot_e3"    "sex1tot_e3"     "sex2tot_e3"

``` r
# zip code
birth9901 = haven::read_sas("./birth_data/zip_code/zipall_9901.sas7bdat")
names(birth9901)
```

    ##   [1] "zip"            "birthtot"       "age1tot"        "age2tot"       
    ##   [5] "age3tot"        "age4tot"        "age5tot"        "age6tot"       
    ##   [9] "age7tot"        "age8tot"        "age9tot"        "eth_bl_tot"    
    ##  [13] "eth_wh_tot"     "eth_hi_tot"     "eth_ap_tot"     "nat1tot"       
    ##  [17] "nat2tot"        "ancs1tot"       "ancs2tot"       "ancs3tot"      
    ##  [21] "ancs4tot"       "ancs5tot"       "ancs6tot"       "ancs7tot"      
    ##  [25] "ancs8tot"       "ancs9tot"       "ancs10tot"      "ancs11tot"     
    ##  [29] "ancs12tot"      "ancs13tot"      "ancs14tot"      "ancs15tot"     
    ##  [33] "ancs_oth_tot"   "bpl1tot"        "bpl2tot"        "bpl3tot"       
    ##  [37] "bpl4tot"        "bpl5tot"        "bpl6tot"        "bpl7tot"       
    ##  [41] "bpl8tot"        "bpl9tot"        "bpl10tot"       "bpl11tot"      
    ##  [45] "bpl12tot"       "bpl13tot"       "bpl14tot"       "bpl15tot"      
    ##  [49] "bpl_oth_tot"    "educ1tot"       "educ2tot"       "educ3tot"      
    ##  [53] "par1tot"        "par2tot"        "prenat1tot"     "prenat2tot"    
    ##  [57] "prenat3tot"     "prenat4tot"     "pay1tot"        "pay2tot"       
    ##  [61] "pay3tot"        "mar1tot"        "mar2tot"        "sex1tot"       
    ##  [65] "sex2tot"        "weight1tot"     "weight2tot"     "weight3tot"    
    ##  [69] "weight4tot"     "bwt1tot"        "bwt2tot"        "bwt3tot"       
    ##  [73] "bwt4tot"        "bwt5tot"        "bwt6tot"        "bwt7tot"       
    ##  [77] "bwt8tot"        "bwt9tot"        "ga1tot"         "ga2tot"        
    ##  [81] "ga3tot"         "ga4tot"         "ga5tot"         "apg1tot"       
    ##  [85] "apg2tot"        "apg3tot"        "apg4tot"        "apg5tot"       
    ##  [89] "plur1tot"       "plur2tot"       "mth1tot"        "mth2tot"       
    ##  [93] "mth3tot"        "mth4tot"        "pob1tot"        "pob2tot"       
    ##  [97] "pob3tot"        "pob4tot"        "pob5tot"        "educ1tot_a1"   
    ## [101] "educ2tot_a1"    "educ3tot_a1"    "nat1tot_a1"     "nat2tot_a1"    
    ## [105] "prenat1ctot_a1" "prenat2ctot_a1" "weight1tot_a1"  "weight2tot_a1" 
    ## [109] "weight3tot_a1"  "weight4tot_a1"  "bwt1ctot_a1"    "bwt2ctot_a1"   
    ## [113] "ga1ctot_a1"     "ga2ctot_a1"     "plur1tot_a1"    "plur2tot_a1"   
    ## [117] "sex1tot_a1"     "sex2tot_a1"     "educ1tot_a2"    "educ2tot_a2"   
    ## [121] "educ3tot_a2"    "nat1tot_a2"     "nat2tot_a2"     "prenat1ctot_a2"
    ## [125] "prenat2ctot_a2" "weight1tot_a2"  "weight2tot_a2"  "weight3tot_a2" 
    ## [129] "weight4tot_a2"  "bwt1ctot_a2"    "bwt2ctot_a2"    "ga1ctot_a2"    
    ## [133] "ga2ctot_a2"     "plur1tot_a2"    "plur2tot_a2"    "sex1tot_a2"    
    ## [137] "sex2tot_a2"     "educ1tot_a3"    "educ2tot_a3"    "educ3tot_a3"   
    ## [141] "nat1tot_a3"     "nat2tot_a3"     "prenat1ctot_a3" "prenat2ctot_a3"
    ## [145] "weight1tot_a3"  "weight2tot_a3"  "weight3tot_a3"  "weight4tot_a3" 
    ## [149] "bwt1ctot_a3"    "bwt2ctot_a3"    "ga1ctot_a3"     "ga2ctot_a3"    
    ## [153] "plur1tot_a3"    "plur2tot_a3"    "sex1tot_a3"     "sex2tot_a3"    
    ## [157] "educ1tot_a4"    "educ2tot_a4"    "educ3tot_a4"    "nat1tot_a4"    
    ## [161] "nat2tot_a4"     "prenat1ctot_a4" "prenat2ctot_a4" "weight1tot_a4" 
    ## [165] "weight2tot_a4"  "weight3tot_a4"  "weight4tot_a4"  "bwt1ctot_a4"   
    ## [169] "bwt2ctot_a4"    "ga1ctot_a4"     "ga2ctot_a4"     "plur1tot_a4"   
    ## [173] "plur2tot_a4"    "sex1tot_a4"     "sex2tot_a4"     "educ1tot_n1"   
    ## [177] "educ2tot_n1"    "educ3tot_n1"    "prenat1ctot_n1" "prenat2ctot_n1"
    ## [181] "weight1tot_n1"  "weight2tot_n1"  "weight3tot_n1"  "weight4tot_n1" 
    ## [185] "bwt1ctot_n1"    "bwt2ctot_n1"    "ga1ctot_n1"     "ga2ctot_n1"    
    ## [189] "plur1tot_n1"    "plur2tot_n1"    "sex1tot_n1"     "sex2tot_n1"    
    ## [193] "educ1tot_n2"    "educ2tot_n2"    "educ3tot_n2"    "prenat1ctot_n2"
    ## [197] "prenat2ctot_n2" "weight1tot_n2"  "weight2tot_n2"  "weight3tot_n2" 
    ## [201] "weight4tot_n2"  "bwt1ctot_n2"    "bwt2ctot_n2"    "ga1ctot_n2"    
    ## [205] "ga2ctot_n2"     "plur1tot_n2"    "plur2tot_n2"    "sex1tot_n2"    
    ## [209] "sex2tot_n2"     "prenat1ctot_e1" "prenat2ctot_e1" "weight1tot_e1" 
    ## [213] "weight2tot_e1"  "weight3tot_e1"  "weight4tot_e1"  "bwt1ctot_e1"   
    ## [217] "bwt2ctot_e1"    "ga1ctot_e1"     "ga2ctot_e1"     "plur1tot_e1"   
    ## [221] "plur2tot_e1"    "sex1tot_e1"     "sex2tot_e1"     "prenat1ctot_e2"
    ## [225] "prenat2ctot_e2" "weight1tot_e2"  "weight2tot_e2"  "weight3tot_e2" 
    ## [229] "weight4tot_e2"  "bwt1ctot_e2"    "bwt2ctot_e2"    "ga1ctot_e2"    
    ## [233] "ga2ctot_e2"     "plur1tot_e2"    "plur2tot_e2"    "sex1tot_e2"    
    ## [237] "sex2tot_e2"     "prenat1ctot_e3" "prenat2ctot_e3" "weight1tot_e3" 
    ## [241] "weight2tot_e3"  "weight3tot_e3"  "weight4tot_e3"  "bwt1ctot_e3"   
    ## [245] "bwt2ctot_e3"    "ga1ctot_e3"     "ga2ctot_e3"     "plur1tot_e3"   
    ## [249] "plur2tot_e3"    "sex1tot_e3"     "sex2tot_e3"

``` r
# census tract
birth0004 = haven::read_sas("./birth_data/census_tract/tractall_0004.sas7bdat")
names(birth0004)
```

    ##  [1] "tract"        "birthtot"     "age1tot"      "age2tot"     
    ##  [5] "age3tot"      "age4tot"      "age5tot"      "age6tot"     
    ##  [9] "age7tot"      "age8tot"      "age9tot"      "eth_bl_tot"  
    ## [13] "eth_wh_tot"   "eth_hi_tot"   "eth_ap_tot"   "nat1tot"     
    ## [17] "nat2tot"      "ancs1tot"     "ancs2tot"     "ancs3tot"    
    ## [21] "ancs4tot"     "ancs5tot"     "ancs6tot"     "ancs7tot"    
    ## [25] "ancs8tot"     "ancs9tot"     "ancs10tot"    "ancs11tot"   
    ## [29] "ancs12tot"    "ancs13tot"    "ancs14tot"    "ancs15tot"   
    ## [33] "ancs_oth_tot" "bpl1tot"      "bpl2tot"      "bpl3tot"     
    ## [37] "bpl4tot"      "bpl5tot"      "bpl6tot"      "bpl7tot"     
    ## [41] "bpl8tot"      "bpl9tot"      "bpl10tot"     "bpl11tot"    
    ## [45] "bpl12tot"     "bpl13tot"     "bpl14tot"     "bpl15tot"    
    ## [49] "bpl_oth_tot"  "educ1tot"     "educ2tot"     "educ3tot"    
    ## [53] "par1tot"      "par2tot"      "prenat1tot"   "prenat2tot"  
    ## [57] "prenat3tot"   "prenat4tot"   "pay1tot"      "pay2tot"     
    ## [61] "pay3tot"      "mar1tot"      "mar2tot"      "sex1tot"     
    ## [65] "sex2tot"      "weight1tot"   "weight2tot"   "weight3tot"  
    ## [69] "weight4tot"   "bwt1tot"      "bwt2tot"      "bwt3tot"     
    ## [73] "bwt4tot"      "bwt5tot"      "bwt6tot"      "bwt7tot"     
    ## [77] "bwt8tot"      "bwt9tot"      "ga1tot"       "ga2tot"      
    ## [81] "ga3tot"       "ga4tot"       "ga5tot"       "apg1tot"     
    ## [85] "apg2tot"      "apg3tot"      "apg4tot"      "apg5tot"     
    ## [89] "plur1tot"     "plur2tot"     "mth1tot"      "mth2tot"     
    ## [93] "mth3tot"      "mth4tot"      "pob1tot"      "pob2tot"     
    ## [97] "pob3tot"      "pob4tot"      "pob5tot"

Problem 1: choose which source of data to do the analysis

``` r
tibble(
  community_district = names(birth9901),
  zip_code = names(birth9901)
  #census_tract = names(birth0004)
) 
```

    ## # A tibble: 251 x 2
    ##    community_district zip_code
    ##    <chr>              <chr>   
    ##  1 zip                zip     
    ##  2 birthtot           birthtot
    ##  3 age1tot            age1tot 
    ##  4 age2tot            age2tot 
    ##  5 age3tot            age3tot 
    ##  6 age4tot            age4tot 
    ##  7 age5tot            age5tot 
    ##  8 age6tot            age6tot 
    ##  9 age7tot            age7tot 
    ## 10 age8tot            age8tot 
    ## # ... with 241 more rows

#### The intended final products

We will determine the top ten causes of death in New York and make stratified analysis to go in depth into the factors which were hidden in the data. Specifically, we will identify the leading causes of death by sex, race, age, place of origin/birth and borough of residence and explore whether there is a socioeconomic disparity in terms of premature death and infant mortality/defect （whether statistics differs between low-income area and high-income area). We could also examine changes in the makeup of leading causes of death during the last decade.

We are also intended to take a look at the birth defect data to investigate the main causes of birth defect, the ratio of birth defect leading to infant mortality and we will also explore relationships between maternal factors and infant factors in the birth defect cases.

The problem is that we cannot access *the birth defect cases* data from the website.

How to combination the birth and death together?

the relationship between Maternal and the infant?

``` r
birth_data = 
  tibble(file_name = list.files(path = "./birth_data/community_district")) %>% 
  
  # iterate over file names and read in data for each subject
  mutate(output = purrr::map(str_c("./birth_data/community_district/", file_name), haven::read_sas)) %>%
  
  # unnest the dataframe
  unnest() 
```
