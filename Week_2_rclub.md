---
title: "Week_2_rclub"
author: "Brennan"
date: "4/28/2020"
output: 
  html_document: 
    keep_md: yes
---

```r
library(tidyverse)
```

```
## ── Attaching packages ───────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
```

```
## ✔ ggplot2 3.2.0     ✔ purrr   0.3.2
## ✔ tibble  2.1.3     ✔ dplyr   0.8.1
## ✔ tidyr   0.8.3     ✔ stringr 1.4.0
## ✔ readr   1.3.1     ✔ forcats 0.4.0
```

```
## Warning: package 'ggplot2' was built under R version 3.5.2
```

```
## Warning: package 'tibble' was built under R version 3.5.2
```

```
## Warning: package 'tidyr' was built under R version 3.5.2
```

```
## Warning: package 'purrr' was built under R version 3.5.2
```

```
## Warning: package 'dplyr' was built under R version 3.5.2
```

```
## Warning: package 'stringr' was built under R version 3.5.2
```

```
## Warning: package 'forcats' was built under R version 3.5.2
```

```
## ── Conflicts ──────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```


```r
data_parsed <- read.csv("~/Downloads/master_flowering_time_data_2019_10_15.csv") 
```


```r
data_parsed$germ = as.Date(data_parsed$germ, format = "%m_%d_%Y")
data_parsed$yellow = as.Date(data_parsed$yellow, format = "%m_%d_%Y")
data_parsed$flowering = as.Date(data_parsed$flowering, format = "%m_%d_%Y")
data_parsed$final = as.Date(data_parsed$final, format = "%m_%d_%Y")
```

```r
str(data_parsed)
```

```
## 'data.frame':	286 obs. of  17 variables:
##  $ DNA_ID   : Factor w/ 282 levels "D132","D133",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ field_ID : Factor w/ 286 levels "SH_A_01","SH_A_02",..: 147 148 149 155 156 157 158 163 164 165 ...
##  $ genotype : Factor w/ 2 levels "elf3","WT": 2 2 2 2 2 2 2 2 2 2 ...
##  $ group    : Factor w/ 4 levels "A","B","C","D": 1 1 1 1 1 1 1 1 1 1 ...
##  $ row      : Factor w/ 9 levels "SH_A","SH_B",..: 6 6 6 6 6 6 6 6 6 6 ...
##  $ end.     : Factor w/ 2 levels "end","middle": 1 1 2 2 2 2 2 2 2 2 ...
##  $ condition: Factor w/ 2 levels "shade","sun": 2 2 2 2 2 2 2 2 2 2 ...
##  $ germ     : Date, format: "2019-06-07" "2019-06-08" ...
##  $ yellow   : Date, format: "2019-07-30" "2019-07-30" ...
##  $ flowering: Date, format: "2019-07-31" "2019-08-01" ...
##  $ final    : Date, format: "2019-08-07" "2019-08-08" ...
##  $ diameter : num  18.6 17.9 15.4 19 13.5 9.8 16.1 16.8 16.3 19.7 ...
##  $ direction: Factor w/ 119 levels "0","10","101",..: 108 93 119 119 31 119 84 69 28 119 ...
##  $ height   : num  93.7 91.1 77.5 87.6 80.2 54 72.7 87.4 89.2 91 ...
##  $ leaf_num : int  32 33 28 30 33 28 28 28 29 31 ...
##  $ ax_heads : int  8 4 8 9 4 11 8 8 7 6 ...
##  $ notes    : Factor w/ 15 levels "bagged","bottom of stem chewed",..: NA NA NA NA NA NA NA NA NA NA ...
```

```r
data_parsed_1 <- mutate(data_parsed, flowering_time = flowering - germ, total_time = final - germ )
```

```r
view (data_parsed_1)
```
