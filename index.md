# Demographic Analysis of Norway
Andriy Zhubryd  
15. ÄŤervna 2016  





# Introduction

This paper is focused on analysing Norway's population and its changes in time. The following main areas will be covered in this paper:

* Population size and structure

* Fertility

* Mortality

* Migration

* Marriages, divorces

* Education

* Life tables

* Population projections

All analyses are based on data from [Eurostat](http://ec.europa.eu/eurostat/web/population-demography-migration-projections/overview)
and are performed in R. 

Code parts are available in this report in the collapsed form and they can be explored by user. Some code elements are explained in the Appendix. Also, raw data description and samples are provided in the Appendix.



# Population size and structure
## Population size in time

First of all, we should look at the population size and its development in time. Here are plots and table showing Norway's population in time:


```r
dt_Population_Age_Sex %>% 
  group_by(Year) %>% summarize(Population = sum(Population, na.rm = T)) %>%
  ggplot(aes(x = Year, y = Population)) + geom_point(size = 2) +
    geom_line() + scale_y_continuous(limits = c(0, 6*10^6), labels = comma) +
    ggtitle("Development of Norway's population in time")
```

<img src="Norway_files/figure-html/pop_size-1.png" style="display: block; margin: auto;" />

```r
dt_Population_Age_Sex %>% 
  group_by(Year, Sex) %>% summarize(Population = sum(Population, na.rm = T)) %>%
  ggplot(aes(x = Year, y = Population, color = Sex)) + geom_point(size = 2) +
  geom_line() + scale_y_continuous(limits = c(0, 3*10^6), labels = comma) +
  ggtitle("Development of Norway's population in time by sex")
```

<img src="Norway_files/figure-html/pop_size-2.png" style="display: block; margin: auto;" />

```r
dt_Population_Age_Sex %>%
  group_by(Year, Sex) %>% summarize(Population = sum(Population, na.rm = T)) %>%
  dcast(Year ~ Sex, fun.aggregate = sum, value.var = "Population") %>%
  mutate(Total = M + F) %>% filter(Year >= 2000) %>% 
  mutate(Total = comma(Total) , M = comma(M), F = comma(F))
```

```
##    Year         F         M     Total
## 1  2000 2,261,357 2,217,140 4,478,497
## 2  2001 2,272,135 2,231,301 4,503,436
## 3  2002 2,282,132 2,241,934 4,524,066
## 4  2003 2,296,145 2,256,107 4,552,252
## 5  2004 2,308,408 2,269,049 4,577,457
## 6  2005 2,322,293 2,284,070 4,606,363
## 7  2006 2,338,238 2,301,981 4,640,219
## 8  2007 2,355,346 2,325,788 4,681,134
## 9  2008 2,377,481 2,359,690 4,737,171
## 10 2009 2,404,199 2,395,053 4,799,252
## 11 2010 2,431,447 2,426,752 4,858,199
## 12 2011 2,459,456 2,460,849 4,920,305
## 13 2012 2,486,999 2,498,871 4,985,870
## 14 2013 2,515,367 2,535,908 5,051,275
## 15 2014 2,540,963 2,567,007 5,107,970
## 16 2015 2,567,291 2,599,202 5,166,493
```

As we can see from the graph, population is steadily growing: approximately `45%` in `55` years, which makes up for `0.67%` increase per annum. In 1960 population comprised around `3.5` mln people and as of 2015 it's nearly `5.2` mln people. 

Also, we can see from the second graph that number of males and females is nearly the same throught the history. More detailed analysis of this will be in the following section.

## Population age structure

Best way to analyze population strucure is to start with the age-sex pyramid graph. Pyramid graph stores the nation's history and to confirm that we will build graphs for different years: 1960, 1980, 2000 and 2013.



```r
dt_Population_Age_Sex %>% 
  filter(Year == 1960) %>% group_by(Age, Sex) %>% summarise(value = sum(Population, na.rm = T)) %>%
  dcast(Age ~ Sex, fun.aggregate = sum, value.var = "value") %>% select(M, F, Age) %>%
  pyramid(Csize = 1, Cstep = 5, AxisFM = 'd', Cgap = .1, Laxis = 10^4 * 0:4,
          main = "Pyramid of Norway's population in 1960")
```

<img src="Norway_files/figure-html/pyramids-1.png" style="display: block; margin: auto;" />

```r
dt_Population_Age_Sex %>% 
  filter(Year == 1980) %>% group_by(Age, Sex) %>% summarise(value = sum(Population, na.rm = T)) %>%
  dcast(Age ~ Sex, fun.aggregate = sum, value.var = "value") %>% select(M, F, Age) %>%
  pyramid(Csize = 1, Cstep = 5, AxisFM = 'd', Cgap = .1, Laxis = 10^4 * 0:4,
          main = "Pyramid of Norway's population in 1980")
```

<img src="Norway_files/figure-html/pyramids-2.png" style="display: block; margin: auto;" />

```r
dt_Population_Age_Sex %>% 
  filter(Year == 2000) %>% group_by(Age, Sex) %>% summarise(value = sum(Population, na.rm = T)) %>%
  dcast(Age ~ Sex, fun.aggregate = sum, value.var = "value") %>% select(M, F, Age) %>%
  pyramid(Csize = 1, Cstep = 5, AxisFM = 'd', Cgap = .1, Laxis = 10^4 * 0:4,
          main = "Pyramid of Norway's population in 2000")
```

<img src="Norway_files/figure-html/pyramids-3.png" style="display: block; margin: auto;" />

```r
dt_Population_Age_Sex %>% 
  filter(Year == 2013) %>% group_by(Age, Sex) %>% summarise(value = sum(Population, na.rm = T)) %>%
  dcast(Age ~ Sex, fun.aggregate = sum, value.var = "value") %>% select(M, F, Age) %>%
  pyramid(Csize = 1, Cstep = 5, AxisFM = 'd', Cgap = .1, Laxis = 10^4 * 0:4,
          main = "Pyramid of Norway's population in 2013")
```

<img src="Norway_files/figure-html/pyramids-4.png" style="display: block; margin: auto;" />

From the pyramid graphs you can see that peaks and gaps of generations are transferring to the further years. Pyramid is actually moving up with time.

We can notice that population was between progressive and stationary in 1960 with significant share of young generation. Currently (2013), we can say that population moved more to the regressive type with more or less stabel distribution of population up to age of 70. This kind of transformation is the natural process due to development of country and living conditions.

In addition, let's show the change of index of masculinity (proportion of males to females) with the age increase:


```r
dt_Population_Age_Sex %>%
  filter(Year %in% c(1960, 1980, 2000, 2013)) %>% group_by(Year, Age, Sex) %>%
  summarize(value = sum(Population, na.rm = T)) %>% 
  dcast(Year + Age ~ Sex, fun.aggregate = sum, value.var = "value") %>%
  mutate(Index.of.Masculinity = M/F, Year = factor(Year)) %>%
  ggplot(aes(x = Age, y = Index.of.Masculinity, color = Year)) + 
  geom_line() + geom_hline(yintercept = 1, size = .2) + 
  ggtitle("Development of Index of Masculinity with Age\nNorway") +
  scale_x_continuous(breaks = 20*0:5, labels = 20*0:5)
```

<img src="Norway_files/figure-html/age_proportion-1.png" style="display: block; margin: auto;" />

From this graph we can make the following conclusions:

1. There are more males at the younger age
2. Proportion of males to females is decreasing with age
3. Parity point's age (equal number of males and females) is increasing with time: from nearly 40 in 1960 to 58 in 2000 and 65 in 2013

This means that mortality rate for males is higher, but it's decreasing in time quicker than for females.

## Generations
### Biological Generations

Biological generations are:

* children (0-14 years)
* parents (15-49 years)
* grandparents (50+ years)

Here's the graph of development of biological generations in time and detail information about biological generations in 2013:


```r
tmp_Population_Age_Sex_Gen <- dt_Population_Age_Sex %>% 
  mutate(Gen_bio = cut(Age, include.lowest = T, breaks = c(0, 15, 50, 100), right = F),
         Gen_eco = cut(Age, include.lowest = T, breaks = c(0, 20, 65, 100), right = F))

tmp_Bio_Generation <- tmp_Population_Age_Sex_Gen %>%
  group_by(Year, Generation = Gen_bio) %>% summarize(Value = sum(Population, na.rm = T)) %>%
  merge(dt_Population_Age_Sex %>% group_by(Year) %>% summarize(pop = sum(Population, na.rm = T)), by = "Year") %>%
  mutate(Share = Value/pop) %>% select(Year, Generation, Share)

tmp_Bio_Generation %>% 
  ggplot(aes(x = Year, y = Share, color = Generation, fill = Generation)) +
    geom_area() + ggtitle("Development of Biological generations in Time\nNorway")
```

<img src="Norway_files/figure-html/bio-1.png" style="display: block; margin: auto;" />

```r
tmp_Bio_Generation %>% filter(Year == 2013) %>%
  ggplot(aes(x = "", y = Share, fill = Generation)) +
    geom_bar(width = 1, stat = "identity") + coord_polar(theta = "y", start = 0) + 
    ggtitle("Norwish population by Biological generations in 2013")
```

<img src="Norway_files/figure-html/bio-2.png" style="display: block; margin: auto;" />

```r
tmp_Bio_Generation %>% filter(Year == 2013) %>% mutate(Share = round(Share*100, digits = 1))
```

```
##   Year Generation Share
## 1 2013     [0,15)  18.4
## 2 2013    [15,50)  47.8
## 3 2013   [50,100]  33.9
```

From the graphs we see that:

* Share of younger generation is constantly decreasing from `25%` in 1960 to `18.4%` in 2013
* In the meanwhile, share of older generation is increasing with approximately the same speed, which is causing steady upward trend of Sauvy's index (shown later)
* Medium generation (parents) is more or less constant in time and volatiles around `47.57%` of population

We can conclude from these numbers that Norway moved from stationary type of population in 1960 to regressive one.

### Economical generations

Same transformations we'll perform for economical generations with slighly different age ranges:

* pre-productive (0-19 years)
* productive (20-64 years)
* post-productive (65+ years)

Development in time of shares of economical generations is shown below:


```r
tmp_Eco_Generation <- tmp_Population_Age_Sex_Gen %>%
  group_by(Year, Generation = Gen_eco) %>% summarize(Value = sum(Population, na.rm = T)) %>%
  merge(dt_Population_Age_Sex %>% group_by(Year) %>% summarize(pop = sum(Population, na.rm = T)), by = "Year") %>%
  mutate(Share = Value/pop) %>% select(Year, Generation, Share)

tmp_Eco_Generation %>% 
  ggplot(aes(x = Year, y = Share, color = Generation, fill = Generation)) +
  geom_area() + ggtitle("Development of Economical generations in Time\nNorway")
```

<img src="Norway_files/figure-html/eco-1.png" style="display: block; margin: auto;" />

```r
tmp_Eco_Generation %>% filter(Year == 2013) %>%
  ggplot(aes(x = "", y = Share, fill = Generation)) +
  geom_bar(width = 1, stat = "identity") + coord_polar(theta = "y", start = 0) + 
  ggtitle("Norwish population by Economical generations in 2013")
```

<img src="Norway_files/figure-html/eco-2.png" style="display: block; margin: auto;" />

```r
tmp_Eco_Generation %>% filter(Year == 2013) %>% mutate(Share = round(Share*100, digits = 1))       
```

```
##   Year Generation Share
## 1 2013     [0,20)  24.8
## 2 2013    [20,65)  59.5
## 3 2013   [65,100]  15.7
```

Similarly to biological generation, we see that:

* Share of pre-productive generation is constantly decreasing from, while share of post-productive is increasing with approximately the same speed. This is causing constant increase of ratio of seniority
* Productive generation is more or less constant in time with slight upward trend and average `57%` of population during last 55 years. 

This means that total dependency ratio is slightly decreasing in time, mostly due to decrease of young-age dependency ratio.

### Indicators

Now we will calculate all the indicators, which are applicable to biological and economical generations and interpret their meaning. Also, we will show development of those indicators in time:


```r
Ratios <- tmp_Population_Age_Sex_Gen %>% #filter (Year >= 2000) %>% 
    dcast(Year ~ Sex, value.var = "Population", fun.aggregate = sum, na.rm = T) %>%
    mutate(Im = M/F, If = F/M) %>% select(Year, Im, If)

Ratios <- tmp_Bio_Generation %>% #filter(Year >= 2000) %>%
    dcast(Year ~ Generation, value.var = "Share", fun.aggregate = sum, na.rm = T) %>%
    setNames(c("Year", "I", "II","III")) %>% return %>%
    mutate(Sauvy = III/I) %>% select(Year, Sauvy) %>% merge(Ratios, by = "Year")

Ratios <- tmp_Eco_Generation %>% #filter(Year >= 2000) %>%
  dcast(Year ~ Generation, value.var = "Share", fun.aggregate = sum, na.rm = T) %>%
  setNames(c("Year", "I", "II","III")) %>% return %>%
  mutate(TDR = (I+III)/II, OADR = III/II, JADR = I/II, Seniority = III/I) %>% 
  select(-I,-II,-III) %>% merge(Ratios, by = "Year")

Ratios %>% filter((Year %% 10 == 0) | Year > 2000) %>% melt(id = c("Year")) %>%
  mutate(value = round(value, 3)) %>% dcast(Year ~ variable)
```

```
##    Year   TDR  OADR  JADR Seniority Sauvy    Im    If
## 1  1960 0.785 0.195 0.590     0.330 1.068 0.993 1.007
## 2  1970 0.822 0.233 0.589     0.396 1.232 0.990 1.010
## 3  1980 0.807 0.265 0.542     0.489 1.405 0.983 1.017
## 4  1990 0.748 0.285 0.462     0.617 1.589 0.978 1.022
## 5  2000 0.699 0.259 0.440     0.589 1.571 0.980 1.020
## 6  2001 0.695 0.256 0.440     0.581 1.577 0.982 1.018
## 7  2002 0.692 0.253 0.439     0.575 1.589 0.982 1.018
## 8  2003 0.688 0.250 0.439     0.570 1.602 0.983 1.018
## 9  2004 0.687 0.249 0.439     0.567 1.624 0.983 1.017
## 10 2005 0.687 0.248 0.439     0.566 1.651 0.984 1.017
## 11 2006 0.686 0.248 0.438     0.566 1.681 0.984 1.016
## 12 2007 0.683 0.246 0.437     0.565 1.711 0.987 1.013
## 13 2008 0.679 0.246 0.434     0.567 1.735 0.993 1.008
## 14 2009 0.676 0.246 0.430     0.572 1.755 0.996 1.004
## 15 2010 0.678 0.250 0.428     0.583 1.774 0.998 1.002
## 16 2011 0.678 0.253 0.425     0.596 1.795 1.001 0.999
## 17 2012 0.679 0.259 0.420     0.615 1.822 1.005 0.995
## 18 2013 0.680 0.263 0.417     0.631 1.844 1.008 0.992
## 19 2014 0.681 0.267 0.413     0.647 1.870 1.010 0.990
## 20 2015 0.681 0.271 0.410     0.663 1.903 1.012 0.988
```

```r
## Development of ratios in time
# Masculinity index
Ratios %>% select(Year, Im, If) %>% melt(id = "Year") %>%
  setNames(c("Year", "Index", "Value")) %>% return %>%
  ggplot(aes(x = Year, y = Value, color = Index)) + geom_point() + geom_line() +
  ylim(c(0.9,1.1)) + ggtitle("Development of masculinity index in time\nGermany") +
  geom_vline(xintercept = 2011, size = 0.5, linetype = "longdash")
```

<img src="Norway_files/figure-html/indicators-1.png" style="display: block; margin: auto;" />

```r
# Sauvy
ggplot(Ratios, aes(x = Year, y = Sauvy)) + geom_point() + geom_line() +
  ggtitle("Development of Sauvy index in time\nNorway") + ylim(c(0,2))
```

<img src="Norway_files/figure-html/indicators-2.png" style="display: block; margin: auto;" />

```r
# Economical generations
Ratios %>% select(Year, TDR, OADR, JADR, Seniority) %>% melt(id = "Year") %>%
  setNames(c("Year", "Index", "Value")) %>% return %>%
  ggplot(aes(x = Year, y = Value, color = Index)) + geom_point() + geom_line() +
  ylim(0:1) + ggtitle("Development of indeces of economical generations in time\nNorway")
```

<img src="Norway_files/figure-html/indicators-3.png" style="display: block; margin: auto;" />

Conclusions from three graphs and table above:

1. For the period of 1960-2000 number of females was higher than males and this proportion was increasing. After 2000 started falling and in 2011 there was nearly equal amount of males and female in Norway. Since then, proportion was in favor of males with steady increasing trend.

2. Sauvy's index can be simply interpreted as number of grandparents per one child. It increased from `1.07` in 1960 to `1.9` in 2015, which confirms our previous statement about Norway transforming from stationary type of population into regressive one. Value of `1.9` is still quite reasonable and healthy for the country.

3. Total dependency ratio was slowly decreasing from `0.78` to `0.68`, which is mostly due to significant fall of young-age dependency ratio (from `0.59` to `0.41`). This level was always in the safe zone - below threshold value of `1`. Index of seniority is behaving in the similar manner to the Sauvy's index, which is expected.




## Education

Education is an indicator of qualitative development of population of the country. We will look at the distribution of population by reached educational level by age and sex. Also, we'll look how this distribution was changing in time:


```r
tmp_Education <- dt_Education %>% group_by(Age, Sex, Year) %>% summarize(Pop = sum(Count)) %>% 
  merge(dt_Education) %>% mutate(Share = 100 * Count / Pop)

tmp_Education$Education <- tmp_Education$Education %>% 
  factor(levels(tmp_Education$Education)[c(2,4,1,5,3)], ordered = T)

tmp_Education %>%
  filter(Year %in% c(2007, 2014) & Age >= 15) %>% 
  mutate(Year = factor(Year), `Age Group` = Age - Age %% 5) %>%
  group_by(Year, `Age Group`, Education, Sex) %>% summarise(Share = 100*sum(Count)/sum(Pop)) %>% 
  ggplot(aes(x = `Age Group`, y = Share, color = Education, fill = Education)) +
  geom_area() + facet_grid(Sex ~ Year) + 
  ggtitle("Educational structure of Norway by sex and 5-year age groups") +
  theme(legend.position = "bottom", legend.direction="horizontal")
```

<img src="Norway_files/figure-html/education-1.png" style="display: block; margin: auto;" />

Education is quite a specific indicator because, similarly to population pyramid, it stores nation's memory. Highest level of education is usually reached at 20-25 years, which means that people in 2014 of age 60 represent cohort that was receiving their education (highest level) at 1974-1979. This kind of memory allows us to estimate historical distribution of 20-25 year-old population by education.

Unfortunately, this estimate strongly depends on 2 elements of population change: death rate and migration rate. For our estimate to be true, there should be no correlation between person's educational level and both death rate and migration. In case of death rate such an assumption can be viable, but for migration it seems unprobable. First of all, people with higher level of education have more opportunities to emigrate, while people with lower education are more grounded. Secondly, with the latest trends in European education, significant part of immigration is either already with tertiary education or is moving to European countries such as Norway to receive higher education (and then, maybe emigrate). Therefore, we see a peak (mode) in share of tretiary education at `25-35` age range and then steady decrease.

Regarding the difference between males and females: it seems that females have higher share of tertiary at the mode zone of `25-35`, but it is followed with much sharper decrease than for males and at the age `60-65` share tertiary education of females goes below the share for females.

Further, we will look at the development of education structure of age group `25-35` in time:


```r
tmp_Education %>% 
  filter(Age >= 25 & Age <= 35) %>% 
  group_by(Sex, Year, Education) %>%
  summarize(Share = sum(Count)/sum(Pop) * 100) %>%
  ggplot(aes(x = Year, y = Share, color = Education, fill = Education)) +
  geom_area() + facet_grid(. ~ Sex) +
  theme(legend.position = "bottom", legend.direction="horizontal") +
  ggtitle("Development of education structure\nof Norway's population for age group 25-35")
```

<img src="Norway_files/figure-html/education_time-1.png" style="display: block; margin: auto;" />

We can notice a suspicious change in distribution from 2011 to 2012, which looks like `Unknown` catogory was partially identified and redistributed between other categories: tertiary for females, tertiary and upper secondary for males. If to neglect this jump, trends were the following:

* Share of lower secondary (also includes primary) education is mostly stable in time and higher for male population
* Share of upper secondary education is steadily decreasing and share for males is higher
* Share of tertiary education is increasing in time and it is significantly higher for females

Conclusions regarding educationcal structure of Norwish population:

1. Females are more educated than males
2. Steadily increasing trend of share of higher (tertiary) education in Norway
3. High migration of people, who receive higher (tertiary) education in Norway

## Marriages

Marriages and partnerships are a vital part of normal development of society. Unfortunately, according to the new trends it is not the main source of fertility (less than 50% or childbirths, details in section `Population change`).

Let's have a quick look at the general trends of marriage indicators in Norway:


```r
tmp_Marriage_General <- dt_Marriage_Age %>% 
  group_by(Year) %>% summarize(Marriages = sum(Count)) %>%
  merge(dt_Population_general %>% filter(Indicator == "Average population") %>% 
          group_by(Year) %>% summarize(Pop = sum(Value, na.rm = T))) %>%
  mutate(`Crude Marriage Rate` = Marriages/Pop*1000) %>% select(-Pop) %>%
  melt(id = "Year") %>% select(variable, Year, value) %>% 
  setNames(c("Indicator", "Year", "Value")) %>% rbind(dt_Divorce)

tmp_Marriage_General %>%
  dcast(Year ~ Indicator, value.var = "Value") %>%
  mutate(Marriages = comma(Marriages), 
         `Crude Marriage Rate` = round(`Crude Marriage Rate`, 2),
         Divorces = comma(Divorces))
```

```
##    Year Marriages Crude Marriage Rate Crude divorce rate Divorces
## 1  2000        NA                  NA                2.2   10,053
## 2  2001        NA                  NA                2.3   10,308
## 3  2002        NA                  NA                2.3   10,450
## 4  2003        NA                  NA                2.4   10,757
## 5  2004        NA                  NA                2.4   11,045
## 6  2005        NA                  NA                2.4   11,040
## 7  2006        NA                  NA                2.3   10,598
## 8  2007    33,053                7.02                2.2   10,280
## 9  2008    35,287                7.40                2.1   10,158
## 10 2009    34,292                7.10                2.1   10,235
## 11 2010    33,031                6.76                2.1   10,224
## 12 2011    31,743                6.41                2.1   10,188
## 13 2012    33,692                6.71                2.0    9,906
## 14 2013    34,018                6.70                2.0   10,212
## 15 2014    33,350                6.49                1.9    9,918
##    Divorces per 100 marriages
## 1                        44.5
## 2                        52.3
## 3                        51.3
## 4                        48.1
## 5                        59.2
## 6                        49.3
## 7                        48.8
## 8                        43.8
## 9                        40.4
## 10                       42.1
## 11                       43.9
## 12                       44.5
## 13                       41.1
## 14                       42.7
## 15                       42.3
```

```r
tmp_Marriage_General %>%
  filter(Year >= 2007 & Indicator %in% c("Crude Marriage Rate", "Crude divorce rate")) %>%
  ggplot(aes(x = Year, y = Value, color = Indicator)) + geom_line() + geom_point() +
  ylab("Crude rate") + ggtitle("Development of marriage indicators in time in Norway") +
  scale_y_continuous(limits = c(0,8))
```

<img src="Norway_files/figure-html/marriage_intro-1.png" style="display: block; margin: auto;" />

Crude marriage rate is showing decreasing trend, which may be an explanation to the increase of share of childbirths outside the marriage. Crude divorce rate is quite stable in time with slight decrease.

Now let's look in details on marriages' statistics:


```r
dt_Marriage_Age %>% 
  filter(Year %in% c(2007, 2010, 2014)) %>% mutate(Year = factor(Year)) %>%
  ggplot(aes(x = Age %>% as.numeric(), y = Count, color = Year)) + 
  geom_line() + geom_point() + facet_grid(Sex ~ .) +
  theme(legend.position = "bottom", legend.direction="horizontal") +
  ggtitle("Distribution of number of marriages in Norway by age groups and sex") +
  ylab("Number of marriages") + xlab("Age Group") +
  scale_x_discrete(breaks = 1:8, labels = levels(dt_Marriage_Age$Age))
```

<img src="Norway_files/figure-html/marriage_main-1.png" style="display: block; margin: auto;" />

```r
dt_Marriage_PrevStatus %>% filter(Year != 2007) %>%
  merge(dt_Marriage_PrevStatus %>% group_by(Sex, Year) %>% 
          summarize(Tot = sum(Count, na.rm = T)), by = c("Sex", "Year")) %>% 
  mutate(Share = Count/Tot*100) %>% arrange(MARSTA) %>%
  ggplot(aes(x = Year, y = Share, fill = MARSTA)) +
  geom_area() + facet_grid(. ~ Sex) + 
  theme(legend.position = "bottom", legend.direction="horizontal") +
  ggtitle("Structure by marital status of bride and groom in Norway")
```

<img src="Norway_files/figure-html/marriage_main-2.png" style="display: block; margin: auto;" />

```r
dt_Marital %>% filter(Year %in% c(2000, 2015) & Age >= 15) %>%
  group_by(Age, Sex, Year) %>% summarize(Tot = sum(Count, na.rm = T)) %>%
  merge(dt_Marital) %>% arrange(Marsta) %>% 
  mutate(Share = 100*Count/Tot, Year = factor(Year),
         Marsta = factor(Marsta, levels(Marsta)[c(5,2,3,1,4,6)], ordered = T)) %>%
  ggplot(aes(x = Age, y = Share, fill = Marsta)) + geom_area() +
  facet_grid(Year ~ Sex) + theme(legend.position = "bottom", legend.direction="horizontal") +
  ggtitle("Structure of population of Norway by marital status") +
  scale_fill_brewer(palette = 2, type = "qual")
```

```
## Warning: Removed 170 rows containing missing values (position_stack).

## Warning: Removed 170 rows containing missing values (position_stack).
```

<img src="Norway_files/figure-html/marriage_main-3.png" style="display: block; margin: auto;" />

```r
dt_Marital %>% filter(Age == 50) %>%
  group_by(Age, Sex, Year) %>% summarize(Tot = sum(Count, na.rm = T)) %>%
  merge(dt_Marital) %>% arrange(Marsta) %>% 
  mutate(Share = Count/Tot) %>%
  filter(Marsta == "Single") %>%
  ggplot(aes(x = Year, y = Share, color = Sex)) +
  geom_point() + geom_line() + ylim(c(0, 0.5)) +
  ggtitle("Share of Norwish population, which have not been\nmarried before age of 50")
```

<img src="Norway_files/figure-html/marriage_main-4.png" style="display: block; margin: auto;" />

```r
cat("Average age of population groups by marital status")
```

```
## Average age of population groups by marital status
```

```r
dt_Marital %>% filter(Age >= 15 & Year == 2015) %>%
  group_by(Sex, Marsta) %>% 
  summarize(Average_Age = round(sum(Age * Count, na.rm = T)/sum(Count, na.rm = T),1)) %>%
  dcast(Marsta ~ Sex, value.var = "Average_Age")
```

```
##                   Marsta    F    M
## 1               Divorced 56.1 56.5
## 2                Married 52.3 54.7
## 3 Registered.partnership 51.9 52.4
## 4              Separated 46.7 50.2
## 5                 Single 31.2 32.3
## 6                Widowed 77.7 76.3
```

Conclusions regarding marriages and marital structure of population in Norway:

1. Males tend to marry at later age than females: mode for males is `30-34`, while `25-29` for females
2. Males tend to re-marry more: share of divorced males in new marriages is higher than females; trend is slowly decreasing
3. Share of widowers in the old age is significantly higher for females, which can be explained by higher life expectancy for females
4. Current visible trend is to marry at later age: share of population, which have not been married before age of 50, had increased from `10%` to `25%` in last 15 years. For females this indicator is on the lower level


# Population change

After analyzing population size and structure, which is based on annual snapshots, it is a logical step to look at what is happenning with the population between those snapshots: births, deaths, migration.

Here's a general picture of those changes as percentage of the average population during the year:


```r
tmp_Population_General <- dt_Population_general %>%
  filter(!(Indicator %in% c("Average population", "Population Start"))) %>%
  group_by(Indicator, Year) %>% summarize(Value = sum(Value, na.rm = T)) %>%
  merge(dt_Population_general %>% filter(Indicator == "Average population") %>% 
          group_by(Year) %>% summarize(Pop = sum(Value, na.rm = T)), by = "Year") %>%
  mutate(Value = round(Value/Pop*100, 3))

tmp_Population_General %>%
  dcast(Year ~ Indicator, fun.aggregate = sum, na.rm = T, value.var = "Value")
```

```
##    Year Deaths Live births Natural change Net migration
## 1  2000  0.980       1.318          0.339         0.216
## 2  2001  0.974       1.256          0.282         0.175
## 3  2002  0.980       1.221          0.242         0.379
## 4  2003  0.931       1.236          0.306         0.246
## 5  2004  0.897       1.240          0.343         0.286
## 6  2005  0.892       1.228          0.336         0.397
## 7  2006  0.885       1.256          0.371         0.507
## 8  2007  0.891       1.241          0.350         0.839
## 9  2008  0.875       1.269          0.394         0.908
## 10 2009  0.858       1.280          0.422         0.799
## 11 2010  0.849       1.257          0.408         0.862
## 12 2011  0.836       1.216          0.380         0.944
## 13 2012  0.837       1.201          0.364         0.939
## 14 2013  0.810       1.159          0.349         0.767
## 15 2014  0.786       1.148          0.362         0.777
##    Total population change
## 1                    0.555
## 2                    0.457
## 3                    0.621
## 4                    0.552
## 5                    0.629
## 6                    0.732
## 7                    0.878
## 8                    1.190
## 9                    1.302
## 10                   1.221
## 11                   1.270
## 12                   1.324
## 13                   1.303
## 14                   1.116
## 15                   1.139
```

```r
tmp_Population_General %>% 
  filter(!(Indicator %in% c("Net migration", "Total population change"))) %>%
  ggplot(aes(x = Year, y = Value, color = Indicator)) + geom_line() + geom_point() + ylim(c(0,1.5)) +
  ggtitle("Norway's natural population change indicators in time\nPercent of average population during the year")
```

<img src="Norway_files/figure-html/general_population-1.png" style="display: block; margin: auto;" />

```r
dt_Population_general %>%
  filter(Indicator %in% c("Deaths", "Live births")) %>%
  group_by(Indicator, Year, SEX) %>% summarize(Value = sum(Value, na.rm = T)) %>%
  merge(dt_Population_general %>% filter(Indicator == "Average population") %>% 
          group_by(Year, SEX) %>% summarize(Pop = sum(Value, na.rm = T)), by = c("Year", "SEX")) %>%
  mutate(Value = round(Value/Pop*100, 2)) %>% 
  dcast(Year + SEX ~ Indicator, fun.aggregate = sum, value.var = "Value") %>%
  mutate(`Natural Change` = `Live births` - Deaths) %>% melt(id = c("Year", "SEX")) %>%
  setNames(c("Year", "Sex", "Indicator", "Share")) %>% return %>%
  ggplot(aes(x = Year, y = Share, color = Indicator)) + geom_line() + geom_point() + facet_grid(. ~ Sex) + 
  ggtitle("Natural change of Norway's population by sex in time\nPercent of average population during the year")
```

<img src="Norway_files/figure-html/general_population-2.png" style="display: block; margin: auto;" />

```r
tmp_Population_General %>% filter(Indicator %in% c("Net migration", "Total population change", "Natural change")) %>%
  ggplot(aes(x = Year, y = Value, color = Indicator)) + geom_line() + geom_point() + ylim(c(0,1.5)) +
  ggtitle("Norway's migration and overall change indicators in time\nPercent of average population during the year")
```

<img src="Norway_files/figure-html/general_population-3.png" style="display: block; margin: auto;" />

These graphs and table were supposed to give general impression on the influence of different factors on the change of Norway's population. Preliminary conslusions are as follows:

1. Norway is constantly demonstating positive natural change of population (for the last 15 years). Death rate is steadily decreasing, while natality rate has slower decrease, which causes slow increase of natural change rate.

2. Major influence on total population change is cause by net migration. Since 2007, net migration is significantly higher than natural change (which is quite stable), so total population change is practically shadowing fluctuation of net migration.

3. Males demonstate slightly lower share of deaths and higher level of births, which results in higher positive level of natural population change. Also, this explains the change in the index of masculinity in the section above.

Further sections will address these components of population change in detail.

## Fertility

First of all, let's review some basic fertility indicators of Norway's population and their change in time:


```r
dt_Fertility %>% filter(Year %% 5 == 0 | Year > 2010) %>% 
  dcast(Indicator ~ Year, fun.aggregate = sum, value.var = "Value")
```

```
##                                    Indicator  2000  2005  2010  2011  2012
## 1  Mean age of women at birth of first child 26.90 27.70 28.00 28.20 28.40
## 2            Mean age of women at childbirth 29.30 29.80 30.10 30.30 30.30
## 3         Percentage first order live births 40.30 41.60 43.60 42.70 42.60
## 4 Proportion of live births outside marriage 49.60 51.80 54.80 55.00 54.90
## 5                       Total fertility rate  1.85  1.84  1.95  1.88  1.85
##    2013  2014
## 1 28.60 28.70
## 2 30.50 30.60
## 3 43.10    NA
## 4 55.20    NA
## 5  1.78  1.75
```

```r
dt_Fertility %>% 
  filter(Indicator %in% c("Mean age of women at birth of first child", "Mean age of women at childbirth")) %>%
  ggplot(aes(x = Year, y = Value, color = Indicator)) + geom_line() + geom_point() + ylim(c(20, 32)) +
  theme(legend.position = "bottom", legend.direction="vertical") + ylab("Age") +
  ggtitle("Mean age of women at childbirth in Norway")
```

<img src="Norway_files/figure-html/fertility_general-1.png" style="display: block; margin: auto;" />

```r
dt_Fertility %>% 
  filter(Indicator == "Total fertility rate") %>%
  ggplot(aes(x = Year, y = Value)) + geom_line() + geom_point() + ylim(c(1.5, 2)) +
  ylab("Fertility Rate") + ggtitle("Total fertility rate in Norway\nwith linear regression fit") + 
  stat_smooth(method = "lm")
```

<img src="Norway_files/figure-html/fertility_general-2.png" style="display: block; margin: auto;" />

```r
dt_Fertility %>% 
  filter(Indicator %in% c("Percentage first order live births", "Proportion of live births outside marriage") & Year != 2014) %>%
  ggplot(aes(x = Year, y = Value, color = Indicator)) + geom_line() + geom_point() + ylim(c(40, 60)) +
  theme(legend.position = "bottom", legend.direction="vertical") + ylab("Percent") +
  ggtitle("Other childbirth indicators in Norway")
```

<img src="Norway_files/figure-html/fertility_general-3.png" style="display: block; margin: auto;" />

We can make the following conclusions from the graphs and table:

1. Mean age of women at childbirth is steadily increasing, which is in line with the aging of population. This trend is present in both all childbirth and birth of firts child.

2. Total fertility rate is fluctiacting for the last 15 years with no definite upward or downward trend visible. Fitted linear regression shows slight increase, but the 95% confidence interval is quite wide to make some data-based forecast. Regarding the value of total rertility rate, it's always below recommended `2.3` for the stable population reproduction.

3. Proportion of live births outside marriage is quite high and especially drastic growth was during 2003-2008 from `50%` to `55%`. This one of the new trends in Western countries.

4. Percentage of first order live births is increasing, which means that average number of children per family is lowering, but number of families should increasing. Although, it can be connected with the increasing trend of propotion of births outside of marriage.

Next step will be to look at the sex ratio of newborns and distribution of age of mothers:


```r
dt_Fertility_Age_Sex %>%
  filter(Year %in% c(2007,2010, 2013) & !(AGE %in% c("[10,15)", "[50,100)"))) %>%
  ggplot(aes(x = AGE, y = Count)) + facet_grid(Year ~ Sex) +
  geom_bar(stat = "identity") + ylab("Count of births") + xlab("Mother's age group") +
  ggtitle("Birth in Norway by child's sex and mother's age")
```

<img src="Norway_files/figure-html/ferility_age_sex-1.png" style="display: block; margin: auto;" />

```r
dt_Fertility_Age_Sex %>%
  filter(Year %in% c(2007,2010, 2013) & !(AGE %in% c("[10,15)", "[50,100)"))) %>%
  mutate(Year = factor(Year)) %>%
  ggplot(aes(x = AGE, y = Count, color = Year)) + facet_grid(. ~ Sex) +
  geom_point() + ylab("Count of births") + xlab("Mother's age group") +
  ggtitle("Birth in Norway by child's sex and mother's age") +
  theme(legend.position = "bottom", legend.direction="horizontal")
```

<img src="Norway_files/figure-html/ferility_age_sex-2.png" style="display: block; margin: auto;" />

```r
dt_Fertility_Age_Sex %>%
  group_by(Year, Sex) %>% summarize(Pop = sum(Count, na.rm = T)) %>%
  dcast(Year ~ Sex, fun.aggregate = sum, value.var = "Pop") %>% 
  mutate(Im = M/F) %>% 
  ggplot(aes(x = Year, y = Im)) + geom_line() + geom_point() +
  geom_hline(yintercept = 1, linetype = "longdash", size = .3) +
  ggtitle("Childbirth index of masculinity in Norway\nwith fitted linear regression model") + 
  ylab("Index of Masculinity") + ylim(c(.95, 1.1)) +
  stat_smooth(method = "lm", se = F)
```

<img src="Norway_files/figure-html/ferility_age_sex-3.png" style="display: block; margin: auto;" />

From the graphs above we can make the following conclusions:

1. First 2 graphs show that children are mainly born by women in age of 25-30 years. This distribution is similar for females and males and is stable in time.

2. Index of masculinity of the newborn was quite stable in the last 7 years with average around `1.055`. This value will be later used for population projection calculation.

And final step will be analysis of annual fertility rates by women's age. This will be one more input into population projection's calculations later on:


```r
dt_Fertility_Rate %>% filter(Year %in% c(2000, 2005, 2010, 2014)) %>%
  mutate(Year = factor(Year)) %>%
  ggplot(aes(x = Age, y = Fertility_Rate, color = Year)) +
  geom_line() + ylab("Fertility Rate") +
  theme(legend.position = "bottom", legend.direction="horizontal") +
  ggtitle("Annual fertility rate in Norway by women's age")
```

<img src="Norway_files/figure-html/ferility_annual-1.png" style="display: block; margin: auto;" />

From this graph we see the following:

* High fertility rate (above `0.1`) is for women at age 25-33, similar to previous graphs.
* Mode fertility rate is concentrated at the age of 28-32.
* Distribution is moving to the right (the whole shape and mode) with time, which is aligned with increase of mean mother's age at the childbirth and general aging of the population

Overall, we can see quite a stable situation in the fertility with patterns of population aging.

## Mortality

First important indicator of mortality, which should have been analysed in fertility section, is infant mortality rate:


```r
tmp_Mortality_Rate <- dt_Death_Age %>% 
  merge(dt_Population_Age_Sex, by = c("Year", "Sex", "Age")) %>%
  mutate(`Mortality Rate` = Deaths/Population)

tmp_Mortality_Rate %>%
  filter(Age == 0) %>% 
  ggplot(aes(x = Year, y = `Mortality Rate`, color = Sex)) +
  geom_point() + geom_line() + ylim(c(0, .005)) + ylab("Infant Mortality Rate") +
  stat_smooth(method = "lm", se = F, linetype = "longdash") +
  ggtitle("Infant mortality rate in Norway by sex\nwith fitted linear regression model")
```

<img src="Norway_files/figure-html/infant_mortality-1.png" style="display: block; margin: auto;" />

```r
tmp_Mortality_Rate %>%
  filter(Year %in% c(2000, 2005, 2010, 2014) & Age > 0 & Age <= 40) %>% 
  mutate(Year = factor(Year), `Age Group` = Age - Age %% 5) %>%
  group_by(Year, Sex, `Age Group`) %>% summarize(`Mortality Rate` = sum(Deaths)/sum(Population)) %>%
  ggplot(aes(x = `Age Group`, y = `Mortality Rate`, color = Year)) +
  theme(legend.position = "bottom", legend.direction="horizontal") +
  geom_line() + facet_grid(. ~ Sex) + ggtitle("Mortality rate by age and sex\nfor population of age 1-40")
```

<img src="Norway_files/figure-html/infant_mortality-2.png" style="display: block; margin: auto;" />

```r
tmp_Mortality_Rate %>%
  filter(Year %in% c(2000, 2005, 2010, 2014) & Age >= 40 & Age <= 70) %>% 
  mutate(Year = factor(Year), `Age Group` = Age - Age %% 5) %>%
  group_by(Year, Sex, `Age Group`) %>% summarize(`Mortality Rate` = sum(Deaths)/sum(Population)) %>%
  ggplot(aes(x = `Age Group`, y = `Mortality Rate`, color = Year)) +
  theme(legend.position = "bottom", legend.direction="horizontal") +
  geom_line() + facet_grid(. ~ Sex) + ggtitle("Mortality rate by age and sex\nfor population of age 40-70")
```

<img src="Norway_files/figure-html/infant_mortality-3.png" style="display: block; margin: auto;" />

```r
tmp_Mortality_Rate %>%
  filter(Year %in% c(2000, 2005, 2010, 2014) & Age >= 70) %>% 
  mutate(Year = factor(Year), `Age Group` = Age - Age %% 5) %>%
  group_by(Year, Sex, `Age Group`) %>% summarize(`Mortality Rate` = sum(Deaths)/sum(Population)) %>%
  ggplot(aes(x = `Age Group`, y = `Mortality Rate`, color = Sex)) +
  theme(legend.position = "bottom", legend.direction="horizontal") +
  geom_line() + facet_wrap( ~ Year) + ggtitle("Mortality rate by age and sex\nfor population of age 70+")
```

<img src="Norway_files/figure-html/infant_mortality-4.png" style="display: block; margin: auto;" />

```r
tmp_Mortality_Rate %>%
  filter(Year %in% c(2000, 2014)) %>% mutate(Year = factor(Year), `Age Group` = Age - Age %% 5) %>%
  group_by(Year, Sex, `Age Group`) %>% summarize(Deaths = sum(Deaths)) %>%
  ggplot(aes(x = `Age Group`, y = Deaths)) + geom_bar(stat = "identity") +
  facet_grid(Year ~ Sex) + ggtitle("Death in Norway by sex and 5-year age groups")
```

<img src="Norway_files/figure-html/infant_mortality-5.png" style="display: block; margin: auto;" />

Following conslusions can be done from the graphs above:

1. Infant mortality rate is decreasing in the last 15 years for both females and males with same speed. Also, infant mortality for males is usually higher, than for the females.

2. Mortality rate is higher for males than for females at any age.

3. Mortality rate is decreasing with time for any age. It can be noticed on second and third graphs that lines for later years are always lower.

4. Distribution of deaths by age is moving to the right and lowering with time, which means that average life expectancy is increasing. Trend is similar for both males and females.

Mortality rates by age and sex were calculated from Eurostat data on number of deaths and data on population structure by age and sex. These values will be used to calculate life table and population projection.

*Note: 5-year age groups on the graphs are marked with single integer instead of range description (`0,5,10` instead of `0-4,5-9,10-14`). That single integer indicates the first year of age group, for example: `0` means group `[0,5)`, `5` - `[5,10)`, `10` - `[10,15)`. This notation will be also used in the next sections.*

## Migration

Migration is not a part of natural change of population, but, as we've seen above, it was the biggest part of total population change in the last 10 years. Analysis of migration will be similar to mortality analysis: distribution by age and migration rates. Unfortunately, detailed data for such kind of analysis was available 


```r
tmp_Migration_Rate <- dt_Migration %>%
  merge(dt_Population_Age_Sex, by = c("Year", "Sex", "Age")) %>%
  mutate(Year = factor(Year))

tmp_Migration_Rate %>%
  mutate(`Age Group` = Age - Age %% 5) %>% group_by(`Age Group`, Sex, Year) %>%
  summarize(Immigration = sum(Immigration)/sum(Population), Emigration = sum(Emigration)/sum(Population),
         Net_Migration = Immigration - Emigration) %>%
  melt(id = c("Year", "Sex", "Age Group")) %>% filter(variable != "Population") %>%
  setNames(c("Year", "Sex", "Age Group", "Indicator", "Value")) %>%
  ggplot(aes(x = `Age Group`, y = Value, color = Indicator)) + geom_line() +
  geom_point() + facet_grid(Year ~ Sex) + ylab("Migration Rate") + 
  ggtitle("Migration rates in Norway by sex and 5-year age groups") +
  theme(legend.position = "bottom", legend.direction="horizontal")
```

<img src="Norway_files/figure-html/migration-1.png" style="display: block; margin: auto;" />

```r
tmp_Migration_Rate %>%
  mutate(`Age Group` = Age - Age %% 5) %>% group_by(`Age Group`, Sex, Year) %>%
  summarize(Immigration = sum(Immigration)/sum(Population), Emigration = sum(Emigration)/sum(Population),
         Net_Migration = Immigration - Emigration) %>%
  melt(id = c("Year", "Sex", "Age Group")) %>% filter(variable != "Population") %>%
  setNames(c("Year", "Sex", "Age Group", "Indicator", "Value")) %>%
  ggplot(aes(x = `Age Group`, y = Value, color = Sex)) + geom_line() +
  geom_point() + facet_grid(Indicator ~ Year) + ylab("Migration Rate") + 
  ggtitle("Migration rates in Norway by sex and 5-year age groups") +
  theme(legend.position = "bottom", legend.direction="horizontal")
```

<img src="Norway_files/figure-html/migration-2.png" style="display: block; margin: auto;" />

```r
tmp_Migration_Rate %>%
  mutate(`Age Group` = Age - Age %% 5) %>% group_by(`Age Group`, Sex) %>%
  summarize(Immigration = sum(Immigration/2), Emigration = sum(Emigration/2), Net_Migration = Immigration - Emigration) %>%
  melt(id = c("Sex", "Age Group")) %>% filter(variable != "Population") %>%
  setNames(c("Sex", "Age Group", "Indicator", "Value")) %>%
  ggplot(aes(x = `Age Group`, y = Value)) + geom_bar(stat = "identity") +
  facet_grid(Sex ~ Indicator) + ylab("# of people") + 
  ggtitle("Migration in Norway by sex and 5-year age groups") +
  theme(legend.position = "bottom", legend.direction="horizontal")
```

```
## Warning: Stacking not well defined when ymin != 0

## Warning: Stacking not well defined when ymin != 0
```

<img src="Norway_files/figure-html/migration-3.png" style="display: block; margin: auto;" />

Following conclusions can be made from the migration graphs:

1. In general, emigration and immigration have similar behavioural pattern by age groups. Although, males' emigration significantly heavier right tale: females' emigration is dropping to zero after age of 55, but males are still quite actively engaged in emigration. On the other hand, tend to emigrate more than males at the age of 20-24.

2. Mode of both emigration and immigration is the `25-29` age group, but `20-24` and `30-34` also demonstrate high levels of migration activity.


# Life Tables

## Calculation

Life tables for females and males will be prepared based on data, described and visualized in the sections above: 

* Population by age and sex, deaths by age and sex
* Data from years 2010-2015 will be taken into account and averaged in the final table
* Age horizon is from 0 to 100

First, let's calculated age-specific mortality rate by sex:


```r
life_Mortality <- dt_Death_Age %>% 
  filter(Year >= 2010) %>%
  merge(dt_Population_Age_Sex, by = c("Year", "Sex", "Age")) %>%
  group_by(Sex, Age) %>% 
  summarize(`Mortality Rate` = sum(Deaths, na.rm = T)/(sum(Population, na.rm = T)-sum(Deaths, na.rm = T)/2))

life_Mortality %>% mutate(`Mortality Rate` = round(`Mortality Rate`, 4)) %>%
  dcast(Age ~ Sex, value.var = "Mortality Rate")
```

```
##     Age      F      M
## 1     0 0.0022 0.0027
## 2     1 0.0003 0.0002
## 3     2 0.0001 0.0001
## 4     3 0.0001 0.0001
## 5     4 0.0001 0.0001
## 6     5 0.0001 0.0001
## 7     6 0.0000 0.0001
## 8     7 0.0001 0.0001
## 9     8 0.0001 0.0001
## 10    9 0.0001 0.0001
## 11   10 0.0001 0.0001
## 12   11 0.0001 0.0001
## 13   12 0.0000 0.0001
## 14   13 0.0001 0.0001
## 15   14 0.0001 0.0001
## 16   15 0.0002 0.0001
## 17   16 0.0002 0.0003
## 18   17 0.0003 0.0004
## 19   18 0.0003 0.0005
## 20   19 0.0002 0.0005
## 21   20 0.0003 0.0007
## 22   21 0.0003 0.0007
## 23   22 0.0003 0.0007
## 24   23 0.0002 0.0006
## 25   24 0.0002 0.0007
## 26   25 0.0003 0.0007
## 27   26 0.0003 0.0007
## 28   27 0.0002 0.0008
## 29   28 0.0002 0.0008
## 30   29 0.0003 0.0008
## 31   30 0.0003 0.0008
## 32   31 0.0004 0.0008
## 33   32 0.0004 0.0008
## 34   33 0.0003 0.0007
## 35   34 0.0004 0.0009
## 36   35 0.0004 0.0009
## 37   36 0.0005 0.0009
## 38   37 0.0005 0.0009
## 39   38 0.0005 0.0009
## 40   39 0.0005 0.0011
## 41   40 0.0006 0.0011
## 42   41 0.0007 0.0012
## 43   42 0.0008 0.0011
## 44   43 0.0008 0.0014
## 45   44 0.0009 0.0014
## 46   45 0.0010 0.0017
## 47   46 0.0012 0.0018
## 48   47 0.0014 0.0020
## 49   48 0.0014 0.0023
## 50   49 0.0017 0.0024
## 51   50 0.0019 0.0027
## 52   51 0.0022 0.0030
## 53   52 0.0022 0.0033
## 54   53 0.0023 0.0036
## 55   54 0.0029 0.0040
## 56   55 0.0032 0.0047
## 57   56 0.0033 0.0049
## 58   57 0.0036 0.0053
## 59   58 0.0036 0.0061
## 60   59 0.0044 0.0067
## 61   60 0.0049 0.0074
## 62   61 0.0049 0.0081
## 63   62 0.0059 0.0089
## 64   63 0.0060 0.0095
## 65   64 0.0068 0.0107
## 66   65 0.0075 0.0119
## 67   66 0.0086 0.0140
## 68   67 0.0095 0.0153
## 69   68 0.0109 0.0168
## 70   69 0.0111 0.0183
## 71   70 0.0118 0.0193
## 72   71 0.0134 0.0226
## 73   72 0.0154 0.0246
## 74   73 0.0163 0.0275
## 75   74 0.0184 0.0301
## 76   75 0.0213 0.0353
## 77   76 0.0242 0.0379
## 78   77 0.0258 0.0433
## 79   78 0.0301 0.0484
## 80   79 0.0342 0.0554
## 81   80 0.0390 0.0614
## 82   81 0.0455 0.0721
## 83   82 0.0502 0.0827
## 84   83 0.0601 0.0917
## 85   84 0.0672 0.1091
## 86   85 0.0793 0.1163
## 87   86 0.0914 0.1330
## 88   87 0.1030 0.1527
## 89   88 0.1210 0.1676
## 90   89 0.1372 0.1976
## 91   90 0.1686 0.2314
## 92   91 0.1900 0.2632
## 93   92 0.2191 0.3011
## 94   93 0.2524 0.3347
## 95   94 0.2836 0.3877
## 96   95 0.3319 0.4147
## 97   96 0.3765 0.4939
## 98   97 0.4357 0.4660
## 99   98 0.4614 0.5606
## 100  99 0.4785 0.5750
## 101 100 0.6822 0.6667
```

Now, based on these age-specific mortality rates, will be constructed the whole life table:


```r
life_Males <- life_Mortality %>% filter(Sex == "M") %>% arrange(Age) %>%
  mutate(qx = `Mortality Rate`, px = 1-qx, lx = cumprod(px)*100000/px,
         dx = lx * qx, Lx = lx-dx/2) %>%
  select(-`Mortality Rate`) %>% arrange(-Age) %>%
  mutate(Tx = cumsum(Lx), ex = Tx/lx) %>% arrange(Age)

cat("Life table for male population of Norway:")
```

```
## Life table for male population of Norway:
```

```r
life_Males %>%
  mutate(qx = round(qx,4), px = round(px,4), lx = comma(lx %>% round(0)),
         dx = round(dx,1), Lx = comma(Lx %>% round(0)), Tx = comma(Tx %>% round(0)), ex = round(ex,1)) %>%
  print(n = nrow(life_Males))
```

```
## Source: local data frame [101 x 9]
## Groups: Sex
## 
##     Sex Age     qx     px      lx     dx     Lx        Tx   ex
## 1     M   0 0.0027 0.9973 100,000  268.3 99,866 7,883,953 78.8
## 2     M   1 0.0002 0.9998  99,732   21.9 99,721 7,784,088 78.1
## 3     M   2 0.0001 0.9999  99,710   10.6 99,704 7,684,367 77.1
## 4     M   3 0.0001 0.9999  99,699   10.6 99,694 7,584,662 76.1
## 5     M   4 0.0001 0.9999  99,689    6.9 99,685 7,484,969 75.1
## 6     M   5 0.0001 0.9999  99,682   14.6 99,674 7,385,283 74.1
## 7     M   6 0.0001 0.9999  99,667    6.4 99,664 7,285,609 73.1
## 8     M   7 0.0001 0.9999  99,661    5.8 99,658 7,185,945 72.1
## 9     M   8 0.0001 0.9999  99,655   11.1 99,649 7,086,287 71.1
## 10    M   9 0.0001 0.9999  99,644    5.2 99,641 6,986,638 70.1
## 11    M  10 0.0001 0.9999  99,639    9.0 99,634 6,886,997 69.1
## 12    M  11 0.0001 0.9999  99,630    8.3 99,626 6,787,363 68.1
## 13    M  12 0.0001 0.9999  99,621    9.4 99,617 6,687,737 67.1
## 14    M  13 0.0001 0.9999  99,612   10.4 99,607 6,588,121 66.1
## 15    M  14 0.0001 0.9999  99,602   13.4 99,595 6,488,514 65.1
## 16    M  15 0.0001 0.9999  99,588   14.5 99,581 6,388,919 64.2
## 17    M  16 0.0003 0.9997  99,574   27.6 99,560 6,289,338 63.2
## 18    M  17 0.0004 0.9996  99,546   35.6 99,528 6,189,778 62.2
## 19    M  18 0.0005 0.9995  99,511   53.2 99,484 6,090,249 61.2
## 20    M  19 0.0005 0.9995  99,457   50.6 99,432 5,990,765 60.2
## 21    M  20 0.0007 0.9993  99,407   69.3 99,372 5,891,333 59.3
## 22    M  21 0.0007 0.9993  99,338   64.6 99,305 5,791,961 58.3
## 23    M  22 0.0007 0.9993  99,273   74.3 99,236 5,692,656 57.3
## 24    M  23 0.0006 0.9994  99,199   63.0 99,167 5,593,420 56.4
## 25    M  24 0.0007 0.9993  99,136   66.0 99,103 5,494,253 55.4
## 26    M  25 0.0007 0.9993  99,070   73.0 99,033 5,395,150 54.5
## 27    M  26 0.0007 0.9993  98,997   69.3 98,962 5,296,117 53.5
## 28    M  27 0.0008 0.9992  98,927   80.6 98,887 5,197,155 52.5
## 29    M  28 0.0008 0.9992  98,847   79.8 98,807 5,098,268 51.6
## 30    M  29 0.0008 0.9992  98,767   77.8 98,728 4,999,461 50.6
## 31    M  30 0.0008 0.9992  98,689   82.5 98,648 4,900,733 49.7
## 32    M  31 0.0008 0.9992  98,607   81.0 98,566 4,802,085 48.7
## 33    M  32 0.0008 0.9992  98,526   81.4 98,485 4,703,518 47.7
## 34    M  33 0.0007 0.9993  98,444   67.9 98,410 4,605,033 46.8
## 35    M  34 0.0009 0.9991  98,377   90.1 98,331 4,506,623 45.8
## 36    M  35 0.0009 0.9991  98,286   93.2 98,240 4,408,291 44.9
## 37    M  36 0.0009 0.9991  98,193   87.4 98,150 4,310,052 43.9
## 38    M  37 0.0009 0.9991  98,106   84.2 98,064 4,211,902 42.9
## 39    M  38 0.0009 0.9991  98,022   92.8 97,975 4,113,838 42.0
## 40    M  39 0.0011 0.9989  97,929  110.3 97,874 4,015,863 41.0
## 41    M  40 0.0011 0.9989  97,819  108.3 97,764 3,917,989 40.1
## 42    M  41 0.0012 0.9988  97,710  121.5 97,650 3,820,225 39.1
## 43    M  42 0.0011 0.9989  97,589  111.5 97,533 3,722,575 38.1
## 44    M  43 0.0014 0.9986  97,477  135.5 97,410 3,625,042 37.2
## 45    M  44 0.0014 0.9986  97,342  135.9 97,274 3,527,632 36.2
## 46    M  45 0.0017 0.9983  97,206  163.2 97,124 3,430,359 35.3
## 47    M  46 0.0018 0.9982  97,043  172.6 96,957 3,333,234 34.3
## 48    M  47 0.0020 0.9980  96,870  189.4 96,776 3,236,278 33.4
## 49    M  48 0.0023 0.9977  96,681  220.8 96,570 3,139,502 32.5
## 50    M  49 0.0024 0.9976  96,460  233.9 96,343 3,042,932 31.5
## 51    M  50 0.0027 0.9973  96,226  255.6 96,098 2,946,589 30.6
## 52    M  51 0.0030 0.9970  95,971  292.6 95,824 2,850,490 29.7
## 53    M  52 0.0033 0.9967  95,678  316.4 95,520 2,754,666 28.8
## 54    M  53 0.0036 0.9964  95,361  344.9 95,189 2,659,146 27.9
## 55    M  54 0.0040 0.9960  95,017  380.2 94,826 2,563,957 27.0
## 56    M  55 0.0047 0.9953  94,636  445.1 94,414 2,469,131 26.1
## 57    M  56 0.0049 0.9951  94,191  461.9 93,960 2,374,717 25.2
## 58    M  57 0.0053 0.9947  93,729  497.5 93,481 2,280,757 24.3
## 59    M  58 0.0061 0.9939  93,232  564.2 92,950 2,187,276 23.5
## 60    M  59 0.0067 0.9933  92,668  621.1 92,357 2,094,326 22.6
## 61    M  60 0.0074 0.9926  92,047  677.0 91,708 2,001,969 21.7
## 62    M  61 0.0081 0.9919  91,370  741.7 90,999 1,910,261 20.9
## 63    M  62 0.0089 0.9911  90,628  804.5 90,226 1,819,262 20.1
## 64    M  63 0.0095 0.9905  89,823  854.6 89,396 1,729,037 19.2
## 65    M  64 0.0107 0.9893  88,969  949.6 88,494 1,639,641 18.4
## 66    M  65 0.0119 0.9881  88,019 1045.3 87,496 1,551,147 17.6
## 67    M  66 0.0140 0.9860  86,974 1219.6 86,364 1,463,651 16.8
## 68    M  67 0.0153 0.9847  85,754 1315.4 85,096 1,377,287 16.1
## 69    M  68 0.0168 0.9832  84,439 1420.7 83,728 1,292,190 15.3
## 70    M  69 0.0183 0.9817  83,018 1522.1 82,257 1,208,462 14.6
## 71    M  70 0.0193 0.9807  81,496 1574.6 80,709 1,126,205 13.8
## 72    M  71 0.0226 0.9774  79,921 1806.1 79,018 1,045,496 13.1
## 73    M  72 0.0246 0.9754  78,115 1921.2 77,155   966,478 12.4
## 74    M  73 0.0275 0.9725  76,194 2094.0 75,147   889,324 11.7
## 75    M  74 0.0301 0.9699  74,100 2229.8 72,985   814,177 11.0
## 76    M  75 0.0353 0.9647  71,870 2534.3 70,603   741,192 10.3
## 77    M  76 0.0379 0.9621  69,336 2630.8 68,020   670,589  9.7
## 78    M  77 0.0433 0.9567  66,705 2888.0 65,261   602,568  9.0
## 79    M  78 0.0484 0.9516  63,817 3089.6 62,272   537,307  8.4
## 80    M  79 0.0554 0.9446  60,727 3365.4 59,045   475,035  7.8
## 81    M  80 0.0614 0.9386  57,362 3522.3 55,601   415,990  7.3
## 82    M  81 0.0721 0.9279  53,840 3882.0 51,899   360,389  6.7
## 83    M  82 0.0827 0.9173  49,958 4133.5 47,891   308,490  6.2
## 84    M  83 0.0917 0.9083  45,824 4202.2 43,723   260,599  5.7
## 85    M  84 0.1091 0.8909  41,622 4542.1 39,351   216,876  5.2
## 86    M  85 0.1163 0.8837  37,080 4314.2 34,923   177,525  4.8
## 87    M  86 0.1330 0.8670  32,766 4356.6 30,587   142,602  4.4
## 88    M  87 0.1527 0.8473  28,409 4337.2 26,241   112,015  3.9
## 89    M  88 0.1676 0.8324  24,072 4033.4 22,055    85,774  3.6
## 90    M  89 0.1976 0.8024  20,039 3959.2 18,059    63,719  3.2
## 91    M  90 0.2314 0.7686  16,079 3720.4 14,219    45,660  2.8
## 92    M  91 0.2632 0.7368  12,359 3253.5 10,732    31,441  2.5
## 93    M  92 0.3011 0.6989   9,105 2741.7  7,735    20,708  2.3
## 94    M  93 0.3347 0.6653   6,364 2129.7  5,299    12,974  2.0
## 95    M  94 0.3877 0.6123   4,234 1641.5  3,413     7,675  1.8
## 96    M  95 0.4147 0.5853   2,593 1075.1  2,055     4,261  1.6
## 97    M  96 0.4939 0.5061   1,517  749.5  1,143     2,206  1.5
## 98    M  97 0.4660 0.5340     768  357.9    589     1,064  1.4
## 99    M  98 0.5606 0.4394     410  229.9    295       475  1.2
## 100   M  99 0.5750 0.4250     180  103.6    128       179  1.0
## 101   M 100 0.6667 0.3333      77   51.1     51        51  0.7
```

```r
life_Females <- life_Mortality %>% filter(Sex == "F") %>% arrange(Age) %>%
  mutate(qx = `Mortality Rate`, px = 1-qx, lx = cumprod(px)*100000/px,
         dx = lx * qx, Lx = lx-dx/2) %>%
  select(-`Mortality Rate`) %>% arrange(-Age) %>%
  mutate(Tx = cumsum(Lx), ex = Tx/lx) %>% arrange(Age)

cat("Life table for female population of Norway:")
```

```
## Life table for female population of Norway:
```

```r
life_Females %>%
  mutate(qx = round(qx,4), px = round(px,4), lx = comma(lx %>% round(0)),
         dx = round(dx,1), Lx = comma(Lx %>% round(0)), Tx = comma(Tx %>% round(0)), ex = round(ex,1)) %>% 
  print(n = nrow(life_Females))
```

```
## Source: local data frame [101 x 9]
## Groups: Sex
## 
##     Sex Age     qx     px      lx     dx     Lx        Tx   ex
## 1     F   0 0.0022 0.9978 100,000  219.1 99,890 8,302,518 83.0
## 2     F   1 0.0003 0.9997  99,781   25.2 99,768 8,202,628 82.2
## 3     F   2 0.0001 0.9999  99,756    7.2 99,752 8,102,859 81.2
## 4     F   3 0.0001 0.9999  99,749    9.8 99,744 8,003,107 80.2
## 5     F   4 0.0001 0.9999  99,739    9.2 99,734 7,903,364 79.2
## 6     F   5 0.0001 0.9999  99,729    8.0 99,725 7,803,630 78.2
## 7     F   6 0.0000 1.0000  99,721    4.0 99,719 7,703,904 77.3
## 8     F   7 0.0001 0.9999  99,717    6.8 99,714 7,604,185 76.3
## 9     F   8 0.0001 0.9999  99,711    7.5 99,707 7,504,471 75.3
## 10    F   9 0.0001 0.9999  99,703    8.1 99,699 7,404,764 74.3
## 11    F  10 0.0001 0.9999  99,695    8.0 99,691 7,305,065 73.3
## 12    F  11 0.0001 0.9999  99,687    8.6 99,683 7,205,374 72.3
## 13    F  12 0.0000 1.0000  99,678    3.3 99,677 7,105,691 71.3
## 14    F  13 0.0001 0.9999  99,675   11.6 99,669 7,006,014 70.3
## 15    F  14 0.0001 0.9999  99,664    6.4 99,660 6,906,345 69.3
## 16    F  15 0.0002 0.9998  99,657   15.4 99,649 6,806,684 68.3
## 17    F  16 0.0002 0.9998  99,642   15.9 99,634 6,707,035 67.3
## 18    F  17 0.0003 0.9997  99,626   26.6 99,613 6,607,401 66.3
## 19    F  18 0.0003 0.9997  99,599   30.4 99,584 6,507,789 65.3
## 20    F  19 0.0002 0.9998  99,569   22.6 99,558 6,408,205 64.4
## 21    F  20 0.0003 0.9997  99,546   25.5 99,533 6,308,647 63.4
## 22    F  21 0.0003 0.9997  99,521   25.4 99,508 6,209,114 62.4
## 23    F  22 0.0003 0.9997  99,495   29.0 99,481 6,109,606 61.4
## 24    F  23 0.0002 0.9998  99,466   22.4 99,455 6,010,125 60.4
## 25    F  24 0.0002 0.9998  99,444   20.7 99,434 5,910,670 59.4
## 26    F  25 0.0003 0.9997  99,423   28.9 99,409 5,811,236 58.4
## 27    F  26 0.0003 0.9997  99,394   28.3 99,380 5,711,827 57.5
## 28    F  27 0.0002 0.9998  99,366   15.7 99,358 5,612,447 56.5
## 29    F  28 0.0002 0.9998  99,350   24.3 99,338 5,513,089 55.5
## 30    F  29 0.0003 0.9997  99,326   33.5 99,309 5,413,750 54.5
## 31    F  30 0.0003 0.9997  99,293   28.4 99,278 5,314,441 53.5
## 32    F  31 0.0004 0.9996  99,264   35.3 99,247 5,215,163 52.5
## 33    F  32 0.0004 0.9996  99,229   36.2 99,211 5,115,916 51.6
## 34    F  33 0.0003 0.9997  99,193   32.4 99,177 5,016,705 50.6
## 35    F  34 0.0004 0.9996  99,160   40.2 99,140 4,917,529 49.6
## 36    F  35 0.0004 0.9996  99,120   38.9 99,101 4,818,389 48.6
## 37    F  36 0.0005 0.9995  99,081   47.1 99,058 4,719,288 47.6
## 38    F  37 0.0005 0.9995  99,034   53.3 99,008 4,620,230 46.7
## 39    F  38 0.0005 0.9995  98,981   48.0 98,957 4,521,223 45.7
## 40    F  39 0.0005 0.9995  98,933   47.2 98,909 4,422,266 44.7
## 41    F  40 0.0006 0.9994  98,886   58.7 98,856 4,323,356 43.7
## 42    F  41 0.0007 0.9993  98,827   70.0 98,792 4,224,500 42.7
## 43    F  42 0.0008 0.9992  98,757   78.2 98,718 4,125,708 41.8
## 44    F  43 0.0008 0.9992  98,679   78.1 98,640 4,026,990 40.8
## 45    F  44 0.0009 0.9991  98,601   89.6 98,556 3,928,350 39.8
## 46    F  45 0.0010 0.9990  98,511   98.1 98,462 3,829,794 38.9
## 47    F  46 0.0012 0.9988  98,413  119.2 98,353 3,731,332 37.9
## 48    F  47 0.0014 0.9986  98,294  138.2 98,225 3,632,978 37.0
## 49    F  48 0.0014 0.9986  98,156  134.3 98,089 3,534,754 36.0
## 50    F  49 0.0017 0.9983  98,021  168.6 97,937 3,436,665 35.1
## 51    F  50 0.0019 0.9981  97,853  184.6 97,761 3,338,728 34.1
## 52    F  51 0.0022 0.9978  97,668  214.7 97,561 3,240,967 33.2
## 53    F  52 0.0022 0.9978  97,454  217.8 97,345 3,143,406 32.3
## 54    F  53 0.0023 0.9977  97,236  225.5 97,123 3,046,062 31.3
## 55    F  54 0.0029 0.9971  97,010  278.7 96,871 2,948,939 30.4
## 56    F  55 0.0032 0.9968  96,732  306.5 96,578 2,852,068 29.5
## 57    F  56 0.0033 0.9967  96,425  321.6 96,264 2,755,489 28.6
## 58    F  57 0.0036 0.9964  96,103  345.1 95,931 2,659,225 27.7
## 59    F  58 0.0036 0.9964  95,758  343.1 95,587 2,563,294 26.8
## 60    F  59 0.0044 0.9956  95,415  421.1 95,205 2,467,707 25.9
## 61    F  60 0.0049 0.9951  94,994  462.0 94,763 2,372,503 25.0
## 62    F  61 0.0049 0.9951  94,532  466.8 94,299 2,277,740 24.1
## 63    F  62 0.0059 0.9941  94,065  557.7 93,787 2,183,441 23.2
## 64    F  63 0.0060 0.9940  93,508  556.9 93,229 2,089,654 22.3
## 65    F  64 0.0068 0.9932  92,951  635.5 92,633 1,996,425 21.5
## 66    F  65 0.0075 0.9925  92,315  694.3 91,968 1,903,792 20.6
## 67    F  66 0.0086 0.9914  91,621  784.0 91,229 1,811,824 19.8
## 68    F  67 0.0095 0.9905  90,837  860.0 90,407 1,720,595 18.9
## 69    F  68 0.0109 0.9891  89,977  978.9 89,488 1,630,188 18.1
## 70    F  69 0.0111 0.9889  88,998  987.2 88,504 1,540,700 17.3
## 71    F  70 0.0118 0.9882  88,011 1036.7 87,493 1,452,196 16.5
## 72    F  71 0.0134 0.9866  86,974 1162.7 86,393 1,364,703 15.7
## 73    F  72 0.0154 0.9846  85,811 1321.8 85,151 1,278,310 14.9
## 74    F  73 0.0163 0.9837  84,490 1375.4 83,802 1,193,160 14.1
## 75    F  74 0.0184 0.9816  83,114 1530.6 82,349 1,109,358 13.3
## 76    F  75 0.0213 0.9787  81,584 1739.0 80,714 1,027,009 12.6
## 77    F  76 0.0242 0.9758  79,845 1930.3 78,880   946,295 11.9
## 78    F  77 0.0258 0.9742  77,914 2006.8 76,911   867,415 11.1
## 79    F  78 0.0301 0.9699  75,908 2287.4 74,764   790,504 10.4
## 80    F  79 0.0342 0.9658  73,620 2515.9 72,362   715,740  9.7
## 81    F  80 0.0390 0.9610  71,104 2772.4 69,718   643,378  9.0
## 82    F  81 0.0455 0.9545  68,332 3107.2 66,778   573,660  8.4
## 83    F  82 0.0502 0.9498  65,225 3276.7 63,586   506,882  7.8
## 84    F  83 0.0601 0.9399  61,948 3722.2 60,087   443,296  7.2
## 85    F  84 0.0672 0.9328  58,226 3914.5 56,269   383,209  6.6
## 86    F  85 0.0793 0.9207  54,311 4309.1 52,157   326,940  6.0
## 87    F  86 0.0914 0.9086  50,002 4571.2 47,717   274,784  5.5
## 88    F  87 0.1030 0.8970  45,431 4679.5 43,091   227,067  5.0
## 89    F  88 0.1210 0.8790  40,751 4930.7 38,286   183,976  4.5
## 90    F  89 0.1372 0.8628  35,821 4912.9 33,364   145,690  4.1
## 91    F  90 0.1686 0.8314  30,908 5212.4 28,302   112,326  3.6
## 92    F  91 0.1900 0.8100  25,695 4881.7 23,254    84,024  3.3
## 93    F  92 0.2191 0.7809  20,814 4560.9 18,533    60,770  2.9
## 94    F  93 0.2524 0.7476  16,253 4102.2 14,202    42,237  2.6
## 95    F  94 0.2836 0.7164  12,151 3446.2 10,427    28,035  2.3
## 96    F  95 0.3319 0.6681   8,704 2889.3  7,260    17,607  2.0
## 97    F  96 0.3765 0.6235   5,815 2189.3  4,720    10,348  1.8
## 98    F  97 0.4357 0.5643   3,626 1579.6  2,836     5,627  1.6
## 99    F  98 0.4614 0.5386   2,046  944.2  1,574     2,791  1.4
## 100   F  99 0.4785 0.5215   1,102  527.4    838     1,217  1.1
## 101   F 100 0.6822 0.3178     575  392.0    379       379  0.7
```

## Summary


```r
life_Females %>% rbind(life_Males) %>%
  ggplot(aes(x = Age, y = lx, color = Sex)) + geom_line() +
  geom_hline(yintercept = 50000, linetype = "longdash") +
  ggtitle("Development of cohort's population with age\nBased on calculated life tables for Norway")
```

<img src="Norway_files/figure-html/life_tbl_graph-1.png" style="display: block; margin: auto;" />

Results from the life tables:

1. Life expectancy at birth is 78.8 years for males and 83 for females, which confirms our previous suspicious about life expectancy of females being higher than of males.
2. Median life lenght is 81 for males and 86 for females, which confirms again longer live of females.

3. Due to higher mortality rates males' cohort's population is decreasing faster than females'.

# Projections
## Calculation
In calculation of projections we will include the following parameters:

* Population size by sex and age at the beginning of 2015
* Age-specific mortality rates by sex (same as for life tables)
* Age-specific net migration rates by sex
* Age-specific fertility rates (for age from 15 to 49)
* Average index of masculinity at childbirth




```r
proj_parameter <- list(pop_change = tmp_Migration_Rate %>% group_by(Age, Sex) %>%
                         summarize(Net.Migration = (sum(Immigration)-sum(Emigration))/sum(Population)) %>%
                         merge(life_Mortality, by = c("Age", "Sex")) %>% 
                         mutate(Net.Change = Net.Migration - `Mortality Rate`) %>%
                         select(Age, Sex, Net.Change),
                       fertility = dt_Fertility_Rate %>% 
                         group_by(Age) %>% 
                         summarize(Fertility_Rate = mean(Fertility_Rate, na.rm = T)) %>%
                         mutate(Sex = factor("F", levels = c("F", "M"))),
                       males_share = dt_Fertility_Age_Sex %>% group_by(Sex) %>% 
                         summarize(Pop = sum(Count, na.rm = T)) %>%
                         mutate(Ratio = Pop/sum(Pop)) %>% `[`(2, "Ratio") %>% max())

proj_result <- dt_Population_Age_Sex %>% filter(Year == 2015)

for (i in (2016:2100))
  proj_result <- proj_result %>% filter(Year == i-1) %>%
    merge(proj_parameter$pop_change, by = c("Age", "Sex")) %>%
    mutate(Population = Population * (1 + Net.Change), Year = i, Age = pmin(100, Age+1)) %>%
    group_by(Age, Year, Sex) %>% summarize(Population = sum(Population) %>% round(0)) %>%
    rbind(data.frame(c(0,0), rep(i, 2), factor(c("M","F")), ((proj_result %>% filter(Year == i-1) %>%
    merge(proj_parameter$fertility, by = c("Age", "Sex")) %>%
    mutate(Population = Population * Fertility_Rate) %>%
    `[`(,"Population") %>% sum())*c(proj_parameter$males_share,1-proj_parameter$males_share)) %>% round(0)) %>%
      setNames(c("Age", "Year", "Sex", "Population"))) %>%
    rbind(proj_result)
optimistic_scenario <- list(proj_parameter = proj_parameter, proj_result = proj_result)
```


```r
optimistic_scenario$proj_result %>% group_by(Year) %>% summarize(Population = sum(Population)) %>%
  ggplot(aes(x = Year, y = Population)) + geom_line() +
  scale_y_continuous(limits = c(0, 1.2*10^7), labels = comma) +
  ggtitle("Projection of Norway's population till 2100\nOptimistic scenario")
```

<img src="Norway_files/figure-html/optimistic-1.png" style="display: block; margin: auto;" />

```r
optimistic_scenario$proj_result %>% group_by(Year, Sex) %>% summarize(Population = sum(Population)) %>%
  ggplot(aes(x = Year, y = Population, color = Sex)) + geom_line() +
  scale_y_continuous(limits = c(0, .6*10^7), labels = comma) +
  ggtitle("Projection of Norway's population by sex till 2100\nOptimistic scenario")
```

<img src="Norway_files/figure-html/optimistic-2.png" style="display: block; margin: auto;" />

```r
optimistic_scenario$proj_result %>% 
  filter(Year == 2100) %>% group_by(Age, Sex) %>% summarise(value = sum(Population, na.rm = T)) %>%
  dcast(Age ~ Sex, fun.aggregate = sum, value.var = "value") %>% select(M, F, Age) %>%
  pyramid(Csize = 1, Cstep = 5, AxisFM = 'd', Cgap = .1,
          main = "Pyramid of Norway's population in 2100\nOptimistic scenario")
```

<img src="Norway_files/figure-html/optimistic-3.png" style="display: block; margin: auto;" />

## Scenarios

As you can see from this projection, population of Norway will continue steady growth and reach `11` mln people in 2100. Also, we notice a gap between male and female population size, which is constantly increasing. This is caused by the net migration parameter. This parameter is quite high because in 2010-s immigration to Norway was high, but we cannot assume that such level will stay till 2100. Therefore, this scenarion is called optimistic and we will calculate 2 more scenarios: pessimistic (half of current net migration) and no-migration scenario.


```r
# Pessimistic scenario
proj_parameter$pop_change <- tmp_Migration_Rate %>% group_by(Age, Sex) %>%
                         summarize(Net.Migration = (sum(Immigration)-sum(Emigration))/sum(Population)/2) %>%
                         merge(life_Mortality, by = c("Age", "Sex")) %>% 
                         mutate(Net.Change = Net.Migration - `Mortality Rate`) %>%
                         select(Age, Sex, Net.Change)
proj_result <- dt_Population_Age_Sex %>% filter(Year == 2015)
for (i in (2016:2100))
  proj_result <- proj_result %>% filter(Year == i-1) %>%
    merge(proj_parameter$pop_change, by = c("Age", "Sex")) %>%
    mutate(Population = Population * (1 + Net.Change), Year = i, Age = pmin(100, Age+1)) %>%
    group_by(Age, Year, Sex) %>% summarize(Population = sum(Population) %>% round(0)) %>%
    rbind(data.frame(c(0,0), rep(i, 2), factor(c("M","F")), ((proj_result %>% filter(Year == i-1) %>%
    merge(proj_parameter$fertility, by = c("Age", "Sex")) %>%
    mutate(Population = Population * Fertility_Rate) %>%
    `[`(,"Population") %>% sum())*c(proj_parameter$males_share,1-proj_parameter$males_share)) %>% round(0)) %>%
      setNames(c("Age", "Year", "Sex", "Population"))) %>%
    rbind(proj_result)

pessimistic_scenario <- list(proj_parameter = proj_parameter, proj_result = proj_result)

# No-migration scenario
proj_parameter$pop_change <- life_Mortality %>% setNames(c("Sex","Age","Net.Change")) %>%
  mutate(Net.Change = -Net.Change)
proj_result <- dt_Population_Age_Sex %>% filter(Year == 2015)
for (i in (2016:2100))
  proj_result <- proj_result %>% filter(Year == i-1) %>%
    merge(proj_parameter$pop_change, by = c("Age", "Sex")) %>%
    mutate(Population = Population * (1 + Net.Change), Year = i, Age = pmin(100, Age+1)) %>%
    group_by(Age, Year, Sex) %>% summarize(Population = sum(Population) %>% round(0)) %>%
    rbind(data.frame(c(0,0), rep(i, 2), factor(c("M","F")), ((proj_result %>% filter(Year == i-1) %>%
    merge(proj_parameter$fertility, by = c("Age", "Sex")) %>%
    mutate(Population = Population * Fertility_Rate) %>%
    `[`(,"Population") %>% sum())*c(proj_parameter$males_share,1-proj_parameter$males_share)) %>% round(0)) %>%
      setNames(c("Age", "Year", "Sex", "Population"))) %>%
    rbind(proj_result)

no.migration_scenario <- list(proj_parameter = proj_parameter, proj_result = proj_result)

proj_result_total <- optimistic_scenario$proj_result %>% mutate(Scenario = "Optimistic") %>%
  rbind(pessimistic_scenario$proj_result %>% mutate(Scenario = "Pessimistic")) %>%
  rbind(no.migration_scenario$proj_result %>% mutate(Scenario = "No-migration")) %>%
  mutate(Scenario = factor(Scenario, levels = c("Optimistic", "Pessimistic", "No-migration"), ordered = T))
```

Now we will compare the results of different scenarios by the following metrics:

* Population size in time
* Pyramid structure (every 30 years)
* Economic generations' indeces

First will be general population size:


```r
proj_result_total %>%
  group_by(Scenario, Year) %>% summarize(Population = sum(Population)) %>%
  ggplot(aes(x = Year, y = Population, color = Scenario)) + geom_line() +
  scale_y_continuous(limits = c(0, 1.2*10^7), labels = comma) +
  ggtitle("Projection of Norway's population till 2100\nAll scenarios") +
  theme(legend.position = "bottom", legend.direction="horizontal")
```

<img src="Norway_files/figure-html/scen_size-1.png" style="display: block; margin: auto;" />

Second - pyramids:


```r
proj_result_total %>% 
  filter(Year == 2015 & Scenario == "Optimistic") %>% 
  group_by(Age, Sex) %>% summarise(value = sum(Population, na.rm = T)) %>%
  dcast(Age ~ Sex, fun.aggregate = sum, value.var = "value") %>% select(M, F, Age) %>%
  pyramid(Csize = .7, Cstep = 10, AxisFM = 'd', Cgap = .1, Laxis = 0:4*20000,
          main = "Pyramid of Norway's population in 2015\nReal situation")
```

<img src="Norway_files/figure-html/proj_pyramids-1.png" style="display: block; margin: auto;" />

```r
cat("Optimistic scenario")
```

```
## Optimistic scenario
```

```r
# Optimistic
proj_result_total %>% 
  filter(Year == 2045 & Scenario == "Optimistic") %>% 
  group_by(Age, Sex) %>% summarise(value = sum(Population, na.rm = T)) %>%
  dcast(Age ~ Sex, fun.aggregate = sum, value.var = "value") %>% select(M, F, Age) %>%
  pyramid(Csize = .7, Cstep = 10, AxisFM = 'd', Cgap = .1, Laxis = 0:4*20000,
          main = "Pyramid of Norway's population in 2045\nOptimistic scenario")
```

<img src="Norway_files/figure-html/proj_pyramids-2.png" style="display: block; margin: auto;" />

```r
proj_result_total %>% 
  filter(Year == 2075 & Scenario == "Optimistic") %>% 
  group_by(Age, Sex) %>% summarise(value = sum(Population, na.rm = T)) %>%
  dcast(Age ~ Sex, fun.aggregate = sum, value.var = "value") %>% select(M, F, Age) %>%
  pyramid(Csize = .7, Cstep = 10, AxisFM = 'd', Cgap = .1, Laxis = 0:4*20000,
          main = "Pyramid of Norway's population in 2075\nOptimistic scenario")
```

<img src="Norway_files/figure-html/proj_pyramids-3.png" style="display: block; margin: auto;" />

```r
proj_result_total %>% 
  filter(Year == 2100 & Scenario == "Optimistic") %>% 
  group_by(Age, Sex) %>% summarise(value = sum(Population, na.rm = T)) %>%
  dcast(Age ~ Sex, fun.aggregate = sum, value.var = "value") %>% select(M, F, Age) %>%
  pyramid(Csize = .7, Cstep = 10, AxisFM = 'd', Cgap = .1, Laxis = 0:4*20000,
          main = "Pyramid of Norway's population in 2100\nOptimistic scenario")
```

<img src="Norway_files/figure-html/proj_pyramids-4.png" style="display: block; margin: auto;" />

```r
# Pessimistic
cat("Pessimistic scenario")
```

```
## Pessimistic scenario
```

```r
proj_result_total %>% 
  filter(Year == 2045 & Scenario == "Pessimistic") %>% 
  group_by(Age, Sex) %>% summarise(value = sum(Population, na.rm = T)) %>%
  dcast(Age ~ Sex, fun.aggregate = sum, value.var = "value") %>% select(M, F, Age) %>%
  pyramid(Csize = .7, Cstep = 10, AxisFM = 'd', Cgap = .1, Laxis = 0:4*20000,
          main = "Pyramid of Norway's population in 2045\nPessimistic scenario")
```

<img src="Norway_files/figure-html/proj_pyramids-5.png" style="display: block; margin: auto;" />

```r
proj_result_total %>% 
  filter(Year == 2075 & Scenario == "Pessimistic") %>% 
  group_by(Age, Sex) %>% summarise(value = sum(Population, na.rm = T)) %>%
  dcast(Age ~ Sex, fun.aggregate = sum, value.var = "value") %>% select(M, F, Age) %>%
  pyramid(Csize = .7, Cstep = 10, AxisFM = 'd', Cgap = .1, Laxis = 0:4*20000,
          main = "Pyramid of Norway's population in 2075\nPessimistic scenario")
```

<img src="Norway_files/figure-html/proj_pyramids-6.png" style="display: block; margin: auto;" />

```r
proj_result_total %>% 
  filter(Year == 2100 & Scenario == "Pessimistic") %>% 
  group_by(Age, Sex) %>% summarise(value = sum(Population, na.rm = T)) %>%
  dcast(Age ~ Sex, fun.aggregate = sum, value.var = "value") %>% select(M, F, Age) %>%
  pyramid(Csize = .7, Cstep = 10, AxisFM = 'd', Cgap = .1, Laxis = 0:4*20000,
          main = "Pyramid of Norway's population in 2100\nPessimistic scenario")
```

<img src="Norway_files/figure-html/proj_pyramids-7.png" style="display: block; margin: auto;" />

```r
# No-migration
cat("No-migration scenario")
```

```
## No-migration scenario
```

```r
proj_result_total %>% 
  filter(Year == 2045 & Scenario == "No-migration") %>% 
  group_by(Age, Sex) %>% summarise(value = sum(Population, na.rm = T)) %>%
  dcast(Age ~ Sex, fun.aggregate = sum, value.var = "value") %>% select(M, F, Age) %>%
  pyramid(Csize = .7, Cstep = 10, AxisFM = 'd', Cgap = .1, Laxis = 0:4*20000,
          main = "Pyramid of Norway's population in 2045\nNo-migration scenario")
```

<img src="Norway_files/figure-html/proj_pyramids-8.png" style="display: block; margin: auto;" />

```r
proj_result_total %>% 
  filter(Year == 2075 & Scenario == "No-migration") %>% 
  group_by(Age, Sex) %>% summarise(value = sum(Population, na.rm = T)) %>%
  dcast(Age ~ Sex, fun.aggregate = sum, value.var = "value") %>% select(M, F, Age) %>%
  pyramid(Csize = .7, Cstep = 10, AxisFM = 'd', Cgap = .1, Laxis = 0:4*20000,
          main = "Pyramid of Norway's population in 2075\nNo-migration scenario")
```

<img src="Norway_files/figure-html/proj_pyramids-9.png" style="display: block; margin: auto;" />

```r
proj_result_total %>% 
  filter(Year == 2100 & Scenario == "No-migration") %>% 
  group_by(Age, Sex) %>% summarise(value = sum(Population, na.rm = T)) %>%
  dcast(Age ~ Sex, fun.aggregate = sum, value.var = "value") %>% select(M, F, Age) %>%
  pyramid(Csize = .7, Cstep = 10, AxisFM = 'd', Cgap = .1, Laxis = 0:4*20000,
          main = "Pyramid of Norway's population in 2100\nNo-migration scenario")
```

<img src="Norway_files/figure-html/proj_pyramids-10.png" style="display: block; margin: auto;" />

And, finally, structure of economic generations:


```r
proj_gen <- proj_result_total %>% 
  mutate(Generation = cut(Age, breaks = c(0,20, 65, 100), include.lowest = T, right = F, labels = c("I","II","III"))) %>%
  group_by(Year, Scenario, Generation) %>% summarise(Population = sum(Population)) %>%
  dcast(Year + Scenario ~ Generation, value.var = "Population") %>%
  mutate(Seniority = III/I, OADR = III/II, JADR = I/II, TDR = (I+III)/II) %>%
  select(-c(I,II,III)) %>% melt(id = c("Year", "Scenario")) %>%
  setNames(c("Year","Scenario","Indicator","Value")) %>%
  mutate(Value = round(Value,2))
  
proj_gen %>%
  filter(Year %in% c(2015, 2045, 2075, 2100)) %>%
  dcast(Year + Scenario ~ Indicator, value.var = "Value") %>%
  arrange(Scenario)
```

```
##    Year     Scenario Seniority OADR JADR  TDR
## 1  2015   Optimistic      0.66 0.27 0.41 0.68
## 2  2045   Optimistic      0.82 0.32 0.39 0.72
## 3  2075   Optimistic      0.88 0.35 0.40 0.74
## 4  2100   Optimistic      0.88 0.35 0.40 0.75
## 5  2015  Pessimistic      0.66 0.27 0.41 0.68
## 6  2045  Pessimistic      0.96 0.38 0.40 0.78
## 7  2075  Pessimistic      0.99 0.40 0.40 0.80
## 8  2100  Pessimistic      0.99 0.40 0.40 0.80
## 9  2015 No-migration      0.66 0.27 0.41 0.68
## 10 2045 No-migration      1.12 0.45 0.40 0.84
## 11 2075 No-migration      1.13 0.45 0.40 0.86
## 12 2100 No-migration      1.13 0.46 0.40 0.86
```

```r
proj_gen %>%
  ggplot(aes(x = Year, y = Value, color = Scenario)) +
  geom_line() + facet_grid(Indicator ~ ., scales = "free_y") +
  theme(legend.position = "bottom", legend.direction="horizontal")
```

<img src="Norway_files/figure-html/proj_gen-1.png" style="display: block; margin: auto;" />

## Summary

Here are the conclusions regarding our population projections:

1. **General**: You can see a stabilization of indeces and smoothing of population pyramid with time, which means that population structure is converging after some amoint of years (~ after year 2040 for indeces). This certainly won't happen in the real world because parameters, on which projections were calculated will always change, with the random deviations that cannot be projected. Also, interesting is that JADR is independent of scenario, which means that it's independent of the net migration.

2. **Optimistic scenario**: This scenario shows quick and steady growth, which is quite feasible as we've seen on data from 1960 to 2015. On the other hand, by it's projection index of masculinity will be constantly more than 1, which will cause icreasing gap between number of males and females. This is caused by the current (2013 and 2014) net migration structure and it won't be the same for 50 years. Regarding the pyramid structure, this scenario provides us with the progressive (and `0.88` index of seniority confirms it) type of population for Norway.

3. **Pessimistic scenario**: According to this scenario, Norway's population is slowly growing without significant change of the pyramid structure. This will keep Norway's population in stationary type with index of seniority around value of `1`. 

4. **No-migration scenario**: This scenarios is quite teoretical because in realiry net migration has bigger influence on the population of Norway than the natural change of population. Nevertheless, this option brings us slow decrease and aging of population: population pyramid is of regressive type with index of seniority equal to `1.13`.

All in all, my personal feeling is that real situation will be somewhere near Pessimistic scenario with elements of aging and regressive type of population.

# Conclusions

Throughout the analysis, Norway showed itself as typical Western developed country with usual trends: increase in life expectancy and aging of population, positive net migration, big share of childbirths outside the marriage.

Overall, population of Norway seems to be quite "healthy" (structural- and tendency-wise) and projection scenarios show confidence in proper developments of country's population.

# Appendix
## Used libraries

Here's the list of libraries used during this assignment:


```r
library(dplyr)      # Work with datasets: filter, group, summarize
library(reshape2)   # Transformation of datasets: wide-to-long format and vice versa
library(pyramid)    # Visualization of demography pyramid
library(ggplot2)    # Building graphs
library(scales)     # Number formatting
```

## Explanation of code parts

`%>%` element is called chaining operator and is a part of *dplyr* package. This operator is handing element on the left to the function on the right as the first argument. It helps to make complicated nested formulas easier to comprehend:


```r
# Simple example
    filter(dataset, field == "value") 
        # is equal to
    dataset %>% filter(field == "value")

# More complex example
    summarize(group_by(filter(dataset, field == "value"), group_field), function(value_field))
        # is equal to
    dataset %>% filter(field == "value") %>% group_by(group_field) %>% summarize(function(value_field))
```

As you can see from the second example, chaining operator makes your code much more easier to read. That's why it's used throughout the paper.

## Source data

Following datasets from the [Eurostat](http://ec.europa.eu/eurostat/web/population-demography-migration-projections/overview) were used in this assignment:

* dt_Population_General: some general indicators for Norway for years 2000-2014

* dt_Population_Age_Sex: Norway's population by age and sex from 1960 to 2015

* dt_Education: Norway's population by age, sex and education level from 2007 to 2014

* dt_Marital: Norway's population by age, sex and marital status from 2000 to 2014

* dt_Marriage_Age: Marriages in Norway by age groups and sex from 2007 to 2014

* dt_Marriage_PrevStatus: Marriages in Norway by sex and previous marital status from 2007 to 2014

* dt_Divorce: Divorce indicators in Norway 

* dt_Fertility: Fertility indicators from 2000 to 2014

* dt_Fertility_Rate: Annual fertility rates for women in Norway by age from 2000 to 2014

* dt_Fertility_Age_Sex: Births by sex and mother's age in Norway from 2007 to 2013

* dt_Death_Age: Deaths in Norway by age and sex from 2007 to 2014

* dt_Migration: Immigration, Emigration and Net Migration by age and sex in Norway for 2013 and 2014

Full tables are attached in the archive. Also, RData file `Eurostat.RData` contains all those source tables after import to R. 

Here are 10 sample rows for each of the tables:

**dt_Population_general**


```r
dt_Population_general %>% head(10)
```

```
##             Indicator SEX Year   Value
## 1  Average population   F 2000 2266746
## 2  Average population   F 2001 2277134
## 3  Average population   F 2002 2289139
## 4  Average population   F 2003 2302277
## 5  Average population   F 2004 2315351
## 6  Average population   F 2005 2330266
## 7  Average population   F 2006 2346792
## 8  Average population   F 2007 2366414
## 9  Average population   F 2008 2390840
## 10 Average population   F 2009 2417823
```

**dt_Population_Age_Sex**


```r
dt_Population_Age_Sex %>% head(10)
```

```
##    Age Sex Year Population
## 1    1   M 1960      31676
## 2    1   M 1961      31644
## 3    1   M 1962      30897
## 4    1   M 1963      31646
## 5    1   M 1964      31353
## 6    1   M 1965      31761
## 7    1   M 1966      33324
## 8    1   M 1967      33540
## 9    1   M 1968      33862
## 10   1   M 1969      33585
```

**dt_Education**


```r
dt_Education %>% head(10)
```

```
##    Age Sex Year       Education Count
## 1    0   M 2007 Lower.Secondary     0
## 2    0   M 2008 Lower.Secondary     0
## 3    0   M 2009 Lower.Secondary     0
## 4    0   M 2010 Lower.Secondary     0
## 5    0   M 2011 Lower.Secondary     0
## 6    0   M 2012 Lower.Secondary     0
## 7    0   M 2013 Lower.Secondary     0
## 8    0   M 2014 Lower.Secondary     0
## 9    1   M 2007 Lower.Secondary     0
## 10   1   M 2008 Lower.Secondary     0
```

**dt_Marital**


```r
dt_Marital %>% head(10)
```

```
##    Age Sex Year Marsta Count
## 1    0   M 2000 Single 30485
## 2    1   M 2000 Single 30134
## 3    2   M 2000 Single 31151
## 4    3   M 2000 Single 31893
## 5    4   M 2000 Single 31595
## 6    5   M 2000 Single 31426
## 7    6   M 2000 Single 31138
## 8    7   M 2000 Single 31890
## 9    8   M 2000 Single 32192
## 10   9   M 2000 Single 32038
```

**dt_Marriage_Age**


```r
dt_Marriage_Age %>% head(10)
```

```
##        Age Sex Year Count
## 1  [15;20)   M 2007    51
## 2  [15;20)   M 2008    46
## 3  [15;20)   M 2009    41
## 4  [15;20)   M 2010    34
## 5  [15;20)   M 2011    35
## 6  [15;20)   M 2012    31
## 7  [15;20)   M 2013    51
## 8  [15;20)   M 2014    22
## 9  [20;25)   M 2007  1273
## 10 [20;25)   M 2008  1270
```

**dt_Marriage_PrevStatus**


```r
dt_Marriage_PrevStatus %>% head(10)
```

```
##     MARSTA Sex Year Count
## 1   Single   M 2007 17770
## 2   Single   M 2008 19091
## 3   Single   M 2009 18727
## 4   Single   M 2010 18092
## 5   Single   M 2011 17459
## 6   Single   M 2012 18499
## 7   Single   M 2013 17780
## 8   Single   M 2014 17529
## 9  Widowed   M 2008   318
## 10 Widowed   M 2009   285
```

**dt_Divorce**


```r
dt_Divorce %>% head(10)
```

```
##    Indicator Year Value
## 1   Divorces 2000 10053
## 2   Divorces 2001 10308
## 3   Divorces 2002 10450
## 4   Divorces 2003 10757
## 5   Divorces 2004 11045
## 6   Divorces 2005 11040
## 7   Divorces 2006 10598
## 8   Divorces 2007 10280
## 9   Divorces 2008 10158
## 10  Divorces 2009 10235
```

**dt_Fertility**


```r
dt_Fertility %>% head(10)
```

```
##                                     Indicator Year Value
## 1  Proportion of live births outside marriage 2000  49.6
## 2  Proportion of live births outside marriage 2001  49.7
## 3  Proportion of live births outside marriage 2002  50.3
## 4  Proportion of live births outside marriage 2003  50.0
## 5  Proportion of live births outside marriage 2004  51.4
## 6  Proportion of live births outside marriage 2005  51.8
## 7  Proportion of live births outside marriage 2006  53.0
## 8  Proportion of live births outside marriage 2007  54.5
## 9  Proportion of live births outside marriage 2008  55.0
## 10 Proportion of live births outside marriage 2009  55.1
```

**dt_Fertility_Rate**


```r
dt_Fertility_Rate %>% head(10)
```

```
##    Age Year Fertility_Rate
## 1   15 2000        0.00066
## 2   15 2001        0.00046
## 3   15 2002        0.00055
## 4   15 2003        0.00046
## 5   15 2004        0.00051
## 6   15 2005        0.00026
## 7   15 2006        0.00029
## 8   15 2007        0.00045
## 9   15 2008        0.00042
## 10  15 2009        0.00045
```

**dt_Fertility_Age_Sex**


```r
dt_Fertility_Age_Sex %>% head(10)
```

```
##        AGE Sex Year Count
## 1  [10,15)   M 2007     3
## 2  [10,15)   M 2008     3
## 3  [10,15)   M 2009     1
## 4  [10,15)   M 2010     1
## 5  [10,15)   M 2011     1
## 6  [10,15)   M 2012     1
## 7  [10,15)   M 2013     1
## 8  [15,20)   M 2007   707
## 9  [15,20)   M 2008   720
## 10 [15,20)   M 2009   759
```

**dt_Death_Age**


```r
dt_Death_Age %>% head(10)
```

```
##    Age Sex Year Deaths
## 1    0   M 2000    130
## 2    0   M 2001    126
## 3    0   M 2002     94
## 4    0   M 2003    105
## 5    0   M 2004     99
## 6    0   M 2005     95
## 7    0   M 2006    111
## 8    0   M 2007     97
## 9    0   M 2008    101
## 10   0   M 2009    119
```

**dt_Migration**


```r
dt_Migration %>% head(10)
```

```
##    Age Sex Year Immigration Emigration Net_Migration
## 1    0   M 2013         635        136           771
## 2    0   M 2014         636        158           794
## 3    1   M 2013         662        175           837
## 4    1   M 2014         612        207           819
## 5    2   M 2013         570        213           783
## 6    2   M 2014         526        272           798
## 7    3   M 2013         491        205           696
## 8    3   M 2014         493        295           788
## 9    4   M 2013         444        208           652
## 10   4   M 2014         437        250           687
```

