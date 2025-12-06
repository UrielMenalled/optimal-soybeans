Soy-rye analysis: 2019 project
================
Uriel D. Menalled
7/1/2020

The nlme models were done using the covariate scheme recommended by
Pinheiro and Bates (2000). While this approach lead to slightly higher
AIC and slightly different p-values than modeling by hand in nlme,
parameters of the nls, nlme-by hand, and nlme-covariate models were the
same. Furthermore, the nlme-covariate approach was the only method that
could properly account for nested random effects.

# Library

``` r
library(tidyverse)
```

    ## Warning: package 'ggplot2' was built under R version 4.5.2

    ## Warning: package 'readr' was built under R version 4.5.2

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.6
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.1     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.2.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(lmerTest)
```

    ## Loading required package: lme4

    ## Warning: package 'lme4' was built under R version 4.5.2

    ## Loading required package: Matrix
    ## 
    ## Attaching package: 'Matrix'
    ## 
    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack
    ## 
    ## 
    ## Attaching package: 'lmerTest'
    ## 
    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     step

``` r
library(openxlsx)
library(emmeans)
```

    ## Welcome to emmeans.
    ## Caution: You lose important information if you filter this package's results.
    ## See '? untidy'

``` r
library(vegan)
```

    ## Loading required package: permute

``` r
library(goeveg)
```

    ## This is GoeVeg 0.7.9 - build: 2025-09-02

``` r
library(labdsv)
```

    ## Loading required package: mgcv
    ## Loading required package: nlme
    ## 
    ## Attaching package: 'nlme'
    ## 
    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmList
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse
    ## 
    ## This is mgcv 1.9-4. For overview type '?mgcv'.
    ## This is labdsv 2.1-0
    ## convert existing ordinations with as.dsvord()
    ## 
    ## Attaching package: 'labdsv'
    ## 
    ## The following objects are masked from 'package:vegan':
    ## 
    ##     calibrate, pca, pco, scores
    ## 
    ## The following object is masked from 'package:lme4':
    ## 
    ##     factorize
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     density, loadings

``` r
library(nls.multstart)
library(ggpubr)
library(corrplot)
```

    ## corrplot 0.95 loaded

``` r
library(nlme)
library(outliers)
```

    ## 
    ## Attaching package: 'outliers'
    ## 
    ## The following object is masked from 'package:labdsv':
    ## 
    ##     scores
    ## 
    ## The following object is masked from 'package:vegan':
    ## 
    ##     scores

``` r
library(multcomp)
```

    ## Loading required package: mvtnorm
    ## Loading required package: survival
    ## Loading required package: TH.data

    ## Warning: package 'TH.data' was built under R version 4.5.2

    ## Loading required package: MASS
    ## 
    ## Attaching package: 'MASS'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select
    ## 
    ## 
    ## Attaching package: 'TH.data'
    ## 
    ## The following object is masked from 'package:MASS':
    ## 
    ##     geyser

``` r
library(here)
```

    ## here() starts at /Users/urielmenalled/Desktop/optimal-soybeans

# Weather

``` r
WeatherData<-read.xlsx(here("Data","WeatherData.xlsx"),sheet=1)
WeatherData$Site<-as.factor(WeatherData$Site)
WeatherData$Month<-factor(WeatherData$Month,
                             levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                                      "Aug","Sep","Oct","Nov","Dec"))
WeatherData$Year<-factor(WeatherData$Year, levels = c("2019", "15-year avg."))
WeatherData$Month2<-as.numeric(WeatherData$Month)
tapply(WeatherData[WeatherData$Month %in% c("May","Jun","Jul","Aug","Sep","Oct"),]$Temp_C,
      WeatherData[WeatherData$Month %in% c("May","Jun","Jul","Aug","Sep","Oct"),]$Site, summary)
```

    ## $Aurora
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   10.72   14.15   17.69   17.05   19.93   22.17 
    ## 
    ## $Geneva
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   10.22   13.90   17.44   16.87   20.07   22.22

``` r
tapply(WeatherData[WeatherData$Month %in% c("May","Jun","Jul","Aug","Sep","Oct"),]$PPT_cm,
      WeatherData[WeatherData$Month %in% c("May","Jun","Jul","Aug","Sep","Oct"),]$Site, summary)
```

    ## $Aurora
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   4.801   8.928   9.779  10.046  11.392  14.275 
    ## 
    ## $Geneva
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   6.807   8.439  10.122  10.412  12.021  14.783

``` r
WeatherData2<-pivot_longer(WeatherData,col=c(Temp_C,PPT_cm),names_to = "Variable",values_to = "val")
WeatherData2$Variable<-factor(WeatherData2$Variable,
                              levels = c("Temp_C","PPT_cm"),
                              labels = c("Mean temperature","Cumulative monthly precipitation"))

ggplot(WeatherData2[WeatherData2$Month %in% c("May","Jun","Jul","Aug","Sep","Oct"),],aes(x=Month2,y=val,color=Site,lty=Year))+
  geom_line()+
  labs(y=expression(atop(paste("Temperature (", degree,"C)"))),
       x="Month")+
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Precipitation (cm)"))+
  scale_x_continuous(breaks = 1:12,
    labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul",
               "Aug","Sep","Oct","Nov","Dec"))+
  theme_bw(base_size = 14)+
  theme(panel.grid.minor = element_blank(),
        plot.margin=unit(c(1,1,1,1), "cm"),
        axis.title.y.left = element_text(vjust=-6),
        axis.title.y.right = element_text(vjust=3))+
  facet_wrap(~Variable)
```

![](SoyRyeAnalysis_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

# Soybeans

``` r
BiomassData<-read.xlsx(here("Data","Soybean_Wt.xlsx"))
table(BiomassData$Plot,BiomassData$Site) #The missing observation was not repeated
```

    ##     
    ##      Aur Gen
    ##   1    3   3
    ##   2    3   3
    ##   3    3   3
    ##   4    3   3
    ##   5    3   3
    ##   6    3   3
    ##   7    3   3
    ##   8    3   3
    ##   9    3   3
    ##   10   3   3
    ##   11   2   3
    ##   12   3   3
    ##   13   3   3
    ##   14   3   3
    ##   15   3   3
    ##   16   3   3
    ##   17   3   3
    ##   18   3   3
    ##   19   3   3
    ##   20   3   3

``` r
BiomassData<-BiomassData[,-c(1)]

BiomassData<-BiomassData %>% 
  mutate(Seeding=ifelse(Site=="Aur" & 
                            Plot%in% c(1,9,14,17),300,
                          ifelse(Site=="Aur" & 
                                 Plot%in% c(2,6,13,19),225,
                                 ifelse(Site=="Aur" &
                                          Plot%in% c(4,8,12,20), 150,
                                        ifelse(Site=="Aur" &
                                                 Plot%in% c(5,7,15,18), 75,
                                               ifelse(Site=="Aur" &
                                                        Plot%in% c(3,10,11,16),0,
                    ifelse(Site=="Gen" & 
                             Plot%in% c(4,10,13,16),300,
                           ifelse(Site=="Gen" &
                                     Plot%in% c(1,8,14,17),225,
                                  ifelse(Site=="Gen" &
                                           Plot%in% c(3,7,15,19),150,
                                         ifelse(Site=="Gen" &
                                                  Plot%in% c(2,9,11,20),75,
                                                0)))))))))) %>%
  dplyr::select(Site,Block,Plot,Treatment,Seeding,everything()) 
BiomassData$Treatment<-dplyr::recode(BiomassData$Treatment,High="120",Low="60",None="0")
BiomassData<-dplyr::rename(BiomassData,Nitrogen=Treatment)
BiomassData<-dplyr::rename(BiomassData,SoyBiomass='dry_wt.(g)')

BiomassData[c("Site","Block","Seeding")] <- 
  lapply(BiomassData[c("Site","Block","Seeding")], factor)

BiomassData$Nitrogen<-as.factor(BiomassData$Nitrogen)
BiomassData$Nitrogen<-factor(BiomassData$Nitrogen,levels = c("0","60","120"))

BiomassData[BiomassData$Site=="Gen",]$Seeding<-
  dplyr::recode(BiomassData[BiomassData$Site=="Gen",]$Seeding,
                "150"="75", "75"="150","225"="300","300"="225","0"="0")
BiomassData$Seeding<-as.factor(BiomassData$Seeding)

ggplot(BiomassData[!BiomassData$Seeding==0,],aes(Seeding,SoyBiomass,fill=Nitrogen))+
  geom_bar(stat='summary',fun="mean",na.rm=TRUE,position = "dodge")+
  stat_summary(geom = "errorbar",fun.data = "mean_se",na.rm = TRUE,
               width=.1,position = position_dodge(.9))+
  ylab("Total soybean biomass (g/0.5m2)")+
  scale_fill_grey(start = 0.8, end = 0.4)+
  theme_bw(base_size = 14)+
  theme(legend.position = "none")+
  facet_wrap(~Site)
```

![](SoyRyeAnalysis_files/figure-gfm/loading%20in%20soy%20biomoss-1.png)<!-- -->

``` r
#Not incluing seeding rate zero

SoyBiomassMod<-lmer(SoyBiomass~Seeding*Nitrogen+(1|Site/Block),
     data = BiomassData[!BiomassData$Seeding==0,])

plot(SoyBiomassMod)
```

![](SoyRyeAnalysis_files/figure-gfm/loading%20in%20soy%20biomoss-2.png)<!-- -->

``` r
hist(resid(SoyBiomassMod))
```

![](SoyRyeAnalysis_files/figure-gfm/loading%20in%20soy%20biomoss-3.png)<!-- -->

``` r
anova(SoyBiomassMod)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                  Sum Sq Mean Sq NumDF DenDF F value   Pr(>F)   
    ## Seeding          121406   40469     3    77  5.5819 0.001616 **
    ## Nitrogen           8390    4195     2    77  0.5786 0.563084   
    ## Seeding:Nitrogen  39013    6502     6    77  0.8969 0.501660   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Aurora
  SoyBiomassModAur<-lmer(SoyBiomass~Seeding*Nitrogen+(1|Block),
       data = BiomassData[BiomassData$Site=="Aur"&!BiomassData$Seeding==0,])

  plot(SoyBiomassModAur)
```

![](SoyRyeAnalysis_files/figure-gfm/loading%20in%20soy%20biomoss-4.png)<!-- -->

``` r
  hist(resid(SoyBiomassModAur))
```

![](SoyRyeAnalysis_files/figure-gfm/loading%20in%20soy%20biomoss-5.png)<!-- -->

``` r
  anova(SoyBiomassModAur)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                  Sum Sq Mean Sq NumDF DenDF F value  Pr(>F)  
    ## Seeding           32591 10863.6     3    33  1.7740 0.17128  
    ## Nitrogen          31195 15597.5     2    33  2.5471 0.09361 .
    ## Seeding:Nitrogen  12000  1999.9     6    33  0.3266 0.91820  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#Geneva
  SoyBiomassModGen<-lmer(SoyBiomass~Seeding*Nitrogen+(1|Block),
       data = BiomassData[BiomassData$Site=="Gen"&!BiomassData$Seeding==0,])

  plot(SoyBiomassModGen)
```

![](SoyRyeAnalysis_files/figure-gfm/loading%20in%20soy%20biomoss-6.png)<!-- -->

``` r
  hist(resid(SoyBiomassModGen))
```

![](SoyRyeAnalysis_files/figure-gfm/loading%20in%20soy%20biomoss-7.png)<!-- -->

``` r
  anova(SoyBiomassModGen)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                  Sum Sq Mean Sq NumDF DenDF F value   Pr(>F)   
    ## Seeding           99151   33050     3    33  4.6265 0.008277 **
    ## Nitrogen          21830   10915     2    33  1.5279 0.231944   
    ## Seeding:Nitrogen  92463   15411     6    33  2.1572 0.072779 . 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
BiomassData2<-BiomassData
BiomassData2$Seeding<-as.numeric(as.character(BiomassData$Seeding))
BiomassData2$Site<-dplyr::recode_factor(BiomassData2$Site,"Aur"="Aurora","Gen"="Geneva")
```

``` r
YieldData<-read.xlsx(here("Data","SoyRye_CompleteYieldData.xlsx"))
unique(YieldData$Treatment)
```

    ## [1] "Low"  "High" "None"

``` r
YieldData<-YieldData %>% 
  mutate(Seeding=ifelse(Site=="Aur" & 
                            Plot%in% c(1,9,14,17),300,
                          ifelse(Site=="Aur" & 
                                 Plot%in% c(2,6,13,19),225,
                                 ifelse(Site=="Aur" &
                                          Plot%in% c(4,8,12,20), 150,
                                        ifelse(Site=="Aur" &
                                                 Plot%in% c(5,7,15,18), 75,
                                               ifelse(Site=="Aur" &
                                                        Plot%in% c(3,10,11,16),0,
                    ifelse(Site=="Gen" & 
                             Plot%in% c(4,10,13,16),300,
                           ifelse(Site=="Gen" &
                                     Plot%in% c(1,8,14,17),225,
                                  ifelse(Site=="Gen" &
                                           Plot%in% c(3,7,15,19),150,
                                         ifelse(Site=="Gen" &
                                                  Plot%in% c(2,9,11,20),75,
                                                0)))))))))) %>%
  dplyr::select(Site,Block,Plot,Treatment,Seeding,everything()) 
YieldData$Treatment<-dplyr::recode(YieldData$Treatment,High="120",Low="60",None="0")
YieldData<-dplyr::rename(YieldData,Nitrogen=Treatment)

#YieldData<-YieldData[-67,]

YieldData$Nitrogen<-as.factor(YieldData$Nitrogen)
YieldData$Nitrogen<-factor(YieldData$Nitrogen,levels = c("0","60","120"))

YieldData[YieldData$Site=="Gen",]$Seeding<-
  dplyr::recode(YieldData[YieldData$Site=="Gen",]$Seeding,
                "150"="75", "75"="150","225"="300","300"="225","0"="0")
YieldData$Seeding<-as.factor(YieldData$Seeding)

YieldData %>% 
  group_by(Site,Seeding) %>% 
  summarise(AvgTestWeight=mean(TestWeight_lbsBu,na.rm=T)) %>% 
  arrange(Site,Seeding)
```

    ## `summarise()` has grouped output by 'Site'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 10 × 3
    ## # Groups:   Site [2]
    ##    Site  Seeding AvgTestWeight
    ##    <chr> <fct>           <dbl>
    ##  1 Aur   0                 0  
    ##  2 Aur   150              56.6
    ##  3 Aur   225              56.0
    ##  4 Aur   300              55.9
    ##  5 Aur   75               55.5
    ##  6 Gen   0                 0  
    ##  7 Gen   150              59.2
    ##  8 Gen   225              56.9
    ##  9 Gen   300              55.6
    ## 10 Gen   75               61.8

``` r
YieldData2<-YieldData %>% 
  filter(!Seeding==0) %>% 
  mutate(AvgSeedWeight_g=SeedWeight_g/Seed_num)

YieldData2$Seeding<-factor(YieldData2$Seeding,
                           levels=c(0,75,150,225,300))
```

\##Emergence -Make sure yield is loaded first -I have confirmed that the
conversions to plants/ha are correct.

``` r
EarlyCounts<-read.xlsx(here("Data","EarlyCounts.xlsx"))
EarlyCounts$Nitrogen<-dplyr::recode(EarlyCounts$Nitrogen,High="120",Mid="60",Low="0")

CountsV1<-EarlyCounts %>% 
  mutate(Soy_num=(43560/(30/12)*(Soy_num/3.28084)),
         Stage="V1")
CountsV1$Seeding<-as.factor(CountsV1$Seeding)
CountsV1[CountsV1$Site=="Gen",]$Seeding<-
  dplyr::recode(CountsV1[CountsV1$Site=="Gen",]$Seeding,
                "150"="75", "75"="150","225"="300","300"="225","0"="0")

#Don't forget to load Yield data above to get the correction for the R8 dataframe
CountsR8<-YieldData %>% 
  dplyr::select(Site, Block, Plot, Nitrogen, Seeding, Soy_num) %>% 
  mutate(Soy_num=(43560/(30/12)*(Soy_num/3.28084)),
         Stage="R8")

EmergenceData<-rbind(CountsV1,CountsR8)
EmergenceData$Stage<-factor(EmergenceData$Stage,levels = c("V1","R8"))
EmergenceData$Seeding<-factor(EmergenceData$Seeding,
                              levels = c(0,75,150,225,300))

EmergenceData_Long<-EmergenceData %>% 
  mutate_if(is.numeric, round,0) %>%
  mutate(ID=paste(Site,Block,Nitrogen,Seeding,sep = ".")) %>% 
  pivot_wider(names_from = Stage,
              values_from = Soy_num)

EmergenceData_Long %>% 
  group_by(Site,Seeding) %>% 
  do(tTestMod=t.test(.$V1,.$R8,alternative="two.sided")) %>% 
  summarise(p_value=tTestMod$p.value)
```

    ## # A tibble: 10 × 1
    ##    p_value
    ##      <dbl>
    ##  1 NaN    
    ##  2   0.333
    ##  3   0.774
    ##  4   0.204
    ##  5   0.913
    ##  6 NaN    
    ##  7   0.121
    ##  8   0.868
    ##  9   0.269
    ## 10   0.355

``` r
EmergenceData_Long %>% 
  group_by(Site,Seeding) %>%
  summarise(SoyNumV1=mean(V1,na.rm=T),SoyDevV1=sd(V1,na.rm=T),
            SoyNumR8=mean(R8,na.rm=T),SoyDevR8=sd(R8,na.rm=T),
            PctChange=((mean(R8,na.rm=T)-mean(V1,na.rm=T))/mean(V1,na.rm=T)*100)) %>% 
  mutate_if(is.numeric, round,0) %>% 
  mutate(SoyNumV1=paste(SoyNumV1,paste("(",SoyDevV1,")",sep="")),
         SoyNumR8=paste(SoyNumR8,paste("(",SoyDevR8,")",sep=""))) %>% 
  dplyr::select(-c(SoyDevV1,SoyDevR8))
```

    ## `summarise()` has grouped output by 'Site'. You can override using the
    ## `.groups` argument.
    ## `mutate_if()` ignored the following grouping variables:

    ## # A tibble: 10 × 5
    ## # Groups:   Site [2]
    ##    Site  Seeding SoyNumV1       SoyNumR8       PctChange
    ##    <chr> <fct>   <chr>          <chr>              <dbl>
    ##  1 Aur   0       0 (0)          0 (0)                NaN
    ##  2 Aur   75      69041 (9268)   64173 (14225)         -7
    ##  3 Aur   150     127239 (12114) 125247 (20313)        -2
    ##  4 Aur   225     183002 (24637) 197386 (28985)         8
    ##  5 Aur   300     212212 (20142) 213761 (44209)         1
    ##  6 Gen   0       0 (0)          0 (0)                NaN
    ##  7 Gen   75      60853 (12783)  52666 (12080)        -13
    ##  8 Gen   150     110421 (20489) 111970 (24333)         1
    ##  9 Gen   225     184773 (16774) 198271 (37164)         7
    ## 10 Gen   300     222170 (34905) 237217 (42627)         7

``` r
EmergenceData[c("Site","Block","Seeding","Stage")] <- 
  lapply(EmergenceData[c("Site","Block","Seeding","Stage")], factor)
EmergenceData$Soy_num<-round(EmergenceData$Soy_num,0)

EmergenceDataTest<-EmergenceData
EmergenceDataTest$SeedingMetric<-
  dplyr::recode(EmergenceDataTest$Seeding,
                "0"="0","75"="185,300","150"="370,700","225"="556,000","300"="741,300")
EmergenceDataTest<-EmergenceDataTest %>% 
  mutate(Soy_num_metric=Soy_num*2.47)
EmergenceDataTest$Nitrogen<-as.factor(EmergenceDataTest$Nitrogen)

EmergenceDataTest2<-EmergenceDataTest[!EmergenceDataTest$Seeding==0,]
EmergenceDataTest2$Block<-as.numeric(as.character(EmergenceDataTest2$Block))
EmergenceDataTest2<-EmergenceDataTest2 %>% 
  mutate(Block=case_when(
    Site=="Gen" ~ Block+4,
    TRUE~Block))

EmTest2<-lmer(log(Soy_num_metric)~Site+SeedingMetric*Stage*Nitrogen+
               (1|Block/SeedingMetric),
             data = EmergenceDataTest2)

plot(EmTest2)
```

![](SoyRyeAnalysis_files/figure-gfm/Emergence%20setup%20and%20analysis-1.png)<!-- -->

``` r
anova(EmTest2)
```

    ## Type III Analysis of Variance Table with Satterthwaite's method
    ##                               Sum Sq Mean Sq NumDF   DenDF  F value  Pr(>F)    
    ## Site                          0.0792  0.0792     1   6.000   2.6344 0.15570    
    ## SeedingMetric                30.9015 10.3005     3  21.001 342.7662 < 2e-16 ***
    ## Stage                         0.0035  0.0035     1 140.000   0.1156 0.73439    
    ## Nitrogen                      0.1193  0.0596     2 140.000   1.9848 0.14126    
    ## SeedingMetric:Stage           0.2240  0.0747     3 140.000   2.4851 0.06322 .  
    ## SeedingMetric:Nitrogen        0.1298  0.0216     6 140.000   0.7200 0.63417    
    ## Stage:Nitrogen                0.0118  0.0059     2 140.000   0.1956 0.82256    
    ## SeedingMetric:Stage:Nitrogen  0.2413  0.0402     6 140.000   1.3385 0.24405    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
###
cld(emmeans(EmTest2, specs = ~SeedingMetric|Stage),
            sort=F,adjust="none",Letters="abcde")
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## Stage = V1:
    ##  SeedingMetric emmean     SE   df lower.CL upper.CL .group
    ##  185,300         12.0 0.0408 63.4     11.9     12.1  a    
    ##  370,700         12.6 0.0408 63.4     12.5     12.7   b   
    ##  556,000         13.0 0.0408 63.4     12.9     13.1    c  
    ##  741,300         13.2 0.0408 63.4     13.1     13.3     d 
    ## 
    ## Stage = R8:
    ##  SeedingMetric emmean     SE   df lower.CL upper.CL .group
    ##  185,300         11.9 0.0408 63.4     11.8     11.9  a    
    ##  370,700         12.6 0.0408 63.4     12.5     12.6   b   
    ##  556,000         13.1 0.0408 63.4     13.0     13.2    c  
    ##  741,300         13.2 0.0408 63.4     13.1     13.3     d 
    ## 
    ## Results are averaged over the levels of: Site, Nitrogen 
    ## Degrees-of-freedom method: kenward-roger 
    ## Results are given on the log (not the response) scale. 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.

``` r
pairs(emmeans(EmTest2, specs = ~Stage|SeedingMetric))
```

    ## NOTE: Results may be misleading due to involvement in interactions

    ## SeedingMetric = 185,300:
    ##  contrast estimate   SE  df t.ratio p.value
    ##  V1 - R8   0.11749 0.05 140   2.348  0.0203
    ## 
    ## SeedingMetric = 370,700:
    ##  contrast estimate   SE  df t.ratio p.value
    ##  V1 - R8   0.00834 0.05 140   0.167  0.8679
    ## 
    ## SeedingMetric = 556,000:
    ##  contrast estimate   SE  df t.ratio p.value
    ##  V1 - R8  -0.06679 0.05 140  -1.335  0.1841
    ## 
    ## SeedingMetric = 741,300:
    ##  contrast estimate   SE  df t.ratio p.value
    ##  V1 - R8  -0.02501 0.05 140  -0.500  0.6181
    ## 
    ## Results are averaged over the levels of: Site, Nitrogen 
    ## Degrees-of-freedom method: kenward-roger 
    ## Results are given on the log (not the response) scale.

``` r
range(EmergenceDataTest[EmergenceDataTest$Stage=="V1",]$Soy_num_metric)
```

    ## [1]      0.0 701801.1

``` r
EmergenceDataTest2<-EmergenceDataTest
EmergenceDataTest2$SeedingMetric<-gsub(",","",EmergenceDataTest2$SeedingMetric)
EmergenceDataTest2$SeedingMetric<-as.numeric(as.character(EmergenceDataTest2$SeedingMetric))

#tiff("test.tiff", units="in", width=5.82, height=4.10, res=300)
ggplot(EmergenceDataTest2,aes(SeedingMetric,Soy_num_metric,color=Stage))+
  geom_smooth(method = "lm",se=F,size=.75)+
  geom_abline(intercept = 0, slope = .9, color="black", 
              linetype="dashed", size=.75)+
  geom_point()+
  labs(x=expression("Seeding rate (seeds"~ha^-1*')'),
       y=expression("Soybean density (plants"~ha^-1*')'),
       fill="Soybean\ngrowth stage")+
  theme_bw(base_size = 14)+
  theme(legend.position = "right")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma,
                     breaks=c(0,185300,370700,556000,741300))
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Ignoring unknown labels:
    ## • fill : "Soybean growth stage"
    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 1 row containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](SoyRyeAnalysis_files/figure-gfm/Emergence%20setup%20and%20analysis-2.png)<!-- -->

``` r
#dev.off()

EmergenceDataTest %>% 
  filter(Stage=="V1") %>% 
  group_by(SeedingMetric) %>% 
  summarize(MeanPlants=mean(Soy_num_metric))
```

    ## # A tibble: 5 × 2
    ##   SeedingMetric MeanPlants
    ##   <fct>              <dbl>
    ## 1 0                     0 
    ## 2 185,300          160419.
    ## 3 370,700          293510.
    ## 4 556,000          454202.
    ## 5 741,300          536462.

\##Yeild

-Yield calculated at 13% moisture and accounts for sampling method. This
was only done for the final metric, SeedWeight_kgha. -The full model
does not work with nlme()

``` r
EmergenceData2<-EmergenceData[EmergenceData$Stage=="V1",]
EmergenceData2$Seeding<-as.numeric(as.character(EmergenceData2$Seeding))
EmergenceData2$Nitrogen<-factor(EmergenceData2$Nitrogen, levels = c("0","60","120"))
EmergenceData2$Site<-dplyr::recode(EmergenceData2$Site,"Aur"="Aurora","Gen"="Geneva")

YieldData3<-YieldData2
YieldData3$Site<-factor(YieldData3$Site,levels = c("Aur","Gen"),labels = c("Aurora","Geneva"))
YieldData3$Seeding<-as.numeric(as.character(YieldData3$Seeding))
YieldData3$Soy_num<-NULL
YieldData3$Block<-as.factor(YieldData3$Block)
YieldData3<-left_join(YieldData3,
          EmergenceData2[!EmergenceData2$Seeding==0,-which(names(EmergenceData2) %in% c("Stage"))])
```

    ## Joining with `by = join_by(Site, Block, Plot, Nitrogen, Seeding)`

``` r
YieldData3<-YieldData3 %>% 
  mutate(Soy_num_metric=Soy_num*2.47,
         SeedWeight_kgha=SeedWeight_g*(5.33/.001)*(2.47/1)*(1/1000)*
           (100/87)) #conversion to kg/ha; last fraction is moisture adju.

YieldData5<-YieldData
YieldData5<-YieldData5 %>% 
  mutate(Soy_num=(43560/(30/12)*(Soy_num/3.28084)))
YieldData5$Site<-factor(YieldData5$Site,levels = c("Aur","Gen"),labels = c("Aurora","Geneva"))
YieldData5$Seeding<-as.numeric(as.character(YieldData5$Seeding))
YieldData5$Soy_num<-NULL
YieldData5$Block<-as.factor(YieldData5$Block)
YieldData5<-left_join(YieldData5,
          EmergenceData2[,-which(names(EmergenceData2) %in% c("Stage"))])
```

    ## Joining with `by = join_by(Site, Block, Plot, Nitrogen, Seeding)`

``` r
YieldData5<-YieldData5 %>% 
  mutate(Soy_num_metric=Soy_num*2.47,
         SeedWeight_kgha=SeedWeight_g*(5.33/.001)*(2.47/1)*(1/1000)*
           (100/87),#conversion to kg/ha; last fraction is moisture adju.
         NitrogenMetric=case_when(
           Nitrogen==0~0,
           Nitrogen==60~70,
           Nitrogen==120~135))
YieldData5$NitrogenMetric<-as.factor(YieldData5$NitrogenMetric)
```

``` r
#asymptotic
boxplot(YieldData5$SeedWeight_kgha)
```

![](SoyRyeAnalysis_files/figure-gfm/yield%20Modeling-1.png)<!-- -->

``` r
##Reduced
Asy_Red<-nlme(data=YieldData5,
    SeedWeight_kgha~a*(1-exp(-exp(b)*Soy_num_metric)),
    start = c(a=3000,b=-11.8), #I had to change a to get a fit when I re-ran code in 2025
    fixed= list(a~1,b ~ 1),
    random = list(Site=pdDiag(list(a~1,b ~ 1)),
                  Block=pdDiag(list(a~1,b ~ 1))),
    na.action = na.omit)

##Site
AsySite_Semi<-nlme(data=YieldData5,
    SeedWeight_kgha~a*(1-exp(-exp(b)*Soy_num_metric)),
    start = c(3500,3000,-11.8),
    fixed= list(a~Site,b ~ 1),
    random = list(Site=pdDiag(list(a~Site,b ~ 1)),
                  Block=pdDiag(list(a~Site,b ~ 1))),
    na.action = na.omit)

#AsySite_Full<-nlme(data=YieldData5,
#    SeedWeight_kgha~a*(1-exp(-exp(b)*Soy_num_metric)),
#    start = c(3573.4,3152.2,-11.8,-11.8),
#    fixed= list(a+b ~ Site),
#    random = list(Site=pdDiag(list(a ~ Site,b~Site)),
#                  Block=pdDiag(list(a ~ Site,b~Site))),
#    na.action = na.omit,
#    control = list(pnlsTol = 1e-6,msMaxIter=5000))

anova(Asy_Red,AsySite_Semi)
```

    ##              Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## Asy_Red          1  7 1944.362 1963.816 -965.1810                        
    ## AsySite_Semi     2 10 1945.786 1973.577 -962.8929 1 vs 2 4.576302  0.2056

``` r
library(sjPlot)
```

    ## 
    ## Attaching package: 'sjPlot'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     set_theme

``` r
coef(summary(AsySite_Semi))
```

    ##                    Value   Std.Error  DF    t-value      p-value
    ## a.(Intercept) 3569.86217 175.5699098 109  20.332995 6.905554e-39
    ## a.SiteGeneva  -417.18107 235.6028839 109  -1.770696 7.940671e-02
    ## b              -11.78563   0.1733751 109 -67.977638 4.438303e-91

``` r
fixef(AsySite_Semi)[1]
```

    ## a.(Intercept) 
    ##      3569.862

``` r
#t.test(fixef(AsySite_Semi)[1]) not sure why this doesn't work
fun.1YieldSite<- function(x) fixef(Asy_Red)[1]*(1-exp(-exp(fixef(Asy_Red)[2])*x))
fun.2.1YieldSite<- function(x) fixef(AsySite_Semi)[1]*(1-exp(-exp(fixef(AsySite_Semi)[3])*x))
fun.2.2YieldSite<- function(x) (fixef(AsySite_Semi)[1]+fixef(AsySite_Semi)[2])*(1-exp(-exp(fixef(AsySite_Semi)[3])*x)) 


library(grid)
#Optimums where used in the publication
optimize(fun.2.1YieldSite, interval=c(0,741300),maximum = T)
```

    ## $maximum
    ## [1] 741300
    ## 
    ## $objective
    ## a.(Intercept) 
    ##      3557.225

``` r
optimize(fun.2.2YieldSite, interval=c(0,741300),maximum = T)
```

    ## $maximum
    ## [1] 741300
    ## 
    ## $objective
    ## a.(Intercept) 
    ##      3141.521

``` r
###Semi-reduced graph-Site
#tiff("Fig4.tiff", units="in", width=5.82, height=5.54, res=300)
ggplot(YieldData5,aes(x=Soy_num_metric,y=SeedWeight_kgha))+#,color=Site))+
    geom_point()+
    stat_function(fun=fun.1YieldSite,size=1.2)+
    theme_bw(base_size = 18)+
    labs(y=expression("Soybean yield (kg"~ha^-1*')'),
         x=expression("Soybean density (plants"~ha^-1*')'),
         color="Site")+
    scale_x_continuous(labels = scales::comma)+
    scale_color_brewer(palette = "Accent")+
    theme(legend.position = "bottom")
```

    ## Ignoring unknown labels:
    ## • colour : "Site"

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](SoyRyeAnalysis_files/figure-gfm/yield%20Modeling-2.png)<!-- -->

``` r
#dev.off()

##Nitrogen-FULL DOES NOT WORK
AsyNitrogen_Semi<-nlme(data=YieldData5,
    SeedWeight_kgha~a*(1-exp(-exp(b)*Soy_num_metric)),
    start = c(3500,3000,3000,-11.8),
    fixed= list(a~Nitrogen,b ~ 1),
    random = list(Site=pdDiag(list(a~Nitrogen,b ~ 1)),
                  Block=pdDiag(list(a~Nitrogen,b ~ 1))),
    na.action = na.omit)

#AsyNitrogen_Full<-nlme(data=YieldData5,
#    SeedWeight_kgha~a*(1-exp(-exp(b)*Soy_num_metric)),
#    start = c(3573.4,3152.2,3152.2,-11.8,-11.8,-11.8),
#    fixed= list(a+b ~ Nitrogen),
#    random = list(Site=pdDiag(list(a ~ Nitrogen,b~Nitrogen)),
#                  Block=pdDiag(list(a ~ Nitrogen,b~Nitrogen))),
#    na.action = na.omit,
#    control = list(pnlsTol = 1e-6,msMaxIter=5000))

anova(Asy_Red,AsyNitrogen_Semi)
```

    ##                  Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## Asy_Red              1  7 1944.362 1963.816 -965.1810                        
    ## AsyNitrogen_Semi     2 13 1953.270 1989.398 -963.6349 1 vs 2 3.092179  0.7972

``` r
fixef(Asy_Red)
```

    ##          a          b 
    ## 3360.14033  -11.78205

``` r
fixef(AsySite_Semi)
```

    ## a.(Intercept)  a.SiteGeneva             b 
    ##    3569.86217    -417.18107     -11.78563

``` r
fixef(AsyNitrogen_Semi)
```

    ## a.(Intercept)  a.Nitrogen60 a.Nitrogen120             b 
    ##    3404.81330     -26.10367    -157.04189     -11.75669

\#Weeds

``` r
SR_weedsRaw<-read.xlsx(here("Data","SR_weedsRaw.xlsx"))
colnames(SR_weedsRaw)[6]<-"Biomass"
unique(SR_weedsRaw$Treatment)
```

    ## [1] "High" "Low"  "None"

``` r
summary(as.factor(SR_weedsRaw$Site))
```

    ## Aur Gen 
    ## 431 274

``` r
table(SR_weedsRaw$Plot[SR_weedsRaw$Site=="Gen"],SR_weedsRaw$Treatment[SR_weedsRaw$Site=="Gen"])
```

    ##     
    ##      High Low None
    ##   1     5   6   10
    ##   2     4   4    9
    ##   3     5   5    4
    ##   4     0   6    6
    ##   5     9   5    7
    ##   6     8   3    6
    ##   7     6   4    6
    ##   8     2   5    3
    ##   9     3   2    6
    ##   10    5   2    8
    ##   11    6   9    6
    ##   12    7   3   10
    ##   13    2   3    2
    ##   14    1   3    5
    ##   15    7   3    6
    ##   16    1   1    2
    ##   17    0   1    7
    ##   18    4   6    4
    ##   19    5   3    2
    ##   20    1   6    4

``` r
SR_weedsRaw<-as.data.frame(
  pivot_wider(SR_weedsRaw,
            names_from = Weed,
            values_from = Biomass,
            values_fill = list(Biomass=0)))

#SR_weedsWide<-createWorkbook()
#addWorksheet(SR_weedsWide,"Wide")
#writeData(SR_weedsWide,"Wide",SR_weedsRaw2,rowNames = T)
#saveWorkbook(SR_weedsWide,"~/Box/Rye-Soybean 2019/Data/Weeds/SR_weedsWide.xlsx",overwrite = T)
SR_weedsWide<-read.xlsx(here("Data","SR_weedsWideEdited.xlsx"),sheet = 1)

SR_weedsWide<-SR_weedsWide %>% 
mutate(Seeding=ifelse(Site=="Aur" & 
                            Plot%in% c(1,9,14,17),300,
                          ifelse(Site=="Aur" & 
                                 Plot%in% c(2,6,13,19),225,
                                 ifelse(Site=="Aur" &
                                          Plot%in% c(4,8,12,20), 150,
                                        ifelse(Site=="Aur" &
                                                 Plot%in% c(5,7,15,18), 75,
                                               ifelse(Site=="Aur" &
                                                        Plot%in% c(3,10,11,16),0,
                    ifelse(Site=="Gen" & 
                             Plot%in% c(4,10,13,16),300,
                           ifelse(Site=="Gen" &
                                     Plot%in% c(1,8,14,17),225,
                                  ifelse(Site=="Gen" &
                                           Plot%in% c(3,7,15,19),150,
                                         ifelse(Site=="Gen" &
                                                  Plot%in% c(2,9,11,20),75,
                                                0)))))))))) %>%
  dplyr::select(Site,Block,Plot,Treatment,Seeding,everything())

#This is the seeding rate correction for the weeds data. All other analysis stems from this data frame.
SR_weedsWide[SR_weedsWide$Site=="Gen",]$Seeding<-
  dplyr::recode(SR_weedsWide[SR_weedsWide$Site=="Gen",]$Seeding,
         "150"="75", "75"="150","225"="300","300"="225", "0"="0")

SR_weedsKey<-read.xlsx(here("Data","SR_weedsWideEdited.xlsx"),sheet = 2)

SR_weedsLong<-as.data.frame(
  pivot_longer(SR_weedsWide,
               -c(Site,Block,Plot,Treatment,Seeding),
               names_to = "Species",
               values_to = "Biomass"))

SR_weedsBiomass<-
  as.data.frame(
  SR_weedsLong %>%
    group_by(Site,Block,Plot,Treatment,Seeding) %>%
    dplyr::select(-Species) %>% 
    summarise(TotalBiomass=sum(Biomass)))
```

    ## `summarise()` has grouped output by 'Site', 'Block', 'Plot', 'Treatment'. You
    ## can override using the `.groups` argument.

``` r
SR_weedsBiomass[c("Site","Block","Treatment","Seeding")]<-lapply(SR_weedsBiomass[c("Site","Block","Treatment","Seeding")], factor)

SR_weedsBiomass$Seeding<-factor(SR_weedsBiomass$Seeding,levels = c("0","75","150","225","300"))
```

\##Biomass EmergenceData2 needs to be loaded. Two outliers (weeds above
300 kg/ha) were removed for model convergence.

``` r
#Setup
SR_weedsBiomass2<-SR_weedsBiomass %>% 
  group_by(Site,Treatment, Seeding)%>% 
  mutate(Mean_lbs..ac=TotalBiomass*((.5/0.000123553)*.0022))
SR_weedsBiomass2$Treatment<-factor(SR_weedsBiomass2$Treatment,
                                   levels = c("None","Low","High"),
                                   labels = c("0","60","120"))
SR_weedsBiomass2<-rename(SR_weedsBiomass2,"Nitrogen"="Treatment")

SR_weedsBiomass3<-SR_weedsBiomass2
SR_weedsBiomass3$Seeding<-as.numeric(as.character(SR_weedsBiomass3$Seeding))
SR_weedsBiomass3$Site<-dplyr::recode(SR_weedsBiomass3$Site,"Aur"="Aurora","Gen"="Geneva")
SR_weedsBiomass3<-
  left_join(SR_weedsBiomass3,EmergenceData2, by = c("Site", "Block", "Plot",
                                                    "Nitrogen", "Seeding"))
BiomassData2<-BiomassData2 %>% 
  dplyr::select(-c(Plant,'Soy#'))
SR_weedsBiomass3<-
  left_join(SR_weedsBiomass3,BiomassData2, by = c("Site", "Block", "Plot",
                                                  "Nitrogen", "Seeding"))

SR_weedsBiomass3<-SR_weedsBiomass3 %>% 
  mutate(Soy_num_metric=Soy_num*2.47105,
         TotalBiomass_metric=TotalBiomass*(2.47105/2.20462),
         log_TotalBiomass_metric=log(TotalBiomass_metric),
         sqrt_TotalBiomass_metric=sqrt(TotalBiomass_metric))

SR_weedsBiomass3<-SR_weedsBiomass3 %>% 
  mutate(NitrogenMetric=case_when(Nitrogen==60~70,
                                  Nitrogen==120~135,
                                  TRUE~0))
#Outliers
boxplot(SR_weedsBiomass3$TotalBiomass_metric)
```

![](SoyRyeAnalysis_files/figure-gfm/biomass%20modeling-1.png)<!-- -->

``` r
grubbs.test(SR_weedsBiomass3$TotalBiomass_metric)
```

    ## 
    ##  Grubbs test for one outlier
    ## 
    ## data:  SR_weedsBiomass3$TotalBiomass_metric
    ## G = 2.9989, U = 0.9218, p-value = 0.1323
    ## alternative hypothesis: highest value 332.466752093331 is an outlier

``` r
#Info
range(SR_weedsBiomass3$TotalBiomass_metric,na.rm = T)
```

    ## [1]   0.02051157 332.46675209

``` r
SR_weedsBiomass3 %>% 
  filter(Seeding==0) %>% 
  group_by(Nitrogen) %>% 
  summarise(Mean=mean(TotalBiomass_metric,na.rm=T))
```

    ## # A tibble: 3 × 2
    ##   Nitrogen  Mean
    ##   <fct>    <dbl>
    ## 1 0         107.
    ## 2 60        182.
    ## 3 120       221.

``` r
#Site - No effect
Weed_Red<-
  nlme(data = SR_weedsBiomass3[SR_weedsBiomass3$TotalBiomass_metric<300,],
     TotalBiomass_metric~
       w0/(1+i*Soy_num_metric),
     start = c(1.793565e+02,4.584e-06),
     fixed= list(w0~1,i ~ 1),
     random = list(Site=pdDiag(list(w0~1,i ~ 1)),
                   Block=pdDiag(list(w0~1,i ~ 1))),
     na.action = na.omit)

Weed_SemiSite<-
  nlme(data = SR_weedsBiomass3[SR_weedsBiomass3$TotalBiomass_metric<300,],
     TotalBiomass_metric~
       w0/(1+i*Soy_num_metric),
     start = c(1.793565e+02,1.793565e+02,4.584e-06),
     fixed= list(w0~Site,i ~ 1),
     random = list(Site=pdDiag(list(w0~Site,i ~ 1)),
                   Block=pdDiag(list(w0~Site,i ~ 1))),
     na.action = na.omit)

Weed_FullSite<-
  nlme(data = SR_weedsBiomass3[SR_weedsBiomass3$TotalBiomass_metric<300,],
     TotalBiomass_metric~
       w0/(1+i*Soy_num_metric),
     start = c(w0=1.793565e+02,0,0, i=4.584e-06),
     fixed= list(w0+i ~ Site),
     random = list(Site=pdDiag(list(w0~Site,i ~ Site)),
                   Block=pdDiag(list(w0~Site,i ~ Site))),
     na.action = na.omit)

anova(Weed_Red,Weed_SemiSite)
```

    ##               Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## Weed_Red          1  7 1280.129 1299.283 -633.0646                        
    ## Weed_SemiSite     2 10 1284.091 1311.453 -632.0455 1 vs 2 2.038163  0.5645

``` r
anova(Weed_Red,Weed_FullSite)
```

    ##               Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## Weed_Red          1  7 1280.129 1299.283 -633.0646                        
    ## Weed_FullSite     2 13 1288.959 1324.530 -631.4794 1 vs 2 3.170281  0.7872

``` r
anova(Weed_SemiSite,Weed_FullSite)
```

    ##               Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## Weed_SemiSite     1 10 1284.091 1311.453 -632.0455                        
    ## Weed_FullSite     2 13 1288.959 1324.530 -631.4794 1 vs 2 1.132118  0.7693

``` r
#Nitrogen effect - semi is best
Weed_SemiNitrogen<-
  nlme(data = SR_weedsBiomass3[SR_weedsBiomass3$TotalBiomass_metric<300,],
     TotalBiomass_metric~
       w0/(1+i*Soy_num_metric),
     start = c(1.793565e+02,1.793565e+02,1.793565e+02,4.584e-06),
     fixed= list(w0~Nitrogen,i ~ 1),
     random = list(Site=pdDiag(list(w0~Nitrogen,i ~ 1)),
                   Block=pdDiag(list(w0~Nitrogen,i ~ 1))),
     na.action = na.omit)

###Estimates for fitting
TestList<-nlsList(data = SR_weedsBiomass3,
     TotalBiomass_metric~
       w0/(1+i*Soy_num_metric)|Nitrogen,
     start = c(w0=1.793565e+02,i=4.584e-06),
     na.action = na.omit)
###

Weed_FullNitrogen<-
  nlme(data = SR_weedsBiomass3[SR_weedsBiomass3$TotalBiomass_metric<300,],
     TotalBiomass_metric~
       w0/(1+i*Soy_num_metric),
     start = c(coef(TestList)[1,1],coef(TestList)[2,1],coef(TestList)[3,1],
               coef(TestList)[1,2],coef(TestList)[2,2],coef(TestList)[3,2]),
     fixed= list(w0+i ~ Nitrogen),
     random = list(Site=pdDiag(w0+i ~ Nitrogen),
                   Block=pdDiag(w0+i ~ Nitrogen)),
     na.action = na.omit)

anova(Weed_Red,Weed_SemiNitrogen)
```

    ##                   Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## Weed_Red              1  7 1280.129 1299.283 -633.0646                        
    ## Weed_SemiNitrogen     2 13 1270.316 1305.887 -622.1582 1 vs 2 21.81267  0.0013

``` r
anova(Weed_SemiNitrogen,Weed_FullNitrogen)
```

    ##                   Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## Weed_SemiNitrogen     1 13 1270.316 1305.887 -622.1582                        
    ## Weed_FullNitrogen     2 19 1277.460 1329.447 -619.7298 1 vs 2 4.856785  0.5623

``` r
anova(Weed_Red,Weed_FullNitrogen)
```

    ##                   Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## Weed_Red              1  7 1280.129 1299.283 -633.0646                        
    ## Weed_FullNitrogen     2 19 1277.460 1329.447 -619.7298 1 vs 2 26.66945  0.0086

``` r
fixef(Weed_Red)
```

    ##           w0            i 
    ## 1.505285e+02 5.060205e-06

``` r
fixef(Weed_SemiSite)
```

    ## w0.(Intercept)  w0.SiteGeneva              i 
    ##   1.660399e+02  -3.220233e+01   4.960351e-06

``` r
fixef(Weed_SemiNitrogen)
```

    ## w0.(Intercept)  w0.Nitrogen60 w0.Nitrogen120              i 
    ##   9.342138e+01   6.156326e+01   1.032571e+02   4.440511e-06

``` r
fixef(Weed_FullSite)
```

    ## w0.(Intercept)  w0.SiteGeneva  i.(Intercept)   i.SiteGeneva 
    ##   1.570277e+02  -1.723225e+01   3.870624e-06   2.013942e-06

``` r
fixef(Weed_FullNitrogen)
```

    ## w0.(Intercept)  w0.Nitrogen60 w0.Nitrogen120  i.(Intercept)   i.Nitrogen60 
    ##   1.052267e+02   6.998139e+01   6.915540e+01   7.276993e-06   2.118876e-07 
    ##  i.Nitrogen120 
    ##  -4.551385e-06

``` r
plot(Weed_SemiNitrogen)
```

![](SoyRyeAnalysis_files/figure-gfm/biomass%20modeling-2.png)<!-- -->

``` r
qqnorm(Weed_SemiNitrogen,abline = c(0,1))
```

![](SoyRyeAnalysis_files/figure-gfm/biomass%20modeling-3.png)<!-- -->

``` r
WeedModFE<-fixef(Weed_SemiNitrogen)
fun.1nit<-function(x) WeedModFE[1]*(1/(1+(WeedModFE[4]*x)))
fun.2nit<-function(x) (WeedModFE[1]+WeedModFE[2])*(1/(1+(WeedModFE[4]*x)))
fun.3nit<-function(x) (WeedModFE[1]+WeedModFE[3])*(1/(1+(WeedModFE[4]*x)))

#tiff("Fig2.tiff", units="in", width=5.82, height=5.54, res=300)
#pdf("Fig2.pdf", width=5.82, height=5.54)
ggplot(SR_weedsBiomass3,
       aes(x=Soy_num_metric,y=TotalBiomass_metric,color=as.factor(NitrogenMetric)))+
    geom_point()+
    stat_function(fun=fun.1nit,size=1.2,aes(color="0"))+
    stat_function(fun=fun.2nit,size=1.2,aes(color="70"))+
    stat_function(fun=fun.3nit,size=1.2,aes(color="135"))+
    theme_bw(base_size = 18)+
    ylab(expression("Weed biomass (kg"~ha^-1*')'))+
    xlab(expression("Soybean density (plants"~ha^-1*')'))+
    labs(color=expression("Nitrogen (kg"~ha^-1*')'))+
    scale_y_continuous(labels = scales::comma)+
    scale_x_continuous(labels = scales::comma)+
    scale_color_brewer(palette = "Accent",labels = c("0", "63", "125"))+
    theme(plot.title = element_text(hjust = 0.5), 
          legend.position = "bottom")
```

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](SoyRyeAnalysis_files/figure-gfm/biomass%20modeling-4.png)<!-- -->

``` r
#dev.off()

fun.1RedTest<-function(x) fixef(Weed_Red)[1]*(1/(1+(fixef(Weed_Red)[2]*x)))

#pdf("Fig2Mod.pdf", width=5.82, height=5.54)
ggplot(SR_weedsBiomass3,
       aes(x=Soy_num_metric,y=TotalBiomass_metric))+
    geom_point()+
    stat_function(fun=fun.1RedTest,size=1.2)+
    theme_bw(base_size = 18)+
    ylab(expression("Weed biomass (kg"~ha^-1*')'))+
    xlab(expression("Soybean density (plants"~ha^-1*')'))+
    scale_y_continuous(labels = scales::comma)+
    scale_x_continuous(labels = scales::comma)+
    theme(plot.title = element_text(hjust = 0.5), 
          legend.position = "bottom")
```

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](SoyRyeAnalysis_files/figure-gfm/biomass%20modeling-5.png)<!-- -->

``` r
#dev.off()


WeedModFE2 <- fixef(Weed_FullNitrogen)
fun.1FullTest<-function(x) WeedModFE2[1]*(1/(1+(WeedModFE2[4]*x)))
fun.2FullTest<-function(x) WeedModFE2[2]*(1/(1+(WeedModFE2[5]*x)))
fun.3FullTest<-function(x) WeedModFE2[3]*(1/(1+(WeedModFE2[6]*x)))

#pdf("Fig2Mod.pdf", width=5.82, height=5.54)
ggplot(SR_weedsBiomass3,
       aes(x=Soy_num_metric,y=TotalBiomass_metric,color=as.factor(NitrogenMetric)))+
    geom_point()+
    stat_function(fun=fun.1FullTest,size=1.2,aes(color="0"))+
    stat_function(fun=fun.2FullTest,size=1.2,aes(color="70"))+
    stat_function(fun=fun.3FullTest,size=1.2,aes(color="135"))+
    theme_bw(base_size = 18)+
    ylab(expression("Weed biomass (kg"~ha^-1*')'))+
    xlab(expression("Soybean density (plants"~ha^-1*')'))+
    labs(color=expression("Nitrogen (kg"~ha^-1*')'))+
    scale_y_continuous(labels = scales::comma)+
    scale_x_continuous(labels = scales::comma)+
    scale_color_brewer(palette = "Accent",labels = c("0", "63", "125"))+
    theme(plot.title = element_text(hjust = 0.5), 
          legend.position = "bottom")
```

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](SoyRyeAnalysis_files/figure-gfm/biomass%20modeling-6.png)<!-- -->

``` r
#dev.off()
```

\##System I had to switch transform = response to type = response in
emmeans to get things to run Fig results did not change (3/1/23). But
its possible that’s because letters were manually input (back from when
transform = response worked).

\##Community

\#Budget

``` r
YieldData6<-YieldData5
YieldData6<-YieldData6 %>% 
  mutate(SeedingMetric=Seeding*1000*2.47,
         NitCostMetric=as.numeric(as.character(NitrogenMetric))*20.35610978,
         SeedCostMetric=SeedingMetric*0.000321429,
         IncomeMetric=SeedWeight_kgha*0.685837984,
         Profit_dollarsha=IncomeMetric-(SeedCostMetric))

YieldData6$SeedingMetric<-as.numeric(as.character(YieldData6$SeedingMetric))

ProfitRed<-nlme(Profit_dollarsha~-a*SeedingMetric^2+b*SeedingMetric,
     data=YieldData6,
     start=c(9.813e-07,1.080e-01),
     fixed= list(a~1,b ~ 1),
     random = list(Site=pdDiag(list(a~1,b ~ 1)),
                   Block=pdDiag(list(a~1,b ~ 1))),
     na.action = na.omit)

#Site
ProfitSite<-nlme(Profit_dollarsha~-a*SeedingMetric^2+b*SeedingMetric,
     data=YieldData6,
     start=c(9.813e-07,9.813e-07,1.080e-01,1.080e-01),
     fixed= list(a~Site,b ~ Site),
     random = list(Site=pdDiag(list(a~Site,b ~ Site)),
                   Block=pdDiag(list(a~Site,b ~ Site))),
     na.action = na.omit)

anova(ProfitRed,ProfitSite)
```

    ##            Model df      AIC      BIC    logLik   Test L.Ratio p-value
    ## ProfitRed      1  7 1864.569 1884.023 -925.2845                       
    ## ProfitSite     2 13 1871.194 1907.322 -922.5969 1 vs 2 5.37521  0.4967

``` r
#Nitrogen
ProfitNitrogen<-nlme(Profit_dollarsha~-a*SeedingMetric^2+b*SeedingMetric,
     data=YieldData6,
     start=c(9.813e-07,9.813e-07,9.813e-07,1.080e-01,1.080e-01,1.080e-01),
     fixed= list(a~Nitrogen,b ~ Nitrogen),
     random = list(Nitrogen=pdDiag(list(a~Nitrogen,b ~ Nitrogen)),
                   Block=pdDiag(list(a~Nitrogen,b ~ Nitrogen))),
     na.action = na.omit)
anova(ProfitRed,ProfitNitrogen)
```

    ##                Model df      AIC      BIC    logLik   Test   L.Ratio p-value
    ## ProfitRed          1  7 1864.569 1884.023 -925.2845                         
    ## ProfitNitrogen     2 19 1888.241 1941.044 -925.1205 1 vs 2 0.3280741       1

``` r
fixef(ProfitRed)
```

    ##            a            b 
    ## 8.033311e-09 8.479967e-03

``` r
fixef(ProfitSite)
```

    ## a.(Intercept)  a.SiteGeneva b.(Intercept)  b.SiteGeneva 
    ##  8.878671e-09 -1.690717e-09  9.265557e-03 -1.571180e-03

``` r
fixef(ProfitNitrogen)
```

    ## a.(Intercept)  a.Nitrogen60 a.Nitrogen120 b.(Intercept)  b.Nitrogen60 
    ##  9.774805e-09 -2.149651e-09 -3.074823e-09  9.580240e-03 -1.315787e-03 
    ## b.Nitrogen120 
    ## -1.985033e-03

``` r
#profit ploy
fun.1Profit <- function(x) -fixef(ProfitRed)[1]*x^2+fixef(ProfitRed)[2]*x  
optimize(fun.1Profit, interval=c(0,741300),maximum = T)
```

    ## $maximum
    ## [1] 527800.3
    ## 
    ## $objective
    ##        a 
    ## 2237.864

``` r
#tiff("Fig5.tiff", units="in", width=5.82, height=5.54, res=300)
ggplot(YieldData6,aes(x=SeedingMetric,y=Profit_dollarsha))+
  geom_point()+
  stat_function(fun=fun.1Profit,size=1.2)+
  geom_segment(aes(x = 527800.3, y = 0, xend = 527800.3, yend = 2237.864),
               size=.3,arrow = arrow(length = unit(0.3, "cm"),type = "closed"))+
  scale_y_continuous(expand = c(0, 0),labels = scales::comma,limits=c(0,4000)) +
  scale_x_continuous(labels = scales::comma,
                     breaks=c(0,185300,370700,556000,741300))+
  scale_color_brewer(palette = "Accent")+
  theme_bw(base_size = 18)+
   labs(y=expression("Partial returns (dollars"~ha^-1*')'),
       x=expression("Seeding rate (seeds"~ha^-1*')'))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(hjust=1),
        legend.position="none")
```

    ## Warning in geom_segment(aes(x = 527800.3, y = 0, xend = 527800.3, yend = 2237.864), : All aesthetics have length 1, but the data has 120 rows.
    ## ℹ Please consider using `annotate()` or provide this layer with data containing
    ##   a single row.

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](SoyRyeAnalysis_files/figure-gfm/money%20modeling-1.png)<!-- -->

``` r
#dev.off()
#################
```
