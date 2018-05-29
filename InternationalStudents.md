106-2 大數據分析方法 作業二
================
B0444241胡洺碩

作業完整說明[連結](https://docs.google.com/document/d/1aLGSsGXhgOVgwzSg9JdaNz2qGPQJSoupDAQownkGf_I/edit?usp=sharing)

學習再也不限定在自己出生的國家，台灣每年有許多學生選擇就讀國外的大專院校，同時也有人多國外的學生來台灣就讀，透過分析[大專校院境外學生人數統計](https://data.gov.tw/dataset/6289)、[大專校院本國學生出國進修交流數](https://data.gov.tw/dataset/24730)、[世界各主要國家之我國留學生人數統計表](https://ws.moe.edu.tw/Download.ashx?u=C099358C81D4876CC7586B178A6BD6D5062C39FB76BDE7EC7685C1A3C0846BCDD2B4F4C2FE907C3E7E96F97D24487065577A728C59D4D9A4ECDFF432EA5A114C8B01E4AFECC637696DE4DAECA03BB417&n=4E402A02CE6F0B6C1B3C7E89FDA1FAD0B5DDFA6F3DA74E2DA06AE927F09433CFBC07A1910C169A1845D8EB78BD7D60D7414F74617F2A6B71DC86D17C9DA3781394EF5794EEA7363C&icon=..csv)可以了解103年以後各大專院校國際交流的情形。請同學分析以下議題，並以視覺化的方式呈現分析結果，呈現103年以後大專院校國際交流的情形。

來台境外生分析
--------------

### 資料匯入與處理

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(jsonlite)
library(readr)
library(curl)
```

    ## 
    ## Attaching package: 'curl'

    ## The following object is masked from 'package:readr':
    ## 
    ##     parse_date

``` r
library(knitr)

data103c <- fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=25f64d5125016dcd6aed42e50c972ed0")
data103s <- fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=a6d1469f39fe41fb81dbfc373aef3331")
data104c <- fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=4d3e9b37b7b0fd3aa18a388cdbc77996")
data104s <- fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=8baeae81cba74f35cf0bb1333d3d99f5")
data105c <- fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=19bedf88cf46999da12513de755c33c6")
data105s <- fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=1a485383cf9995da679c3798ab4fd681")
data106c <- fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=50e3370f9f8794f2054c0c82a2ed8c91")
data106s <- fromJSON("https://quality.data.gov.tw/dq_download_json.php?nid=6289&md5_url=883e2ab4d5357f70bea9ac44a47d05cc")

i <- NULL
j <- NULL
TotalCountry <- NULL
Total <- NULL
```

### 哪些國家來台灣唸書的學生最多呢？

``` r
for (i in 3:11) {
  data103c[,i]<-as.numeric(data103c[,i])
  data104c[,i]<-as.numeric(data104c[,i])
  data105c[,i]<-as.numeric(data105c[,i])
  data106c[,i]<-as.numeric(data106c[,i])
}
data103c$total103 <- rowSums(data103c[,3:11])
data104c$total104 <- rowSums(data104c[,3:11])
data105c$total105 <- rowSums(data105c[,3:11])
data106c$total106 <- rowSums(data106c[,3:11])
TotalCountry <- full_join(data103c,data104c,by="國別")
TotalCountry[,3:11] <- list(NULL)
TotalCountry[,4:13] <- list(NULL)
TotalCountry[,1] <- list(NULL)
TotalCountry <- full_join(TotalCountry,data105c,by="國別")
TotalCountry[,4:13] <- list(NULL)
TotalCountry <- full_join(TotalCountry,data106c,by="國別")
TotalCountry[,5:14] <- list(NULL)
TotalCountry[is.na(TotalCountry)] <- 0
TotalCountry$total <- rowSums(TotalCountry[,2:4])
TotalCountry[,2:5] <- list(NULL)

ans1_1 <- TotalCountry[order(TotalCountry$total,decreasing = T),]
kable(head(ans1_1,10))
```

|     | 國別     |   total|
|-----|:---------|-------:|
| 1   | 中國大陸 |  117220|
| 2   | 馬來西亞 |   44750|
| 3   | 香港     |   23179|
| 4   | 日本     |   19813|
| 5   | 澳門     |   15161|
| 6   | 越南     |   13806|
| 8   | 印尼     |   13167|
| 7   | 南韓     |   12224|
| 121 | 美國     |   11032|
| 9   | 泰國     |    4897|

### 哪間大學的境外生最多呢？

``` r
for(i in 4:12){
  if(i==10){
    data103s[,i] <- gsub("…",0,data103s[,i])
    data104s[,i] <- gsub("…",0,data104s[,i])
  }
  data103s[,i]<-as.numeric(data103s[,i])
  data104s[,i]<-as.numeric(data104s[,i])
  data105s[,i]<-as.numeric(data105s[,i])
  data106s[,i]<-as.numeric(data106s[,i])
}

data104s <- data104s[-151,]
data105s <- data105s[-151,]
data106s <- data106s[-151,]
Total <- data103s
Total[,4:12] <- list(NULL)
Total$total103 <- rowSums(data103s[,4:12])
Total$total104 <- rowSums(data104s[,4:12])
Total$total105 <- rowSums(data105s[,4:12])
Total$total106 <- rowSums(data106s[,4:12])
Total$total <- rowSums(Total[,4:7])
Total[,4:7] <- list(NULL)

ans1_2 <- Total[order(Total$total,decreasing = T),]
kable(head(ans1_2,10))
```

|     | 學校類型 | 學校代碼 | 學校名稱         |  total|
|-----|:---------|:---------|:-----------------|------:|
| 4   | 大專校院 | 0004     | 國立臺灣師範大學 |  22113|
| 3   | 大專校院 | 0003     | 國立臺灣大學     |  18199|
| 55  | 大專校院 | 1006     | 中國文化大學     |  14894|
| 54  | 大專校院 | 1005     | 淡江大學         |  13670|
| 65  | 大專校院 | 1016     | 銘傳大學         |  13212|
| 1   | 大專校院 | 0001     | 國立政治大學     |  11626|
| 6   | 大專校院 | 0005     | 國立成功大學     |  10982|
| 53  | 大專校院 | 1004     | 中原大學         |   8971|
| 56  | 大專校院 | 1007     | 逢甲大學         |   8512|
| 51  | 大專校院 | 1002     | 輔仁大學         |   8243|

### 各個國家來台灣唸書的學生人數條狀圖

``` r
library(ggplot2)
ggplot(data=TotalCountry,
       aes(x=國別,y=total))+geom_bar(
       stat = "identity",fill="cornflowerblue")+
  geom_text(aes(label=TotalCountry$total),color = "red",vjust = -1)+
  theme_light()
```

![](InternationalStudents_files/figure-markdown_github/ToTWNCountryBar-1.png)

### 各個國家來台灣唸書的學生人數面量圖

``` r
library(choroplethr)
```

    ## Loading required package: acs

    ## Loading required package: stringr

    ## Loading required package: XML

    ## 
    ## Attaching package: 'acs'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:base':
    ## 
    ##     apply

``` r
countryname<-read_csv("C:/Users/mins/Desktop/CountriesComparisionTable.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   ISO3 = col_character(),
    ##   English = col_character(),
    ##   Taiwan = col_character()
    ## )

``` r
colnames(countryname)<-c("ISO3","English","國別")
ETotalCountry<-merge(TotalCountry,countryname,by="國別")
colnames(ETotalCountry)<-c("國別","value","ISO3","region")
ETotalCountry[5,2]<-ETotalCountry[5,2]+ETotalCountry[91,2]+ETotalCountry[159,2]
ETotalCountry[107,2]=ETotalCountry[107,2]+ETotalCountry[108,2]
ETotalCountry<-ETotalCountry%>%
  subset(region!="Unmatch")%>%
  subset(國別!="索馬利蘭共和國")
ans3<-country_choropleth(ETotalCountry)
```

    ## Warning in super$initialize(country.map, user.df): Your data.frame contains
    ## the following regions which are not mappable: Singapore

    ## Warning in self$bind(): The following regions were missing and are being
    ## set to NA: afghanistan, angola, montenegro, qatar, western sahara, south
    ## sudan, somaliland, east timor, taiwan, vanuatu, central african republic,
    ## northern cyprus, djibouti, eritrea, antarctica, equatorial guinea, kosovo,
    ## lesotho

``` r
ans3
```

![](InternationalStudents_files/figure-markdown_github/ToTWNCountryMap-1.png)

台灣學生國際交流分析
--------------------

### 資料匯入與處理

``` r
twout <- read_csv("C:/Users/mins/Desktop/Student_RPT_07.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   學年度 = col_integer(),
    ##   學期 = col_integer(),
    ##   設立別 = col_character(),
    ##   學校類別 = col_character(),
    ##   學校代碼 = col_character(),
    ##   學校名稱 = col_character(),
    ##   系所代碼 = col_integer(),
    ##   系所名稱 = col_character(),
    ##   學制 = col_character(),
    ##   `對方學校(機構)國別(地區)` = col_character(),
    ##   `對方學校(機構)中文名稱` = col_character(),
    ##   `對方學校(機構)英文名稱` = col_character(),
    ##   小計 = col_integer(),
    ##   男 = col_integer(),
    ##   女 = col_integer()
    ## )

    ## Warning: 2 parsing failures.
    ## row # A tibble: 2 x 5 col     row col      expected               actual file                        expected   <int> <chr>    <chr>                  <chr>  <chr>                       actual 1 23579 系所代碼 no trailing characters A2     'C:/Users/mins/Desktop/Stu~ file 2 34284 系所代碼 no trailing characters A2     'C:/Users/mins/Desktop/Stu~

``` r
twout[,14:15] <- list(NULL)
twout<-twout[-c(1:14977),]
```

### 台灣大專院校的學生最喜歡去哪些國家進修交流呢？

``` r
outCountry <- twout%>%
  group_by(`對方學校(機構)國別(地區)`)%>%
  summarise(sum=sum(小計))
ans4_1<-outCountry[order(outCountry$sum,decreasing=T),]
kable(head(ans4_1,10))
```

| 對方學校(機構)國別(地區) |   sum|
|:-------------------------|-----:|
| 中國大陸                 |  8375|
| 日本                     |  7142|
| 美國                     |  4427|
| 南韓                     |  2050|
| 大陸地區                 |  1516|
| 德國                     |  1466|
| 法國                     |  1258|
| 英國                     |   742|
| 加拿大                   |   689|
| 西班牙                   |   642|

### 哪間大學的出國交流學生數最多呢？

``` r
outSchool<-twout%>%
  group_by(學校名稱)%>%
  summarise(sum=sum(小計))
ans4_2<-outSchool[order(outSchool$sum,decreasing=T),]
kable(head(ans4_2,10))
```

| 學校名稱     |   sum|
|:-------------|-----:|
| 國立臺灣大學 |  2224|
| 淡江大學     |  2038|
| 國立政治大學 |  1876|
| 逢甲大學     |  1346|
| 元智大學     |  1106|
| 國立臺北大學 |   956|
| 國立交通大學 |   951|
| 東海大學     |   931|
| 東吳大學     |   873|
| 國立成功大學 |   846|

### 台灣大專院校的學生最喜歡去哪些國家進修交流條狀圖

``` r
ggplot(data=outCountry,
       aes(x=`對方學校(機構)國別(地區)`,y=sum))+geom_bar(
       stat = "identity",fill="cornflowerblue")+
  geom_text(aes(label=outCountry$sum),color = "blue",vjust = -1)+
  theme_light()
```

![](InternationalStudents_files/figure-markdown_github/FromTWNCountryBar-1.png)

### 台灣大專院校的學生最喜歡去哪些國家進修交流面量圖

``` r
countryname<-read_csv("C:/Users/mins/Desktop/CountriesComparisionTable.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   ISO3 = col_character(),
    ##   English = col_character(),
    ##   Taiwan = col_character()
    ## )

``` r
colnames(countryname)<-c("ISO3","English","對方學校(機構)國別(地區)")
FTotalCountry<-merge(outCountry,countryname,by="對方學校(機構)國別(地區)")
colnames(FTotalCountry)<-c("國別","value","ISO3","region")
FTotalCountry<-FTotalCountry%>%
  subset(region!="Unmatch")
ans6 <- country_choropleth(FTotalCountry,num_colors = 9)
```

    ## Warning in super$initialize(country.map, user.df): Your data.frame contains
    ## the following regions which are not mappable: Singapore

    ## Warning in self$bind(): The following regions were missing and are being
    ## set to NA: afghanistan, angola, azerbaijan, moldova, madagascar, macedonia,
    ## mali, myanmar, montenegro, mozambique, mauritania, burundi, namibia,
    ## nigeria, nicaragua, pakistan, papua new guinea, benin, paraguay, rwanda,
    ## western sahara, sudan, burkina faso, south sudan, senegal, sierra leone,
    ## el salvador, somaliland, somalia, suriname, syria, chad, togo, tajikistan,
    ## turkmenistan, east timor, bulgaria, trinidad and tobago, taiwan, united
    ## republic of tanzania, uganda, ukraine, uruguay, uzbekistan, the bahamas,
    ## venezuela, vanuatu, yemen, zambia, zimbabwe, bosnia and herzegovina,
    ## belarus, albania, belize, bolivia, bhutan, botswana, central african
    ## republic, united arab emirates, ivory coast, cameroon, democratic republic
    ## of the congo, republic of congo, cuba, northern cyprus, cyprus, argentina,
    ## djibouti, dominican republic, algeria, eritrea, armenia, ethiopia,
    ## fiji, gabon, georgia, ghana, antarctica, guinea, gambia, guinea bissau,
    ## equatorial guinea, guatemala, guyana, honduras, haiti, iran, iraq, jamaica,
    ## kazakhstan, kenya, kyrgyzstan, kosovo, laos, lebanon, liberia, libya,
    ## lesotho, luxembourg

``` r
ans6
```

![](InternationalStudents_files/figure-markdown_github/FromTWNCountryMap-1.png)

台灣學生出國留學分析
--------------------

### 資料匯入與處理

``` r
cometotw <- read_csv("https://ws.moe.edu.tw/Download.ashx?u=C099358C81D4876CC7586B178A6BD6D5062C39FB76BDE7EC7685C1A3C0846BCDD2B4F4C2FE907C3E7E96F97D24487065577A728C59D4D9A4ECDFF432EA5A114C8B01E4AFECC637696DE4DAECA03BB417&n=4E402A02CE6F0B6C1B3C7E89FDA1FAD0B5DDFA6F3DA74E2DA06AE927F09433CFBC07A1910C169A1845D8EB78BD7D60D7414F74617F2A6B71DC86D17C9DA3781394EF5794EEA7363C&icon=..csv")
```

    ## Warning: Missing column names filled in: 'X4' [4], 'X5' [5], 'X6' [6]

    ## Parsed with column specification:
    ## cols(
    ##   洲別 = col_character(),
    ##   國別 = col_character(),
    ##   總人數 = col_number(),
    ##   X4 = col_character(),
    ##   X5 = col_character(),
    ##   X6 = col_character()
    ## )

``` r
cometotw[,4:6] <-list(NULL)
```

### 台灣學生最喜歡去哪些國家留學呢？

``` r
  ans7<-cometotw[order(cometotw$總人數,decreasing=T),]
kable(head(ans7,10))
```

| 洲別   | 國別     | 總人數 |
|:-------|:---------|:------:|
| 美洲   | 美國     |  21127 |
| 大洋洲 | 澳大利亞 |  13582 |
| 亞洲   | 日本     |  8444  |
| 美洲   | 加拿大   |  4827  |
| 歐洲   | 英國     |  3815  |
| 歐洲   | 德國     |  1488  |
| 大洋洲 | 紐西蘭   |  1106  |
| 歐洲   | 波蘭     |   561  |
| 亞洲   | 馬來西亞 |   502  |
| 歐洲   | 奧地利   |   419  |

### 台灣學生最喜歡去哪些國家留學面量圖

``` r
countryname<-read_csv("C:/Users/mins/Desktop/CountriesComparisionTable.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   ISO3 = col_character(),
    ##   English = col_character(),
    ##   Taiwan = col_character()
    ## )

``` r
colnames(countryname)<-c("ISO3","English","國別")
result8<-merge(ans7,countryname,by="國別")
colnames(result8)<-c("國別","洲別","value","ISO3","region")
result8 <- result8%>%
    subset(region!="Unmatch")
ans8 <- country_choropleth(result8,num_colors = 9)
```

    ## Warning in super$initialize(country.map, user.df): Your data.frame contains
    ## the following regions which are not mappable: Singapore

    ## Warning in self$bind(): The following regions were missing and are being
    ## set to NA: afghanistan, angola, azerbaijan, moldova, madagascar, mexico,
    ## macedonia, mali, myanmar, montenegro, mongolia, mozambique, mauritania,
    ## burundi, malawi, namibia, france, niger, nigeria, nicaragua, oman,
    ## pakistan, panama, peru, papua new guinea, north korea, benin, portugal,
    ## paraguay, israel, qatar, romania, rwanda, western sahara, saudi arabia,
    ## sudan, burkina faso, south sudan, senegal, solomon islands, sierra leone,
    ## el salvador, somaliland, somalia, republic of serbia, suriname, slovakia,
    ## slovenia, swaziland, syria, chad, togo, tajikistan, turkmenistan, east
    ## timor, bulgaria, trinidad and tobago, tunisia, turkey, taiwan, united
    ## republic of tanzania, uganda, ukraine, uruguay, uzbekistan, the bahamas,
    ## venezuela, vanuatu, yemen, south africa, zambia, zimbabwe, bosnia and
    ## herzegovina, belarus, albania, belize, bolivia, brazil, bhutan, botswana,
    ## central african republic, switzerland, chile, united arab emirates, china,
    ## ivory coast, cameroon, democratic republic of the congo, republic of
    ## congo, colombia, costa rica, cuba, northern cyprus, cyprus, argentina,
    ## czech republic, djibouti, dominican republic, algeria, ecuador, egypt,
    ## eritrea, spain, armenia, estonia, ethiopia, fiji, gabon, georgia, ghana,
    ## antarctica, guinea, gambia, guinea bissau, equatorial guinea, greece,
    ## guatemala, guyana, honduras, croatia, haiti, hungary, ireland, iran, iraq,
    ## italy, jamaica, jordan, kazakhstan, kenya, kyrgyzstan, cambodia, south
    ## korea, kosovo, kuwait, laos, lebanon, liberia, libya, lesotho, lithuania,
    ## luxembourg, latvia, morocco

``` r
ans8
```

![](InternationalStudents_files/figure-markdown_github/FromTWNAbMap-1.png)

綜合分析
--------

請問來台讀書與離台讀書的來源國與留學國趨勢是否相同(5分)？想來台灣唸書的境外生，他們的母國也有很多台籍生嗎？請圖文並茂說明你的觀察(10分)。
