setwd("C:\\Users\\김문정\\Desktop\\2019\\데이터사이언스입문\\skt")
getwd
chk_1<-read.csv("./CALL_CHICKEN_01MONTH.csv",header=TRUE,sep = ",")
head(chk_1)
chk_2<-read.csv("./CALL_CHICKEN_02MONTH.csv",header=TRUE,sep = ",")
chk_3<-read.csv("./CALL_CHICKEN_03MONTH.csv",header=TRUE,sep = ",")
chk_4<-read.csv("./CALL_CHICKEN_04MONTH.csv",header=TRUE,sep = ",")
chk_6<-read.csv("./CALL_CHICKEN_06MONTH.csv",header=TRUE,sep = ",")
chk_5<-read.csv("./CALL_CHICKEN_05MONTH.csv",header=TRUE,sep = ",")
chk_7<-read.csv("./CALL_CHICKEN_07MONTH.csv",header=TRUE,sep = ",")
chk_8<-read.csv("./CALL_CHICKEN_08MONTH.csv",header=TRUE,sep = ",")
chk_9<-read.csv("./CALL_CHICKEN_09MONTH.csv",header=TRUE,sep = ",")
chk_10<-read.csv("./CALL_CHICKEN_10MONTH.csv",header=TRUE,sep = ",")
chk_11<-read.csv("./CALL_CHICKEN_11MONTH.csv",header=TRUE,sep = ",")
chk_12<-read.csv("./CALL_CHICKEN_12MONTH.csv",header=TRUE,sep = ",")

chk<-rbind(chk_1,chk_2,chk_3,chk_4,chk_5,chk_6,chk_7,chk_8,chk_9,chk_10,chk_11,chk_12)
head(chk)

write.csv(chk,file="chk.csv",row.names = FALSE)


for(i in 1:12) { 
  cfood = rbind("r_",i,sep = "")
  assign (cfood, read.table(files[i] , sep="," , header=TRUE)) 
}



species <- tbl(mammals, "species")

left_join(surveys, species) %>%
  filter(taxa == "Rodent") %>%
  group_by(taxa, year) %>%
  tally %>%
  collect()

##날씨 불러오기

setwd("C:\\Users\\김문정\\Desktop\\2019\\데이터사이언스입문\\weather")
getwd()

file_ad = c("C:\\Users\\김문정\\Desktop\\2019\\데이터사이언스입문\\weather")
files = list.files(file_ad)

for(i in 1:25) { 
  rain = paste("rain_",i,sep = "")
  assign (rain, read.table(files[i] , sep="," , header=TRUE)) 
}

rain = rbind(rain_1,rain_2,rain_3,rain_4,rain_5,rain_6,rain_7,rain_8,rain_9,rain_10,rain_11,rain_12,
             rain_13,rain_14,rain_15,rain_16,rain_17,rain_18,rain_19,rain_20,rain_21,rain_22,rain_23,
             rain_24, rain_25)

head(rain)


rain_gu<-read.csv("rain.csv",header = TRUE, sep=",")
head(rain_gu)

rain_gu$region<-paste(rain_gu[,1],rain_gu[,2],sep = "")
head(rain_gu)

pizza<-read.csv("pizza.csv",header =TRUE,sep=",")

rain_gu$date<-as.character(rain_gu$date)
pizza$date<-as.character(pizza$date)

data_all<-left_join(pizza,rain_gu)

View(data_all)

#-----------------------------------------------------------------------------------

# rain과 음식 데이터 합치기
setwd("C:\\Users\\김문정\\Desktop\\2019\\데이터사이언스입문\\weather")
getwd()

library(tidyverse)


# RAIN 데이터 만드는 과정
rain_gu$region<-paste(rain_gu[,1],rain_gu[,2],sep = "")
tail(rain_gu)
rain = rain_gu[,c(1,3,4,5,6,7)]

#1. cfood

# 이미 만들어 놓은 RAIN, CFOOD 불러오기 
rain <- read.csv("rain.csv",header = TRUE, sep = ",")
cfood <- read.csv("cfood.csv",header=TRUE, sep=",")


# 기본키 일치시키기
rain$date = rain$date %>% str_remove_all(pattern = '-')
cfood$date = as.character(cfood$date)
rain$date = as.character(rain$date)

cfood$region = as.character(cfood$region)
rain$region = as.character(rain$region)

# RAIN과 CFOOD 데이터를 date와 region에 따라 병합
cfood_final= left_join(cfood,rain,by=c("date","region"))
View(cfood_final)

write.csv(x = cfood_final, file = 'C:\\Users\\김문정\\Desktop\\2019\\데이터사이언스입문\\weather\\cfood_final.csv')

#2. pizza

pizza <- read.csv("pizza.csv",header=TRUE, sep=",")


# 기본키 일치시키기
rain$date = rain$date %>% str_remove_all(pattern = '-')
pizza$date = as.character(pizza$date)
rain$date = as.character(rain$date)

pizza$region = as.character(pizza$region)
rain$region = as.character(rain$region)

# RAIN과 pizza 데이터를 date와 region에 따라 병합
pizza_final= left_join(pizza,rain,by=c("date","region"))
View(pizza_final)

write.csv(x = pizza_final, file = 'C:\\Users\\김문정\\Desktop\\2019\\데이터사이언스입문\\weather\\pizza_final.csv')

#3. chicken

chicken<-read.csv("chicken.csv",header=TRUE, sep=",")

# 기본키 일치시키기
rain$date = rain$date %>% str_remove_all(pattern = '-')
chicken$date = as.character(chicken$date)
rain$date = as.character(rain$date)
chicken$region = as.character(chicken$region)

# RAIN과 chicken 데이터를 date와 region에 따라 병합
chicken_final= left_join(chicken,rain,by=c("date","region"))
View(chicken_final)

write.csv(x = chicken_final, file = 'C:\\Users\\김문정\\Desktop\\2019\\데이터사이언스입문\\weather\\chicken_final.csv')

#최종 합친 데이터
data_final<-rbind.data.frame(cfood_final,pizza_final,chicken_final)
View(data_final)

write.csv(x=data_final,file ='C:\\Users\\김문정\\Desktop\\2019\\데이터사이언스입문\\weather\\data_final.csv' )

