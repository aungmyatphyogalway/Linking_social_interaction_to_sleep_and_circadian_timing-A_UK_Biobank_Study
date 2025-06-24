#16 June 2025
#Aung
#To prepare core variables of UKB pre-master dataset 
setwd("~/Downloads")
library(dplyr)

#required data file = ukb673864_core.tab, UKB_master_vitd.Rda
bd <- read.delim("/Users/aungphyo/Downloads/social_sleep_data_files/ukb673864_core.tab", header=TRUE)
load("/Users/aungphyo/Downloads/social_sleep_data_files/UKB_master_vitd.Rda")
#leveling and labelling
lvl.0009 <- c(0,1)
lbl.0009 <- c("Female","Male")
bd$f.31.0.0 <- ordered(bd$f.31.0.0, levels=lvl.0009, labels=lbl.0009)
lvl.0008 <- c(1,2,3,4,5,6,7,8,9,10,11,12)
lbl.0008 <- c("January","February","March","April","May","June","July","August","September","October","November","December")
bd$f.52.0.0 <- ordered(bd$f.52.0.0, levels=lvl.0008, labels=lbl.0008)
bd$f.53.0.0 <- as.Date(bd$f.53.0.0)
bd$f.53.1.0 <- as.Date(bd$f.53.1.0)
bd$f.53.2.0 <- as.Date(bd$f.53.2.0)
bd$f.53.3.0 <- as.Date(bd$f.53.3.0)
lvl.100258 <- c(1,2,9)
lbl.100258 <- c("Yes - pounds and ounces","Yes - Kilograms","No")
bd$f.120.0.0 <- ordered(bd$f.120.0.0, levels=lvl.100258, labels=lbl.100258)
bd$f.120.1.0 <- ordered(bd$f.120.1.0, levels=lvl.100258, labels=lbl.100258)
bd$f.120.2.0 <- ordered(bd$f.120.2.0, levels=lvl.100258, labels=lbl.100258)
lvl.0170 <- c(-1)
lbl.0170 <- c("Location could not be mapped")
lvl.100301 <- c(-3,-1,1,2,3,4)
lbl.100301 <- c("Prefer not to answer","Do not know","Never/rarely","Sometimes","Usually","Always")
bd$f.826.0.0 <- ordered(bd$f.826.0.0, levels=lvl.100301, labels=lbl.100301)
bd$f.826.1.0 <- ordered(bd$f.826.1.0, levels=lvl.100301, labels=lbl.100301)
bd$f.826.2.0 <- ordered(bd$f.826.2.0, levels=lvl.100301, labels=lbl.100301)
bd$f.826.3.0 <- ordered(bd$f.826.3.0, levels=lvl.100301, labels=lbl.100301)
lvl.100306 <- c(-3,-2,-1)
lbl.100306 <- c("Prefer not to answer","Never went to school","Do not know")
lvl.100329 <- c(-10,-3,-1)
lbl.100329 <- c("Less than an hour a day","Prefer not to answer","Do not know")
lvl.100291 <- c(-3,-1)
lbl.100291 <- c("Prefer not to answer","Do not know")
lvl.100341 <- c(-3,-1,1,2,3,4)
lbl.100341 <- c("Prefer not to answer","Do not know","Not at all easy","Not very easy","Fairly easy","Very easy")
bd$f.1170.0.0 <- ordered(bd$f.1170.0.0, levels=lvl.100341, labels=lbl.100341)
bd$f.1170.1.0 <- ordered(bd$f.1170.1.0, levels=lvl.100341, labels=lbl.100341)
bd$f.1170.2.0 <- ordered(bd$f.1170.2.0, levels=lvl.100341, labels=lbl.100341)
bd$f.1170.3.0 <- ordered(bd$f.1170.3.0, levels=lvl.100341, labels=lbl.100341)
lvl.100342 <- c(-3,-1,1,2,3,4)
lbl.100342 <- c("Prefer not to answer","Do not know","Definitely a \'morning\' person","More a \'morning\' than \'evening\' person","More an \'evening\' than a \'morning\' person","Definitely an \'evening\' person")
bd$f.1180.0.0 <- ordered(bd$f.1180.0.0, levels=lvl.100342, labels=lbl.100342)
bd$f.1180.1.0 <- ordered(bd$f.1180.1.0, levels=lvl.100342, labels=lbl.100342)
bd$f.1180.2.0 <- ordered(bd$f.1180.2.0, levels=lvl.100342, labels=lbl.100342)
bd$f.1180.3.0 <- ordered(bd$f.1180.3.0, levels=lvl.100342, labels=lbl.100342)
lvl.100343 <- c(-3,1,2,3)
lbl.100343 <- c("Prefer not to answer","Never/rarely","Sometimes","Usually")
bd$f.1190.0.0 <- ordered(bd$f.1190.0.0, levels=lvl.100343, labels=lbl.100343)
bd$f.1190.1.0 <- ordered(bd$f.1190.1.0, levels=lvl.100343, labels=lbl.100343)
bd$f.1190.2.0 <- ordered(bd$f.1190.2.0, levels=lvl.100343, labels=lbl.100343)
bd$f.1190.3.0 <- ordered(bd$f.1190.3.0, levels=lvl.100343, labels=lbl.100343)
bd$f.1200.0.0 <- ordered(bd$f.1200.0.0, levels=lvl.100343, labels=lbl.100343)
bd$f.1200.1.0 <- ordered(bd$f.1200.1.0, levels=lvl.100343, labels=lbl.100343)
bd$f.1200.2.0 <- ordered(bd$f.1200.2.0, levels=lvl.100343, labels=lbl.100343)
bd$f.1200.3.0 <- ordered(bd$f.1200.3.0, levels=lvl.100343, labels=lbl.100343)
lvl.100345 <- c(-3,-1,1,2)
lbl.100345 <- c("Prefer not to answer","Do not know","Yes","No")
bd$f.1210.0.0 <- ordered(bd$f.1210.0.0, levels=lvl.100345, labels=lbl.100345)
bd$f.1210.1.0 <- ordered(bd$f.1210.1.0, levels=lvl.100345, labels=lbl.100345)
bd$f.1210.2.0 <- ordered(bd$f.1210.2.0, levels=lvl.100345, labels=lbl.100345)
bd$f.1210.3.0 <- ordered(bd$f.1210.3.0, levels=lvl.100345, labels=lbl.100345)
lvl.100346 <- c(-3,-1,0,1,2,3)
lbl.100346 <- c("Prefer not to answer","Do not know","Never/rarely","Sometimes","Often","All of the time")
bd$f.1220.0.0 <- ordered(bd$f.1220.0.0, levels=lvl.100346, labels=lbl.100346)
bd$f.1220.1.0 <- ordered(bd$f.1220.1.0, levels=lvl.100346, labels=lbl.100346)
bd$f.1220.2.0 <- ordered(bd$f.1220.2.0, levels=lvl.100346, labels=lbl.100346)
bd$f.1220.3.0 <- ordered(bd$f.1220.3.0, levels=lvl.100346, labels=lbl.100346)
lvl.100402 <- c(-3,1,2,3,4,5,6)
lbl.100402 <- c("Prefer not to answer","Daily or almost daily","Three or four times a week","Once or twice a week","One to three times a month","Special occasions only","Never")
bd$f.1558.0.0 <- ordered(bd$f.1558.0.0, levels=lvl.100402, labels=lbl.100402)
bd$f.1558.1.0 <- ordered(bd$f.1558.1.0, levels=lvl.100402, labels=lbl.100402)
bd$f.1558.2.0 <- ordered(bd$f.1558.2.0, levels=lvl.100402, labels=lbl.100402)
bd$f.1558.3.0 <- ordered(bd$f.1558.3.0, levels=lvl.100402, labels=lbl.100402)
lvl.100420 <- c(-3,-1,1,2,3,4,5,6)
lbl.100420 <- c("Prefer not to answer","Do not know","England","Wales","Scotland","Northern Ireland","Republic of Ireland","Elsewhere")
bd$f.1647.0.0 <- ordered(bd$f.1647.0.0, levels=lvl.100420, labels=lbl.100420)
bd$f.1647.1.0 <- ordered(bd$f.1647.1.0, levels=lvl.100420, labels=lbl.100420)
bd$f.1647.2.0 <- ordered(bd$f.1647.2.0, levels=lvl.100420, labels=lbl.100420)
lvl.100349 <- c(-3,-1,0,1)
lbl.100349 <- c("Prefer not to answer","Do not know","No","Yes")
bd$f.1677.0.0 <- ordered(bd$f.1677.0.0, levels=lvl.100349, labels=lbl.100349)
bd$f.1677.1.0 <- ordered(bd$f.1677.1.0, levels=lvl.100349, labels=lbl.100349)
bd$f.1677.2.0 <- ordered(bd$f.1677.2.0, levels=lvl.100349, labels=lbl.100349)
lvl.100430 <- c(-3,1,2,3)
lbl.100430 <- c("Prefer not to answer","Right-handed","Left-handed","Use both right and left hands equally")
bd$f.1707.0.0 <- ordered(bd$f.1707.0.0, levels=lvl.100430, labels=lbl.100430)
bd$f.1707.1.0 <- ordered(bd$f.1707.1.0, levels=lvl.100430, labels=lbl.100430)
bd$f.1707.2.0 <- ordered(bd$f.1707.2.0, levels=lvl.100430, labels=lbl.100430)
bd$f.1767.0.0 <- ordered(bd$f.1767.0.0, levels=lvl.100349, labels=lbl.100349)
bd$f.1767.1.0 <- ordered(bd$f.1767.1.0, levels=lvl.100349, labels=lbl.100349)
bd$f.1767.2.0 <- ordered(bd$f.1767.2.0, levels=lvl.100349, labels=lbl.100349)
bd$f.1767.3.0 <- ordered(bd$f.1767.3.0, levels=lvl.100349, labels=lbl.100349)
bd$f.1787.0.0 <- ordered(bd$f.1787.0.0, levels=lvl.100349, labels=lbl.100349)
bd$f.1787.1.0 <- ordered(bd$f.1787.1.0, levels=lvl.100349, labels=lbl.100349)
bd$f.1787.2.0 <- ordered(bd$f.1787.2.0, levels=lvl.100349, labels=lbl.100349)
bd$f.2090.0.0 <- ordered(bd$f.2090.0.0, levels=lvl.100349, labels=lbl.100349)
bd$f.2090.1.0 <- ordered(bd$f.2090.1.0, levels=lvl.100349, labels=lbl.100349)
bd$f.2090.2.0 <- ordered(bd$f.2090.2.0, levels=lvl.100349, labels=lbl.100349)
bd$f.2090.3.0 <- ordered(bd$f.2090.3.0, levels=lvl.100349, labels=lbl.100349)
bd$f.2100.0.0 <- ordered(bd$f.2100.0.0, levels=lvl.100349, labels=lbl.100349)
bd$f.2100.1.0 <- ordered(bd$f.2100.1.0, levels=lvl.100349, labels=lbl.100349)
bd$f.2100.2.0 <- ordered(bd$f.2100.2.0, levels=lvl.100349, labels=lbl.100349)
bd$f.2100.3.0 <- ordered(bd$f.2100.3.0, levels=lvl.100349, labels=lbl.100349)
lvl.100508 <- c(-3,-1,1,2,3,4)
lbl.100508 <- c("Prefer not to answer","Do not know","Excellent","Good","Fair","Poor")
bd$f.2178.0.0 <- ordered(bd$f.2178.0.0, levels=lvl.100508, labels=lbl.100508)
bd$f.2178.1.0 <- ordered(bd$f.2178.1.0, levels=lvl.100508, labels=lbl.100508)
bd$f.2178.2.0 <- ordered(bd$f.2178.2.0, levels=lvl.100508, labels=lbl.100508)
bd$f.2178.3.0 <- ordered(bd$f.2178.3.0, levels=lvl.100508, labels=lbl.100508)
bd$f.2188.0.0 <- ordered(bd$f.2188.0.0, levels=lvl.100349, labels=lbl.100349)
bd$f.2188.1.0 <- ordered(bd$f.2188.1.0, levels=lvl.100349, labels=lbl.100349)
bd$f.2188.2.0 <- ordered(bd$f.2188.2.0, levels=lvl.100349, labels=lbl.100349)
bd$f.2188.3.0 <- ordered(bd$f.2188.3.0, levels=lvl.100349, labels=lbl.100349)
bd$f.3426.0.0 <- ordered(bd$f.3426.0.0, levels=lvl.100301, labels=lbl.100301)
bd$f.3426.1.0 <- ordered(bd$f.3426.1.0, levels=lvl.100301, labels=lbl.100301)
bd$f.3426.2.0 <- ordered(bd$f.3426.2.0, levels=lvl.100301, labels=lbl.100301)
bd$f.3426.3.0 <- ordered(bd$f.3426.3.0, levels=lvl.100301, labels=lbl.100301)
lvl.100305 <- c(-7,-3,1,2,3,4,5,6)
lbl.100305 <- c("None of the above","Prefer not to answer","College or University degree","A levels/AS levels or equivalent","O levels/GCSEs or equivalent","CSEs or equivalent","NVQ or HND or HNC or equivalent","Other professional qualifications eg: nursing, teaching")
bd$f.6138.0.0 <- ordered(bd$f.6138.0.0, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.0.1 <- ordered(bd$f.6138.0.1, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.0.2 <- ordered(bd$f.6138.0.2, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.0.3 <- ordered(bd$f.6138.0.3, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.0.4 <- ordered(bd$f.6138.0.4, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.0.5 <- ordered(bd$f.6138.0.5, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.1.0 <- ordered(bd$f.6138.1.0, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.1.1 <- ordered(bd$f.6138.1.1, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.1.2 <- ordered(bd$f.6138.1.2, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.1.3 <- ordered(bd$f.6138.1.3, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.1.4 <- ordered(bd$f.6138.1.4, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.1.5 <- ordered(bd$f.6138.1.5, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.2.0 <- ordered(bd$f.6138.2.0, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.2.1 <- ordered(bd$f.6138.2.1, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.2.2 <- ordered(bd$f.6138.2.2, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.2.3 <- ordered(bd$f.6138.2.3, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.2.4 <- ordered(bd$f.6138.2.4, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.2.5 <- ordered(bd$f.6138.2.5, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.3.0 <- ordered(bd$f.6138.3.0, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.3.1 <- ordered(bd$f.6138.3.1, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.3.2 <- ordered(bd$f.6138.3.2, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.3.3 <- ordered(bd$f.6138.3.3, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.3.4 <- ordered(bd$f.6138.3.4, levels=lvl.100305, labels=lbl.100305)
bd$f.6138.3.5 <- ordered(bd$f.6138.3.5, levels=lvl.100305, labels=lbl.100305)
lvl.100295 <- c(-7,-3,1,2,3,4,5,6,7)
lbl.100295 <- c("None of the above","Prefer not to answer","In paid employment or self-employed","Retired","Looking after home and/or family","Unable to work because of sickness or disability","Unemployed","Doing unpaid or voluntary work","Full or part-time student")
bd$f.6142.0.0 <- ordered(bd$f.6142.0.0, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.0.1 <- ordered(bd$f.6142.0.1, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.0.2 <- ordered(bd$f.6142.0.2, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.0.3 <- ordered(bd$f.6142.0.3, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.0.4 <- ordered(bd$f.6142.0.4, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.0.5 <- ordered(bd$f.6142.0.5, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.0.6 <- ordered(bd$f.6142.0.6, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.1.0 <- ordered(bd$f.6142.1.0, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.1.1 <- ordered(bd$f.6142.1.1, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.1.2 <- ordered(bd$f.6142.1.2, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.1.3 <- ordered(bd$f.6142.1.3, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.1.4 <- ordered(bd$f.6142.1.4, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.1.5 <- ordered(bd$f.6142.1.5, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.1.6 <- ordered(bd$f.6142.1.6, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.2.0 <- ordered(bd$f.6142.2.0, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.2.1 <- ordered(bd$f.6142.2.1, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.2.2 <- ordered(bd$f.6142.2.2, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.2.3 <- ordered(bd$f.6142.2.3, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.2.4 <- ordered(bd$f.6142.2.4, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.2.5 <- ordered(bd$f.6142.2.5, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.2.6 <- ordered(bd$f.6142.2.6, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.3.0 <- ordered(bd$f.6142.3.0, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.3.1 <- ordered(bd$f.6142.3.1, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.3.2 <- ordered(bd$f.6142.3.2, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.3.3 <- ordered(bd$f.6142.3.3, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.3.4 <- ordered(bd$f.6142.3.4, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.3.5 <- ordered(bd$f.6142.3.5, levels=lvl.100295, labels=lbl.100295)
bd$f.6142.3.6 <- ordered(bd$f.6142.3.6, levels=lvl.100295, labels=lbl.100295)
lvl.100510 <- c(-7,-3,-1,1,2,3)
lbl.100510 <- c("None of the above","Prefer not to answer","Do not know","Attendance allowance","Disability living allowance","Blue badge")
bd$f.6146.0.0 <- ordered(bd$f.6146.0.0, levels=lvl.100510, labels=lbl.100510)
bd$f.6146.0.1 <- ordered(bd$f.6146.0.1, levels=lvl.100510, labels=lbl.100510)
bd$f.6146.0.2 <- ordered(bd$f.6146.0.2, levels=lvl.100510, labels=lbl.100510)
bd$f.6146.1.0 <- ordered(bd$f.6146.1.0, levels=lvl.100510, labels=lbl.100510)
bd$f.6146.1.1 <- ordered(bd$f.6146.1.1, levels=lvl.100510, labels=lbl.100510)
bd$f.6146.1.2 <- ordered(bd$f.6146.1.2, levels=lvl.100510, labels=lbl.100510)
bd$f.6146.2.0 <- ordered(bd$f.6146.2.0, levels=lvl.100510, labels=lbl.100510)
bd$f.6146.2.1 <- ordered(bd$f.6146.2.1, levels=lvl.100510, labels=lbl.100510)
bd$f.6146.2.2 <- ordered(bd$f.6146.2.2, levels=lvl.100510, labels=lbl.100510)
bd$f.6146.3.0 <- ordered(bd$f.6146.3.0, levels=lvl.100510, labels=lbl.100510)
bd$f.6146.3.1 <- ordered(bd$f.6146.3.1, levels=lvl.100510, labels=lbl.100510)
bd$f.6146.3.2 <- ordered(bd$f.6146.3.2, levels=lvl.100510, labels=lbl.100510)
lvl.0090 <- c(-3,0,1,2)
lbl.0090 <- c("Prefer not to answer","Never","Previous","Current")
bd$f.20116.0.0 <- ordered(bd$f.20116.0.0, levels=lvl.0090, labels=lbl.0090)
bd$f.20116.1.0 <- ordered(bd$f.20116.1.0, levels=lvl.0090, labels=lbl.0090)
bd$f.20116.2.0 <- ordered(bd$f.20116.2.0, levels=lvl.0090, labels=lbl.0090)
bd$f.20116.3.0 <- ordered(bd$f.20116.3.0, levels=lvl.0090, labels=lbl.0090)
lvl.0091 <- c(1,2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18)
lbl.0091 <- c("England/Wales - Urban - sparse","England/Wales - Town and Fringe - sparse","England/Wales - Village - sparse","England/Wales - Hamlet and Isolated dwelling - sparse","England/Wales - Urban - less sparse","England/Wales - Town and Fringe - less sparse","England/Wales - Village - less sparse","England/Wales - Hamlet and Isolated Dwelling - less sparse","Postcode not linkable","Scotland - Large Urban Area","Scotland - Other Urban Area","Scotland - Accessible Small Town","Scotland - Remote Small Town","Scotland - Very Remote Small Town","Scotland - Accessible Rural","Scotland - Remote Rural","Scotland - Very Remote Rural")
bd$f.20118.0.0 <- ordered(bd$f.20118.0.0, levels=lvl.0091, labels=lbl.0091)
bd$f.20119.0.0 <- ordered(bd$f.20119.0.0, levels=lvl.100295, labels=lbl.100295)
lvl.1001 <- c(-3,-1,1,2,3,4,5,6,1001,1002,1003,2001,2002,2003,2004,3001,3002,3003,3004,4001,4002,4003)
lbl.1001 <- c("Prefer not to answer","Do not know","White","Mixed","Asian or Asian British","Black or Black British","Chinese","Other ethnic group","British","Irish","Any other white background","White and Black Caribbean","White and Black African","White and Asian","Any other mixed background","Indian","Pakistani","Bangladeshi","Any other Asian background","Caribbean","African","Any other Black background")
bd$f.21000.0.0 <- ordered(bd$f.21000.0.0, levels=lvl.1001, labels=lbl.1001)
bd$f.21000.1.0 <- ordered(bd$f.21000.1.0, levels=lvl.1001, labels=lbl.1001)
bd$f.21000.2.0 <- ordered(bd$f.21000.2.0, levels=lvl.1001, labels=lbl.1001)
bd$f.21000.3.0 <- ordered(bd$f.21000.3.0, levels=lvl.1001, labels=lbl.1001)
lvl.100010 <- c(0,1)
lbl.100010 <- c("No","Yes")
bd$f.100580.0.0 <- ordered(bd$f.100580.0.0, levels=lvl.100010, labels=lbl.100010)
bd$f.100580.1.0 <- ordered(bd$f.100580.1.0, levels=lvl.100010, labels=lbl.100010)
bd$f.100580.2.0 <- ordered(bd$f.100580.2.0, levels=lvl.100010, labels=lbl.100010)
bd$f.100580.3.0 <- ordered(bd$f.100580.3.0, levels=lvl.100010, labels=lbl.100010)
bd$f.100580.4.0 <- ordered(bd$f.100580.4.0, levels=lvl.100010, labels=lbl.100010)

#core variables
core_vars <- bd
rm(bd)

#make new data frame to store variables
UKB_master <- data.frame(core_vars$f.eid) 
comment(UKB_master )<-c("Core variables for 'Linking social interaction to sleep and circadian timing' project")
colnames(UKB_master)[1]<-"eid"

## Primary demographics ####################

#sex (31)
sex <- core_vars$f.31.0.0
comment(sex)<-c("Datafield=31.0.0")

#age (21003)
age <- core_vars$f.21003.0.0
comment(age)<-c("Datafield = 21003.0.0")

#year of birth (34)
year_born <- core_vars$f.34.0.0
comment(year_born)<-c("Datafield = 34.0.0")

#month of birth (52)
month_born <- core_vars$f.52.0.0
comment(month_born)<-c("Datafield = 52.0.0")

#ethnicity (21000)
ethnicity <- core_vars$f.21000.0.0
comment(ethnicity)<-c("Datafield = 21000.0.0")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, sex, age, year_born, month_born, ethnicity)

#remove environment variables no longer needed outside of dataframe
rm(sex, age, year_born, month_born, ethnicity)

## Assessment Centre ####################

#assess_date (53)
assess_date <- core_vars$f.53.0.0
comment(assess_date)<-c("Datafield = 53.0.0")

#assess_centre (54)
assess_centre <- core_vars$f.54.0.0
comment(assess_centre)<-c("Datafield = 54.0.0")



#	Home area population density - urban or rural (20118)
urban <- core_vars$f.20118.0.0
comment(urban)<-c("Datafield = 20118.0.0")

#	Index of multiple deprivation England (26410)
deprivation_index_england <- core_vars$f.26410.0.0
comment(deprivation_index_england )<-c("Datafield = 26410.0.0")

#	Index of multiple deprivation Scotland (26427)
deprivation_index_scotland <- core_vars$f.26427.0.0
comment(deprivation_index_scotland )<-c("Datafield = 26427.0.0")

#	Index of multiple deprivation Wales (26426)
deprivation_index_wales <- core_vars$f.26426.0.0
comment(deprivation_index_wales )<-c("Datafield = 26426.0.0")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, urban, deprivation_index_england, 
                    deprivation_index_scotland, deprivation_index_wales)

UKB_master <- cbind(UKB_master,assess_centre,assess_date)
#remove environment variables no longer needed outside of dataframe
rm(deprivation_index_england, 
   deprivation_index_scotland, deprivation_index_wales, urban, assess_centre, assess_date)

## Education & Employment #############################################

#	Age completed full-time education (845)
age_completed_education <- core_vars$f.845.0.0
comment(age_completed_education)<-c("Datafield = 845")

#	Qualifications (6138)
qualifications <- core_vars$f.6138.0.0
comment(qualifications)<-c("Datafield = 6138")

#	Current employment status (6142)
employed <- core_vars$f.6142.0.0
comment(employed)<-c("Datafield = 6142")

#	Current employment status (corrected) (20119)
employed_corr <- core_vars$f.20119.0.0
comment(employed_corr)<-c("Datafield = 20119")

#	Job involves night shift work (3426)
night_shift <- core_vars$f.3426.0.0
comment(night_shift)<-c("Datafield = 3426")

#	Job involves shift work (826)
shift_work <- core_vars$f.826.0.0
comment(shift_work)<-c("Datafield = 826")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, age_completed_education, qualifications, employed, 
                    employed_corr, night_shift, shift_work)

#remove environment variables no longer needed outside of dataframe
rm(age_completed_education, qualifications, employed, employed_corr, night_shift,
   shift_work)

## Early life  #############################################

#Adopted as a child (1767)
adopted <- core_vars$f.1767.0.0
comment(adopted)<-c("Datafield = 1767")

#Birth weight (20022)
birth_weight  <- core_vars$f.20022.0.0
comment(birth_weight)<-c("Datafield = 20022")

#Birth weight metric (120)
birth_weight_metric  <- core_vars$f.120.0.0
comment(birth_weight_metric)<-c("Datafield = 120")

#Breastfed as a baby (1677)
breastfed <- core_vars$f.1677.0.0
comment(breastfed)<-c("Datafield = 1677")

#Country of Birth (non-UK origin) (20115)
country_birth_nonuk <- core_vars$f.20115.0.0
comment(country_birth_nonuk)<-c("Datafield = 20115")

#Country of birth (UK/elsewhere) (1647)
country_birth_uk <- core_vars$f.1647.0.0
comment(country_birth_uk)<-c("Datafield = 1647")

#Handedness (chirality/laterality) (1707)
handedness <- core_vars$f.1707.0.0
comment(handedness)<-c("Datafield = 1707")

#maternal smoking around birth (1787)
maternal_smoking <- core_vars$f.1787.0.0
comment(maternal_smoking)<-c("Datafield = 1787")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, adopted, birth_weight, birth_weight_metric,
                    breastfed, country_birth_uk, country_birth_nonuk, handedness,
                    maternal_smoking)

#remove environment variables no longer needed outside of dataframe
rm(adopted, birth_weight, birth_weight_metric, breastfed, country_birth_uk,
   country_birth_nonuk, maternal_smoking, handedness)

## Health  #############################################

#smoking status (20116)
smoking_status <- core_vars$f.20116.0.0
comment(smoking_status)<-c("Datafield = 20116")

#alcohol intake frequency (1558)
alcohol_intake <- core_vars$f.1558.0.0
comment(alcohol_intake)<-c("Datafield = 1558")

#overall health rating (2178)
health_self_report <- core_vars$f.2178.0.0
comment(health_self_report)<-c("Datafield = 2178")

#number of medications (137)
medication_number <- core_vars$f.137.0.0
comment(medication_number)<-c("Datafield = 137")

#medication code (20003)
medication_code <- core_vars$f.20003.0.0
comment(medication_code)<-c("Datafield = 20003")

#attendance/disability/mobility allowance (6146)
disability_allowance <- core_vars$f.6146.0.0
comment(disability_allowance)<-c("Datafield = 6146")

#longstanding illness or disability (2188)
disability_self_report <- core_vars$f.2188.0.0
comment(disability_self_report)<-c("Datafield = 2188")

#seen psychiatrist for nerves/anxiety/depression (2100)
depress_psych <- core_vars$f.2100.0.0
comment(depress_psych)<-c("Datafield = 2100")

#seen GP for nerves/anxiety/depression (2090)
depress_gp <- core_vars$f.2090.0.0
comment(depress_gp)<-c("Datafield = 2090")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, smoking_status, alcohol_intake, health_self_report,
                    medication_number, medication_code, disability_allowance, disability_self_report,
                    depress_psych, depress_gp)

#remove environment variables no longer needed outside of dataframe
rm(smoking_status, alcohol_intake, health_self_report, medication_number, 
   medication_code, disability_allowance, disability_self_report, depress_gp, depress_psych)

## Seasonal#############################################

#time spent outdoors in summer (1050)
time_outdoors_summer <- core_vars$f.1050.0.0
comment(time_outdoors_summer)<-c("Datafield = 1050")

#time spent outdoors in winter (1060)
time_outdoors_winter <- core_vars$f.1060.0.0
comment(time_outdoors_winter)<-c("Datafield = 1060")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, time_outdoors_summer, time_outdoors_winter)

#remove environment variables no longer needed outside of dataframe
rm(time_outdoors_summer, time_outdoors_winter)

## Sleep #############################################

#Sleep duration (1160)
sleep_duration<- core_vars$f.1160.0.0
comment(sleep_duration)<-c("Datafield = 1160")

#Getting up in morning	(1170)
getting_up  <- core_vars$f.1170.0.0
comment(getting_up)<-c("Datafield = 1170")

#Morning/evening person (chronotype) (1180)
chronotype <- core_vars$f.1180.0.0
comment(chronotype)<-c("Datafield = 1180")

#Nap during day (1190)
day_naps <- core_vars$f.1190.0.0
comment(day_naps)<-c("Datafield = 1190")

#Sleeplessness / insomnia (1200)
insomnia <- core_vars$f.1200.0.0
comment(insomnia)<-c("Datafield = 1200")

#Snoring (1210)
snoring <- core_vars$f.1210.0.0
comment(snoring)<-c("Datafield = 1210")

#Daytime dozing / sleeping (1220)
day_sleepiness <- core_vars$f.1220.0.0
comment(day_sleepiness)<-c("Datafield = 1220")

#Alcohol consumed yesterday (100580)
alcohol_yesterday <- core_vars$f.100580.0.0
comment(alcohol_yesterday)<-c("Datafield = 100580")

#add variables to master dataframe
UKB_master <- cbind(UKB_master, sleep_duration, getting_up, chronotype, day_naps, insomnia,
                    snoring, day_sleepiness, alcohol_yesterday)

#remove environment variables no longer needed outside of dataframe
rm(sleep_duration, getting_up, chronotype, day_naps, day_sleepiness, insomnia,
   snoring, alcohol_yesterday)

## Physical Measures #############

#acceleration average (90012)
accel_mean <- core_vars$f.90012.0.0
comment(accel_mean)<-c("Datafield = 90012.0.0")

#systolic automated reading (4080)
systolic <- core_vars$f.4080.0.0
comment(systolic)<-c("Datafield = 4080.0.0")

#diastolic automated reading (4079)
diastolic <- core_vars$f.4079.0.0
comment(diastolic)<-c("Datafield = 4079.0.0")

#pulse (102)
pulse <- core_vars$f.102.0.0
comment(pulse)<-c("Datafield = 102.0.0")

#body fat percentage from IP (23099)
body_fat <- core_vars$f.23099.0.0
comment(body_fat)<-c("Datafield = 23099.0.0")

#BMI (21001)
BMI <- core_vars$f.21001.0.0
comment(BMI)<-c("Datafield = 21001.0.0")

#hand grip left (46)
handgrip_l <- core_vars$f.46.0.0
comment(handgrip_l)<-c("Datafield = 46")

#hand grip right (47)
handgrip_r <- core_vars$f.47.0.0
comment(handgrip_r)<-c("Datafield = 47")

#FEV (3063)
FEV <- core_vars$f.3063.0.0
comment(FEV)<-c("Datafield = 3063")

#FVV (3062)
FVC <- core_vars$f.3062.0.0
comment(FVC)<-c("Datafield = 3062")

#PEF (3064)
PEF <- core_vars$f.3064.0.0
comment(PEF)<-c("Datafield = 3064")

#add variables to master dataframe
UKB_master <- cbind(UKB_master,accel_mean, systolic, diastolic, pulse, body_fat,
                    BMI, handgrip_l, handgrip_r, FEV, FVC, PEF)

#remove environment variables no longer needed outside of dataframe
rm(accel_mean, systolic, diastolic, pulse, body_fat, BMI, handgrip_l, handgrip_r,
   FEV, FVC, PEF)

#change into five ethnicity
UKB_master<- UKB_master %>%
  mutate(ethnicity_5 = case_when(
    ethnicity == "Prefer not to answer" ~ NA,
    ethnicity == "Do not know" ~ NA,
    ethnicity == "White" ~ "White",
    ethnicity == "Mixed" ~ "Mixed",
    ethnicity == "Asian or Asian British" ~ "Asian",
    ethnicity == "Black or Black British" ~ "Black",
    ethnicity == "Chinese" ~ "Chinese",
    ethnicity == "Other ethnic group" ~ "Other",
    ethnicity == "British" ~ "White",
    ethnicity == "Irish" ~ "White",
    ethnicity == "Any other white background" ~ "White",
    ethnicity == "White and Black Caribbean" ~ "Mixed",
    ethnicity == "White and Black African" ~ "Mixed",
    ethnicity == "White and Asian" ~ "Mixed",
    ethnicity == "Any other mixed background" ~ "Mixed",
    ethnicity == "Indian" ~ "Asian",
    ethnicity == "Pakistani" ~ "Asian",
    ethnicity == "Bangladeshi" ~ "Asian",
    ethnicity == "Any other Asian background" ~ "Asian",
    ethnicity == "Caribbean" ~ "Black",
    ethnicity == "African" ~ "Black",
    ethnicity == "Any other Black background" ~ "Black",
    TRUE ~ NA
  ))

#BMI_cat
UKB_master$BMI_cat <- cut(UKB_master$BMI, breaks = c(0, 18.5, 24.9, 29.9, Inf), 
                   labels = c( "Underweight", "Normal weight","Overweight", "Obese"))

#townsend column
townsend<- swh %>% select(1,76)
UKB_master<- inner_join(UKB_master, townsend, by= "eid")
rm(townsend)
rm(swh)
rm(core_vars)
### Save core variables 'UKB_master' as R datafile ######
save(UKB_master,file="UKB_premaster_core.Rda")

###create csv file
write.csv(UKB_master, file = "UKB_premaster_core.csv", row.names = FALSE)

#remove lvl lbl  for clean environment
rm(list=ls(pattern="lvl"))
rm(list=ls(pattern="lbl"))
