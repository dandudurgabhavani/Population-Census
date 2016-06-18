# FINDING1:GGPLOT
countrypop <- table(population_final$Year)
countrypop
countrypopulation <- c(279623,247999,288011)
df <- data.frame(countrypopulation=1:3, population_count= countrypopulation)
library(ggplot2)

ggplot(df, aes(x=countrypopulation, y=population_count)) + 
geom_point() + 
geom_smooth(method="lm", fullrange=T) + xlim(1,10)



#FINDING2:

statepop <- table(population_final$State_id,population_final$Year)
statepop
counts <- table(population_final$Year, population_final$State_id)
barplot(counts, main="Population over Decades",xlab="States",ylab = "Population", col=c("darkblue","red","dimgray"),legend = rownames(counts), beside=TRUE)

FINDING:LITERACY RATE


stateAP <- subset(population_final, (population_final$Year == "2011") & (population_final$Age >= "7") & (population_final$State_id == "Andhra Pradesh") & (population_final$Qualification == "UnEducated"))
lenAP <- length(stateAP$Id)
lenAP
stateAP
stateAPED <- subset(population_final, (population_final$Year == "2011") & (population_final$Age >= "7") & (population_final$State_id == "Andhra Pradesh"))
stateAPED
lentotalap <- length(stateAPED$Id)
lentotalap
literatesAP <- 10189

literacyrateAP <- 10189/10972
literacyrateAP


stateAAN <- subset(population_final, (population_final$Year == "2011") & (population_final$Age >= "7") & (population_final$State_id == "Andaman and Nicobar Island") & (population_final$Qualification == "UnEducated"))
lenAAN <- length(stateAAN$Id)
lenAAN

stateAANT <- subset(population_final, (population_final$Year == "2011") & (population_final$Age >= "7") & (population_final$State_id == "Andaman and Nicobar Island"))
lenAANT <- length(stateAANT$Id)
lenAANT

literatesAAN <- 9213-666
literatesAAN

literacyrateAAN <- literatesAAN/lenAANT
literacyrateAAN



stateARUNA <- subset(population_final, (population_final$Year == "2011") & (population_final$Age >= "7") & (population_final$State_id == "Arunachal Pradesh") & (population_final$Qualification == "UnEducated"))
lenAAN <- length(stateAAN$Id)
lenAAN
lenARUNA <-  length(stateARUNA$Id)
lenARUNA

stateAANT <- subset(population_final, (population_final$Year == "2011") & (population_final$Age >= "7") & (population_final$State_id == "Andaman and Nicobar Island"))
lenAANT <- length(stateAANT$Id)
stateARUNAT<- subset(population_final, (population_final$Year == "2011") & (population_final$Age >= "7") & (population_final$State_id == "Arunachal Pradesh"))
lenARUNATT <- length(stateARUNAT$Id)
lenAANT

lenARUNATT

literatesARUNA<-11022-807
literatesARUNA

literacyrateARUNA <- literatesARUNA/lenARUNATT
literacyrateARUNA




stateASSAM<- subset(population_final, (population_final$Year == "2011") & (population_final$Age >= "7") & (population_final$State_id == "Assam") & (population_final$Qualification == "UnEducated"))
lenASSAM<- length(stateASSAM$Id)
lenASSAM

stateASSAMT<- subset(population_final, (population_final$Year == "2011") & (population_final$Age >= "7") & (population_final$State_id == "Assam"))
lenASSAMT <- length(stateASSAMT$Id)
lenASSAMT

literatesASSAM<-5214-354
literatesASSAM

literacyrateASSAM <- literatesASSAM/lenASSAMT
literacyrateASSAM



stateCHAN<- subset(population_final, (population_final$Year == "2011") & (population_final$Age >= "7") & (population_final$State_id == "Chandigarh") & (population_final$Qualification == "UnEducated"))
lenCHAN <- length(stateCHAN$Id)
lenCHAN

stateCHANT<- subset(population_final, (population_final$Year == "2011") & (population_final$Age >= "7") & (population_final$State_id == "Chandigarh"))
lenCHANT<- length(stateCHANT$Id)
lenCHANT

literatesCHAND<-6188-416
literatesCHAND

literacyrateCHAN<- literatesCHAN/lenCHANT

literacyrateCHAN<- literatesCHAND/lenCHANT
literacyrateCHAN





stateBIHAR<- subset(population_final, (population_final$Year == "2011") & (population_final$Age >= "7") & (population_final$State_id == "Bihar") & (population_final$Qualification == "UnEducated"))
lenBIHAR<- length(stateBIHAR$Id)
lenBIHAR

stateBIHART<- subset(population_final, (population_final$Year == "2011") & (population_final$Age >= "7") & (population_final$State_id == "Bihar"))

lenBIHART<- length(stateBIHART$Id)
lenBIHART

literatesBIHAR<-8017-586
literatesBIHAR

literacyrateBIHAR<- literatesBIHAR/lenBIHART
literacyrateBIHAR


library(plotrix)
literacy <- c(92.86365,92.77108,92.67828,93.21059,93.27731,92.69053)
stateslit <- c("Andhra Pradesh","Andaman and Nicobar","Arunachal Pradesh","Assam","Chandigarh","Bihar")

barplot(literacy,main = "Literacy Rate",names.arg = stateslit,xlab = "Years",ylab = "lit",col=c("springgreen3","hotpink4","purple","lightsalmon4","goldenrod"))

FINDING:RANKING OF STATES BASED ON  PER CAPTIA INCOME OF DECADE IN 1991

popul1991 <- c(population_final$Year=="1991")
popul1991
population1991 <- population_final[popul1991,]
population1991


p1.1 <- c(population1991$State_id == "Andaman and Nicobar Island")
p1.1
p1.2 <- population1991[p1.1,]
p1.3 <- sum(as.double(p1.2$Annualincome))
p1.3
[1] 81260046325
length(p1.2$Id)
[1] 43600
p1.4 <- length(p1.2$Id)
pcian <- p1.3/p1.4
pcian


p2.1 <- c(population1991$State_id == "Andhra Pradesh")
p2.1
p2.2 <- population1991[p2.1,]
p2.2
p2.3 <- sum(as.double(p2.2$Annualincome))
p2.3

length(p2.2$Id)

p2.4 <- length(p2.2$Id)
pciap <- p2.3/p2.4
pciap
p3.1 <- c(population1991$State_id == "Arunachal Pradesh")
p3.1
p3.2 <- population1991[p3.1,]
p3.2
p3.3 <- sum(as.double(p3.2$Annualincome))
p3.3

length(p3.2$Id)

p3.4 <- length(p3.2$Id)
pciarp <- p3.3/p3.4
pciarp



p4.1 <- c(population1991$State_id == "Assam")
p4.1
p4.2 <- population1991[p4.1,]
p4.2
p4.3 <- sum(as.double(p4.2$Annualincome))
p4.3

length(p4.2$Id)

p4.4 <- length(p4.2$Id)
pcias <- p4.3/p4.4
pcias



p5.1 <- c(population1991$State_id == "Bihar")
p5.1
p5.2 <- population1991[p5.1,]
p5.2
p5.3 <- sum(as.double(p5.2$Annualincome))
p5.3

length(p5.2$Id)

p5.4 <- length(p5.2$Id)
pcib <- p5.3/p5.4
pcib

p6.1 <- c(population1991$State_id == "Chandigarh")
p6.1
p6.2 <- population1991[p6.1,]
p6.2
p6.3 <- sum(as.double(p6.2$Annualincome))
p6.3

length(p6.2$Id)

p6.4 <- length(p6.2$Id)
pcich <- p6.3/p6.4
pcich



pic1991 <- c(1863763,1865955,1863423,1862501,1869719,1833196)
rank(-pic1991)
rankc1991 <- rank(-pic1991)
pics1991 <- c("Andaman","AndhraPradesh","ArunachalPradesh","Assam","Bihar","Chandigarh")
barplot(rankc1991,names.arg = pics1991,xlab="States",ylab = "Ranks",main = "Income of State Ranks in 1991",col=c("navy","red2","dimgrey","goldenrod","seagreen","hotpink"))

#FINDING:PREDICTION OF POPULATION

countrypop <- table(population_final$Year)

countrypop
country1991 <- 279623

country2001 <- 247999

country2011 <- 288011
inc1 <- 247999 - 279623
inc1


inc2 <- 288011 - 247999
inc2

avginc <- (inc1 + inc2)/2
avginc

pop2021 <- country2011 +(avginc * 1)
pop2021

pop2031 <- country2011 +(avginc * 2)
pop2031


poppre<-c(279623,247999,288011,292205,296399)
popyear <- c("1991","2001","2011","2021","2031")


barplot(poppre,main = "Population Prediction",names.arg = popyear,xlab = "Years",ylab = "population",col=c("springgreen3","hotpink4","purple","lightsalmon4","goldenrod"))



#FINDING:CLASSIFICATION BASED ON AGE

test6 <- population_final[c(1:1000),]
test6
View(test6)
test5$X.1=NULL
test5$X=NULL
View(test6)
test5$X.1=NULL
View(test6)
test6$X.1=NULL
View(test6)
namevector = c("Class")
test6[,namevector]=""
for(i in 1:length(test6$Age)){
  if(test6$Age[i] >= 70){
    test6$Class[i]=1}
  if(test6$Age[i] >= 60) & (test6$Age[i] < 70){
    for(i in 1:length(test6$Age)){
      if(test6$Age[i] >= 70){
        test6$Class[i]=1}
      if(test6$Age[i] >= 60) & (test6$Age[i] < 70)){
        for(i in 1:length(test6$Age)){
          if(test6$Age[i] >= 70){
            test6$Class[i]=1}
          if((test6$Age[i] >= 60) & (test6$Age[i] < 70)){
            test6$Class[i]=2}
          if((test6$Age[i] >= 50) & (test6$Age[i] < 60)){
            test6$Class[i]=3}
          if((test6$Age[i] >= 40) & (test6$Age[i] < 50)){
            test6$Class[i]=4}
          if((test6$Age[i] >= 30) & (test6$Age[i] < 40)){
            test6$Class[i]=5}
          if((test6$Age[i] >= 20) & (test6$Age[i] < 30)){
            test6$Class[i]=6}
          if((test6$Age[i] >= 10) & (test6$Age[i] < 20)){
            test6$Class[i]=7}
          if((test6$Age[i] >= 1) & (test6$Age[i] < 10)){
            test6$Class[i]=8}
        }
        View(test6)
        
        write.csv(test6,file="test6.csv")
        
        
        test6 <- read.csv("~/test6.csv")
        View(test6)
        test6$Gender=NULL
        test6$Aadhar=NULL
        test6$State_id=NULL
        test6$X=NULL
        test6$X=NULL
        test6$District_id=NULL
        test6$Marital=NULL
        test6$Religion=NULL
        test6$voter_id=NULL
        test6$ration_card=NULL
        test6$Year=NULL
        test6$EmployeeName=NULL
        library(e1071)
        library(rpart)
        
        
        
        index<- 1:nrow(test6)
        
        testindex <- sample(index, trunc(length(index)/3))
        
        testset<- test6[testindex,]
        
        trainset <- test6[-testindex,]
        
        
        svm.model <- svm(Class ~ ., data = trainset, cost = 100, gamma = 1)
        svm.model
        
        
        
        svm.model <- svm(Class ~ ., data = trainset, type="C-classification", cost = 100, gamma = 1)
        svm.model
        
        
        svm(formula = Class ~ ., data = trainset, type = "C-classification", cost = 100, 
            +     gamma = 1)
        svm.pred <- predict(svm.model, testset[,-7])
        table(pred = svm.pred, true = testset[,7])
        true
        pred  1  2  3  4  5  6  7  8
        1 39 14  1  0  0  0  0  0
        2 13 24  4  1  0  0  0  0
        3  1 13 31  9  1  0  0  0
        4  0  1  9 21  7  2  0  0
        5  0  0  0  5 22  3  0  0
        6  0  0  0  0  4 29  1  0
        7  0  0  0  0  0  1 39  1
        8  0  0  0  0  0  0  5 32
        
        svm.pred 
        
        FINDING: PERCAPITA INCOME OF DIFFERENT SECTORS
        s <- c(population_final$Year == 2011)
        s <- population_final[s,]
        DL <- (s$Occupation == "Daily Laboures")
        daily <- s[DL,]
        lendaily <- length(daily$Id)
        lendaily 
        
        ann2011daily <- sum(as.double(daily$Annualincome))
        dailywages <- ann2011daily/lendaily
        dailywages
        
        
        PB <- (s$Occupation == "Pretty Business")
        prettybus <- s[PB,]
        pre <- s[PB,]
        ann2011PB <- sum(as.double(pre$Annualincome))
        ann2011PB
        
        lenPB <- length(pre$Id)
        prettybusiness <- ann2011PB/lenPB
        prettybusiness
        
        
        PS <- (s$Occupation == "Private sector")
        pri <- s[PS,]
        ann2011PS <- sum(as.double(pri$Annualincome))
        ann2011PS
        
        lenPS <- length(pri$Id)
        privatesector <- ann2011PS/lenPS
        privatesector
        
        
        
        GS <- (s$Occupation == "Government sector")
        Govt <- s[GS,]
        ann2011GS <- sum(as.double(Govt$Annualincome))
        ann2011GS
        
        lenGS <- length(Govt$Id)> lenGS
        governmentsector <- ann2011GS/lenGS
        governmentsector
        
        PT <- (s$Occupation == "Private technical")
        PTE <- s[PT,]
        len <- length(PTE$Id)
        ann2011PT<- sum(as.double(PTE$Annualincome))
        privatetechnical <- ann2011PT/len
        privatetechnical 
        
        
        
        soft <- (s$Occupation == "Software")
        softw <- s[soft,]
        lensoft <- length(softw$Id)
        ann2011soft<- sum(as.double(softw$Annualincome))
        ann2011soft
        
        software <- ann2011soft/lensoft
        software
        
        
        
        doctor <- (s$Occupation == "Doctor")
        d <- s[doctor,]
        lend <- length(d$Id)
        ann2011d<- sum(as.double(d$Annualincome))
        ann2011d
        
        doctorPI <- ann2011d/lend
        doctorPI
        
        
        picos2011 <- c(202091.3,199923.6,797435.4,1620100,2091031,4143676,5242978)
        rank(-picos2011)
        
        rank2011os<- rank(-picos2011)
        picoccupation <- c("Daily Laboures","Pretty Business","Private sector","Government sector","Private technical","Software","Doctor")
        
        barplot(rank2011os,names.arg = picoccupation,xlab="Employment Sectors",ylab = "Ranks",main = "Income of Different Sectors Ranks in 2011",col=c("red3","seagreen4","maroon3","lightcoral","magenta3","goldenrod","chocolate"))
        
        FINDING:UNEMPLOYMENT IN INDIA
        
        occupop <- table(population_final$Occupation,population_final$Year)
        occupop
        o <- c(55851,49569,57942)
        p <- c("1991", "2001", "2011")
        barplot(o,names.arg = p,xlab = "Years",ylab = "Unemployed People",col =c("goldenrod","seagreen","purple4"),main = "Unemployement chart",border = "red")
        
        #FINDING:-UNEDUCATION IN INDIA
        
        qualipop <- table(population_final$Qualification,population_final$Year)
        qualipop
        q <- c(29967,26566,31150)
        pq <- c("1991", "2001", "2011")
        barplot(q,names.arg = pq,xlab = "Years",ylab = "UnEducated People",col = c("seagreen","sienna2","lightcoral"),main = "UnEducation chart",border = "red")
        
        #FINDING :- GENDER PLOT:
        
        gender <- table(population_final$Gender,population_final$Year)
        gender
        colors <- c("orange","brown")
        years <- c("1991","2001","2011")
        regions <- c("Male","Female")
        Values <- matrix(c(139570,124297,143815,140053,123702,144196),nrow = 2,ncol = 3,byrow = TRUE)
        barplot(Values,main = "Gender",names.arg = years,xlab = "Years",ylab = "Population",col = colors)
        legend("top", regions, cex = 0.8, fill = colors)
        
        
        
        
        
        #FINDING:- PERCAPITA
        
        annu <- c(population_final$Year == "1991")
        annu
        annual <- population_final[annu,]
        annual
        annualincome <- sum(as.double(annual$Annualincome))
        annualincome
        length(annual$Id)
        pop <- length(annual$Id)
        pop
        annual1991 <- annualincome/pop
        annual1991
        annu1<- c(population_final$Year == "2001")
        annu1
        annual1 <- population_final[annu1,]
        annual1
        annualincome1 <- sum(as.double(annual1$Annualincome))
        annualincome1
        length(annual1$Id)
        pop1 <- length(annual1$Id)
        pop1
        annual2001 <- annualincome1/pop1
        annual2001
        annu2<- c(population_final$Year == "2011")
        annu2
        annual2 <- population_final[annu2,]
        annual2
        annualincome2 <- sum(as.double(annual2$Annualincome))
        annualincome2
        
        length(annual2$Id)
        pop2 <- length(annual2$Id)
        pop2
        annual2011 <- annualincome2/pop2
        annual2011
        s <- c(1860557,1867931,1857288)
        l <- c("1991", "2001", "2011")
        barplot(s,main = "PERCAPITA INCOME",names.arg = l,xlab = "Decades",ylab = "Percapita Income",col=c("seagreen","purple","darkorange"))
        
        #FINDING:-
        
        occupopstate <- table(population_final$Occupation == "UnEmployement",population_final$State_id,population_final$Year)
        occupopstate
        o1991<- c(8629,9774,11348,8126,9837,8137)
        oc1991<-c("Andaman","AndhraPradesh","Arunachal", "Assam", "Bihar", "Chandigarh")
        barplot(o1991,main = "Unemployment in 1991",names.arg = oc1991,xlab = "states",ylab = "population",col=c("dimgray","dimgray","darkorange","dimgray","dimgray","dimgray"))
        o2001 <- c(6942,7629,9799,5033,11072,9094)
        oc2001 <- c("Andaman","AndhraPradesh","Arunachal", "Assam", "Bihar", "Chandigarh")
        barplot(o2001,main = "Unemployment in 2001",names.arg = oc2001,xlab = "states",ylab = "population",col=c("dimgray","dimgray","dimgray","dimgray","seagreen","dimgray"))
        o2011 <- c(10578,12387,12673,5929,9431,6944)
        oc2011 <- c("Andaman","AndhraPradesh","Arunachal", "Assam", "Bihar", "Chandigarh")
        barplot(o2011,main = "Unemployment in 2011",names.arg = oc2011,xlab = "states",ylab = "population",col=c("dimgray","dimgray","darkblue","dimgray","dimgray","dimgray"))
        