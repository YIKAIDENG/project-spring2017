#################################
#  Programming Assessment in R  #
#     Bud Lab - Internship      #
#        by Hanlin Zhang        #
#################################

################################################
#            Customize some funtions           #
################################################

#require library, @author Mohammed 25-12-2016
checkInstallLoad <- function(libName) 
{
  if(!require(libName, character.only=TRUE)) 
  {
    install.packages(libName)
    require(libName, character.only=TRUE)
  }
}

#multiplot viz, @author unknown
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {  
  library(grid)  
  
  # Make a list from the ... arguments and plotlist  
  plots <- c(list(...), plotlist)  
  
  numPlots = length(plots)  
  
  # If layout is NULL, then use 'cols' to determine layout  
  if (is.null(layout)) {  
    # Make the panel  
    # ncol: Number of columns of plots  
    # nrow: Number of rows needed, calculated from # of cols  
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),  
                     ncol = cols, nrow = ceiling(numPlots/cols))  
  }  
  
  if (numPlots==1) {  
    print(plots[[1]])  
    
  } else {  
    # Set up the page  
    grid.newpage()  
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))  
    
    # Make each plot, in the correct location  
    for (i in 1:numPlots) {  
      # Get the i,j matrix positions of the regions that contain this subplot  
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))  
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,  
                                      layout.pos.col = matchidx$col))  
    }  
  }  
}  


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

################################################
#                 Environment set              #
################################################
getwd()
setwd("C:/Users/miley/Desktop/LIS590DV/")

checkInstallLoad("pairsD3")
checkInstallLoad("ggplot2")
checkInstallLoad("Hmisc")
checkInstallLoad("car")
checkInstallLoad("psych")

#setup ggplot environment
theme_update(plot.title = element_text(hjust = 0.5))

################################################
#          load data from online file          #
################################################

df = read.table("sample_flat.csv", sep=",",header=T, fill=FALSE,strip.white=T)

################################################
#              frature enginering              #
################################################

df.sel = subset(df, select = c(categorical, quant1, quant2, quant3))

pairs.panels(df.sel, col="red")

df.sel$quant1 = 10 ^ (df.sel$quant1)

#==========Step.1 understanding dataset========#

fit = lm(quant3 ~ categorical + quant1 + quant2, data = df.sel)

#summary stats 

#age
boxplot(train_bkup$age)
summary(train_bkup$age)

#workclass
summary(train_bkup$workclass)

#fnlwgt
boxplot(train_bkup$fnlwgt)
summary(train_bkup$fnlwgt)

#education
#This factor can be replaced by the education index,
#omit this value here

#education-num
boxplot(train_bkup$education.num)
summary(train_bkup$education.num)

#marital-status
summary(train_bkup$marital.status)

#occupation
summary(train_bkup$occupation)

#relationship
summary(train_bkup$relationship)

#race
summary(train_bkup$race)

#sex
summary(train_bkup$sex)

#capital-gain
summary(train_bkup$capital.gain)
boxplot(train_bkup$capital.gain)

#capital-loss
summary(train_bkup$capital.loss)
boxplot(train_bkup$capital.loss)

#hours-per-week
summary(train_bkup$hours.per.week)
boxplot(train_bkup$hours.per.week)

#native-country
summary(train_bkup$native.country)

#income
summary(train_bkup$income)

#some histograms

h1 = qplot(train_bkup$age, geom="histogram", xlab = "age")
h2 = qplot(as.numeric(train_bkup$workclass), geom="histogram", xlab = "workclass")
h3 = qplot(train_bkup$education.num, geom="histogram", xlab = "education.num")
h4 = qplot(as.numeric(train_bkup$marital.status), geom="histogram", xlab = "marital.status")
h5 = qplot(as.numeric(train_bkup$occupation), geom="histogram", xlab="occupation")
h6 = qplot(as.numeric(train_bkup$relationship), geom="histogram", xlab = "relationship")
h7 = qplot(as.numeric(train_bkup$race), geom="histogram", xlab = "race")
h8 = qplot(as.numeric(train_bkup$sex), geom="histogram", xlab = "sex")
h9 = qplot(train_bkup$capital.gain, geom="histogram", xlab = "capital.gain")
h10 = qplot(train_bkup$capital.loss, geom="histogram", xlab = "capital.loss")
h11 = qplot(train_bkup$hours.per.week, geom="histogram", xlab = "hours.per.week")
h12 = qplot(as.numeric(train_bkup$native.country), geom="histogram", xlab = "native.country")

png(file="hist_bf_cl.png", width = 1366, height = 768,bg="transparent")
multiplot(h1, h2, h3, h4,
          h5,h6,h7,h8,h9,
          h10,h11,h12, cols=4)  
dev.off()

#It looks the dataset is already cleaned.

#Missing value strategy:
#Strategy1: use mean(for contineous values) and mode(for discrete values) to replace "?"
#Strategy2: omit the missing values

#Try to omit at first, will look at the difference of obs. between train &train_bkup
is.na(train) = train=='?'
train = na.omit(train)

#train set contains 30162 obs only 2399 less than the origional one.
#Strategy2 is satisfy enough, will not go Strategy1

#==========Step.2 reducing the complexity========#

#Setup binning

#native-country
#set native.country to character allows for editing
train$native.country = as.character(train$native.country)

#classifier achieved online,
#accessble at http://scg.sdsu.edu/dataset-adult_r/

train$native.country[train$native.country=="Cambodia"] = "SE-Asia"
train$native.country[train$native.country=="Canada"] = "British-Commonwealth"    
train$native.country[train$native.country=="China"] = "China"       
train$native.country[train$native.country=="Columbia"] = "South-America"    
train$native.country[train$native.country=="Cuba"] = "Other"        
train$native.country[train$native.country=="Dominican-Republic"] = "Latin-America"
train$native.country[train$native.country=="Ecuador"] = "South-America"     
train$native.country[train$native.country=="El-Salvador"] = "South-America" 
train$native.country[train$native.country=="England"] = "British-Commonwealth"
train$native.country[train$native.country=="France"] = "Euro_1"
train$native.country[train$native.country=="Germany"] = "Euro_1"
train$native.country[train$native.country=="Greece"] = "Euro_2"
train$native.country[train$native.country=="Guatemala"] = "Latin-America"
train$native.country[train$native.country=="Haiti"] = "Latin-America"
train$native.country[train$native.country=="Holand-Netherlands"] = "Euro_1"
train$native.country[train$native.country=="Honduras"] = "Latin-America"
train$native.country[train$native.country=="Hong"] = "China"
train$native.country[train$native.country=="Hungary"] = "Euro_2"
train$native.country[train$native.country=="India"] = "British-Commonwealth"
train$native.country[train$native.country=="Iran"] = "Other"
train$native.country[train$native.country=="Ireland"] = "British-Commonwealth"
train$native.country[train$native.country=="Italy"] = "Euro_1"
train$native.country[train$native.country=="Jamaica"] = "Latin-America"
train$native.country[train$native.country=="Japan"] = "Other"
train$native.country[train$native.country=="Laos"] = "SE-Asia"
train$native.country[train$native.country=="Mexico"] = "Latin-America"
train$native.country[train$native.country=="Nicaragua"] = "Latin-America"
train$native.country[train$native.country=="Outlying-US(Guam-USVI-etc)"] = "Latin-America"
train$native.country[train$native.country=="Peru"] = "South-America"
train$native.country[train$native.country=="Philippines"] = "SE-Asia"
train$native.country[train$native.country=="Poland"] = "Euro_2"
train$native.country[train$native.country=="Portugal"] = "Euro_2"
train$native.country[train$native.country=="Puerto-Rico"] = "Latin-America"
train$native.country[train$native.country=="Scotland"] = "British-Commonwealth"
train$native.country[train$native.country=="South"] = "Euro_2"
train$native.country[train$native.country=="Taiwan"] = "China"
train$native.country[train$native.country=="Thailand"] = "SE-Asia"
train$native.country[train$native.country=="Trinadad&Tobago"] = "Latin-America"
train$native.country[train$native.country=="United-States"] = "United-States"
train$native.country[train$native.country=="Vietnam"] = "SE-Asia"
train$native.country[train$native.country=="Yugoslavia"] = "Euro_2"

summary(factor(train$native.country))


#capital-gain&capital-loss
train$capital.gain = ifelse(train$capital.gain > 0 & 
                            train$capital.gain < mean(train$capital.gain),"Low",
                            ifelse(train$capital.gain > mean(train$capital.gain),"High","None"))

train$capital.loss = ifelse(train$capital.loss > 0 & 
                            train$capital.loss < mean(train$capital.loss),"Low",
                            ifelse(train$capital.loss > mean(train$capital.loss),"High","None"))

#age
train$age = ifelse(train$age <= 18,"youth",
            ifelse(train$age > 18 & train$age <= 35, "adult",
            ifelse(train$age > 35 & train$age <= 60, "middle-aged", "elderly")))

#hours-per-week
train$hours.per.week = ifelse(train$hours.per.week <= 25,"Part-time",
                       ifelse(train$hours.per.week > 25 & train$hours.per.week <= 42, "Fulltime",
                       ifelse(train$hours.per.week > 42 & train$hours.per.week <= 60, "Overtime", "Superman")))

#education
train$education.num = ifelse(train$education.num <= 9,"Low",
                      ifelse(train$education.num > 9 & train$education.num <= 10.08, "Average",
                      ifelse(train$education.num > 10.08 & train$education.num <= 12, "Edu", "Edu_Byd")))


#some empirical pre-analysis viz

#income based on age
based_on_age<-subset(train, select=c(age,income))
p1 = qplot(factor(based_on_age$age), data=based_on_age, geom="bar", fill=factor(based_on_age$income),
      xlab = deparse(substitute(Age)), ylab = deparse(substitute(Num_of_obs.)))+ggtitle("Income based on Age")

#based_on_Worktime
based_on_wktm<-subset(train, select=c(hours.per.week,income))
p2 = qplot(factor(based_on_wktm$hours.per.week), data=based_on_wktm, geom="bar", fill=factor(based_on_wktm$income),
      xlab = deparse(substitute(Worktime)), ylab = deparse(substitute(Num_of_obs.)))+ggtitle("Income based on Worktime") +scale_fill_manual(values = wes_palette("Royal1"))

#based_on_sex
based_on_sex<-subset(train, select=c(sex,income))
p3 = qplot(factor(based_on_sex$sex), data=based_on_sex, geom="bar", fill=factor(based_on_sex$income),
      xlab = deparse(substitute(Gender)), ylab = deparse(substitute(Num_of_obs.)))+ggtitle("Income based on Gender") +scale_fill_manual(values = wes_palette("Moonrise1"))

#based_on_Edu
based_on_edu<-subset(train, select=c(education.num,income))
p4 = qplot(factor(based_on_edu$education.num), data=based_on_edu, geom="bar", fill=factor(based_on_edu$income),
      xlab = deparse(substitute(Education)), ylab = deparse(substitute(Num_of_obs.)))+ggtitle("Income based on Education") +scale_fill_manual(values = wes_palette("Chevalier"))

png(file="empirical_analysis.png", width = 1366, height = 768,bg="transparent")
multiplot(p1, p2, p3, p4, cols=2)  
dev.off()

#==========Step.3 preparing for analysis ========#

train[["fnlwgt"]]=NULL
train[["education"]]=NULL

# Transform char into int
int_train = as.data.frame(sapply(names(sapply(train, class)), # Colnames of factor columns
                                 function(i) as.integer(as.factor(train[, i])) - 1 ))
# Scaling
sca_test = scale(int_train[, 1:(ncol(int_train)-1)])
int_set = as.data.frame(cbind(sca_test,income = int_train$income))

hb1 = qplot(int_set$age, geom="histogram", xlab = "age")
hb2 = qplot(int_set$workclass, geom="histogram", xlab = "workclass")
hb3 = qplot(int_set$education.num, geom="histogram", xlab = "education.num")
hb4 = qplot(int_set$marital.status, geom="histogram", xlab = "marital.status")
hb5 = qplot(int_set$occupation, geom="histogram", xlab="occupation")
hb6 = qplot(int_set$relationship, geom="histogram", xlab = "relationship")
hb7 = qplot(int_set$race, geom="histogram", xlab = "race")
hb8 = qplot(int_set$sex, geom="histogram", xlab = "sex")
hb9 = qplot(int_set$capital.gain, geom="histogram", xlab = "capital.gain")
hb10 = qplot(int_set$capital.loss, geom="histogram", xlab = "capital.loss")
hb11 = qplot(int_set$hours.per.week, geom="histogram", xlab = "hours.per.week")
hb12 = qplot(int_set$native.country, geom="histogram", xlab = "native.country")

png(file="hist_af_sc.png", width = 1366, height = 768,bg="transparent")
multiplot(hb1, hb2, hb3, hb4,
          hb5, hb6, hb7, hb8,
          hb9, hb10,hb11,hb12, cols=4)  
dev.off()

#==========Step.4 save finished file ========#
write.csv(int_set, file = "cleaned_train.csv", row.names = F)


################################################
#             Repeat for test set              #
################################################

#==========Warning: nothing worth to read after this line ========#


#Try to omit at first, will look at the difference of obs. between test &test_bkup
is.na(test) = test=='?'
test = na.omit(test)

#Setup binning

#native-country
#set native.country to character allows for editing
test$native.country = as.character(test$native.country)

#classify standard suggested by online resource,
#accessble at http://scg.sdsu.edu/dataset-adult_r/

test$native.country[test$native.country=="Cambodia"] = "SE-Asia"
test$native.country[test$native.country=="Canada"] = "British-Commonwealth"    
test$native.country[test$native.country=="China"] = "China"       
test$native.country[test$native.country=="Columbia"] = "South-America"    
test$native.country[test$native.country=="Cuba"] = "Other"        
test$native.country[test$native.country=="Dominican-Republic"] = "Latin-America"
test$native.country[test$native.country=="Ecuador"] = "South-America"     
test$native.country[test$native.country=="El-Salvador"] = "South-America" 
test$native.country[test$native.country=="England"] = "British-Commonwealth"
test$native.country[test$native.country=="France"] = "Euro_1"
test$native.country[test$native.country=="Germany"] = "Euro_1"
test$native.country[test$native.country=="Greece"] = "Euro_2"
test$native.country[test$native.country=="Guatemala"] = "Latin-America"
test$native.country[test$native.country=="Haiti"] = "Latin-America"
test$native.country[test$native.country=="Holand-Netherlands"] = "Euro_1"
test$native.country[test$native.country=="Honduras"] = "Latin-America"
test$native.country[test$native.country=="Hong"] = "China"
test$native.country[test$native.country=="Hungary"] = "Euro_2"
test$native.country[test$native.country=="India"] = "British-Commonwealth"
test$native.country[test$native.country=="Iran"] = "Other"
test$native.country[test$native.country=="Ireland"] = "British-Commonwealth"
test$native.country[test$native.country=="Italy"] = "Euro_1"
test$native.country[test$native.country=="Jamaica"] = "Latin-America"
test$native.country[test$native.country=="Japan"] = "Other"
test$native.country[test$native.country=="Laos"] = "SE-Asia"
test$native.country[test$native.country=="Mexico"] = "Latin-America"
test$native.country[test$native.country=="Nicaragua"] = "Latin-America"
test$native.country[test$native.country=="Outlying-US(Guam-USVI-etc)"] = "Latin-America"
test$native.country[test$native.country=="Peru"] = "South-America"
test$native.country[test$native.country=="Philippines"] = "SE-Asia"
test$native.country[test$native.country=="Poland"] = "Euro_2"
test$native.country[test$native.country=="Portugal"] = "Euro_2"
test$native.country[test$native.country=="Puerto-Rico"] = "Latin-America"
test$native.country[test$native.country=="Scotland"] = "British-Commonwealth"
test$native.country[test$native.country=="South"] = "Euro_2"
test$native.country[test$native.country=="Taiwan"] = "China"
test$native.country[test$native.country=="Thailand"] = "SE-Asia"
test$native.country[test$native.country=="Trinadad&Tobago"] = "Latin-America"
test$native.country[test$native.country=="United-States"] = "United-States"
test$native.country[test$native.country=="Vietnam"] = "SE-Asia"
test$native.country[test$native.country=="Yugoslavia"] = "Euro_2"

summary(factor(test$native.country))


#capital-gain&capital-loss
test$capital.gain = ifelse(test$capital.gain > 0 & 
                    test$capital.gain < mean(test$capital.gain),"Low",
                    ifelse(test$capital.gain > mean(test$capital.gain),"High","None"))

test$capital.loss = ifelse(test$capital.loss > 0 & 
                    test$capital.loss < mean(test$capital.loss),"Low",
                    ifelse(test$capital.loss > mean(test$capital.loss),"High","None"))

#age
test$age = ifelse(test$age <= 18,"youth",
           ifelse(test$age > 18 & test$age <= 35, "adult",
           ifelse(test$age > 35 & test$age <= 60, "middle-aged", "elderly")))

#hours-per-week
test$hours.per.week = ifelse(test$hours.per.week <= 25,"Part-time",
                      ifelse(test$hours.per.week > 25 & test$hours.per.week <= 42, "Fulltime",
                      ifelse(test$hours.per.week > 42 & test$hours.per.week <= 60, "Overtime", "Superman")))

#education
test$education.num = ifelse(test$education.num <= 9,"Low",
                     ifelse(test$education.num > 9 & test$education.num <= 10.08, "Average",
                     ifelse(test$education.num > 10.08 & test$education.num <= 12, "Edu", "Edu_Byd")))

test[["fnlwgt"]]=NULL
test[["education"]]=NULL

#convert to int
int_test = as.data.frame(sapply(names(sapply(test, class)), # Colnames of factor columns
                                function(i) as.integer(as.factor(test[, i])) - 1 ))

# Scaling
sca_test = scale(int_test[, 1:(ncol(int_test)-1)])
int_test = as.data.frame(cbind(sca_test,income = int_test$income))

write.csv(int_test, file = "cleaned_test.csv", row.names = F)