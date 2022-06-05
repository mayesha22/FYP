install.packages("hrbrthemes")

install.packages("writexl")

install.packages("sjPlot")
install.packages("caTools")
install.packages('car')
library(lme4)


library(writexl)
library(readxl)
library(ggplot2)
library(dplyr)
#libraries
library(tidyverse)
library(viridis)


library(hrbrthemes)
library(ape)
library(geiger)
library(nlme)

library(phytools)
library(data.table)


library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(caTools)
library(car)

library(forcats)



fyp_dataset_1_ <- read_excel("~/fyp dataset (2).xlsx", 
                             sheet = "egg and clutch size")
View(fyp_dataset_1_)#importing sperm data 
sperm_data <- read_excel("~/fyp dataset (2).xlsx", 
                             sheet = "sperm data")
View(sperm_data)

# importing egg data 
egg_data <- read_excel("~/fyp dataset (2).xlsx", 
                             sheet = "egg and clutch size")
View(egg_data)
 
egg_data <- data.frame(egg_data)

egg_data %>% colnames()



egg_mass_data <- egg_data %>% dplyr::select(ID, "egg_mass" = "egg.mass") %>%   #selects 2 column from egg data and changed into numerical form. changed from egg.mass to egg_mass
  mutate(egg_mass = as.double(egg_mass))

#egg_data %>% dplyr::select("ID", "egg.mass") %>%   #selects 2 column from egg data and changed into numerical form. changed from egg.mass to egg_mass
#  mutate(egg.mass = as.double(egg.mass))





View(egg_mass_data)

#DATA FOR egg_skew
egg_data_plot <- egg_mass_data %>% 
  group_by(egg_mass) %>%
  mutate(number_of_species = n()) %>% 
  filter(!is.na(egg_mass))

View(egg_data_plot)

ggplot(egg_data_plot, aes(x=egg_mass, y = number_of_species)) + geom_line() + ggtitle("egg_skew") + geom_point()


filtered_egg_mass <- egg_data_plot %>% 
  filter(egg_mass <= 5)



View(filtered_egg_mass)


ggplot(filtered_egg_mass, aes(x=egg_mass, y = number_of_species)) + geom_line() + ggtitle("filtered_egg_skew for egg mass less than 5")



filtered_egg_mass <- egg_data_plot %>% 
  filter(egg_mass <= 50)

View(filtered_egg_mass)

ggplot(filtered_egg_mass, aes(x=egg_mass, y = number_of_species)) + geom_line() + ggtitle("filtered_egg_skew for egg mass less than 50")





#data for sperm
View(sperm_data)
sperm_data <- data.frame(sperm_data)
sperm_data %>% colnames()



Total_Length <- sperm_data

Total_length_data <- sperm_data %>% select(Total_Length) %>%
 mutate(Total_Length = as.double(Total_Length))


View(Total_length_data)

sperm_data_plot <- Total_length_data %>% 
  group_by(Total_Length) %>%
  mutate(number_of_species = n()) %>% 
  filter(!is.na(Total_Length))

View(sperm_data_plot)
  
ggplot(sperm_data_plot, aes(x=Total_Length, y = number_of_species)) + 
  geom_line() + ggtitle("sperm_skew") + geom_point()


ggplot(sperm_data_plot, aes(x=Total_Length, y = number_of_species)) + ggtitle("sperm_skew") + geom_point()


filtered_Total_length <- sperm_data_plot %>% 
filter(Total_Length <= 120)
View(filtered_Total_length)
ggplot(filtered_Total_length, aes(x=Total_Length, y = number_of_species)) + geom_line() + ggtitle("filtered_sperm_skew for total length less than 120")


#dataset for histogram
#empty output here
#p <- ggplot(egg_mass_data,aes(x=egg_mass_data)) +geom_histogram()
#p



# load dataset from github 
data <-read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/1_OneNum.csv", header=TRUE)
data %>% head()
# plot
p <- egg_mass_data %>%
  filter( egg_mass <=180 ) %>%
  ggplot( aes(x=egg_mass)) +
  geom_histogram( binwidth=10, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Histogram of egg mass") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=20)
  )
p

p.labs <- p + labs(x = "Egg mass(grams) ", y = "Number of species")

p.labs

#cor3<-cor.test(ani$ssd, ani$log.final.anisogamy)
#cor3

egg_mass_data %>% head()
# histogram for sperm length
head(Total_length_data)
ggplot(Total_length_data, aes (Total_Length)) + geom_histogram(binwidth = 30)

ggplot(Total_length_data, aes (Total_Length, fill=Total_Length)) + geom_freqpoly()

ggplot(Total_length_data, aes (Total_Length, fill=Total_Length)) + geom_histogram() + ggtitle("Histogram of sperm length")



ggplot(Total_length_data, aes (Total_Length, fill=Total_Length)) + geom_histogram() + ggtitle("Histogram of sperm length") + labs(x= "Total length of sperm in micrometers" , y= "number of species")
 # calculating degree of anisogamy

#egg_mass_data/Total_length_data
na.omit(egg_mass_data)
options(max.print=9999)
na.omit(Total_length_data)
#? max.print

#  dividing columns 

egg_mass_data %>% head()
Total_length_data %>% head()

sperm_data %>% colnames()
egg_data %>% colnames()

egg_mass_species <- egg_data %>% 
  select("species", "egg.mass", "ssd") %>%
  mutate(egg_mass = as.double(egg.mass))

egg_data %>% colnames()

egg_mass_species[,3] %>% class()

sperm_species_length <- sperm_data %>% 
  select("species" = "phylo", "Total_Length") #renamed to species from phylo
sperm_species_length[,2] %>% class()  # type : charachter or numeric 


  
sperm_data %>% head()



sperm_species_length %>% head()
egg_data %>% head()
view(sperm_species_length)
#X5 making ani
#add columns here to add to ani table
#changed and combined 2 columns


ani <- sperm_species_length %>%
  full_join(egg_data %>%
              select("species","egg.mass", "ssd", "Order" = "order", "m.mass", "f.mass", "fpg.scr", "mpg.scr", "inc_2", "postf.feed_2","postf.grd_2","nest.bld","nest.grd_2", "chick.brd_2", "chick.dfc_2", "chick.feed_2", "pl.head", "pl.back", "pl.belly", "pl.wings", "pl.tail","mating.sys", "f.wing","m.wing") %>%
              mutate(egg_mass = as.double(egg.mass)),
            by = "species") %>%
  mutate(anisogamy = Total_Length/ egg_mass)

colnames(sperm_species_length)
  

#ani <- sperm_species_length %>% 
 # full_join(egg_data %>% 
#              select("species","egg.mass", "ssd", "Order"= "order", "m.mass", "f.mass", "fpg.scr", "mpg.scr", "inc_2", "postf.feed_2","postf.grd_2","nest.bld","nest.grd_2", "chick.brd_2", "chick.dfc_2", "chick.feed_2", "pl.head", "pl.back", "pl.belly", "pl.wings", "pl.tail","mating.sys", "f.wing","m.wing") %>%   
 #             mutate(egg_mass = as.double(egg.mass)), 
  #          by = "species") %>% 
  #mutate(anisogamy = Total_Length / egg_mass) %>%
  
  
colnames(ani)

ani %>%
  mutate(div = ((egg_mass/1.031)*3)/ 4*pi)

colnames(ani)


view(ani)
colnames(ani)


#x4 mutating new columns into ani, logifying variables

ani <- ani %>% 
  mutate(log_egg_mass = log10(egg_mass),
         log_total_length = log10(Total_Length))
ani <- ani %>% 
  mutate(log_egg_mass = log10(egg_mass),
         log_total_length = log10(Total_Length))

       

#ani <- ani %>% 
 # mutate(final.anisogamy = as.double(final.anisogamy),
  #       log.final.anisogamy = log10(final.anisogamy),
   #      log.final.anisogamy = as.double(log.final.anisogamy))
ani <- ani%>%
  mutate(l.anisogamy = log10(anisogamy))
ani <- ani %>%
  mutate(m.mass = as.double(m.mass))
ani <- ani %>%
  mutate(log.m.mass = log10(m.mass))
ani <- ani %>%
  mutate(div = (egg_mass/1.031)*3)
ani <- ani %>%
  mutate(ndiv = div/ (4*pi))



cuberoot = function(x){
  if(x < 0)
  { - (-x)^(1/3)}
  else
  {x^(1/3)}
}

ani <- ani %>%
  mutate(radius = cuberoot(ndiv))

ani <- ani %>%
  mutate(final.egg.mass = radius*10000)

colnames(ani)
view(ani)
ani <- ani %>%
  mutate(f.mass = as.double(f.mass))
ani <- ani %>%
  mutate(log.f.mass = log10(f.mass))
ani <- ani %>%
  mutate(ssd = as.double(ssd))
ani <- ani%>%
  mutate(m.wing = as.double(m.wing))
ani <- ani%>%
  mutate(f.wing = as.double(f.wing))

mass.ssd<-log10(ani$m.mass/ani$f.mass)
wing.ssd<-log10(ani$m.wing/ani$f.wing)

ssdimo<-as.data.frame(cbind(mass.ssd,wing.ssd))
ssdimo$species<-species
colnames(ani)

hist(ssdimo$mass.ssd, main="Histogram of Mass SSD", xlab="Mass SSD")
view(ssdimo)
hist(ani$ssd, xlab = "Mass ssd")
view(ani)
#correlation tests
#cor0<-cor.test(ssdimo$mass.ssd, ssdimo$wing.ssd)

#ssd.cor9 <- cor.test(ani$log.final.anisogamy, ani$ssd)
#ssd.cor9

ssdone <- cor.test(ani$log.final.anisogamy, ani$ssd)
ssdone
#Pearson's product-moment correlation

#data:  ani$log.final.anisogamy and ani$ssd
#t = -3.8654, df = 565, p-value = 0.0001238
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
 #-0.23968436 -0.07921415
#sample estimates:
#       cor 
#-0.1605097 


ssfg <- cor.test(ani$log.final.anisogamy, ani$ssd)
ssfg

eggspermcor <- cor.test(ani$log_total_length, ani$log.final.egg.mass)
eggspermcor
#orna.cor8 <- cor.test(ani$log.final.anisogamy, orna$pl.mean3)
#orna.cor8

#Pearson's product-moment correlation

#data:  ani$log.final.anisogamy and orna$pl.mean3
#t = 3.0548, df = 568, p-value = 0.002357
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.04548765 0.20710104
#sample estimates:
 #     cor 
#0.1271381 


mate.cor7 <- cor.test(ani$log.final.anisogamy, ani$mating.sys)
mate.cor7

#Pearson's product-moment correlation

#data:  ani$log.final.anisogamy and ani$mating.sys
#t = 1.931, df = 506, p-value = 0.05404
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
 # -0.001477732  0.171252549
#sample estimates:
#  cor 
#0.08553004 


#parent.cor6 <- cor.test(ani$log.final.anisogamy, ani$meanCare)
#parent.cor6

#Pearson's product-moment correlation

#data:  ani$log.final.anisogamy and ani$meanCare
#t = -3.6892, df = 501, p-value = 0.0002496
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.24654983 -0.07628301
#sample estimates:
#  cor 
#-0.1626269 


mating.sys<-ani$mating.sys
mate.sys<-mating.sys[!is.na(mating.sys)]
length(mate.sys)

ani <- ani %>%
  mutate(mating.sys = as.double(mating.sys))

#histogram of mating system
#hist(mating.sys,xlab="Mating system")

colnames(ani)
mshist <- ani %>%
  filter( mating.sys <= 2000 ) %>%
  mutate(mating.sys = as.double(mating.sys)) %>%
  ggplot(aes(x = mating.sys))  %>% +
  geom_histogram( binwidth=1, fill="violet", color="green", alpha=0.9) +
  ggtitle("Histogram of mating system") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=20)
  )
mshist


ani %>%
  ggplot( aes(x=mating.sys, y=log.final.anisogamy, fill=as.factor(mating.sys))) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() + ylab("log of Anisogamy") + xlab("mating system: sed difference in polygamy score (male- female)")
view(ani)

#cor0
#view(ssdimo)

 #ani <- ani%>%
 # mutate(wing.ssd)
#ani <- ani%>%
 # mutate(wing.ssd = as.double(wing.ssd))

ani <- ani %>% 
  mutate(nest.bld = as.double(nest.bld))

ani <- ani %>%
  mutate(inc_2 = as.double(inc_2))
ani <- ani %>%
  mutate(chick.brd_2 = as.double(chick.brd_2))
ani <- ani %>%
  mutate(chick.feed_2 = as.double(chick.feed_2))
ani <- ani %>%
  mutate(chick.dfc_2 = as.double(chick.dfc_2))
ani <- ani %>%
   mutate(nest.grd_2 = as.double(nest.grd_2))
ani <- ani %>%
  mutate(postf.feed_2 = as.double(postf.feed_2))
ani <- ani %>%
  mutate(postf.grd_2 = as.double(postf.grd_2))
ani <- ani %>%
  mutate(mating.sys = as.double(mating.sys))
ani <- ani %>%
  mutate(final.anisogamy = Total_Length / final.egg.mass)
ani <- ani %>%
  mutate(log.final.anisogamy = log10(final.anisogamy))

view(ani)
colnames(ani)


ani <- ani %>% 
  mutate(final.anisogamy = as.double(final.anisogamy),
         log.final.anisogamy = log10(final.anisogamy),
         log.final.anisogamy = as.double(log.final.anisogamy))

colnames(egg_data)
colnames(ani)
view(ani)
  
view(egg_data)
colnames(egg_data)
ani [,6]
# mutate(lg_m_mass = log(m.mass),

#log(ani$m.mass)

#ani %>%
 # ani$lg.m.mass <- log10(m.mass)
#ani$lg_m_mass <- log10(m.mass)


colnames(egg_data)
view(ani)
  #mutate(l.anisog)= log10(anisogamy) 

#ani$l_anisogamy <- log(anisogamy)

#ani <- sperm_species_length %>% 
 # full_join(egg_mass_species,
  #          by = "species") %>% 
  #mutate(anisogamy = Total_Length / egg_mass)

egg_mass_species %>% colnames()
colnames(ani)

View(ani)

#* Z1 DIST OF ANISOGAMY
#extent of anisogamy




# how to turn axis
# + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

colnames(ani)

#ggplot(ani, aes(x=species, y = log.final.anisogamy)) + ggtitle("extent_aniso") + geom_point()

# sperm against egg = logyfied across orders

#coloured graph

ggplot(ani,aes(x= log_total_length, y=log.final.egg.mass, color=Order)) + 
  geom_point(size=6) +
  theme_ipsum()
colnames(ani)
view(ani)

ggplot(ani,aes(x= ssd, y= final.anisogamy, color= Order)) +
  geom_point(size=6) +
  theme_ipsum()

ggplot(ani,aes(x= ssd, y= log.final.anisogamy, color= Order)) +
  geom_point(size=6) +
  theme_ipsum() + ylab("Log of Anisogamy")

view(ani)


# SPERM AND EGG BOXPLOT

colnames(ani)

ani %>%
  ggplot( aes(x=log_total_length, y=log.final.egg.mass, fill=Order)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot of log egg/sperm across orders") +
  xlab("")

#with jitter
ani %>%
  ggplot( aes(x=log_total_length, y=log.final.egg.mass, fill=Order)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="purple", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Log sperm/egg boxplot with jitter") +
  xlab("")
  # regression plot total length against egg mass
  
model1 <- lm(log_total_length ~ log_egg_mass, data = ani)

#linear model of sperm ang egg

masslinear <- lm(log.final.egg.mass ~ log_total_length, data= ani )
masslinear
summary(masslinear)
plot(masslinear)

tab_model(masslinear)


view(ani)
colnames(ani)

summary(model1)
plot(model1)

spegg <- ani %>%
  select("Total_length", "egg.mass","species") 
colnames(ani)
View(spegg)

colnames(egg_data)
model0 <- lm(Total_Length ~ egg_mass, data = ani)
summary(model0)
plot(model0)

summary(model0.1)$coefficient
confint(model0.1)

model0.1 <- lm(log_total_length ~ log.final.egg.mass, data = ani)
summary(model0.1)
plot(model0.1)

colnames(ani)
colnames(Total_Length)
#make sure total length has capital L

ggplot(ani, aes(x=final.egg.mass, y =Total_Length)) + 
  geom_line() + ggtitle("egg against sperm") + geom_point()

#regression pt 2
model8 <-lm(log.final.egg.mass ~ log_total_length, data = ani)
summary(model8)
plot(model8)
summary(model8)$coefficient
confint(model8)

colnames(ani)

#scatter of log sperm and log egg
ggplot(ani, aes(x=log_total_length, y=log.final.egg.mass)) + xlab("log of sperm length (??m)") + ylab("Log of Egg mass (g)") + 
  geom_point() +
  geom_smooth(method="lm", se= FALSE)
  

corani <- cor.test(ani$log_total_length, ani$log.final.egg.mass)
corani
#high sig correlated signnifacant, pearsons r- negatively correlated -0.161 , law angle
summary(model1)$coefficient
species <- ani[]


view(ani)

ani[] %>% head()

#ani <- ani %>% 
  # mutate(anisogamy= log(anisogamy))
ani <- ani %>%
  mutate(log.final.egg.mass = log10(final.egg.mass))

view(ani)
#boxplot of log egg against orders
eggorder <- ggplot(ani, aes(x=as.factor(Order), y= log.final.egg.mass)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("order")

eggorder + geom_boxplot() +  theme(axis.text.x=element_text(angle=-90)) +
  ylab("Egg mass(g)")

#boxplot of legg sperm against orders 

spermboxplot <- ggplot(ani, aes(x=as.factor(Order), y= log_total_length)) + 
  geom_boxplot(fill="orange", alpha=0.7) + 
  xlab("order")

spermboxplot + geom_boxplot() +  theme(axis.text.x=element_text(angle=-90)) +
  ylab("Sperm length(??m)")


colnames(ani)
view(ani)
# Z2 ssd across species
  
ssd <- egg_data %>% 
  select("species", "ssd") %>%
mutate(ssd = as.double(ssd))

#ani %>%
 # group_by(species) %>%
  #mutate(c.species= n()) 

view (ani)

# change to numeric from charachter 

ssdi <- ani %>%
  filter( ssd <= 2000 ) %>%
  mutate(ssd = as.double(ssd)) %>%
  ggplot( aes(x = ssd))  %>% +
  geom_histogram( binwidth=0.05, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Histogram of SSD") +
  theme_ipsum() + (ylab("Number of species")) + xlab("SSD (Male mass(g)/ Female mass (g)")
ssdi
ani[,4] %>% class()

colnames(ani)
# basic histogram

#ani %>%
 # ani$ssd
#ssd$Title <- as.numeric(ssd$Title)
#?stat=count
head(ani)
view (ani)

#ani %>%
 # ggplot(ani,aes(ssd, fill=ssd)) + geom_histogram() + ggtitle ("histogram of ssd")


colnames (ani)
# assign value with name by x <- value

#  Z2.0log sperm length against log male mass and log total length
ggplot(ani, aes(x=log.m.mass, y=log_total_length)) + 
  geom_point() + ylab("log sperm length (??m)") + xlab("log male body mass(g)")

ani %>%
  

colnames(ani)

#z2.1 5male mass against female mass and log form

h <- ggplot(ani, aes(x=m.mass, y=f.mass)) + geom_point()
filtered_h <- ani %>%
  filter(m.mass <=15000 , f.mass <= 4000) 

ggplot(filtered_h, aes(x= m.mass, y = f.mass)) + geom_line() + ggtitle("filtered m.mass for values than 15000")

ggplot(ani, aes(x=log.m.mass, y=log.f.mass)) + 
  geom_point() + ylab("Log of female mass (g)") + xlab("Log of male mass (g)")

malesperm <- ggplot(ani, aes(x=m.mass, y=Total_Length)) + geom_point()

malesperm
filt_malesperm <- ani %>%
  filter(m.mass <=200 , Total_Length <= 300)

filt_malesperm
ggplot(filt_malesperm, aes(x= m.mass, y = Total_Length)) + geom_point()
view(ani)
# Z2.2 log female length against log female mass and normal
ggplot(ani, aes(x=f.mass, y=final.egg.mass)) + 
  geom_point()

ggplot(ani, aes(x=log.f.mass, y=log.final.egg.mass)) + 
  geom_point() + ylab("Log of egg mass(g)") + xlab("Log of Female body mass (g)")
colnames(ani)
# z2.3 anisogamy against SSD
#scatter of ssd and anisogamy
ggplot(ani, aes(x=ssd, y=log.final.anisogamy)) +
  geom_point() + ylab("log of anisogamy (sperm length (??m)/egg mass (g))") + xlab("SSD (male mass (g) / female mass(g))")

ggplot(ani, aes(x=ssd, y=final.anisogamy)) + 
  geom_point()
#ggplot(ani, aes(x=log10(ssd), y=log10(anisogamy))) + 
 # geom_point()
#linear model ssd
model2 <- lm(log.final.anisogamy ~ ssd, data = ani)
plot(model2)
summary(model2)
summary(model2)$coefficient

tab_model(model2)

plot(model2)
summary(model2)$coefficient


model2.1 <- lm(final.anisogamy ~ ssd, data = ani)

summary(model2.1)

plot(model2.1)
summary(model2.1)$coefficient


colnames(egg_data)

# Z3 MATING
# Famale polyandry score
head(ani)
female_poly_hist <- ani %>%
  filter( fpg.scr<=5 ) %>%
  mutate(fpg.scr = as.double(fpg.scr)) %>%  
  ggplot( aes(x=fpg.scr))  %>% +
  geom_histogram( binwidth=1, fill="pink", color="#e9ecef", alpha=0.9) +
  ggtitle("Histogram of female polyandry score") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=20)
  ) 
female_poly_hist




# male polygamy score

male_poly_hist <- ani %>%
  filter( mpg.scr<=5 ) %>%
  mutate(mpg.scr = as.double(mpg.scr)) %>%  
  ggplot( aes(x=mpg.scr))  %>% +
  geom_histogram( binwidth=1, fill="blue", color="#e9ecef", alpha=0.9) +
  ggtitle("Histogram of male polygamy score") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=20)
  ) 
male_poly_hist




#Male mating system- female mating system

mate_sys_hist <- ani %>% 
  filter( mating.sys <= 6 ) %>%
  mutate(mating.sys = as.double(mating.sys)) %>%  
  ggplot( aes(x=mating.sys))  %>% +
  geom_histogram( binwidth=1, fill="orange", color="#e9ecef", alpha=2) +
  ggtitle("Histogram of mating system") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=20)
  ) + ylab("Number of species") + xlab("mating system (sex difference in polygamy score (male - female)")
mate_sys_hist

ani %>%
  na.omit(mating.sys)


options(max.print = 999000)




# Represent it

# anisogamy against mating system  (mpg - fpg)
ggplot(ani, aes(x=final.anisogamy, y=mating.sys)) +
  geom_point()
ggplot(ani, aes(x=mating.sys, y=log.final.anisogamy)) +
  geom_point()

# box plot
ani %>%
  ggplot( aes(x=log.final.anisogamy, y=mating.sys, fill=log.final.anisogamy)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot of log ani against mating sys across orders") +
  xlab("")




# k2 omitting data
ani %>%
  na.omit(anisogamy)
ani %>%
  na.omit(final.anisogamy)
options(max.print = 999000)
ani %>%
  na.omit(log.final.anisogamy)
options(max.print = 999000)

ani%>%
 na.omit(chick.dfc_2)
ani %>%
  na.omit(chick.feed_2)
ani %>%
  na.omit(chick.brd_2)
ani %>%
  na.omit(inc_2)
ani %>%
  na.omit(nest.bld, nest.grd_2,postf.feed_2, postf.grd_2)
# bar plot

#write.table(ani, file = "ani.txt", sep = ",", quote = FALSE, row.names = F)
#write_xlsx(ani,"myfiles\Mr868 (H:)\fypdata\\mayfyptable.xlsx")

head(ani)
ani %>%
#cor test ssd and anisogamy 
cor3<-cor.test(ani$ssd, ani$log.final.anisogamy)
cor3


# Barplot
  
ani %>%
  ggplot(ani, aes(x=mating.sys, y=final.anisogamy)) +
  geom_bar(stat = "indentity")

ggplot(ani, aes(x=mating.sys, y=log.final.anisogamy)) + 
  geom_bar(stat = "identity", width=0.5) 
view(ani)

#ggplot(ani, aes(x=mating.sys, y=log.final.anisogamy)) + 
  #geom_bar(stat = "identity", width=0.5) 
#scatter
ggplot(ani, aes(x=mating.sys, y = final.anisogamy)) + ggtitle("anisogamy against mating system scatter") + geom_point()

ggplot(ani, aes(x=mating.sys, y = log.final.anisogamy)) + ggtitle("anisogamy against mating system scatter log") + geom_point()

# regression model of anisogamy against mating system

model3 <-lm(log.final.anisogamy ~ mating.sys, data = ani)
summary(model3)
plot(model3)
summary(model3)$coefficient
confint(model3)
view(ani)


# Z4PARENT

nest<-length(na.omit(ani$nest.bld)) 
incu<-length(na.omit(ani$inc_2)) 
nestgr<-length(na.omit(ani$nest.grd_2)) 
brood<-length(na.omit(ani$chick.brd_2)) 
feed<-length(na.omit(ani$chick.feed_2))
def<-length(na.omit(ani$chick.dfc_2)) 
postf<-length(na.omit(ani$postf.feed_2))

postfg<-length(na.omit(ani$postf.grd_2)) 

# histogram of parental care 1. female biased 2. biparental and 3. male biased
##Correlation between the mean for the 4 variables for which there is data for more than 1 000 species with the mean obtained for all care categories, for species for which we have data on all variables.   

setNames(c(nest,incu, nestgr, brood, feed, def, postf, postfg), c("NestBuilding","Incubation","NestGuarding","ChickBrooding","ChickFeeding","ChickDefence","PostFledgFeeding","PostFledgeGuarding"))

#pcare <-(ani,(nest.bld,inc_2,chick.brd_2,chick.feed_2,chick.dfc_2,nest.grd_2,postf.feed_2,postf.grd_2))
care<-with(ani, data.frame(species,nest.bld,inc_2,chick.brd_2,chick.feed_2,chick.dfc_2,nest.grd_2,postf.feed_2,postf.grd_2))
careALL<-na.omit(care)

#a p care wihtout speciies cos spp is not numeric
Pcare<-with(ani, data.frame (nest.bld,inc_2,chick.brd_2,chick.feed_2,chick.dfc_2,nest.grd_2,postf.feed_2,postf.grd_2))

care4<-with(careALL, data.frame(nest.bld, inc_2, chick.brd_2, chick.feed_2))
mean4<-rowMeans(care4)
meanA<-rowMeans(careALL[,2:9])
view(meanA)
#pc <- data.frame(nest.bld, inc_2)

hist(ani$meanCare, xlab = "Average relative investment of the sexes in parental care" , ylab= "number of species")
lm(ani)
#gcare <-nest.bld,inc_2,chick.brd_2,chick.feed_2,chick.dfc_2,nest.grd_2,postf.feed_2,postf.grd_2

#ani<- ani %>%
 # mutate(jcare = rowMeans(nest.bld,incu))
  #rowMeans(nest.bld,inc_2)
  
#KF <- data.frame(nest.bld)
  # 67pl mean and mean care

ani <-ani %>%
  mutate(Pcare = as.double(Pcare))

colnames(ani)

ani <- ani %>%
  mutate( parentcare = rowMeans(chick.brd_2, chick.dfc_2, chick.feed_2))

colnames(care)
view(care)
ani <- ani %>%
  mutate(meanA = as.double(meanA))

ani <- ani %>%
  mutate(meanCare = as.double(meanCare))

ani <- ani %>%
  mutate(pl.mean3 = as.double(pl.mean3))
careALL[,9] %>% class() 
colnames(ani)
view(ani)

#correlation test between the means
corel<-cor.test(mean4, meanA)
corel
#t=12.12 df=49 p val: < 2.2e-16

#use the mean of all variables to represent male parental investment.

meanCare<-rowMeans(care[,2:9], na.rm=TRUE)
meanCare<-meanCare-2
mean(meanCare,na.rm=TRUE)

colnames(ani)

#+ve values indicate conventional sex roles and negative values reversed sex roles

meanCareN<--meanCare
mean(meanCareN,na.rm=TRUE)
Histparent <- hist(meanCareN)
plot(Histparent + (xlab="Mean of Parental care (average relative investment of the sexes in parental care"), ylab = "number of species", col = ("gray"))
plot(Histparent, xlab = "Mean of Parental care (average relative investment of the sexes in parental care)", ylab = "Number of species", col = "gray")

c4 = c("-2","-1","0","1","2")
histcareani<- ggplot(ani, aes(x= meanCareN, y=final.anisogamy)) +
  geom_bar(stat = "identity", width=1 , fill = "black") + xlab("Mean of Parental Care") +
  ylab("Anisogamy(Sperm length (??m) / Egg mass (g)) ")
histcareani
view(ani)



plot(histcareani,xlab="Mean of Parental care", ylab = "Anisogamy (Sperm length (??m)/Egg mass (g))")


ani %>%
  ggplot( aes(x=meanCareN, y=log.final.anisogamy, fill= Order)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot of log ani against parent care mean across orders") +
  xlab("")



ggplot(ani, aes(x=meanCareN, y=log.final.anisogamy)) +
  geom_point(aes(color = passeriformes)) + ylab("Anisogamy (Sperm length (??m)/ Egg mass(g)") + 
  xlab("Mean of Parental Care")

ggplot(ani, aes(x=meanCareN, y=final.anisogamy)) +
  geom_point(aes(color = passeriformes)) + ylab("Log of Anisogamy (Sperm length (µm)/ Egg mass(g)") + 
  xlab("Mean of Parental Care")

ggplot(ani, aes(x=pl.mean3, y=log.final.anisogamy)) +
  geom_point(aes(color = passeriformes)) + ylab("Log of Anisogamy (Sperm length (??m)/ Egg mass(g)") + 
  xlab("Mean of Plumage dimorphism")

# scatterplot by order , mating sys and anisogamy

ani["passeriformes"]=ani["Order"]== "Passeriformes"

ggplot(ani, aes(x = mating.sys, y = log.final.anisogamy)) +
  geom_point(aes(color = passeriformes)) + ylab("Log of Anisogamy") +
  xlab("Mating system")
view(ani)
ani %>%
  mutate()

colnames(ani)
#care <- ani %>% select(final.anisogamy) %>%
 # mutate(final.anisogamy = as.double(final.anisogamy)
  #       )
view(care)

ggplot(ani, aes(meanCareN,final.anisogamy))

care %>%
  mutate(ani, )

view(care)
# regression for parental
model4 <-lm(final.anisogamy ~ meanCareN, data = ani)
summary(model4)
plot(model4)

summary(model4)$coefficient
confint(model4)

model5 <- lm(log.final.anisogamy ~ meanCareN, data = ani)
summary(model5)
plot(model5)
summary(model5)$coefficient
coffint(model5)

ssd[,2] %>% class()

colnames(ani)
ssd %>%
  mutate(species=as.double(species))
ani %>%
  as.double(nest.bld,inc_2, chick.brd_2, chick.feed_2)
ani %>%           
  as.numeric(nest.bld)

colnames(careALL)

#box plot
ani %>%
  ggplot( aes(x=log.final.anisogamy, y=meanCareN, fill=Order)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot of log ani against mean parental care  across orders") +
  xlab("")

# k1parental care to numbers
ani <- ani %>%
  mutate(nest.bld = as.double(nest.bld))
ani <- ani %>%
  mutate(inc_2 = as.double(inc_2))
ani <- ani %>%
  mutate(nestgr = as.double(nestgr))
ani <- ani %>%
  mutate(chick.dfc_2 = as.double(chick.dfc_2))
ani <- ani %>%
  mutate(chick.feed_2 = as.double(chick.feed_2))
ani <- ani %>%
  mutate(chick.brd_2 = as.double(chick.brd_2))
ani <- ani %>%
  mutate(postf = as.double(postf))
ani <- ani %>%
  mutate(postfg = as.double(postfg))
# numeric for sexual dichromatism
colnames(orna)
ani <- ani %>%
  mutate(pl.head = as.double(pl.head))
ani <- ani %>%
  mutate(pl.back = as.double(pl.back))

ani <- ani %>%
  mutate(pl.belly = as.double(pl.belly))
ani <- ani %>%
  mutate(pl.wings = as.double(pl.wings))
ani <- ani %>%
  mutate(pl.tail = as.double(pl.tail))

colnames(orna)



#careK <- na.omit(nest.bld, inc_2)

view(ani)
colnames(ani)
#p1anisogamy against parental care? 


colnames(ani)

print(rowMeans(care, na.rm = TRUE)) 
carefromparent < -with(ani, data.frame(nest.bld,inc_2,chick.brd_2,chick.feed_2,chick.dfc_2,nest.grd_2,postf.feed_2,postf.grd_2))

careALLparent<-na.omit(carefromparent)
# a dataframe for parental care and the row means column 

PC <- with(ani, data.frame(species, nest.bld, inc_2, chick.brd_2, chick.feed_2, chick.dfc_2, nest.grd_2, postf.feed_2, postf.grd_2))

omittedPC <- na.omit(PC)

print(rowMeans(omittedPC, na.rm = TRUE)) 
view(omittedPC)
view(PC)
RM<-rowMeans(omittedPC[,2:9])

#adding a row means column , can do similar for SSD

omittedPC$Rmean <- rowMeans(omittedPC [,2:9])

view(omittedPC)

omittedPC$final.anisogamy <- select(ani, final.anisogamy)

# trying ot create a dataframe of unequal
omittedPC$new.final.anisogamy <- 
OmmitedPC
colnames(ani)
ggplot()
ggplot(ani,aes(x=care, y=final.anisogamy)) + 
  geom_point() 

colnames(care)

ggplot(ani, aes(x= nest.bld, y=final.anisogamy)) + 
  geom_bar(stat = "identity", width=0.5) 
colnames(ani)  

# Z5 ORNAMENTATION
#looking at plumage. graph of ornamentation score freq dist histo. 
# anisogamy against orna?

# making a table for ornamentation
orna <- ani %>%
  select("pl.head", "pl.back", "pl.belly", "pl.wings", "pl.tail","log.final.anisogamy", "Order") 

ornament <- cbind(orna$pl.head, orna$pl.back, orna$pl.belly, orna$pl.wings, orna$pl.tail)

orna$pl.mean3 <- rowMeans(ornament, na.rm = TRUE)
  
?cbind
colnames(ani)
View(orna)
orna[,4] %>% class()

#orna <- ani %>% select(orna) %>%
 # mutate(pl.mean3 = as.double(pl.mean3))

#ani <- orna %>% 
 # select("pl.mean3") %>%
  #mutate(pl.mean3 = as.double(pl.mean3))
view(ani)
colnames(ani)
#Histogram of distribution of sexual dichromatism values:

hist(orna$pl.mean3, xlab="Plumage dimorphism", ylab = "Number of species", main="")

plot(ani$pl.mean3, ani$log.final.anisogamy, xlab = "Mean of Plumage dimorphism",
     ylab = "Log of Anisogamy: Log (Sperm length (??m)/ Egg mass (g)) ")
abline(lm(ani$log.final.anisogamy~ani$pl.mean3), col="red")

#regression 
model6 <-lm(log.final.anisogamy ~ pl.mean3, data = ani)
summary(model6)
plot(model6)
summary(model6)$coefficient
confint(model6)

#scatter
colnames(orna)
#boxplot log ani against orna
orna %>%
  ggplot( aes(x=log.final.anisogamy, y=pl.mean3, fill=Order)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot of log ani against plumage mean across orders") +
  xlab("")

# scatter of orna agaisnt ani
ggplot(orna, aes(x=log.final.anisogamy, y=pl.mean3)) +
  geom_point()

 
  # select is adding a column you already have and mutate is adding a column you want to make a new of like sum of
  
# allometry in SSD
  
egg_data %>% colnames()
  
  view(egg_data)
sperm_data %>% View()
colnames(sperm_data)
df=data.frame(phylo="species")

view(ani)
  

# seeing how many rows after cancelling out NA
ani %>% filter(!is.na(anisogamy)) %>% nrow()
ani %>% filter(!is.na(ssd)) %>% nrow()
ani %>% filter(!is.na(mating.sys)) %>% nrow()
ani %>% filter(!is.na(pl.head)) %>% nrow()
ani %>% filter(!is.na(final.anisogamy)) %>% nrow()

ani%>% colnames()

ani <- data.frame(ani)
ani %>% colnames()
anisogamy <- ani

anisogamy_data <- ani %>% select(anisogamy) %>%
  mutate(anisogamy = as.double(anisogamy))


#View(anisogamy_data)
#log(anisogamy_data)
#options(max.print=9999) 


#ani_data_plot <- anisogamy_data %>% 
#  group_by(anisogamy) %>%
#  species %>% 
#  filter(!is.na(anisogamy))

View(ani_data_plot)

#extent of anisogamy

#ggplot(ani, aes(x=species, y = log(anisogamy))) + 
 # geom_line() + ggtitle("extent_ani") + geom_point()

ggplot(ani, aes(x=species, y =log.final.anisogamy)) + ggtitle("extent_ani") + geom_point()

# regression plot

model1 <- lm(log_total_length ~ log_egg_mass, data = ani)

newregression <- lm(log.final.egg.mass ~ log_total_length, data=ani)
newregression
summary(model1)
model1
plot(model1)

# funelling which mean its heteroscidastic, pr sigmificant , adj compare other
# r square small, resideual erros . fitted - as fitted value icmrease error imcreases changung variance

#violates assumption of homoscedascity . 2normality asumption
#less of then homo  loop at end so skew 
#sqaure root: help acess homoscedacity further
#upward slop end line- violate asumption same as first graphh, want line to be
#flat not curved. cooks: about outliers
#cooks: doted line near begining. cooks distance for eevery point, lines marked in it
#heteroscedascity first - norm might ssort out
#homo sequal . seeing hetero not equal , pronounced. lm make regressio violzting assumption of regression 
#skew variable , not both of them not skewed in same way and log might be responsibelf or skewing




#scatter of log sperm and log egg
#ggplot(ani, aes(x=log_total_length, y=log_egg_mass)) + 
 # geom_point()

#summary(model1)$coefficient

#write_xlsx(ani,"This PC\Documents\\anis.xlsx")

species <- ani[]


view(ani)

ani[] %>% head()


#x6 all means
pl.mean3<-orna$pl.mean/sd(na.omit(orna$pl.mean))

SSDMean<-ssdMean$ssdM/sd(na.omit(ssdMean$ssdM))

Mating.sys<-mating.sys/sd(na.omit(mating.sys))

MeanCare1<-meanCareN/sd(na.omit(meanCareN))

SexroleSD<-as.data.frame(cbind(SSDMean, pl.mean2, Mating.sys,MeanCare1))
SexroleSD<-cbind(dat$species, SexroleSD)
colnames(SexroleSD)<-c("species","mean.ssd","pl.mean","mating.sys","care")

# r syntax matching dataframe with common column , oringal data frame and the datframe you get from tree org match these two using species name => speciies that exist both from data dn website
# dataframe with 4 compoenents of sex role 
Sexrole<-as.data.frame(cbind(ssdMean, dat$pl.mean, mating.sys, meanCareN), stringsAsFactors=FALSE)
colnames(Sexrole)<-c("species", "ssd", "dichro", "mating.sys", "care")
# stack overflow, r cookbook
 # mutate(anisogamy= log(anisogamy))

ani_2 <- ani %>% filter(!is.na(final.anisogamy)) %>% distinct()


View(ani_2)  
colnames(ani_2)

colnames(omittedPC)

ani_2 %>%
  mutate(meanCareN= as.double(meanCareN))

ani_3 <- ani_2 %>%
  full_join(omittedPC %>%
              select("Rmean", "species") %>%
              mutate(Rmean = as.double(Rmean)),
            by = "species")
View(ani_3)

ani_2 %>% colnames()
colnames(omittedPC)

ani_2 %>% nrow()

#left_join 
ani_4 <- ani_2 %>%
  left_join(omittedPC %>%
              select("Rmean", "species") %>%
              mutate(Rmean = as.double(Rmean)),
            by = "species") %>% 
  distinct()

ani_4 %>% nrow()
omittedPC %>% View()

write_xlsx(ani_2,"H:\\fyp data\\anitwo.xlsx")

#taking the mins of each total length 
ani_min <- ani_2 %>% group_by(species) %>% 
  filter(Total_Length == min(Total_Length))

view(ani_min)
view(ani_max)

ani_max <- ani_2 %>% group_by(species) %>%
  filter(Total_Length == max(Total_Length))

write_xlsx(ani_min,"H:\\fyp data\\anidatamin.xlsx" )

write_xlsx(ani_max,"H:\\fyp data\\anidatamax.xlsx" )

#anii <- read.table(text="Total_length", header = TRUE)

#keys <- colnames(ani_2)[!grepl('Total_length',colnames(ani_2))]
#X <- as.data.table(ani_2)
#X[,list(mm= mean(Total_length)),keys]
#view(ani_2)

hist(ani$log.final.anisogamy,xlab="Log of Anisogamy( Sperm length (??m)/ Egg mass (g)")
hist(ani$log.final.anisogamy)
hist(ani$mating.sys)

hist(ani$log.final.anisogamy,xlab = "log of Anisogamy (Sperm length (??m)/ Egg mass (g))", ylab = "Number of species")

ani_min$Order <- as.factor(ani_min$Order)

modellinear<- lm(log.final.anisogamy ~ ssd + mating.sys + meanCareN +pl.mean3, data= ani)
summary(modellinear)
qqnorm(resid(modellinear))
hist(resid(modellinear), xlab = "residuals")
tab_model(modellinear)

#mixed effects model | 
ani$Order <- as.factor(ani$Order)
modellinear2<- lm(log.final.anisogamy ~ mating.sys + meanCare + ssd +pl.mean3 + relevel(ani$Order, ref= "Passeriformes"), data= ani)
summary(modellinear2)
vif(modellinear2)

modellinear2
tab_model(modellinear2)
colnames(ani_min)

modelfinal <- lmer(log.final.anisogamy ~ mating.sys + meanCare + ssd + pl.mean3 +
                     (1|Order), data = ani)
summary(modelfinal)
vif(modelfinal)
tab_model(modelfinal)
# make another varibale 
boxplot(ani$log.final.anisogamy ~ ani$Order)

boxssd<- ggplot(ani, aes(Order, ssd))
boxssd + geom_boxplot() +  theme(axis.text.x=element_text(angle=-90)) +  ylab("SSD (male mass/ female mass)")

boxmating <- ggplot(ani, aes(log.final.anisogamy, mating.sys))
boxmating + geom_boxplot()



twoboxplot<- ggplot(ani, aes(Order, log.final.anisogamy))
twoboxplot + geom_boxplot() +  theme(axis.text.x=element_text(angle=-90)) + ylab("Log of Anisogamy")

boxofcare <- ggplot(ani, aes(Order, meanCareN))
boxofcare + geom_boxplot() +  theme(axis.text.x=element_text(angle=-90)) + ylab("Mean of Parental Care")

#rotating labels
twoboxplot

corssd <- cor.test(ani$ssd, ani$log.final.anisogamy, 
                method = "pearson")
corssd
corcare <- cor.test(ani$log.final.anisogamy, ani$meanCare, 
                   method = "pearson")
corcare

corplmean <- cor.test(ani$log.final.anisogamy, ani$pl.mean3, 
                    method = "pearson")
corplmean
shapiro.test(ani$ssd)
hist(ani$ssd)
shapiro.test(ani$mating.sys)
shapiro.test(ani$log.final.anisogamy)
hist(ani$log.final.anisogamy)
hist(ani$pl.mean3)

corrssd <- cor.test(x=ani$ssd, y=ani$log.final.anisogamy, method = 'spearman')
corrssd

corpass <-cor.test(x= ani$meanCareN, y= ani$log.final.anisogamy, method='pearson')

corpass
shapiro.test(ani$log.final.anisogamy)
summary(ani$mating.sys - model3$fitted.values)

fakemodel <- lm(log.final.anisogamy ~ meanCare, data= ani)

fakemodel
summary(fakemodel)

hist(ani$meanCare)
hist(ani$meanCare)
modelrealcare <- lm(log.final.anisogamy ~ careALL, data = ani)


sperm <- c(1,2,3,4,5)
sperm  
an <- sperm/10
log10(an)
# the less difference between egg and sperm size (e.g 5) the lower thenegative log value will be
# the more difference between egg and sperm (e.g 1) the higher up the log scale it will be 
#more down less difference 

#distinct (1/10) - more down log - bigger log - (-1)
#same (5/10) - higher up it will go  (-0.3) closer to 0 

#- up here OO SAME O O
#- down here o 0  MORE DIFFERENT

ggplot(ani, aes(x=ssd, y=final.anisogamy)) +
  geom_point(aes(color = passeriformes)) + ylab("Log of Anisogamy (Sperm length (µm)/ Egg mass(g)") + 
  xlab("Mean of Parental Care")
an

hist(ani$pl.mean3)

