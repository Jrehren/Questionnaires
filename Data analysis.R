################################################################################
############################# SETTING THE SCENE ################################
################################################################################

# ------------------------------ EXPLANATION  ----------------------------------
# This script is used to analyse the questionnaires and create an estimate
# of B/Bunfished for as many functional groups as possible. 
# 
# ------------------------------------------------------------------------------

# --------------------------- LOAD PACKAGES AND FUNCTIONS ----------------------
# 1.) CLEAR SCREEN AND SET WORKING DIRECTORY ----
ls()
rm(list=ls())

# 2.) LOAD PACKAGES ----
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(parallel)
library(stringr)
library(ggthemr)

# Let us get the colors from the ggthemr package
light <- c("#785d37", "#62bba5", "#ffb84d", "#aaa488", "#b2432f", "#3a6589", 
           "#9b5672", "#908150", "#373634")
pale <- c("#444444", "#de6757", "#EB9050", "#3262AB", "#FF8D7D", "#C8E370",
          "#C45B4D", "#41a65c", "#5E2C25", "#78695F")
fresh <- c("#111111", "#65ADC2", "#233B43", "#E84646", "#C29365", "#362C21", 
           "#316675", "#168E7F", "#109B37")
solarized <- c("#073642", "#268bd2", "#dc322f", "#2aa198", "#b58900", "#6c71c4",
               "#d33682")
dust <- c("#555555", "#db735c", "#EFA86E", "#9A8A76", "#F3C57B", "#7A6752",
          "#2A91A2", "#87F28A", "#6EDCEF")


# 3.) SOURCES FUNCTIONS ----
swatch_pal <- function() {
  function(n) {
    if(n > length(swatch()) - 1) warning(paste("Swatch only has", length(swatch())-1), " colours")
    color_list <- as.character(swatch()[2:length(swatch())])
    return(color_list[1:n])
  }
}

scale_colour_ggthemr_d <- function(...) {
  ggplot2::discrete_scale(
    "colour", "ggthemr_swatch_color",
    swatch_pal(),
    ...
  )
}

scale_color_ggthemr_d <- scale_colour_ggthemr_d

# ------------------------------------------------------------------------------
################################################################################
############################### DATA ANALYSIS ##################################
################################################################################
# 1.) LOADING THE DATA ----

# This is the information about the demographics and fishing behaviour of each 
# participant
pDat <- read.table("Data/csv files/Information_participants.csv", sep=";", 
                   header=T, skip = 1, na.strings=c(""," ","NA"))
str(pDat)

# This file contains information on which of the listed species/groups is caught
# often, occassionally and not at all
spL <- read.table("Data/csv files/Current target species.csv", sep=";", header = T)
str(spL)

# This file is the current and historical total catches of each participant
totC <- read.table("Data/csv files/Total current and historical catch.csv", 
                   sep=";", header=T)
str(totC)

spC <- read.table("Data/csv files/Current and historical catch per species.csv", 
                  sep=";", header=T)
str(spC)

# Now let us load the original data used to set the sample size 

sampEst <- read.table("Data/csv files/Estimation of sample size.csv", 
                      sep=";", header=T)

# Some Checks:
# (1) Do we have the same Id's in all data files?

u <- unique(pDat$Id);length(u)
u1 <- unique(spL$Id);length(u1)
u2 <- unique(spC$Id);length(u2)
u3 <- unique(totC$Id);length(u3)

u == u1; which(u!=u1)
u == u2; which(u!=u2)
u == u3; which(u!=u3)

# (2) Is the spelling of the species the same in all data sets?

us <- unique(spL$Species);length(us)
us

us1 <- unique(spC$Species);length(us1)
us2 <- unique(sampEst$Species);length(us2)

setdiff(us1, us)

setdiff(us2, us)

# (3) What about the levels of the variables in spC?

unique(spC$Time)
unique(spC$Gear)
unique(spC$Unit)


#-------------------------------------------------------------------------------
# 2.) DATA DESCRIPTION ----
 ## 2.1.) EXPERIENCE OF FISHER ----

table(pDat$Years_fishing)

ggplot(pDat, aes(x=Years_fishing)) + 
  geom_histogram(binwidth = 5)

# all fisher with more than 10 years experience

pDatExp <- subset(pDat, pDat$Years_fishing>=15)
length(pDatExp$Number_participant)

ggplot(pDatExp, aes(x=Years_fishing))+
  geom_bar()

# bins <- c(-Inf, 10000, 31000, Inf)
# > students$Income.cat1 <- cut(students$Income, breaks = b)
pDatExp$experience_bins <- cut(pDatExp$Years_fishing, breaks=10)

ggplot(pDatExp, aes(x=experience_bins))+
  geom_bar()+
  scale_y_continuous(breaks=seq(1,13,1))

idExp <- pDatExp$Id

 ## 2.2.) PERCENTAGE OF GEARS INTERVIEWED ----
# Ideally, I wanted to ask the following number of fisher based on my prior
# information on which species they catch:

# Handline	 8 
# Dragnet   10
# Spear	     8
# Trap	    10
# Gillnet	   5
# Longline	 5
# MigSpear	 5
# Miguu	     5
# Fence	     1
# Floatnet	 5

ggplot(pDatExp, aes(x=Main_gear)) + 
  geom_histogram()

table(pDatExp$Main_gear)

# Missing

# FootFisher  4
# Floatnet    3



 ## 2.3.) NUMBERS OF QUESTIONNAIRES PER SPECIES/GROUP ----
idExp
spCExp <- subset(spC, spC$Id %in% idExp)
tail(spCExp)
table(spCExp$Species)

SpNum <- spCExp %>% 
  filter(Time=="Aliopanza") %>% 
  count(Species)

# Let us join the estimated samples sizes (keeping the column Kategory)

sampEstLong <- sampEst %>% 
  gather(key="Gear", value="Percentage", Dragnet:FootFisher) %>% 
  select(Species, Category, Gear, Percentage)


# Now we rearrange and group the information about the different species obtained
# by the surveys
spCExpGr <- spCExp %>% 
  filter(Time=="Aliopanza") %>% 
  select(Species, Gear) %>% 
  group_by(Species) %>% 
  count(Gear)

# now we join them
x <- sampEstLong %>%  
  full_join(spCExpGr, by = c("Gear", "Species"))
colSums(is.na(x))
# Let us remove all entries with NA and percentage below 5 % 

# x <- x1[!(x1$Percentage<0.01 & is.na(x1$n)),]

   ### A.) KEY SPECIES -----

# First we check if the target species have at least n=5

keySpNam <- c("Siganus_sutor", "Leptoscarus_vaigiensis", "Scarus_ghobban", 
           "Lutjanus_fulviflamma", "Lethrinus_lentjan", "Lethrinus_borbonicus")

subset(SpNum, SpNum$Species %in% keySpNam)

# No! Leptoscarus vaigiensis has only an n of 4. 
# In general Leptoscarus_vaigiensis and Lutjanus fulviflamma have both not enough
# it would be better to have an n=10 for the key species!

# Also let us check if the sample for each species is representative of the 
# main gears

xkeySp <- subset(x, x$Species %in% keySpNam)


ggplot(xkeySp)+
  geom_bar(aes(x=Gear, y=as.numeric(as.numeric(n))), 
           stat="identity")+
  geom_text(aes(x=Gear,y=6,label=Percentage))+
  scale_y_continuous(breaks=seq(1,12,1))+
  facet_wrap(~Species)+
  coord_flip()
  
# Seems like Dragnet and Spear fisher should be asked about Leptoscarus vaigiensis
# and more dragnet fisher should be asked for Lutjanus fulviflamma.
    
    ### B.) INVERTEBRATES ----

inv <- subset(x, x$Species %in% c("Octopus", "Squids", "Crabs_Lobsters", "Crabs",
                                  "Lobsters", "Snails"))

ggplot(inv)+
  geom_bar(aes(x=Gear, y=as.numeric(as.numeric(n))), 
           stat="identity")+
  geom_text(aes(x=Gear,y=6,label=Percentage))+
  scale_y_continuous(breaks=seq(1,11,1))+
  facet_wrap(~Species)+
  coord_flip()

# All good except for Snails


    ### C.) PELAGICS ----
pelNam <- c("Belonidae", "Chirocentridae", "Sphyraenidae")

pel <- subset(x, x$Species %in% pelNam)

ggplot(pel, aes(x=Gear))+
  geom_bar(aes(x=Gear, y=as.numeric(as.numeric(n))), 
           stat="identity")+
  geom_text(aes(x=Gear,y=6,label=Percentage))+
  scale_y_continuous(breaks=seq(1,8,1))+
  facet_wrap(~Species)+
  coord_flip()


# The question is, if I could group the sample instead of treating each group/
# species as seperate samples with seperate sample size.
# I have to check the other species that I did not list, but they reported on

    ### D.) ZOOPLANKTIVORE ----
zooNam <- c("Siganus_stellatus", "Caesionidae", "Sardinella_spp") 

zoo <- subset(x, x$Species %in% zooNam)

ggplot(zoo, aes(x=Gear))+
  geom_bar(aes(x=Gear, y=as.numeric(as.numeric(n))), 
           stat="identity")+
  geom_text(aes(x=Gear,y=6,label=Percentage))+
  scale_y_continuous(breaks=seq(1,8,1))+
  facet_wrap(~Species)+
  coord_flip()

# Nothing there :-(

    ### E.) HERBIVORE ----
herbNam <- c("Acanthuridae", "Hipposcarus_harid", "Calatomus_carolinus") 

herb <- subset(x, x$Species %in% herbNam)

ggplot(herb, aes(x=Gear))+
  geom_bar(aes(x=Gear, y=as.numeric(as.numeric(n))), 
           stat="identity")+
  geom_text(aes(x=Gear,y=6,label=Percentage))+
  scale_y_continuous(breaks=seq(1,8,1))+
  facet_wrap(~Species)+
  coord_flip()


# That works!


    ### F.) OMNIVORE ----
omNam <- c("Balistidae", "Mugilidae", "Monodactylus_argentus") 

om <- subset(x, x$Species %in% omNam)

ggplot(om, aes(x=Gear))+
  geom_bar(aes(x=Gear, y=as.numeric(as.numeric(n))), 
           stat="identity")+
  geom_text(aes(x=Gear,y=6,label=Percentage))+
  scale_y_continuous(breaks=seq(1,8,1))+
  facet_wrap(~Species)+
  coord_flip()


    ### G.) FISH & INVERTEBRATE FEEDER ----
carniNam <- c("Lethrinus_harak", "Lethrinus_variegatus", "Lethrinus_mahsena",
              "Serranidae", "Plectorhinchus_gibbus", "Rhinobatidae", "Mullidae",
              "Anguilliformes", "Cheilinus_trilobatus") 

carni <- subset(x, x$Species %in% carniNam)

ggplot(carni, aes(x=Gear))+
  geom_bar(aes(x=Gear, y=as.numeric(as.numeric(n))), 
           stat="identity")+
  geom_text(aes(x=Gear,y=6,label=Percentage))+
  scale_y_continuous(breaks=seq(1,11,1))+
  facet_wrap(~Species)+
  coord_flip()

# Also good!

#-------------------------------------------------------------------------------
# 3.) ESTIMATING B/Bunfished ----
 ## 3.1.) STEP 1: Extract expert fishers ----
# Here, we want to extract all the fishers that have an experience of more than 
# 15 years. So that the ultimate comparison will be at least between 2014 and 2006

pDatExp <- subset(pDat, pDat$Years_fishing>=15)
idExp <- pDatExp$Id

spCExp <- subset(spC, spC$Id %in% idExp)

totCExp <- subset(totC, totC$Id %in% idExp)

 ## 3.2.) STEP 2: Is the fisher always fishing in Chwaka village? ----

table(totCExp$Chwaka)

# All participants report that they fished in Chwaka 
 
 ## 3.3.) STEP 3: Did the fishing hours change over time? ----

# Which fisher changed fishing effort over time?
totCExpTimeL <- split(totCExp, totCExp$Time) # splitting the df by time for better
                                             # handling

# changes between Sasa (2021) and Aliopanza (When fisher started fishing)
notEqual <- totCExpTimeL$Aliopanza[which(totCExpTimeL$Aliopanza$Duration_fishing_trip!=totCExpTimeL$Sasa$Duration_fishing_trip),]
notEqualId <- notEqual$Id # id's

# Now we calculate a ratio for each of these fishers 
totCExpNotequal <- subset(totCExp, totCExp$Id %in% notEqualId & 
                            totCExp$Time!="Kati_kati")
totCExpNotequal[,c(1:4,7)]
totCExpNotequalL <- split(totCExpNotequal, totCExpNotequal$Time)

# Here, we use the ratio between fishing effort in 2021 (Sasa) and
# when the fisher started (aliopanza)

cAdjust <- as.numeric(totCExpNotequalL$Sasa$Duration_fishing_trip)/
  as.numeric(totCExpNotequalL$Aliopanza$Duration_fishing_trip)

# We add the id's again to match with the df in question later on

dfAdjust <- data.frame(Id=notEqualId, Effort_change=cAdjust) 

# Now we add another column for effort change in our df of interest

spCExp$Effort_change <- 1
# now we adjust the ones with the change in effort:

spCExp$Effort_change <- dfAdjust$Effort_change[match(spCExp$Id,dfAdjust$Id)]
unique(spCExp$Effort_change)

spCExp$Effort_change[is.na(spCExp$Effort_change)] <- 1

spCExp$Effort_change[spCExp$Time=="Aliopanza"] <- 1
spCExp$Effort_change[spCExp$Time=="Kati_kati"] <- 1


 ## 3.4.) STEP 4: Does the fisher report on individual catch or on collective catch? ----

dfKladde <- pDatExp[,c(1,22,23)]
dfKladde$Number_fisher <- 1

ind <- which(dfKladde$Individual_catch =="no" & !(is.na(dfKladde$Individual_catch)))

dfKladde$Number_fisher[ind] <- dfKladde$Fisher_number[ind]


dfKladde <- dfKladde[,c(1,3,4)]

# and now we add this to our df of interest
names(spCExp)
spCExp$Number_fisher <- 1

test <- dfKladde[,c("Id", "Number_fisher")]
spCExp$Number_fisher <- test$Number_fisher[match(spCExp$Id,test$Id)]
unique(spCExp$Number_fisher)


 ## 3.5.) STEP 5: Does the fisher use the same gear in Sasa and Aliopanza? ----

spCExpL <- split(spCExp, spCExp$Time)

which(spCExpL$Aliopanza$Gear!=spCExpL$Sasa$Gear)

spCExpL$Aliopanza[82:92,]

# Only the entry NS0213U2 seems to change, but this is simply because Nuru
# forgot/did not fill the gear for aliopanza. I will ask her about it. 

 ## 3.6.) STEP 6: Does the Unit reported by fisher change between the Time?

ind2 <- which(spCExpL$Aliopanza$Unit!=spCExpL$Sasa$Unit)

spCExpL$Aliopanza[ind2, 1:7]

# Yes, they change for 8 participants. However, it might be just a mistake by
# the interviewer. I will ask

 ## 3.6.) STEP 6: Has the fisher been fishing all time in Chwaka? ----

ind3 <- which(pDatExp$Years_fishing!=pDatExp$Years_fishing_Chwaka)
pDatExp[c(ind3),]

# So I should be using years fished in Chwaka and not years fishing. 

 ## 3.7.) STEP 7: Remove kati_kati ----

spCExp2 <- subset(spCExp, spCExp$Time!="Kati_kati")
head(spCExp2)
 ## 3.8.) STEP 8: Correct the catch for effort change and fisher number ----
str(spCExp2)
spCExp2$Catch <- as.numeric(spCExp2$Catch)

spCExp2$Catch_corrected <- (spCExp2$Catch/spCExp2$Effort_change)/spCExp2$Number_fisher
spCExp2
 ## 3.9.) STEP 9: Calculate the ratio between Sasa and Aliopanza catch ----

head(spCExp2, 20)

# I will create a new df as I am not sure how to do it within the same one

spCExp2L <- split(spCExp2, spCExp2$Time)

spCExpFin <- spCExp2L$Sasa

spCExpFin$Ratio <- spCExp2L$Sasa$Catch_corrected/spCExp2L$Aliopanza$Catch_corrected
spCExpFin[c(85:97),]

# we add back the aliopanza catch and the fishing years

spCExpFin$Catch_aliopanza <- spCExp2L$Aliopanza$Catch_corrected
spCExpFin$Years_fishing <- 2021-spCExp2L$Aliopanza$Time_year
names(spCExpFin)
spCExpFin2 <- spCExpFin[c(1:11, 13, 14, 12)]

 ## 3.10.) STEP 10: Calculate the Median for the ratio of each species  ----

spCExpFin2 <- spCExpFin2 %>% 
  group_by(Species) %>% 
  mutate(Ratio_median=median(Ratio, na.rm = T))

head(as.data.frame(spCExpFin2), 20)

# Let us check for which participant the catch was higher in Aliopanza:

ind5 <- which(spCExpFin2$Ratio>1)

as.data.frame(spCExpFin2[ind5,])
spCExp2L$Aliopanza[ind5,]

# Let us check how much they fished kati_kati
ids <- spCExpFin2$Id[ind5]
subset(spCExp, spCExp$Id %in% ids)

# We remove them for the time being:

spCExpFin3 <- spCExpFin2[-ind5,]

# 4.) VISUALIZING RESULTS ----
 ## 4.1.) KEY SPECIES ----
# Let us add the fishing years to the data set and plot the ratios over fishing
# years
keySpNam <- c("Siganus_sutor", "Leptoscarus_vagiensis", "Lethrinus_lentjan", 
              "Lethrinus_borbonicus", "Lutjanus_fulviflamma", "Scarus_ghobban")
keySp <- subset(spCExpFin3, spCExpFin3$Species %in% keySpNam)

# medSp <- unique(keySp$Ratio_median) 

keySp <- as.data.frame(keySp)
names(keySp)
ggplot(keySp)+
  geom_point(aes(x=Years_fishing, y=Ratio))+
  geom_line(data= keySp, aes(y=Ratio_median, 
                             x=seq(0, 60, length.out=length(Ratio_median))))+
  facet_grid(~Species)

# No trend not much pattern. 


 ## 4.2.) INVERTEBRATES ----
# Let us add the fishing years to the data set and plot the ratios over fishing
# years
invNam <- c("Crabs_Lobsters", "Octopus", "Snails", "Squids")
inv <- subset(spCExpFin3, spCExpFin3$Species %in% invNam)

# medSp <- unique(keySp$Ratio_median) 

inv <- as.data.frame(inv)
names(inv)
ggplot(inv)+
  geom_point(aes(x=Years_fishing, y=Ratio))+
  geom_line(data= inv, aes(y=Ratio_median, 
                             x=seq(0, 60, length.out=length(Ratio_median))))+
  facet_grid(~Species)

# Works for Octopus and Squids

 ## 4.3.) PELAGICS ----
# Let us add the fishing years to the data set and plot the ratios over fishing
# years
pelNam <- c("Belonidae", "Chirocentridae", "Sphyraenidae")
pel <- subset(spCExpFin3, spCExpFin3$Species %in% pelNam)

# medSp <- unique(keySp$Ratio_median) 

pel <- as.data.frame(pel)
names(pel)
ggplot(pel)+
  geom_point(aes(x=Years_fishing, y=Ratio))+
  geom_line(aes(y=Ratio_median, 
                             x=seq(0, 60, length.out=length(Ratio_median))))+
  facet_grid(~Species)

# Not usueful. I could check the other pelagic species which are reported by
# the fishers



 ## 4.4.) FISH AND INVERTEBRATES ----
# Let us add the fishing years to the data set and plot the ratios over fishing
# years
carniNam <- c("Lethrinus_harak","Lethrinus_variegatus", "Lethrinus_mahsena",
              "Serranidae", "Plectorhinchus_gibbus", "Rhinobatidae", 
              "Mullidae", "Anguilliformes", "Cheilinus_trilobatus")
carni <- subset(spCExpFin3, spCExpFin3$Species %in% carniNam)

# medSp <- unique(keySp$Ratio_median) 

carni <- as.data.frame(carni)
names(carni)
ggplot(carni)+
  geom_point(aes(x=Years_fishing, y=Ratio))+
  geom_line(aes(y=Ratio_median, 
                x=seq(0, 60, length.out=length(Ratio_median))))+
  facet_grid(~Species)

# I guess I could use all species except Lethrinus variegatus and Rhinobatidae.
# This works because these species/groups contribute over 60 % to this group.
# Serranidae seem to be very consistent...
# I am wondering how I will use this for the group



 ## 4.5.) OMNIVORE ----
# Let us add the fishing years to the data set and plot the ratios over fishing
# years
omNam <- c("Balistidae", "Mugilidae", "Monodactylus_argentus")
om <- subset(spCExpFin3, spCExpFin3$Species %in% omNam)

# medSp <- unique(keySp$Ratio_median) 

om <- as.data.frame(om)
names(om)
ggplot(om)+
  geom_point(aes(x=Years_fishing, y=Ratio))+
  geom_line(aes(y=Ratio_median, 
                x=seq(0, 60, length.out=length(Ratio_median))))+
  facet_grid(~Species)

# Does not seem to be usueful :-(

 ## 4.6.) HERBIVORE ----
# Let us add the fishing years to the data set and plot the ratios over fishing
# years
herbNam <- c("Acanthurus_spp", "Hipposcarus_harid", "Calatomus_carolinus")
herb <- subset(spCExpFin3, spCExpFin3$Species %in% herbNam)

# medSp <- unique(keySp$Ratio_median) 

herb <- as.data.frame(herb)
names(herb)
ggplot(herb)+
  geom_point(aes(x=Years_fishing, y=Ratio))+
  geom_line(aes(y=Ratio_median, 
                x=seq(0, 60, length.out=length(Ratio_median))))+
  facet_grid(~Species)

# This works because both groups contribute over 70 % to the group

 ## 4.7.) ZOOPLANKTER ----
# Let us add the fishing years to the data set and plot the ratios over fishing
# years
zooNam <- c("Siganus_stellatus", "Caesionidae", "Sardinella_spp")
zoo <- subset(spCExpFin3, spCExpFin3$Species %in% zooNam)

# medSp <- unique(keySp$Ratio_median) 

zoo <- as.data.frame(zoo)
names(zoo)
ggplot(zoo)+
  geom_point(aes(x=Years_fishing, y=Ratio))+
  geom_line(aes(y=Ratio_median, 
                x=seq(0, 60, length.out=length(Ratio_median))))+
  facet_grid(~Species)

# This does not work
