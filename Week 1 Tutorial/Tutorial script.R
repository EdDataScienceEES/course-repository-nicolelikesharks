# Coding Club Workshop 1 - R Basics
# Learning how to import and explore data, and make graphs about Edinburgh's biodiversity
# Written by Nicole Yap 26/09/2020 University of Edinburgh
getwd()
setwd("C:/Users/nicol/Documents/Data Science Course/Week 1 Tutorial")
edidiv <- read.csv("~/Data Science Course/Week 1 Tutorial/CC-RBasics-master/edidiv.csv")
head(edidiv$taxonGroup) 
class(edidiv$taxonGroup) 
edidiv$taxonGroup
as.factor(edidiv$taxonGroup) 
class(edidiv$taxonGroup)
edidiv$taxonGroup <- as.factor(edidiv$taxonGroup) 
class(edidiv$taxonGroup) 
dim(edidiv)
summary(edidiv) 
summary(edidiv$taxonGroup)
library(dplyr)
Beetle <- filter(edidiv, taxonGroup == "Beetle")
Bird <- filter(edidiv, taxonGroup == "Bird")
Butterfly <-filter(edidiv,taxonGroup == "Butterfly")
Dragonfly <- filter(edidiv, taxonGroup == "Dragonfly")
Flowering.Plants <- filter(edidiv, taxonGroup == "Flowering.Plants")
Fungus <- filter(edidiv, taxonGroup == "Fungus")
Hymenopteran <-filter(edidiv, taxonGroup == "Hymenopteran")
Lichen <- filter(edidiv, taxonGroup == "Lichen")
Liverwort <- filter(edidiv, taxonGroup == "Liverwort")
Mollusc <- filter(edidiv, taxonGroup == "Mollusc")
Mammal <- filter(edidiv, taxonGroup == "Mammal")

NoBeetle <- length(unique(Beetle$taxonName)) #Number of distinct beetle species 37
NoBird <- length(unique(Bird$taxonName)) # Number of distinct bird species 86
NoButterfly <- length(unique(Butterfly$taxonName))#25
NoDragonfly <- length(unique(Dragonfly$taxonName)) #11
NoFlowering.Plants <- length(unique(Flowering.Plants$taxonName))#521
NoFungus <- length(unique(Fungus$taxonName))#219
NoHymenopteran <- length(unique(Hymenopteran$taxonName))#112
NoLichen <- length(unique(Lichen$taxonName))#94 
NoLiverwort <- length(unique(Liverwort$taxonName))#40
NoMammal <- length(unique(Mammal$taxonName))#33 
NoMollusc <- length(unique(Mollusc$taxonName))#97 

biodiv <- c(NoBeetle,NoBird,NoButterfly,NoDragonfly,NoFlowering.Plants,NoFungus,NoHymenopteran,NoLichen,NoLiverwort,NoMammal,NoMollusc) 
names(biodiv) <- c("Beetle",
                   "Bird",
                   "Butterfly",
                   "Dragonfly",
                   "Flowering.Plants",
                   "Fungus",
                   "Hymenopteran",
                   "Lichen",
                   "Liverwort",
                   "Mammal",
                   "Mollusc")

barplot(biodiv, xlab="Taxa", ylab="Number of species", ylim=c(0,600), cex.names=1, cex.axis=2, cex.lab=2)
dev.off()

taxa <- c("Beetle",
          "Bird",
          "Butterfly",
          "Dragonfly",
          "Flowering.Plants",
          "Fungus",
          "Hymenopteran",
          "Lichen",
          "Liverwort",
          "Mammal",
          "Mollusc")
taxa_f <- factor(taxa)
richness <- c(NoBeetle,NoBird,NoButterfly,NoDragonfly,NoFlowering.Plants,NoFungus,NoHymenopteran,NoLichen,NoLiverwort,NoMammal,NoMollusc)
biodata <- data.frame(taxa_f, richness)
write.csv(biodata, file="~/Data Science Course/Week 1 Tutorial/biodata.csv") #change the file path to tutorial 1 folder

png("barplot2.png", width=1600, height=600)
barplot(biodata$richness, names.arg=c("Beetle",
                                      "Bird",
                                      "Butterfly",
                                      "Dragonfly",
                                      "Flowering.Plants",
                                      "Fungus",
                                      "Hymenopteran",
                                      "Lichen",
                                      "Liverwort",
                                      "Mammal",
                                      "Mollusc"),
        xlab="Taxa", ylab="Number of species", ylim=c(0,600))
dev.off()
