#print name
print("Richa")


#install packages
install.packages("tidyr")
install.packages("tidyverse")


#load libraries
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)


#import csv and load in bio
bio <- read.csv("inchBio.csv")
bio

#head,tail,structure of bio
print("Head of bio:")
head(bio)

print("Tail of bio:")
tail(bio)

print("Structure of bio:")
str(bio)


#count and list species record
species_column = bio[[3]]
counts <- count(species_column)
counts


#display 8 levels of species
data.frame(unique(species_column))


#displays the different species and the number of record 
tmp <- data.frame(count(species_column))
tmp


#create a subset of species variable and display the first five records
tmp2 <- data.frame(bio[1:5, c('species')])
tmp2


#create table of species and display class of table
w <- table(bio$species)
w
class(w)


#convert w to data frame and display
t <- data.frame(w)
t
class(t)


#extract and display freq values
freq <- t[2]
freq


#create table and display number of species
cSpec <- table(bio$species)
cSpec


#create table and display percentage of record
cSpecPct <- (table(bio$species))/sum(freq)
cSpecPct
class(cSpecPct)


#convert the table to a data frame 
u <- data.frame(cSpecPct)
u
class(u)


#barplot
barplot(cSpec,
        main = "Fish Count",
        ylab = "COUNTS",
        col = "lightgreen",
        horiz = TRUE,
        cex.names = 0.6)


#barplot
barplot(cSpecPct,
        main = "Fish Relative Frequency",
        ylim = c(0,4),
        ylab = "Frequency",
        col.lab = "lightblue")


#rearrange in descending order of relative freq
d <- u %>%
  arrange(desc(freq))
d

#rename columns
d <- rename(d, c(Species=Var1, RelFreq= Freq))
d


#add new variables to d
library(dplyr)
cumfreq <- cumsum(d$RelFreq)
counts <- tmp$freq
cumcounts <- cumsum(counts)
d <- mutate(d,cumfreq,counts,cumcounts)
d

#create parameter variable
def_par <- c(d$cumfreq,d$counts,d$cumcounts)
def_par


#create barplot
pc = barplot(d$counts, 
             width = 1, 
             space = 0.15,
             border = NA,
             axes = FALSE,
             main = "Species Pareto",
             ylab = "Cummulative Counts",
             cex.names = 0.7,
             ylim = c(0, 3.05 * max(d$counts, na.rm = TRUE)),
             las = 2,
             names.arg = d$Species)


#add count line
lines(pc, type = "b", col="cyan4", cex = 0.7, pch = 19, d$cumcounts)


#place grey box
box(col="gray")


#add left side axis
axis(side = 2, at = c(0,d$cumcounts), col = "grey62", col.axis = "grey62", cex.axis = 0.8)


#add right side axis
axis(side = 4, at = c(0,d$cumcounts), labels = c(0,round(d$cumfreq*100)), col.axis="cyan4", col = "cyan4", cex.axis = 0.8)


#display final plot
pc = barplot(d$counts, 
             width = 1, 
             space = 0.15,
             border = NA,
             axes = FALSE,
             main = "Species Pareto  (RAMBHIA)",
             ylab = "Cummulative Counts",
             cex.names = 0.7,
             ylim = c(0, 3.05 * max(d$counts, na.rm = TRUE)),
             las = 2,
             names.arg = d$Species)

lines(pc, type = "b", col="cyan4", cex = 0.7, pch = 19, d$cumcounts)
box(col="gray")
axis(side = 2, at = c(0,d$cumcounts), col = "grey62", col.axis = "grey62", cex.axis = 0.8)
axis(side = 4, at = c(0,d$cumcounts), labels = c(0,round(d$cumfreq*100)), col.axis="cyan4", col = "cyan4", cex.axis = 0.8)

