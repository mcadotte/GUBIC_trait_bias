##query genbank for taxa
library(tidyverse)
library(httr2)
library(jsonlite)
library(rentrez)
library(ggplot2)
library(patchwork)

#setwd("~/Dropbox/2018WorkingFiles/Marc2018/GUBIC/Trait perspective paper")

# June 2026 World Flora Online Plant List
wfo <- read_tsv("classification.csv",show_col_types = FALSE)
names(wfo)
head(wfo)


#----------------------------------------------------------
# 2. Select accepted flowering-plant species
#----------------------------------------------------------

angio <- wfo %>% filter(taxonRank == "species",taxonomicStatus == "Accepted")

# Number of accepted species per family
family_richness <- angio %>% filter(!is.na(family),family != "") %>%
  count(family, name = "Accepted_species_WFO") %>%
  arrange(desc(Accepted_species_WFO))

head(family_richness, 20)

#----------------------------------------------------------
# GenBank accession count
#----------------------------------------------------------


get_genbank_count <- function(family) {
  
  query <- paste0('"', family, '"[Organism]')
  result <- entrez_search(db = "nuccore",term = query,retmax = 0)
  
  Sys.sleep(0.35)
  
  result$count
}

#get_genbank_count("Asteraceae")
#get_genbank_count("Poaceae")
#get_genbank_count("Fabaceae")
#get_genbank_count("Orchidaceae")


dat <- family_richness %>%
  mutate(GenBank_accessions = map_dbl(family,get_genbank_count))

dat$Acc_per_spp<-dat$GenBank_accessions/dat$Accepted_species_WFO

write.csv(dat,"spp_accessions.csv")
#dat<-read.csv("spp_accessions.csv",row.names=NULL)


GenP<-ggplot(dat, aes(x = Accepted_species_WFO,y = GenBank_accessions)) +
  geom_point(size = 4,colour = "aquamarine4",alpha = 0.75) +
  scale_x_log10() +
  scale_y_log10() +
  labs( x = "Number of species",y = "Number of GenBank accessions"
        ,title="Data for plant families") +
  theme_minimal(base_size = 14)

quartz()
GenP

GenH<-ggplot(dat,aes(x=Acc_per_spp))+
  geom_histogram(fill="aquamarine4",color = "white",linewidth = 1.2)+
  scale_x_log10() +
  labs(x="Accessions per species",y="Count",title="Data for plant families")+
  theme_minimal()+
  theme_minimal(base_size = 14)
quartz()
GenH

quartz()
(GenP + GenH) + plot_annotation(tag_levels = "A",tag_suffix = ")")

#example range for familyes with 90-110 species 922 families)
fams<-dat$Accepted_species_WFO
mid<-dat[fams > 89 & fams < 111,]
summary(mid)

big<-dat[fams > 1000 ,]
summary(big)

mon<-dat[fams == 1 ,]
summary(mon)

shared_lims<-c(0,100000)
shared_bin <- 0.5

Gen_mid<-ggplot(mid,aes(x=Acc_per_spp))+
  geom_histogram(fill="aquamarine4",color = "white",linewidth = 1.2,binwidth = shared_bin)+
  scale_x_log10() +
  labs(x="Accessions per species",y="Count",title="Mid-sized families (90-110 species)")+
  theme_minimal()+
  theme_minimal(base_size = 14)
 
Gen_mon<-ggplot(mon,aes(x=Acc_per_spp))+
  geom_histogram(fill="aquamarine4",color = "white",linewidth = 1.2,binwidth = shared_bin)+
  scale_x_log10() +
  labs(x="Accessions per species",y="Count",title="Monotypic families")+
  theme_minimal()+
  theme_minimal(base_size = 14)
Gen_big<-ggplot(big,aes(x=Acc_per_spp))+
  geom_histogram(fill="aquamarine4",color = "white",linewidth = 1.2,binwidth = shared_bin)+
  scale_x_log10() +
  labs(x="Accessions per species",y="Count",title="Large families (> 1000 species)")+
  theme_minimal()+
  theme_minimal(base_size = 14)

quartz()
(Gen_mon + Gen_mid + Gen_big) + plot_annotation(tag_levels = "A",tag_suffix = ")")

###overlay distributions

mon$size<-"Monotypic"
mid$size<-"Mid-sized"
big$size<-"Large"

three<-rbind(mon,mid,big)

quartz()
Gen_MML<-ggplot(three,aes(x=Acc_per_spp))+
  geom_histogram(aes(fill = size), color="white",position = "identity")+
  scale_x_log10() +
  facet_wrap(~size)+
  labs(x="Accessions per species (log scale)",y="Count")+
  scale_fill_manual(values = c("darkorange3","aquamarine4","goldenrod3")) +
  theme_minimal(base_size = 14)+
  theme(legend.position = "none")+
  theme(strip.text = element_text(size = 16, face = "bold"))
  
Gen_MML
