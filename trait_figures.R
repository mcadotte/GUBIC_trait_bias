library(tidyverse)
library(knitr)

setwd("~/Dropbox/2018WorkingFiles/Marc2018/GUBIC/Trait perspective paper")

traits<-readRDS("data/trait_plant_all_standardized_names.rds")

trait_sp_summary = read.csv("data/trait_sp_summary.csv")

trait_sp_summary$native_species_with_traits<-trait_sp_summary$number_sp_with_trait_value_within_urban-trait_sp_summary$number_sp_with_trait_value_within_urban_exotic

trait_sp_summary$prop_native_species_with_traits<-trait_sp_summary$native_species_with_traits/58570
  
trait_sp_summary$prop_exotic_species_with_traits<-trait_sp_summary$number_sp_with_trait_value_within_urban_exotic/7792

trait_sp_summary$native_diff_global<-trait_sp_summary$prop_native_species_with_traits - trait_sp_summary$prop_sp_with_trait_value

trait_sp_summary$exotic_diff_global<-trait_sp_summary$prop_exotic_species_with_traits - trait_sp_summary$prop_sp_with_trait_value

city_dat<- readRDS("data/gbif_gubic_list_per_city_final2_20241003.rds")
#rename two traits

trait_sp_summary$Trait[trait_sp_summary$Trait=="SLA"]<-"LMA"
trait_sp_summary$Trait[trait_sp_summary$Trait=="Wood density"]<-"SSD"
#trait_sp_summary = mutate(trait_sp_summary,
#                          nsp = paste(format(number_sp_with_trait_value, big.mark = ","),
#                                      paste0("(", round(prop_sp_with_trait_value * 100, 2), "%)")),
#                          nsp_urban = paste(format(number_sp_with_trait_value_within_urban, big.mark = ","),
#                                            paste0("(", round(prop_sp_with_trait_value_within_urban * 100, 2), "%)")),
#                          nsp_urban_exotic = paste(format(number_sp_with_trait_value_within_urban_exotic, big.mark = ","),
#                                                   paste0("(", round(prop_sp_with_trait_value_within_urban_exotic * 100, 2), "%)")))
#kable(select(trait_sp_summary, Trait, starts_with("nsp")))

p1 = select(trait_sp_summary, Trait, prop_within_urban = prop_sp_with_trait_value_within_urban) |> 
  mutate(prop_within_urban = prop_within_urban * 100,
         Trait = fct_reorder(Trait, prop_within_urban)) |> 
  ggplot(aes(x = Trait, y = prop_within_urban)) +
  geom_col(fill = "brown") +
  geom_hline(yintercept = 19, linetype = 2, linewidth= 1.5) +
  labs(x = "", y = "Percent of species with traits \nthat are found in urban areas") +
  cowplot::theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) 


ggsave("figures/trait_summary.pdf", plot = p1, width = 7, height = 5)
ggsave("figures/trait_summary.png", width = 7, height = 5)


p2 = select(trait_sp_summary, Trait, Native = native_diff_global, 
       `Non-native` = exotic_diff_global) %>% 
  pivot_longer(cols = 2:3, names_to = "Status", values_to = "diff") %>% 
  mutate(Status = factor(Status, levels = c("Native", "Non-native")),
         Trait = factor(Trait, levels = trait_sp_summary$Trait)) %>% 
  ggplot(aes(x = Trait, y = diff, fill = Status)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("blue", "orange")) +
  labs(x = "", y = "Difference from global proportion") +
  cowplot::theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = c(0.7, 0.9)) 

ggsave("figures/trait_summary2.pdf", plot = p2, width = 7, height = 5)
ggsave("figures/trait_summary2.png", plot = p2, width = 7, height = 5)

p12 = cowplot::plot_grid(p1, p2, ncol = 2, labels = c("A)","B)"))
ggsave("figures/trait_summary12.pdf", plot = p12, width = 9, height = 5)
ggsave("figures/trait_summary12.png", plot = p12, width = 9, height = 5)
 p12
