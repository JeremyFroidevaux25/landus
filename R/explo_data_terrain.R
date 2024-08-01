#----- load tables

library(openxlsx)
releves = read.xlsx("/Users/jeremyfroidevaux/Documents/Herbiland/CameraTrap/saisieRELEVES_gpesFct_20220914_final.xlsx", sheet = "SURVEYS")
head(releves)

soil = read.xlsx("/Users/jeremyfroidevaux/Documents/Herbiland/CameraTrap/saisieRELEVES_gpesFct_20220914_final.xlsx", sheet = "PROTOCOL_SOIL_SAISIE")
head(soil)

biomass = read.xlsx("/Users/jeremyfroidevaux/Documents/Herbiland/CameraTrap/saisieRELEVES_gpesFct_20220914_final.xlsx", sheet = "PROTOCOL_BIOMASS")
head(biomass)

hcan = biomass %>% group_by(plot_project) %>% summarise(hcan = mean(height), heterog = sd(height))

pfg =read.xlsx("/Users/jeremyfroidevaux/Documents/Herbiland/CameraTrap/saisieRELEVES_gpesFct_20220914_final.xlsx", sheet = "PROTOCOL_PPFG_SUMMARY")
head(pfg)

#----- shape table PFG
library(dplyr)
library(tidyr)

## check for erreures saisie ##
summary(pfg$nb_pin_touch)
table(pfg$ref_fg)
## what is PA?? -> picea abies??
## TLB -> TLD  et LRH -> LRF non! (hirsutum)
pfg = pfg %>% mutate(ref_fg = replace(ref_fg, ref_fg == "TLB", "TLD"))
pfg = pfg %>% filter(ref_fg != "PA")
table(pfg$ref_fg)

pfg[duplicated(pfg [,c("ref_fg", "plot_project")]),]
plot_with_errors = pfg[duplicated(pfg [,c("ref_fg", "plot_project")]),"plot_project"]

## ====

pfg_large = pfg %>% filter(!plot_project %in% plot_with_errors) %>% dplyr::select(plot_project, ref_fg, nb_pin_touch) %>% pivot_wider(names_from = ref_fg, values_from = nb_pin_touch, values_fill = 0) 

fdiv = pfg %>% group_by(plot_project) %>% summarise(nPFG = n())

lh = pfg %>% group_by(plot_project) %>% summarise(totL = n(grep("L"), .))


row_sum = rowSums(dplyr::select(pfg_large, -plot_project))
pfg_com_rel = pfg_large %>% mutate_if(is.numeric, ~ ./row_sum)
pfg_com_rel = as.matrix(pfg_com_rel[,-1])
rownames(pfg_com_rel) = pfg_large$plot_project

data_hauteurs = as.data.frame(pfg_com_rel) %>% mutate(plot_project = rownames(pfg_com_rel)) %>% left_join(hcan, join_by("plot_project")) %>% left_join(fdiv) %>% left_join(soil) %>% left_join(releves)

data_hauteurs %>% ggplot(aes(x = nPFG, y = LVM)) + geom_point()
hist(data_hauteurs$LRF)
hist(val_brutes$nPFG)

hist(val_brutes$heterog)

val_brutes = pfg_large%>% left_join(hcan, join_by("plot_project")) %>% left_join(fdiv) %>% left_join(soil)  %>% left_join(releves)

val_brutes  %>% ggplot(aes(x = hcan, y = LVM)) + geom_point() + geom_smooth(method="lm")

val_brutes  %>% ggplot(aes(x = hcan, y = LRF)) + geom_point() + geom_smooth(method="lm")

val_brutes  %>% ggplot(aes(x = LVU, y = LVM)) + geom_point() + geom_smooth()


val_brutes  %>% ggplot(aes(x = elevation, y = LRF)) + geom_point() + geom_smooth()



case_group = function(x){
  names(x) = c( "LVU", "LRF", "LVVI", "LCV", "LVM")
  paste(names(x[which(x==1)]),  collapse = "_")
}
pfg_large$group =  pfg_large %>% select(LVU, LRF, LVVI, LCV, LVM) %>% mutate(across(everything(), ~ ifelse(. > 20, 1, 0))) %>% apply(1, case_group) 
sort(table(pfg_large$group)) 

# pfg_large$gLVU = pfg_large %>% select(LVU, LRF, LVVI, LCV, LVM) %>% mutate(across(everything(), ~ ifelse(. > 25, 1, 0))) %>% pull(LVU) 
# pfg_large$gLRF = pfg_large %>% select(LVU, LRF, LVVI, LCV, LVM) %>% mutate(across(everything(), ~ ifelse(. > 25, 1, 0))) %>% pull(LRF)
# pfg_large$gLVVI = pfg_large %>% select(LVU, LRF, LVVI, LCV, LVM) %>% mutate(across(everything(), ~ ifelse(. > 25, 1, 0))) %>% pull(LVVI)
# pfg_large$gLCV = pfg_large %>% select(LVU, LRF, LVVI, LCV, LVM) %>% mutate(across(everything(), ~ ifelse(. > 25, 1, 0))) %>% pull(LCV)
# pfg_large$gLVM = pfg_large %>% select(LVU, LRF, LVVI, LCV, LVM) %>% mutate(across(everything(), ~ ifelse(. > 25, 1, 0))) %>% pull(LVM)
pfg_large2 = pfg_large %>% filter(group != "")

## nb en selectionnant 25%, seuls LVU, LRF, LVVI, LCV et LVM sortent seuls

write.csv(val_brutes,"/Users/jeremyfroidevaux/Documents/Herbiland/CameraTrap/landus_20231109.csv")

#------ quick visu community data
library(vegan)
PFGnmds = metaMDS(select(pfg_large2, -c(plot_project, group)), k=2)
PFGnmds = metaMDS(select(pfg_large, -c(plot_project, group)), k=2)
species.scores = as.data.frame(scores(PFGnmds, "species"))
species.scores$species = rownames(species.scores)
head(species.scores)
sites.scores = as.data.frame(scores(PFGnmds, "sites"))
rownames(sites.scores) = pfg_large$plot_project
sites.scores$sites = pfg_large$plot_project
head(sites.scores)

ordiplot(PFGnmds,type="n")
orditorp(PFGnmds,display="species",col="red",air=0.01)
orditorp(PFGnmds,display="sites",cex=1.25,air=0.01)

library(ggplot2)
sites.scores %>% 
  ggplot(aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(size = 1, color = 1))

species.scores %>% 
  ggplot(aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(size = 1, color = species)) +
  stat_ellipse(geom = "polygon", aes(group = group, color = group, fill = group), alpha = 0.3) +
  annotate("text", x = -2, y = 0.95, label = paste0("stress: ", format(PFGnmds$stress, digits = 4)), hjust = 0) +
  theme_bw() + scale_fill_brewer(palette="Paired") + scale_color_brewer(palette="Paired")


#----- extract climato-topo
#TODO

#---------combine all info


