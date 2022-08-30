## Loading Packages--- 
library(tidyverse) #this contains the ggplot package for graphing 
library(dplyr)
getwd()

#MOZAMBIQUE----

Mozambique <- read.csv("violin_plot_df/mozambique/moz_dist_damage_track_&_coast.csv")
## Data wrangling Mozambique----
Mozambique$ZS_mean <- Mozambique$ZS_mean*29
Mozambique$ZS_track_mean <- Mozambique$ZS_track_mean*29
Mozambique$ZS_mean <- Mozambique$ZS_mean/1000
Mozambique$ZS_track_mean <- Mozambique$ZS_track_mean/1000

Mozambique <- Mozambique %>%
  select(c(1, 2, 5, 8))

## Plottig----
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
#this is the source necessary for the violin box plots (found trough the our coding club website tutorial, here:https://ourcodingclub.github.io/tutorials/dataviz-beautification-synthesis/)

## Plot distance from shore----
(violin_moz_shoreline <- ggplot(Mozambique, aes(DN, ZS_mean, color = as.factor(DN), fill = as.factor(DN))) + 
    geom_flat_violin(position = position_nudge(x = 0.15, y = 0.15), alpha = 0.5, trim = FALSE) + 
    #^The half violins and their positions and shading
    geom_boxplot(width = 0.1, alpha = 0.95) + #The boxplots
    labs(y = "Distance from shoreline (km)", x = "") +
    scale_color_manual(values=c('#000000'))+
    scale_fill_manual(values=c("#455e89")) +
    coord_flip() + #flipping the x and y for easier readability 
    theme_minimal() +
    theme(axis.text = element_text(size = 10),
         axis.title = element_text(size = 13, face = "plain"), 
         legend.position="none",
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank()))

#ggsave(violin_moz_shoreline, file = "violin_plot_df/mozambique/img/Idai_shore_dist.png", height = 12, width = 18)

## Plot distance from track----
(violin_moz_track <- ggplot(Mozambique, aes(DN, ZS_track_mean, color = as.factor(DN), fill = as.factor(DN))) + 
   geom_flat_violin(position = position_nudge(x = 0.15, y = 0.15), alpha = 0.4, trim = FALSE) + 
   #^The half violins and their positions and shading
   geom_boxplot(width = 0.1, alpha = 0.95) + #The boxplots
   labs(y = "Distance from storm-track (km)", x = "") +
   scale_color_manual(values=c('#000000'))+
   scale_fill_manual(values=c("#a01a58")) +
   coord_flip() + #flipping the x and y for easier readability 
   theme_minimal() +
   theme(axis.text = element_text(size = 10),
         axis.title = element_text(size = 13, face = "plain"), 
         legend.position="none",
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank()))

#ggsave(violin_moz_track, file = "violin_plot_df/mozambique/img/Idai_track_dist.png", height = 12, width = 18)

#Florida----
Florida_coast<- read.csv("violin_plot_df/florida/coast_distance.csv")
## Data wrangling Florida----
Florida_coast$ZS_Coastlinemean <- Florida_coast$ZS_Coastlinemean/1000

Florida_coast <- Florida_coast %>%
  select(c(1, 2, 5))

Florida_track<- read.csv("violin_plot_df/florida/track_distance.csv")
## Data wrangling Florida----
Florida_track$ZS_trackmean <- Florida_track$ZS_trackmean*29
Florida_track$ZS_trackmean <- Florida_track$ZS_trackmean/1000

Florida_track <- Florida_track %>%
  select(c(1, 5))

Florida <- merge(Florida_coast, Florida_track,by="fid")

##Florida Plotting----
## Plot distance from shore----
(violin_flo_shoreline <- ggplot(Florida, aes(DN, ZS_Coastlinemean, color = as.factor(DN), fill = as.factor(DN))) + 
   geom_flat_violin(position = position_nudge(x = 0.15, y = 0.15), alpha = 0.5, trim = FALSE) + 
   #^The half violins and their positions and shading
   geom_boxplot(width = 0.1, alpha = 0.95) + #The boxplots
   labs(y = "Distance from shoreline (km)", x = "") +
   scale_color_manual(values=c('#000000'))+
   scale_fill_manual(values=c("#455e89")) +
   coord_flip() + #flipping the x and y for easier readability 
   theme_minimal() +
   theme(axis.text = element_text(size = 10),
         axis.title = element_text(size = 13, face = "plain"), 
         legend.position="none",
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank()))

#ggsave(violin_flo_shoreline, file = "violin_plot_df/florida/img/Irma_shore_dist.png", height = 12, width = 18)

## Plot distance from track----
(violin_flo_track <- ggplot(Florida, aes(DN, ZS_trackmean, color = as.factor(DN), fill = as.factor(DN))) + 
   geom_flat_violin(position = position_nudge(x = 0.15, y = 0.15), alpha = 0.4, trim = FALSE) + 
   #^The half violins and their positions and shading
   geom_boxplot(width = 0.1, alpha = 0.95) + #The boxplots
   labs(y = "Distance from storm-track (km)", x = "") +
   scale_color_manual(values=c('#000000'))+
   scale_fill_manual(values=c("#a01a58")) +
   coord_flip() + #flipping the x and y for easier readability 
   theme_minimal() +
   theme(axis.text = element_text(size = 10),
         axis.title = element_text(size = 13, face = "plain"), 
         legend.position="none",
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank()))

#ggsave(violin_flo_track, file = "violin_plot_df/florida/img/Idai_track_dist.png", height = 4, width = 6)


