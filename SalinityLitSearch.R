#  Salinity Disease Review -----------------------------------------------------
#  code by Reyn Yoshioka

# A review of salinity in marine disease ecology
# 

# 1: Getting Started -----------------------------------------------------------
# > 1.1: Clear everything ----- 
# clear workspace
# rm(list=ls())
# clear console
cat("\014")

# > 1.2: Load packages -----
library(here)
library(tidyverse)
library(extrafont)
library(patchwork)

# > 1.3: here:i_am() -----
here::i_am("SalinityLitSearch.R")

# > 1.3: Read in data -----
df_MDS_E = read.csv("MSD_Env_all.csv")
df_MDS_T = read.csv("MSD_Temp_all.csv")
df_MDS_S = read.csv("MSD_Sal_all.csv")

# > 1.4: Load fonts -----
loadfonts(device="win")
fonts()

# 2: Subset for checks -----
# Only done once, hence, this is commented out to avoid overwriting files

# df_MDS_E_samp = df_MDS_E[sample(1:length(df_MDS_E$Authors),
#                             size = 200),]
# df_MDS_T_samp = df_MDS_T[sample(1:length(df_MDS_T$Authors),
#                             size = 200),]
# df_MDS_S_samp = df_MDS_S[sample(1:length(df_MDS_S$Authors),
#                             size = 200),]

# write.csv(df_MDS_E_samp,
#           "df_MDS_E_samp.csv")
# write.csv(df_MDS_T_samp,
#           "df_MDS_T_samp.csv")
# write.csv(df_MDS_S_samp,
#           "df_MDS_S_samp.csv")

#

# 3: Read in checks & calculate correction -----
df_MDS_E_samp = read.csv("df_MDS_E_samp_checked.csv")
df_MDS_T_samp = read.csv("df_MDS_T_samp_checked.csv")
df_MDS_S_samp = read.csv("df_MDS_S_samp_checked.csv")

# Proportion relevant
rlvt_E = mean(df_MDS_E_samp$Relevant)
rlvt_T = mean(df_MDS_T_samp$Relevant)
rlvt_S = mean(df_MDS_S_samp$Relevant)

# 4: Summarize, apply correction, and plot -----
df_MDS_E_summ =
  df_MDS_E |>
  group_by(Year) |>
  summarize(papers = length(Title),
            papers_corr = papers * rlvt_E,
            set = "environment")

df_MDS_T_summ =
  df_MDS_T |>
  group_by(Year) |>
  summarize(papers = length(Title),
            papers_corr = papers * rlvt_T,
            set = "temperature")

df_MDS_S_summ =
  df_MDS_S |>
  group_by(Year) |>
  summarize(papers = length(Title),
            papers_corr = papers * rlvt_S,
            set = "salinity")

# Combine into single df
df_MDS_full_summ =
  rbind(df_MDS_E_summ,
        df_MDS_T_summ,
        df_MDS_S_summ)

# Add zero values
df_MDS_full_summ = 
  df_MDS_full_summ |>
  complete(set, Year,
           fill = list(papers = 0,
                       papers_corr = 0))

# order sets 
df_MDS_full_summ$set = factor(df_MDS_full_summ$set,
                              levels = c("salinity",
                                         "temperature",
                                         "environment"))
# Cut overall disease to focus figure
ggplot(data = df_MDS_full_summ |>
         filter(set != "disease")) +
  geom_line(aes(x = Year,
                y = papers_corr,
                color = set),
            alpha = 0.7,
            linewidth = 2) +
  scale_color_manual(values = c("#0072B2",
                                "#D55E00",
                                "#009E73"),
                     labels = c("salinity-disease articles",
                                "temperature-disease articles",
                                "environment-disease articles")) +
  scale_x_continuous(breaks = seq(1950, 2025, 10)) +
  labs(x = "year",
       y = "# articles\n(corrected)",
       color = "search set",
       fill = "search set") +
  theme_bw() +
  theme(legend.position.inside = c(0.2, 0.8),
        legend.background = element_blank(),
        text = element_text(family = "Times New Roman"))

ggsave("Rocker_etal_Salinity_Fig1.png",
       device = "png",
       units = "in",
       dpi = 300,
       width = 6,
       height = 4)
