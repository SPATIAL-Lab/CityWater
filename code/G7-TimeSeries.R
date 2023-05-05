###BOXPLOTS time slices for SLC and SF####
###Data import & prep###
library(tidyverse) 
tapData <- read_csv("data/cityWater.csv", 
                    col_types = cols(Cluster_ID = col_character()))

#names(tapData)
tapData$Cluster_ID <- factor(tapData$Cluster_ID)
tapData$Cluster_Location <- factor(tapData$Cluster_Location)
tapData$Cluster_Location_Time <- factor(tapData$Cluster_Location_Time)
tapData$Cluster_State <- factor(tapData$Cluster_State)
tapData$Project_ID <- factor(tapData$Project_ID)

#Boxplot SLC and SF time slices
timeSlice <- tapData %>%
  filter(Cluster_ID %in% c("1.01","1.02","1.03","1.04","1.05","1.06","1.07","1.08",
                           "1.09","1.10","1.11","25.1","25.2","25.3",
                           "25.3","25.4","25.5","25.6","25.7")) %>%
  mutate(Cluster_ID = fct_relevel(Cluster_ID,
                       "1.01","1.02","1.03","1.04","1.05","1.06","1.07","1.08",
                           "1.09","1.10","1.11","25.1","25.2","25.3",
                           "25.4","25.5","25.6","25.7")) %>%
  ggplot(aes(y = Cluster_ID, x = d18O, fill = Cluster_Location)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.15) + 
  geom_boxplot() +
  theme_classic() + 
  scale_fill_manual(values = c("#006a9b", "#d2042d")) + #this blue is lighter than the normal palette
  labs(
    x = expression(paste(delta^18, "O", " (\u2030, VSMOW)")), 
    y = "Cluster ID"
  ) + 
  theme(legend.position = 'none')

timeSlice
ggsave("figures/boxplot_time_slice.tiff", width=7, height=5, units="in", dpi=300)

# Let's look a little more in-depth for time series 
#First, create a grouped dataframe for quickly looking at things by time

SLC_timeseries <- group_by(subset(tapData, Cluster_Location == "Salt Lake City"), 
                           Cluster_ID) 

summary(aov(SLC_timeseries$d18O ~ SLC_timeseries$Cluster_ID))

tally(SLC_timeseries)

timeseriessummarySLC <- subset(tapData, Cluster_Location == "Salt Lake City") %>%
  group_by(Cluster_ID) %>%
  summarize(across(c(d18O, d2H, d_ex), list(
    min = min, 
    max = max, 
    mean = mean,
    sd = sd
  ))) %>% 
# mutate(range = d18O_max - d18O_min)
  mutate(across(ends_with("max"), ~ . - get(gsub( "max", "min", cur_column())), 
                .names = '{.col}_range'))

timeseriessummarySLC2 <- subset(tapData, Cluster_Location == "Salt Lake City") %>%
  group_by(Cluster_ID) %>%
  summarize(n = n())

timeseriessummarySLC <- timeseriessummarysLC %>% 
  left_join(timeseriessummarySLC2) %>% 
  mutate_at(2:13, round, 2)

SF_timeseries <- group_by(subset(tapData, Cluster_Location == "San Francisco"), 
                           Cluster_ID) 

summary(aov(SF_timeseries$d18O ~ SF_timeseries$Cluster_ID))

tally(SF_timeseries)
timeseriessummarySF <- subset(tapData, Cluster_Location == "San Francisco") %>%
  group_by(Cluster_ID) %>%
  summarize(across(c(d18O, d2H, d_ex), list(
    min = min, 
    max = max, 
    mean = mean,
    sd = sd
  ))) %>% 
  # mutate(range = d18O_max - d18O_min)
  mutate(across(ends_with("max"), ~ . - get(gsub( "max", "min", cur_column())), 
                .names = '{.col}_range'))

timeseriessummarySF2 <- subset(tapData, Cluster_Location == "San Francisco") %>%
  group_by(Cluster_ID) %>%
  summarize(n = n())

timeseriessummarySF <- timeseriessummarySF %>% 
  left_join(timeseriessummarySF2) %>% 
  mutate_at(2:13, round, 2)

# Comparing means -------------------------------------------------------

summary(aov(SLC_timeseries$d18O~ SLC_timeseries$Cluster_ID))
summary(aov(SF_timeseries$d18O~ SF_timeseries$Cluster_ID))

