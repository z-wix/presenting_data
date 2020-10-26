# Load Packages -----------------------------------------------------------

library(tidyverse)
library(fastDummies)

# Import Data -------------------------------------------------------------

solar_df <- read.csv(here::here("data_r", "SolarMax.csv"))


# Rename Vars -------------------------------------------------------------

solar <- solar_df %>% 
  rename(
    overall_nps = Overall.NPS..0.to.10.,
    tech_nps = Hire.Tech.NPS..0.to.10.,
    install_nps = Hire.Installer.NPS..0.to.10.,
    service_csat = Service.CSAT..1.to.5.,
    install_csat = Install.CSAT..1.to.5.,
    tech_communication = Tech.communicate.what.work.was.done,
    knowledge = Knowledgable..1.to.5.,
    tech_clean_up = Tech.clean.up.after.themselves,
    install_communication = Install.communicate.what.work.was.done,
    teach_use = Teach.use
  )


# Calculate Percentiles ---------------------------------------------------

# create average scores variables
solar_na <- solar %>% 
  group_by(RID) %>%
  mutate(
    ave_nps = mean(c(overall_nps, tech_nps, install_nps), na.rm = TRUE),
    ave_csat = mean(c(service_csat, install_csat), na.rm = TRUE),
    ave_salary= 35000
  )

# csat percentile
csat_df <- as.data.frame(rep(1:5))
quantile(csat_df[,1], c(.25, .50, .75, .90))

# nps percentile
nps_df <- as.data.frame(rep(1:10))
quantile(nps_df[,1], c(.25, .50, .75, .90))


# Quartile Joining
solar_na$csat_percentile <- 0
solar_na$nps_percentile <- 0

solar_clean <- solar_na %>% 
  drop_na(ave_nps, ave_csat)

# CSAT Percentiles
for (i in seq_along(solar_clean$ave_csat)) {
  if (solar_clean$ave_csat[i] >= quantile(csat_df[,1], .90)) {
    solar_clean$csat_percentile[i] = 90
  }else{
    if(solar_clean$ave_csat[i] >= quantile(csat_df[,1], .75)){
      solar_clean$csat_percentile[i] = 75
    }else{
      if(solar_clean$ave_csat[i] >= quantile(csat_df[,1], .50)){
        solar_clean$csat_percentile[i] = 50
      }else{
        if(solar_clean$ave_csat[i] >= quantile(csat_df[,1], .25)){
          solar_clean$csat_percentile[i] = 25
        }else{
          if(is.na(solar_clean$ave_csat[i])){
            solar_clean$csat_percentile[i] = NA
          }else{
            solar_clean$csat_percentile[i] = 0
          }
        }
      }
    }
  }
}

# NPS Percentiles
for (i in seq_along(solar_clean$ave_nps)) {
  if (solar_clean$ave_nps[i] >= quantile(nps_df[,1], .90)) {
    solar_clean$nps_percentile[i] = 90
  }else{
    if(solar_clean$ave_nps[i] >= quantile(nps_df[,1], .75)){
      solar_clean$nps_percentile[i] = 75
    }else{
      if(solar_clean$ave_nps[i] >= quantile(nps_df[,1], .50)){
        solar_clean$nps_percentile[i] = 50
      }else{
        if(solar_clean$ave_nps[i] >= quantile(nps_df[,1], .25)){
          solar_clean$nps_percentile[i] = 25
        }else{
          if(is.na(solar_clean$ave_nps[i])){
            solar_clean$nps_percentile[i] = NA
          }else{
            solar_clean$nps_percentile[i] = 0
          }
        }
      }
    }
  }
}

# Select only important Variables
solar_select <- solar_clean %>% select(RID, ave_salary, ave_csat, csat_percentile, ave_nps, nps_percentile)

# Calculate bonuses and totals
solar_bonus <- solar_select %>% 
   mutate(
    csat_bonus = (if(csat_percentile==90){
          ave_salary *.25
          }else{
      if(csat_percentile==75){
              ave_salary *.15
          }else{
        if(csat_percentile==50){
              ave_salary *.10
            }else{
          if(csat_percentile==25){
                ave_salary *.05
              }else{
            0
          }
        }
      }
    }),
    nps_bonus = if(nps_percentile==90){
      ave_salary *.25
    }else{
      if(nps_percentile==75){
        ave_salary *.15
      }else{
        if(nps_percentile==50){
          ave_salary *.10
        }else{
          if(nps_percentile==25){
            ave_salary *.05
          }else{
            0
          }
        }
      }
    },
    total_csat = ave_salary + csat_bonus,
    total_nps = ave_salary + nps_bonus,
    diff_csat_to_nps = total_csat - total_nps
  ) %>% 
  dummy_cols(
    select_columns = c("csat_percentile", "nps_percentile")
  )


write.csv(solar_bonus, "solar_bonus.csv")

solar_nps <- solar_bonus %>% 
  group_by(nps_percentile) %>% 
  summarize(
    n = n()
  )

solar_csat <- solar_bonus %>% 
  group_by(csat_percentile) %>% 
  summarize(
    n = n()
  )


write.csv(solar_nps, "solar_nps.csv")

write.csv(solar_csat, "solar_csat.csv")

solar_bonus %>% 
  ggplot()+
  geom_bar(aes(x = total_csat, fill = csat_percentile))+
  geom_bar(aes(x = total_nps, fill = nps_percentile))

solar_bonus %>% 
  ggplot() + 
  geom_density(aes(x = total_csat, fill = "r"), alpha = 0.3) +
  geom_density(aes(x = total_nps, fill = "b"), alpha = 0.3) +
  scale_colour_manual(name ="solar", values = c("r" = "red", "b" = "blue"), labels=c("b" = "blue values", "r" = "red values")) +
  scale_fill_manual(name ="solar", values = c("r" = "red", "b" = "blue"), labels=c("b" = "blue values", "r" = "red values"))


ggplot() + 
  geom_col(data = solar_csat, aes(x = n,y = csat_percentile, fill = "r"), alpha = 0.3) +
  geom_col(data = solar_nps , aes(x = n,y = nps_percentile, fill = "b"), alpha = 0.3) +
  scale_colour_manual(name ="solar", values = c("r" = "red", "b" = "blue"), labels=c("b" = "blue values", "r" = "red values")) +
  scale_fill_manual(name ="solar", values = c("r" = "red", "b" = "blue"), labels=c("b" = "blue values", "r" = "red values"))

