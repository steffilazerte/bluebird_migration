library(car)
library(emmeans)
library(here)
library(tidyverse)
library(DHARMa)
library(broom)

migration <- read.csv(here("Data/Datasets/migration_details_july.csv"))

migration[migration$species == "Western Bluebird" & migration$year %in% c(2009, 2012),
          c("fall_begin_yday", "fall_end_yday",
            "fall_median_lon", "breeding_median_lon", "nonbreeding_median_lon")] <- NA

migration.Mountain.Bluebird <- subset(migration, species == "Mountain Bluebird")
migration.Eastern.Bluebird <- subset(migration, species == "Eastern Bluebird")
migration.Western.Bluebird <- subset(migration, species == "Western Bluebird")

migration$species <- factor(migration$species)
contrasts(migration$species) <- contr.sum(n = 3)

#Longitude

##Spring

Mod1 = lm( spring_median_lon ~ year*species, data = migration)
Anova(Mod1, type = "III")

mod1.1=lm(spring_median_lon~year, data=migration.Mountain.Bluebird)
summary(mod1.1)

mod1.2=lm(spring_median_lon~year, data=migration.Eastern.Bluebird)
summary(mod1.2)

mod1.3=lm(spring_median_lon~year, data=migration.Western.Bluebird)
summary(mod1.3)

e <- emtrends(Mod1, var = "year", specs = "species")
summary(e, adjust = "fdr", infer = c(FALSE, TRUE))

R1 <- simulateResiduals(Mod1, plot = TRUE)

##Fall

Mod2 = lm( fall_median_lon ~ year*species, data = migration)
Anova(Mod2, type = "III")

mod1.4=lm(fall_median_lon~year, data=migration.Mountain.Bluebird)
summary(mod1.4)

mod1.5=lm(fall_median_lon~year, data=migration.Eastern.Bluebird)
summary(mod1.5)

mod1.6=lm(fall_median_lon~year, data=migration.Western.Bluebird)
summary(mod1.6)

e <- emtrends(Mod2, var = "year", specs = "species")
summary(e, adjust = "fdr", infer = c(FALSE, TRUE))

R2 <- simulateResiduals(Mod2, plot = TRUE)

##Breeding

Mod3 = lm( breeding_median_lon ~ year*species, data = migration)
Anova(Mod3, type = "III")

mod1.7=lm(breeding_median_lon~year, data=migration.Mountain.Bluebird)
summary(mod1.7)

mod1.8=lm(breeding_median_lon~year, data=migration.Eastern.Bluebird)
summary(mod1.8)

mod1.9=lm(breeding_median_lon~year, data=migration.Western.Bluebird)
summary(mod1.9)

e <- emtrends(Mod3, var = "year", specs = "species")
summary(e, adjust = "fdr", infer = c(FALSE, TRUE))

R3 <- simulateResiduals(Mod3, plot = TRUE)

#Latitude

Mod4 = lm( max_lat ~ year*species, data = migration)
Anova(Mod4, type = "III")

modB=lm(max_lat~year+species, data=migration)
Anova(modB, type = "III")
summary(modB)

R4 <- simulateResiduals(Mod4, plot = TRUE)

#Timing

##Spring start

Mod5 = lm(spring_begin_yday ~ year*species, data = migration)
Anova(Mod5, type = "III")

modC=lm(spring_begin_yday~year+species, data=migration)
Anova(modC, type = "III")

R5 <- simulateResiduals(Mod5, plot = TRUE)

##Spring end

Mod6 = lm( spring_end_yday ~ year*species, data = migration)
Anova(Mod6, type = "III")

modD=lm(spring_end_yday~year+species, data=migration)
Anova(modD, type = "III")

R6 <- simulateResiduals(Mod6, plot = TRUE)

##Fall start

Mod7 = lm( fall_begin_yday ~ year*species, data = migration)
Anova(Mod7, type = "III")

modE=lm(fall_begin_yday~year+species, data=migration)
Anova(modE, type = "III")

R7 <- simulateResiduals(Mod7, plot = TRUE)

##Fall end

Mod8 = lm( fall_end_yday ~ year*species, data = migration)
Anova(Mod8, type = "III")

modF=lm(fall_end_yday~year+species, data=migration)
Anova(modF, type = "III")

R8 <- simulateResiduals(Mod8, plot = TRUE)

#Speed

##Spring

Mod9 = lm( speed_spring ~ year*species, data = migration)
Anova(Mod9, type = "III")

modG=lm(speed_spring~year+species, data=migration)
Anova(modG, type = "III")

R9 <- simulateResiduals(Mod9, plot = TRUE)

##Fall

Mod10 = lm( speed_fall ~ year*species, data = migration)
Anova(Mod10, type = "III")

modH=lm(speed_fall~year+species, data=migration)
Anova(modH, type = "III")

R10 <- simulateResiduals(Mod10, plot = TRUE)


#Table of Results

m <- list(Mod1, Mod2, Mod3, modB, modC, modD, modE, modF, modG, modH)

tibble(model = m) %>%
  mutate(name = map_chr(model, ~as.character(.x$call)[2]),
         anova = map(model, ~tidy(Anova(.x, type = "III")))) %>%
  unnest(cols = anova) %>%
  select(-model) %>%
  DT::datatable(migration, extensions = 'Buttons',
                options = list(dom = 'Bfrtip', buttons = c('csv', 'excel')))

#Reproductibility

DT::datatable(migration, extensions = 'Buttons',
              options = list(dom = 'Bfrtip', buttons = c('csv', 'excel')))


