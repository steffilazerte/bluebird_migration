library(car)
library(lsmeans)
library(plyr)

migration<-read.csv(here("./Data/Datasets/migration_details"), guess_max = 2000)

spring_median_lon <- migration$spring_median_lon


contrasts(data$species) <- contr.sum(n = 3)

#Longitude

##Spring

Mod1 = lm(spring_median_lon ~ year*species, data = migration)
Anova(Mod1, type = "III")
lsmeans(Mod1, pairwise~year*species, adjust="tukey")

##Fall

Mod2 = lm( fall_median_lon~ year*species, data = migration)
Anova(Mod2, type = "III")
lsmeans(Mod2, pairwise~year*species, adjust="tukey")


##Breeding

Mod3 = lm( breeding_median_lon ~ year*species, data = migration)
Anova(Mod3, type = "III")
lsmeans(Mod3, pairwise~year*species, adjust="tukey")

modA=lm(breeding_median_lon~year+species, data=migration)
summary(modA)


####
Graph1.1 = ggplot(migration, aes(x = Petal.Width, y = Sepal.Length)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

#Latitude

Mod4 = lm( max_lat ~ year*species, data = migration)
Anova(Mod4, type = "III")
lsmeans(Mod4, pairwise~year*species, adjust="tukey")

modB=lm(max_lat~year+species, data=migration)
summary(modB)

#Timing

##Spring start

Mod5 = lm( spring_begin_yday ~ year*species, data = migration)
Anova(Mod5, type = "III")
lsmeans(Mod5, pairwise~year*species, adjust="tukey")

modC=lm(spring_begin_yday~year+species, data=migration)
summary(modC)

##Spring end

Mod6 = lm( spring_end_yday ~ year*species, data = migration)
Anova(Mod6, type = "III")
lsmeans(Mod6, pairwise~year*species, adjust="tukey")

modD=lm(spring_end_yday~year+species, data=migration)
summary(modD)

##Fall start

Mod7 = lm( fall_begin_yday~ year*species, data = migration)
Anova(Mod7, type = "III")
lsmeans(Mod7, pairwise~year*species, adjust="tukey")

modE=lm(fall_begin_yday~year+species, data=migration)
summary(modE)

##Fall end

Mod8 = lm( fall_end_yday ~ year*species, data = migration)
Anova(Mod8, type = "III")
lsmeans(Mod8, pairwise~year*species, adjust="tukey")

modF=lm(fall_end_yday~year+species, data=migration)
summary(modF)

#Speed

##Spring

Mod9 = lm( speed_spring ~ year*species, data = migration)
Anova(Mod9, type = "III")
lsmeans(Mod9, pairwise~year*species, adjust="tukey")

modG=lm(speed_spring~year+species, data=migration)
summary(modG)

##Fall

Mod10 = lm( speed_fall ~ year*species, data = migration)
Anova(Mod10, type = "III")
lsmeans(Mod10, pairwise~year*species, adjust="tukey")

modH=lm(speed_fall~year+species, data=migration)
summary(modH)
