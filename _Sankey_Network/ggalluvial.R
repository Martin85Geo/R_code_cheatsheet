

# install.packages("ggalluvial")
library("tidyverse")
library("ggalluvial")
df <- as.data.frame(Titanic)
ggplot(df,
       aes(y = Freq,
           axis1 = Sex, 
           axis2 = Class, 
           axis3 = Survived)) +
  geom_alluvium(aes(fill = Class),
                width = 1/4, knot.pos = 0.3, reverse = FALSE) +
  guides(fill = FALSE) +
  geom_stratum(width = 1/4, reverse = FALSE) +
  geom_text(stat = "stratum", label.strata = TRUE, reverse = FALSE) +
  scale_x_continuous(breaks = 1:3, labels = c("Survived", "Sex", "Class")) +
  scale_fill_manual(values=c('#e7298a','#1b9e77','#d95f02','#7570b3'))+
  ggtitle("Titanic survival by class and sex")+
  theme_bw()





df2 <- links[,1:3] %>% gather(key = 'class', value = 'val')

ggplot(df,
       aes(y = value,
           axis1 = Sex, 
           axis2 = Class, 
           axis3 = Survived)) +
  geom_alluvium(aes(fill = Class),
                width = 1/4, knot.pos = 0.3, reverse = FALSE) +
  # guides(fill = FALSE) +
  geom_stratum(width = 1/4, reverse = FALSE) +
  geom_text(stat = "stratum", label.strata = TRUE, reverse = FALSE) +
  scale_x_continuous(breaks = 1:3, labels = c("Survived", "Sex", "Class")) +
  # scale_fill_manual(values=c('#e7298a','#1b9e77','#d95f02','#7570b3'))+
  # ggtitle("Titanic survival by class and sex")+
  theme_bw()























data(vaccinations)
dfff <- vaccinations %>% as.data.frame()
levels(vaccinations$response) <- rev(levels(vaccinations$response))

vaccinations <- vaccinations[, -2]
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           y = freq,
           fill = response, label = response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("vaccination survey responses at three points in time")
















# rightward flow aesthetics for vaccine survey data
data(vaccinations)
levels(vaccinations$response) <- rev(levels(vaccinations$response))
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           y = freq, fill = response, label = round(a, 3))) +
  geom_lode() + geom_flow() +
  geom_stratum(alpha = 0) +
  geom_text(stat = "stratum")

