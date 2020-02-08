Species <- factor(c('Persea_borbonia', 'Pinus_taeda', 'Pinus_elliottii', 'Cedrus_libani'))
Height_tree <- c(20, 32, 33, 38)
Cause_mortality <- factor(c('Disease', 'Silvicultural', 'Insect', 'Vegetation', 'Unknown'))
mortality <- round(runif(20, min = 1, max=50), 0)
ci <- runif(20, min = 0, max = 3)


trees <- data.frame(Species = rep(Species, 5),
                    Height_tree = rep(Height_tree, 5),
                    Cause_mort = rep(Cause_mortality,4),
                    Height = mortality,
                    lowerCI = mortality-ci,
                    upperCI = mortality+ci)

cedrus <- png::readPNG("figs/Cedrus_libani.png")
persea <- png::readPNG("figs/Persea.png")
pinus <- png::readPNG("figs/Pinus_taeda.png")

library(tidyverse)
library(ggplot2)
library(viridis)
library(rphylopic)

trees <- trees %>% 
  group_by(Species) %>% 
  arrange(Species)

plot1 <- ggplot(data = trees,
                mapping = aes(x = Species,
                              y = Height,
                              fill = Cause_mort))+
  geom_col(position = 'dodge')+
  scale_fill_viridis(discrete = TRUE)+
  geom_errorbar(aes(ymin = lowerCI,
                    ymax = upperCI),
                width = 0.2,
                position=position_dodge(.9))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "bottom",
        legend.text = element_text(size=10),
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.5, "line"))+
  scale_x_discrete(position = "top") +
  add_phylopic(cedrus,alpha = 0.2, x = 1, y = 10, ysize = 20)+
  add_phylopic(persea, alpha = 0.2, x = 2, y = 15, ysize = 35)+
  add_phylopic(pinus, alpha = 0.2, x = 3, y = 20, ysize = 40)+
  add_phylopic(pinus, alpha = 0.2, x = 4, y = 22.5, ysize = 45)
plot1
