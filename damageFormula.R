# Library
library(tidyverse)

figherAtKBase <- 100 
figtherCPM <- 0.9

MovePower <- 9
Multipliers <- 1.2


defnderDefBase <- 100
defenderCPM <- 0.9

defenderStm <- 115


scale <- expand.grid(0:15, 0:15)
names(scale) <- c('Atk', 'Def')

scale$KOTime <- mapply(function(Atk, Def) defenderStm / floor(1/2*MovePower*((figherAtKBase + Atk) * figtherCPM)/
                                                           ((defnderDefBase + Def) * defenderCPM) * Multipliers) + 1,
                                      Atk = scale$Atk, Def = scale$Def)

ggplot(scale, aes(x=KOTime)) + 
  geom_histogram(color="black", fill="white")

# Basic scatterplot
ggplot(scale, aes(x=Atk, y=Def, colour=KOTime)) +
  geom_point() +
  scale_colour_gradientn(colours = terrain.colors(10))
