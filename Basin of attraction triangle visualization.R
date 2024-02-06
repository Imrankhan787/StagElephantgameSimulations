
#install.packages("ggtern")
library(ggtern)

library(tidyverse)

trade_Simulation <- read_csv(file.choose())

ggtern(data = trade_Simulation, aes(x = m_inital, y = b_initial, z = u_initial))+
  geom_point(aes(colour = coop_outcome))+
  scale_color_discrete(name = "Pattern of Cooperation")+
  scale_L_continuous(name ="M"  , breaks = NULL, labels = NULL) +
  scale_R_continuous(name ="U" , breaks = NULL, labels = NULL ) +
  scale_T_continuous(name ="B" , breaks = NULL , labels = NULL ) +
  labs(
    title = "Case : International Trade \n errors in learning cooperation strategies"
  )+
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.50))



