
##**********************Uniform Probability******************************##
##***********************************************************************##
library(tidyverse)
library(readxl)

uniform_probability = read_xlsx(file.choose(), sheet = 2)

colnames(uniform_probability)

ggplot(data = uniform_probability, mapping = aes(x = `Net payoff ratio`, y = `E U(M)`))+
  geom_smooth(aes(color = BestStrategyChoice))+
  labs(
    x = "Net payoff ratio \n\n Figure 5",
    y = "Expected Utility (multilateral)",
    title = "Changing pattern of cooperation with payoff ratio",
    subtitle = "Uniformly distributed strategies",
  ) +
  geom_vline(xintercept = 5.18, color = "green",linetype =5)+
  scale_color_discrete(name = "Best Strategy")+
  geom_text(x = 5.18, y = -2.0, mapping = aes(label = "5.18"))+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.50),
        plot.subtitle = element_text( size = 10, hjust = 0.50),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ggtitle(expression(bold(paste("Changing pattern of cooperation with payoff ratio"," ", frac(B[m]-C[m], 2*B[b]-C[b])))))

  ggsave("uniform_dist2.png")

##***********International trade*****************************************## 
  
trade = read_xlsx(file.choose(), sheet = 3)

colnames(trade)

ggplot(data = trade, mapping = aes(x = `P(M)`, y = `E U(M)`))+
  geom_smooth(aes(color = BestStrategyChoice))+
  labs(
    x = "Proportion of players playing multilateral strategy \n\n Figure 3",
    y = "Expected Utility (multilateral)",
    title = "Changing pattern of cooperation with multilateral probability simulations",
    subtitle = "Issue area: International trade\nStag-elephant hunt game",
  ) +
  geom_vline(xintercept = 0.275, color = "green",linetype =5)+
  scale_color_discrete(name = "Best Strategy")+
  geom_text(x = 0.27, y = -2.5, mapping = aes(label = "0.27"))+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.50),
        plot.subtitle = element_text( size = 10, hjust = 0.50),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
 ggsave("Trade_figure_3proportion.png")

##**********Foreign Direct Investment************************************##
##***********************************************************************##

fdi = read_xlsx(file.choose(), sheet = 5)

colnames(fdi)

ggplot(data = fdi, mapping = aes(x = `P(B)`, y = `E U(B)`))+
  geom_smooth(aes(color = BestStrategyChoice))+
  labs(
    x = "Proportion of players playing bilateral strategy \n\n Figure 1 (a)",
    y = "Expected Utility (bilateral)",
    title = "Changing pattern of cooperation with bilateral probability simulations",
    subtitle = "Issue area: Foreign direct investment\nStag-elephant hunt game",
  ) +
  geom_vline(xintercept = 0.15, color = "green",linetype =5)+
  scale_color_discrete(name = "Best Strategy")+
  geom_text(x = 0.15, y = -2.5, mapping = aes(label = "0.15"))+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.50),
        plot.subtitle = element_text( size = 10, hjust = 0.50),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave("FDI_1aproportion.png")

##**F************Foreign Direct Investment****
##********************************************

fdi = read_xlsx(file.choose(), sheet = 4)

colnames(fdi)

ggplot(data = fdi, mapping = aes(x = `P(M)`, y = `E U(M)`))+
  geom_smooth(aes(color = BestStrategyChoice))+
  labs(
    x = "Proportion of players playing multilateral strategy \n\n Figure 1 (b)",
    y = "Expected Utility (multilateral)",
    title = "Changing pattern of cooperation with multilateral probability simulations",
    subtitle = "Issue area: Foreign direct investment\nStag-elephant hunt game",
  ) +
  geom_vline(xintercept = 0.48, color = "green",linetype =5)+
  scale_color_discrete(name = "Best Strategy")+
  geom_text(x = 0.48, y = -2.5, mapping = aes(label = "0.48"))+
  theme_bw()+
  theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.50),
        plot.subtitle = element_text( size = 10, hjust = 0.50),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
 ggsave("FDI_1bproportion.png")
