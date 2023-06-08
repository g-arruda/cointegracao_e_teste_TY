library(tidyverse);library(janitor);library(lubridate)



# Produção anual ----------------------------------------------------------

anual.prod <- read_csv("bibliometria/Annual_Production.csv") %>% 
  clean_names() %>% 
  mutate(year = ymd(.$year, truncated = 2L))

plt.anual.prod <- anual.prod %>% 
  ggplot(aes(x=year, y=articles))+
  geom_line()+
  labs(x="Ano", y="Artigos")+
  theme_bw()+
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 13))


ggsave("bibliometria/img/anual-prod.png", plot = plt.anual.prod,
       width = 20, height = 8, units = "cm")



# impacto da fonte --------------------------------------------------------

source.impact <- read_csv("bibliometria/Source_Impact.csv")[1:10,] %>% 
  clean_names()

plt.src.impact <- source.impact %>% 
  select(element, h_index) %>% 
  arrange(h_index) %>% 
  mutate(element = factor(element, levels = element)) %>% 
  ggplot(aes(x = element, y = h_index)) +
  geom_segment(aes(xend = element, yend = 0))+
  geom_point(size = 7.5, pch = 21, bg = 4, col = 1)+
  geom_text(aes(label = h_index), color = "white", size = 3) +
  coord_flip()+
  labs(y = "Indice H", x="")+
  theme_bw()+
  theme(axis.title = element_text(size = 13))


ggsave("bibliometria/img/src-impact.png", plot = plt.src.impact,
       width = 20, height = 8, units = "cm")


# produção por pais -------------------------------------------------------

country.prod <- read_csv("bibliometria/Country_Production.csv") %>% 
  clean_names()

plt.country.prod <- country.prod %>% 
  arrange(freq) %>% 
  mutate(region = factor(region, levels = region)) %>% 
  ggplot(aes(x = region, y = freq)) +
  geom_segment(aes(xend = region, yend = 0))+
  geom_point(size = 7.5, pch = 21, bg = 4, col = 1)+
  geom_text(aes(label = freq), color = "white", size = 3) +
  coord_flip()+
  labs(y = "Artigos produzidos", x="")+
  theme_bw()+
  theme(axis.title = element_text(size = 13))

ggsave("bibliometria/img/country-prod.png", plot = plt.country.prod,
       width = 15, height = 8, units = "cm")


# artigos citados ---------------------------------------------------------

influent.paper <- read_csv("bibliometria/Most_Global_Cited_Documents.csv") %>% 
  clean_names()

plt.influent.paper <- influent.paper %>% 
  select(paper, total.cit = total_citations) %>% 
  arrange(total.cit) %>% 
  mutate(paper = factor(paper, levels = paper)) %>%
  ggplot(aes(x = paper, y = total.cit)) +
  geom_segment(aes(xend = paper, yend = 0))+
  geom_point(size = 7.5, pch = 21, bg = 4, col = 1)+
  geom_text(aes(label = total.cit), color = "white", size = 3) +
  coord_flip()+
  labs(y = "Artigos mais citados", x="")+
  theme_bw()+
  theme(axis.title = element_text(size = 13))

ggsave("bibliometria/img/influent-paper.png", plot = plt.influent.paper,
       width = 20, height = 8, units = "cm")



# author impact -----------------------------------------------------------

author.impact <- read_csv("bibliometria/Author_Impact.csv") %>% 
  clean_names()

plt.author.impact <- author.impact %>% 
  select(element, h_index) %>% 
  arrange(h_index) %>% 
  mutate(element = factor(element, levels = element)) %>% 
  ggplot(aes(x = element, y = h_index)) +
  geom_segment(aes(xend = element, yend = 0))+
  geom_point(size = 7.5, pch = 21, bg = 4, col = 1)+
  geom_text(aes(label = h_index), color = "white", size = 3) +
  coord_flip()+
  labs(y = "Indice H", x="")+
  theme_bw()+
  theme(axis.title = element_text(size = 13))
  

ggsave("bibliometria/img/author-impact.png", plot = plt.author.impact,
       width = 15, height = 8, units = "cm")
