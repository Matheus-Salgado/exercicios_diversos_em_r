library(lubridate)
library(dplyr)
library(ggplot2)
library(magrittr)
library(plotly)


imdb <- readRDS("C:/Users/matheus/Desktop/analise_filmes/imdb.rds")

View(imdb)

imdb %>%
  select(orcamento, receita) %>%
  mutate(orcamento = orcamento/1000000, receita = receita/1000000) %>%
  ggplot(aes(x = orcamento, y = receita)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Mapeamento de filmes dado sua receita e seu orçamento", x = "orçamento [milhões]", y = "receita [milhões]")
  
# A quantidade de filmes que dão lucro é maior do que a quantidade de filmes que dão prejuízo
# outra forma

a<-imdb %>%
  mutate(receita = receita/1000000,
         orcamento = orcamento/1000000,
         lucro = receita - orcamento,
         lucro = ifelse(lucro<=0, "nao", "sim")) %>%
  filter(!is.na(lucro)) %>%
  ggplot(aes(x = orcamento, y = receita, col = lucro)) +
  geom_point() +
  labs(title = "Mapeamento de filmes dado sua receita e seu orçamento", x = "orçamento [milhões]", y = "receita [milhões]")
  
ggplotly(a)

# qual a porcentagem de filmes que deram lucro?
contador <- imdb %>%
  mutate(receita = receita/1000000,
         orcamento = orcamento/1000000,
         lucro = receita - orcamento,
         lucro = ifelse(lucro<=0, "nao", "sim")) %>%
  count(lucro) %>%
  filter(!is.na(lucro))
  
porcentagem_lucro <- contador[2,2]/sum(contador$n)*100
porcentagem_lucro


# como estão as notas médias ao longo dos anos?


b<-imdb %>%
  group_by(ano) %>%
  mutate(nota_media = mean(nota_imdb, na.rm = TRUE)) %>%
  ggplot(aes(x = ano, y = nota_media)) +
  geom_line() +
  geom_point()

ggplotly(b)

# há uma tendência de queda


# quais são os 20 atores com maiores médias?

imdb %>%
  group_by(ator_1) %>%
  mutate(nota_media = mean(nota_imdb, na.rm = TRUE)) %>%
  arrange(desc(nota_media)) %>%
  head(20) %>%
  ggplot(aes(x = nota_media, y = reorder(ator_1, nota_media), fill = nota_media)) +
  geom_col() +
  geom_label(aes(label = nota_media)) +
  labs(title = "Top atores de acordo com as notas médidas", x = "nota media", y = "ator")

# quais são os 20 atores com menores médias?

imdb %>%
  group_by(ator_1) %>%
  mutate(nota_media = mean(nota_imdb, na.rm = TRUE),
         nota_media = round(nota_media, 2)) %>%
  arrange((nota_media)) %>%
  head(20) %>%
  ggplot(aes(x = nota_media, y = reorder(ator_1, nota_media), fill = nota_media)) +
  geom_col() +
  geom_label(aes(label = nota_media)) +
  labs(title = "Atores com as piores médias", x = "nota media", y = "ator")



# quais diretores fizeram mais filmes?

imdb %>%
  filter(!is.na(diretor)) %>%
  count(diretor) %>%
  top_n(10,n) %>%
  ggplot(aes(x = n, y = reorder(diretor, n), fill = n)) +
  geom_col() + 
  geom_label(aes(x = n+1, label = n), fill = 0.5) +
  labs(title = "Diretores com mais produções", x = "quantidade de filmes", y = "diretor") +
  theme(legend.position = "none")


# como está o lucro dos diretores que produziram mais do que 16 filmes?

imdb %>%
  filter(!is.na(diretor)) %>%
  group_by(diretor) %>%
  filter(n() >= 16) %>%
  mutate(lucro = (receita - orcamento)/1000000) %>%
  filter(!is.na(lucro)) %>%
  ggplot(aes(x = reorder(diretor,lucro), y = lucro)) +
  geom_boxplot() +
  labs(x = "diretor", y = "lucro [milhoes]")
  
# apesar de o woody allen ter feito bastante filmes, ele obteve lucro em apenas 2, que são considerados como outliers
# martin scorsese e spike lee tiveram lucro em cerca de 50% dos seus filmes
# Tim burton teve lucro em cerca de 75% dos seus filmes
# Já Steven spielberg teve lucro em mais de 75% dos seus filmes, alem de ter o filme com o maior lucro entre esses diretores











