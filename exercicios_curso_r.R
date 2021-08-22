library(lubridate)
library(dplyr)
library(ggplot2)
library(magrittr)

# exercícios 7.2 - O pacote dplyr

imdb <- readRDS("C:/Users/matheus/Desktop/analise_filmes/imdb.rds")

#Teste aplicar a função glimpse() do pacote {dplyr} à base imdb. O que ela faz?

glimpse(imdb) # me da um resumo da base: n° linhas, colunas, variáveis, etc.

# Crie uma tabela com apenas as colunas titulo, diretor, e orcamento. Salve em um objeto chamado imdb_simples.

imdb_simples <- imdb %>%
                  select(titulo, diretor, orcamento)

# Selecione apenas as colunas ator_1, ator_2 e ator_3 usando o ajudante contains()

?contains

imdb %>%
  select(contains("ator")) # nao preciso passar o nome completo, ela ira pegar todos que têm "ator"

#Usando a função select() (e seus ajudantes), escreva códigos que retornem a base IMDB sem as colunas ator_1,
#ator_2 e ator_3. Escreva todas as soluções diferentes que você conseguir pensar

imdb %>%
  select(!contains("ator"))

imdb %>%
  select(!starts_with("ator"))

imdb %>%
  select(-ator_1,-ator_2,-ator_3)

imdb %>%
  select(-contains("ator"))

imdb %>%
  select(-(ator_1:ator_3))

# Ordene os filmes em ordem crescente de ano e decrescente de receita e salve em um objeto chamado filmes_ordenados.

filmes_ordenados <- imdb %>%
                      arrange(ano, desc(receita))

# Selecione apenas as colunas titulo e orcamento e então ordene de forma decrescente pelo orcamento.
                      
imdb %>%
  select(titulo, orcamento) %>%
  arrange(desc(orcamento))

# Crie um objeto chamado filmes_pb apenas com filmes preto e branco

# nao preciso ir no dataframe ver os valores unicos, posso usar:

unique(imdb$cor)

filmes_pb <- imdb %>%
  filter(cor == "Black and White")

# Crie um objeto chamado curtos_legais com filmes de 90 minutos ou menos de duração e nota no imdb maior do que 8.5.


curtos_legais <- imdb %>%
  filter(duracao <= 90, nota_imdb > 8.5)

# Retorne tabelas (tibbles) apenas com

#filmes coloridos anteriores a 1950;
imdb %>%
  filter(cor == "Color", ano < 1950)

# filmes do “Woody Allen” ou do “Wes Anderson”;
imdb %>%
  filter(diretor %in% c("Woody Allen", "Wes Anderson"))

imdb %>%
  filter(diretor == "Woody Allen" | diretor == "Wes Anderson")


# filmes do “Steven Spielberg” ordenados de forma decrescente por ano, mostrando apenas as colunas titulo e ano

imdb %>%
  filter(diretor == "Steven Spielberg") %>%
  select(titulo, ano) %>%
  arrange(desc(ano))

# filmes que tenham “Action” ou “Comedy” entre os seus gêneros;
library(stringr)

imdb %>%
  filter(str_detect(generos, "Action") | str_detect(generos, "Comedy"))

# filmes que tenham “Action” e “Comedy” entre os seus gêneros e tenha nota_imdb maior que 8;

imdb %>%
  filter(str_detect(generos, "Action") & str_detect(generos, "Comedy") & nota_imdb > 8)

# por conta da biblioteca dplyr ter C e C++ atrás dela, | e & são válidos, que é o que aprendi em C

# há um erro na resposta do site, ele não levou em conta a nota > 8

# filmes que não possuem informação tanto de receita quanto de orçamento (isto é, possuem NA em ambas as colunas).

apenas_na <- imdb %>%
  filter(is.na(orcamento), is.na(receita))  # terá apenas valores NA nessas colunas, se eu quiser excluir elas, faço:

sem_na <- imdb %>%
  filter(!is.na(orcamento), !is.na(receita)) # nao tera nenhuma NA nessas colunas


# parei no tópico 7.2.5












