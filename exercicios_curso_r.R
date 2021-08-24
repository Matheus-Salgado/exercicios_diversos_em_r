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


# Crie uma coluna chamada prejuizo (orcamento - receita) e salve a nova tabela em um objeto chamado imdb_prejuizo.
#Em seguida, filtre apenas os filmes que deram prejuízo e ordene a tabela por ordem crescente de prejuízo.


imdb_prejuizo <- imdb %>%
  mutate(prejuizo = orcamento - receita) 

imdb_prejuizo %>%
  filter(prejuizo > 0) %>%
  arrange(prejuizo)

# Fazendo apenas uma chamada da função mutate(), crie as seguintes colunas novas na base imdb:

a<-imdb %>%
  mutate(lucro = receita - orcamento,
         lucro_medio = mean(lucro, na.rm = TRUE), 
         lucro_relativo = (lucro - lucro_medio)/lucro_medio, 
         houve_lucro = ifelse(lucro > 0, "sim", "nao")
  )

#  Crie uma nova coluna que classifique o filme em "recente" (posterior a 2000) e "antigo" (de 2000 para trás).

imdb %>%
  mutate(tempo = ifelse (ano > 2000, "recente", "antigo")) %>%
  group_by(tempo) %>%
  filter(!is.na(receita)) %>%
  summarise(receita_media = mean(receita, na.rm = TRUE))


?summarise


# Calcule a duração média e mediana dos filmes da base

imdb %>%
  summarise(media_duracao = mean(duracao, na.rm = TRUE), 
            mediana_duracao = median(duracao, na.rm = TRUE))

# Calcule o lucro médio dos filmes com duração menor que 60 minutos.

imdb %>%
  filter(duracao < 60) %>%
  mutate(lucro = receita - orcamento) %>%
  summarise(lucro_medio = mean(lucro, na.rm = TRUE))


# Apresente na mesma tabela o lucro médio dos filmes com duracao menor que 60 minutos e o
# lucro médio dos filmes com duracao maior ou igual a 60 minutos.

imdb %>%
  mutate(
    tempo_filme = ifelse(duracao >= 60, "longo", "curto"),
    lucro = receita - orcamento
  ) %>%
  group_by(tempo_filme) %>%
  filter(!is.na(lucro)) %>%
  summarise(lucro_medio = mean(lucro, na.rm = TRUE))

# Retorne tabelas (tibbles) apenas com:

# a nota IMDB média dos filmes por tipo de classificacao;

imdb %>%
  group_by(classificacao) %>%
  summarise(nota_media = mean(nota_imdb))

# a receita média e mediana dos filmes por ano;

imdb %>%
  group_by(ano) %>%
  filter(!is.na(ano) & !is.na(receita)) %>%
  summarise(receita_media = mean(receita, na.rm = TRUE),
            receita_mediana = median(receita, na.rm = TRUE)
            )

# apenas o nome dos diretores com mais de 10 filmes.

imdb %>%
  filter(!is.na(diretor)) %>%
  group_by(diretor) %>%
  summarise(num_filmes = n() ) %>%
  filter(num_filmes > 10) %>%
  select(diretor)

# Salve em um novo objeto uma tabela com a nota média dos filmes de cada diretor.
#Essa tabela deve conter duas colunas (diretor e nota_imdb_media) e cada linha deve ser um diretor diferente.


media_diretores <- imdb %>%
  group_by(diretor) %>%
  summarise(nota_imdb_diretor = mean(nota_imdb, na.rm = TRUE))

# Use o left_join() para trazer a coluna nota_imdb_media da tabela do item anterior para a tabela imdb original.

imdb <- imdb %>%
  left_join(media_diretores, by = "diretor")










