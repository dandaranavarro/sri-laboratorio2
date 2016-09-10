library(tm)
library(FastKNN)
library(SnowballC)

#path <- "C:/Users/Dandara/Documents/UFCG/2016.1/SRI/laboratorio2/legendas/"
path <- "C:/Users/Dandara/Documents/UFCG/2016.1/SRI/laboratorio2/legendas/"


file.names <- dir(path)

#Colocando todas as legendas em uma lista em formato de documento
legendas <- c()

for(i in 1:length(file.names)){
  filename <- paste(path, file.names[i], sep="")
  doc <- readChar(filename,file.info(filename)$size)
  legendas <- c(legendas, doc)
}

filme1 <- "Ace Ventura Pet Detective.DVDRip.BugBunny.br.srt"
filme2 <- "Poltergeist(1982).br.srt"
filme3 <- "Lord of the Rings The Two Towers The.DVDRip.SecretMyth.br.srt"
filme4 <- "Fantastic Four.DVDRip.br.srt"
filme5 <- "Frozen.720p.BlueRay.YIFY.br.srt"

filmes_teste <- c(filme1,filme2, filme3, filme4, filme5)
docs_teste <- c()

#Transformando os filmes que serao testes em documentos
for(i in 1:length(filmes_teste)){
  filme_teste <- paste(path, filmes_teste[i], sep="")
  doc_teste <- readChar(filme_teste,file.info(filme_teste)$size)
  docs_teste <- c(docs_teste, doc_teste)
}

#Adicionando esses filmes ao vetor completo
my.treino <- VectorSource(c(legendas, docs_teste))

# Corpus do treino sendo tratado
my.treino.corpus <- Corpus(my.treino)
my.treino.corpus <- tm_map(my.treino.corpus, removePunctuation)
my.treino.corpus <- tm_map(my.treino.corpus, removeWords, stopwords("portuguese"))
my.treino.corpus <- tm_map(my.treino.corpus, stemDocument)
my.treino.corpus <- tm_map(my.treino.corpus, removeNumbers)
my.treino.corpus <- tm_map(my.treino.corpus, stripWhitespace)

matrix.treino.stm <- DocumentTermMatrix(my.treino.corpus)

matrix.treino <- as.matrix(matrix.treino.stm)

tfidf.matrix.treino = weightTfIdf(matrix.treino.stm)

#O treino são todas as matrizes TF-IDF das legendas
treino <-  tfidf.matrix[1:646, ]

#Cada teste eh uma matriz TF-IDF do filme pedido
teste1 <- tfidf.matrix[647,]
teste2 <- tfidf.matrix[648,]
teste3 <- tfidf.matrix[649,]
teste4 <- tfidf.matrix[650,]
teste5 <- tfidf.matrix[651,]

#Distancias de cada filme para os outros
matriz.distancia1 <-  Distance_for_KNN_test(teste1, treino)
matriz.distancia2 <-  Distance_for_KNN_test(teste2, treino)
matriz.distancia3 <-  Distance_for_KNN_test(teste3, treino)
matriz.distancia4 <-  Distance_for_KNN_test(teste4, treino)
matriz.distancia5 <-  Distance_for_KNN_test(teste5, treino)

#Os cinco filmes mais relacionados de cada filme 
proximos1 <- k.nearest.neighbors(1,matriz.distancia1, k=5)
proximos2 <- k.nearest.neighbors(1,matriz.distancia2, k=5)
proximos3 <- k.nearest.neighbors(1,matriz.distancia3, k=5)
proximos4 <- k.nearest.neighbors(1,matriz.distancia4, k=5)
proximos5 <- k.nearest.neighbors(1,matriz.distancia5, k=5)

#Criando uma lista com os nomes de cada um dos filmes mais proximos,pois 
#antes so tinha as posicoes dos filmes na lista de legendas.
filmes.proximos1 <- c()
filmes.proximos2 <- c()
filmes.proximos3 <- c()
filmes.proximos4 <- c()
filmes.proximos5 <- c()

for (j in 1:5){
  filmes.proximos1[j] <- file.names[proximos1[j]]
  filmes.proximos2[j] <- file.names[proximos2[j]]
  filmes.proximos3[j] <- file.names[proximos3[j]]
  filmes.proximos4[j] <- file.names[proximos4[j]]
  filmes.proximos5[j] <- file.names[proximos5[j]]
}

#Para imprimir as listas dos filmes mais proximos de cada um
filmes.proximos1
filmes.proximos2
filmes.proximos3
filmes.proximos4
filmes.proximos5


