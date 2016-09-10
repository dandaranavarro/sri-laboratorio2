library(tm)
library(FastKNN)
library(SnowballC)

dir_treino <- "C:/Users/Treinamento Xiaomi/Desktop/Dandara/treino-parte2/"
dir_teste <- "C:/Users/Treinamento Xiaomi/Desktop/Dandara/teste-parte2/"

treino.names <- dir(dir_treino)
teste.names <- dir(dir_teste)

#Colocando todas as legendas de treino e teste em uma lista em formato de documento
conj.treinos <- c()
conj.testes <- c()

#Treino
for(i in 1:length(treino.names)){
  filename <- paste(path, treino.names[i], sep="")
  doc <- readChar(filename,file.info(filename)$size)
  conj.treinos <- c(conj.treinos, doc)
}
#Teste
for(j in 1:length(teste.names)){
  filename <- paste(path, teste.names[j], sep="")
  doc <- readChar(filename,file.info(filename)$size)
  conj.testes <- c(conj.testes, doc)
}

#COlocando o treino e o teste em um vetor
my.docs <- VectorSource(c(conj.treinos, conj.testes))

# Corpus do treino sendo tratado
my.docs.corpus <- Corpus(my.docs)
my.docs.corpus <- tm_map(my.docs.corpus, removePunctuation)
my.docs.corpus <- tm_map(my.docs.corpus, removeWords, stopwords("portuguese"))
my.docs.corpus <- tm_map(my.docs.corpus, stemDocument)
my.docs.corpus <- tm_map(my.docs.corpus, removeNumbers)
my.docs.corpus <- tm_map(my.docs.corpus, stripWhitespace)

matrix.docs.stm <- DocumentTermMatrix(my.docs.corpus)

matrix.docs <- as.matrix(matrix.docs.stm)

tfidf.matrix.docs = weightTfIdf(matrix.docs.stm)

#O treino são todas as matrizes TF-IDF dos filmes de treino
treino <-  tfidf.matrix.docs[1:636, ]
treino.matriz <- as.matrix(treino)

#O teste sao todas as matrizes TF-IDF dos filmes passados como teste
teste <- tfidf.matrix.docs[637:646,]

#Distancias dos testes para os treinos
matriz.distancia <- Distance_for_KNN_test(teste, treino)

label.training <- read.table("C:/Users/Treinamento Xiaomi/Desktop/Dandara/label-treino.txt", sep="&")

generos.matrix <- as.matrix(label.training)

#Generos existentes: Aventura, ação, crime, comédia, drama, terror,comédia romântica, 
#ficção científica, mistério, infantil, documentário, musical

just.genres <- c(generos.matrix[1:636,2])

# KNN 
knn_test_function(treino, teste, matriz.distancia, just.genres, k = 20)


knn_training_function(treino, treino.matriz, just.genres, k = 1)


