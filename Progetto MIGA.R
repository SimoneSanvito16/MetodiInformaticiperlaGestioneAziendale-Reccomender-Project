#Sanvito Simone 844794
library(psych)
require(Amelia)
library(car)
library(plotly)
library(moments)
library(corrplot)
library(ggplot2)
library(gplots)
library(lattice)
library(lsa)
library(shiny)
library(shinyjs)


#DATA ACQUISITION
#DATA ACQUISITION
#DATA ACQUISITION

#importo il dataset
DFvideo <- read.csv("CAvideos.csv")
View(DFvideo)

#prima occhiata al dataset
dim(DFvideo)
str(DFvideo)
head(DFvideo)
tail(DFvideo)

summary(DFvideo)
describe(DFvideo)

#data cleaning
#data cleaning
missmap(DFvideo, main = "Valori che mancano")
any(is.na(DFvideo))

#rimuovo colonne che non servono, testuali
DFvideo$tags = NULL
DFvideo$thumbnail_link = NULL
DFvideo$description = NULL
na.omit(DFvideo)

#tolgo i video rimossi o che danno errore
DFvideo <- DFvideo[!(DFvideo$video_error_or_removed == "True"),]

#tolgo i video con 0 visualizzazioni
DFvideo <- DFvideo[!(DFvideo$views == 0),]

summary(DFvideo)
head(DFvideo)
tail(DFvideo)
some(DFvideo)
dim(DFvideo)


#EXPLORATORY ANALYSIS
#EXPLORATORY ANALYSIS
#EXPLORATORY ANALYSIS

#views
views_mean <- mean(DFvideo$views)
views_sd <- sd(DFvideo$views)
views_var <- var(DFvideo$views) [1]
views_kurt <- kurtosis(DFvideo$views)
views_skew <- skewness(DFvideo$views)
views_q <- quantile(DFvideo$views)
summary(DFvideo$views)

#likes
likes_mean <- mean(DFvideo$likes)
likes_sd <- sd(DFvideo$likes)
likes_var <- var(DFvideo$likes) [1]
likes_kurt <- kurtosis(DFvideo$likes)
likes_skew <- skewness(DFvideo$likes)
likes_q <- quantile(DFvideo$likes)
summary(DFvideo$likes)


#dislikes
dislikes_mean <- mean(DFvideo$dislikes)
dislikes_sd <- sd(DFvideo$dislikes)
dislikes_var <- var(DFvideo$dislikes) [1]
dislikes_kurt <- kurtosis(DFvideo$dislikes)
dislikes_skew <- skewness(DFvideo$dislikes)
dis_q <- quantile(DFvideo$dislikes)
summary(DFvideo$dislikes)


#commenti
comment_mean <- mean(DFvideo$comment_count)
comment_sd <- sd(DFvideo$comment_count)
comment_var <- var(DFvideo$comment_count) [1]
comment_kurt <- kurtosis(DFvideo$comment_count)
comment_skew <- skewness(DFvideo$comment_count)
com_q <- quantile(DFvideo$comment_count)
summary(DFvideo$comment_count)

dev.off()
#ISTOGRAMMI
#istogrammi di likes, dislikes, views e commenti
#views
hist(DFvideo$views, breaks = 1000, xlim = c(0, 8000000), ylim = c(0, 8000),
     xlab = "Numero di views per video", main = "Istogramma delle views", col = "steelblue")
abline(v = views_mean, type = "l", col = "yellow", lwd = 2)
abline(v=views_q[1], col="red", lwd=2) # 0% (min)
abline(v=views_q[2], col="blue", lwd=2) # 1st quartile 25%
abline(v=views_q[3], col="green", lwd=2.5) # Median value distribution 50%
abline(v=views_q[4], col="blue", lwd=2) # 3rd quartile 75%
abline(v=views_q[5], col="red", lwd=2) # 100% (max)

legend("topright", legend = c("media", "Minimo (0%)", "1° quantile (25%)", "Mediana (50%)", "3° quantile (75%)", "Massimo (100%)"), 
       lwd =2, col = c("yellow", "red", "blue", "green", "blue", "red"))

#zoom sui valori più vicini allo zero per fare migliori osservazioni
hist(DFvideo$views, breaks = 1000, xlim = c(0, 2000000), ylim = c(0, 8000), col = "steelblue")

#likes
hist(DFvideo$likes, xlim = c(0, 450000), breaks = 1000, ylim = c(0, 20000), 
     xlab = "Numero di likes per video", main = "Istogramma dei likes", col = "pink")
abline(v = likes_mean, type = "l", col = "yellow", lwd = 2)
abline(v=likes_q[1], col="red", lwd=2) # 0% (min)
abline(v=likes_q[2], col="blue", lwd=2) # 1st quartile 25%
abline(v=likes_q[3], col="green", lwd=2.5) # Median value distribution 50%
abline(v=likes_q[4], col="blue", lwd=2) # 3rd quartile 75%
abline(v=likes_q[5], col="red", lwd=2) # 100% (max)
legend("topright", legend = c("media", "Minimo (0%)", "1° quantile (25%)", "Mediana (50%)", "3° quantile (75%)", "Massimo (100%)"), 
       lwd =2, col = c("yellow", "red", "blue", "green", "blue", "red"))

#plot per vedere dove sono maggiormente concentrati
plot(DFvideo$likes,type="p")
plot(DFvideo$likes,type="p", ylim = c(0, 500000)) #zoom 1
plot(DFvideo$likes,type="p", ylim = c(0, 50000)) #zoom 2


#dislikes
hist(DFvideo$dislikes, xlim = c(0, 18000), breaks = 1000, ylim = c(0, 40000),
     xlab = "Numero di dislikes per video", main = "Istogramma dei dislikes", col = "gray")
abline(v = dislikes_mean, type = "l", col = "yellow", lwd = 2)
abline(v=dis_q[1], col="red", lwd=2) # 0% (min)
abline(v=dis_q[2], col="blue", lwd=2) # 1st quartile 25%
abline(v=dis_q[3], col="green", lwd=2.5) # Median value distribution 50%
abline(v=dis_q[4], col="blue", lwd=2) # 3rd quartile 75%
abline(v=dis_q[5], col="red", lwd=2) # 100% (max)
legend("topright", legend = c("media", "Minimo (0%)", "1° quantile (25%)", "Mediana (50%)", "3° quantile (75%)", "Massimo (100%)"), 
       lwd =2, col = c("yellow", "red", "blue", "green", "blue", "red"))

#commenti
hist(DFvideo$comment_count, xlim = c(0, 28000), breaks = 1000, ylim = c(0, 20000),
     xlab = "Numero di commenti per video", main = "Istogramma dei commenti", col = "orange")
abline(v = comment_mean, type = "l", col = "yellow", lwd = 2)
abline(v=com_q[1], col="red", lwd=2) # 0% (min)
abline(v=com_q[2], col="blue", lwd=2) # 1st quartile 25%
abline(v=com_q[3], col="green", lwd=2.5) # Median value distribution 50%
abline(v=com_q[4], col="blue", lwd=2) # 3rd quartile 75%
abline(v=com_q[5], col="red", lwd=2) # 100% (max)
legend("topright", legend = c("media", "Minimo (0%)", "1° quantile (25%)", "Mediana (50%)", "3° quantile (75%)", "Massimo (100%)"), 
       lwd =2, col = c("yellow", "red", "blue", "green", "blue", "red"))


#BOXPLOT
#views
boxplot(DFvideo$views, horizontal = TRUE, staplewex = 3)
boxplot(DFvideo$views, horizontal = TRUE, outline = FALSE) #zoommato


#likes
boxplot(DFvideo$likes, horizontal = TRUE, staplewex = 3)
boxplot(DFvideo$likes, horizontal = TRUE, outline = FALSE) #zoommato


#dislikes
boxplot(DFvideo$dislikes, horizontal = TRUE, staplewex = 3)
boxplot(DFvideo$dislikes, horizontal = TRUE, outline = FALSE) #zoommato

#commenti
boxplot(DFvideo$comment_count, horizontal = TRUE, staplewex = 3)
boxplot(DFvideo$comment_count, horizontal = TRUE, outline = FALSE) #zoommato


#QQPLOT
#views
qqnorm(DFvideo$views, pch = 1, frame = FALSE, main = "QQPlot delle views")
qqline(DFvideo$views, col = "steelblue", lwd = 3)

#likes
qqnorm(DFvideo$likes, pch = 1, frame = FALSE, main = "QQPlot dei likes")
qqline(DFvideo$likes, col = "pink", lwd = 3)

#dislikes
qqnorm(DFvideo$dislikes, pch = 1, frame = FALSE, main = "QQPlot dei dislikes")
qqline(DFvideo$dislikes, col = "gray", lwd = 3)

#commenti
qqnorm(DFvideo$comment_count, pch = 1, frame = FALSE, main = "QQPlot dei commenti")
qqline(DFvideo$comment_count, col = "orange", lwd = 3)



#ALTRI GRAFICI
#correlazione tra views, likes, dislikes e commenti
corrplot.mixed(corr=cor(DFvideo[ , c(7:10)],
                        use="complete.obs"),upper="ellipse", tl.pos="lt",
               lower.col = colorpanel(50, "red", "gray60", "blue4"),
               upper.col = colorpanel(50, "red", "gray60", "blue4"))

#scatterplot di confronto tra i 4 indici considerati
pairs(cbind(DFvideo$views, DFvideo$likes, DFvideo$dislikes, DFvideo$comment_count), 
      main = "correlazione tra views, likes, dislikes e commenti", 
      labels=c("views", "likes", "dislikes", "commenti"))
      


#video in tendenza in base alla categoria
ca_df <- as.data.frame(sort(table(DFvideo$category_id), decreasing = TRUE))
names(ca_df) <- c("category_id","count")

ggplot(ca_df, aes(x = category_id, y = count, fill = factor(category_id))) + geom_bar(stat = "identity") + 
  scale_x_discrete(name = "Categoria") + scale_y_continuous(name = "Numero di video") + 
  labs(title = "Video in tendenza in Canada in base alla categoria") 


#diagramma a torta
p5 <- plot_ly(DFvideo, labels = DFvideo$category_id, type = 'pie') %>%
  layout(title = 'Numero di video per categoria',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p5


#boxplot per views per categoria
p <- plot_ly(DFvideo, x = DFvideo$category_id, y = DFvideo$views, type = "box", boxmean = "sd", 
             xlab = "Categorie", ylab = "Numero visualizzazioni") 
p <- layout(p, yaxis = list(type = "log"), title = "Boxplot delle views per categoria")
p

#boxplot per commenti per categoria
p1 <- plot_ly(DFvideo, x = DFvideo$category_id, y = DFvideo$comment_count, type = "box", boxmean = "sd",
              xlab = "Categorie", ylab = "Numero commenti") 
p1 <- layout(p1, yaxis = list(type = "log"), title = "Boxplot dei commenti per categoria")
p1

#boxplot per likes/dislikes per categoria
p2 <- plot_ly(DFvideo, x = DFvideo$category_id, y = DFvideo$likes, type = "box", boxmean = "sd", name = "Likes") 
p2 <- layout(p2, yaxis = list(type = "log"))

p3 <- plot_ly(DFvideo, x = DFvideo$category_id, y = DFvideo$dislikes, type = "box", boxmean = "sd", name = "Dislikes") 
p3 <- layout(p3, yaxis = list(type = "log"))

p4 <- subplot(p2,p3)
p4 <- layout(p4, title = "Boxplot dei likes/dislikes per categoria") 
p4


# 16 canali con maggior numero di views in media (> 20mln)
prova_view = aggregate(DFvideo$views, by=list(DFvideo$channel_title), mean)
colnames(prova_view) = c("canale", "numero_views")
prova_view = prova_view[order(prova_view$numero_views, decreasing=TRUE),]
prova_view = filter(prova_view, numero_views >= 20000000)
ggplot(prova_view,
       aes(x = numero_views, y = canale)) +
  geom_point(size = 4, color = c("red")) + 
  labs(x = 'media delle views', y = 'canale')


#canali con maggior numero di likes in media (> 700000)
prova_like = aggregate(DFvideo$likes, by=list(DFvideo$channel_title), mean)
colnames(prova_like) = c("canale", "numero_likes")
prova_like = prova_like[order(prova_like$numero_likes, decreasing=TRUE),]
prova_like = filter(prova_like, numero_likes >= 700000)
ggplot(prova_like,
       aes(x = numero_likes, y = canale)) +
  geom_point(size = 4, color = c("blue")) + 
  labs(x = 'media dei likes', y = 'canale')


#canali con maggior numero di dislikes in media (> 70000)
prova_dis = aggregate(DFvideo$dislikes, by=list(DFvideo$channel_title), mean)
colnames(prova_dis) = c("canale", "numero_dislikes")
prova_dis = prova_dis[order(prova_dis$numero_dislikes, decreasing=TRUE),]
prova_dis = filter(prova_dis, numero_dislikes >= 70000)
ggplot(prova_dis,
       aes(x = numero_dislikes, y = canale)) +
  geom_point(size = 4, color = c("green")) + 
  labs(x = 'media dei dislikes', y = 'canale')




#ML algorithm
#ML algorithm
#ML algorithm

DF_appoggio <- DFvideo

DF_appoggio$trending_date = NULL
DF_appoggio$channel_title = NULL
DF_appoggio$publish_time = NULL
DF_appoggio$comments_disabled = NULL
DF_appoggio$ratings_disabled = NULL
DF_appoggio$video_error_or_removed = NULL

View(DF_appoggio)

#elimino i valori doppi dei video_id
DF_appoggio <- DF_appoggio[!duplicated(DF_appoggio$video_id), ]

str(DF_appoggio)

#riordino gli indici dei video
rownames(DF_appoggio) <- 1:nrow(DF_appoggio)

DF_matrix <- data.matrix(DF_appoggio, rownames.force = NA)

#inizializzo la matrice di similarita
# similarity_matrix <- matrix(, ncol = nrow(DF_appoggio), nrow = nrow(DF_appoggio))
# 
# for (i in 1:nrow(DF_appoggio)) {
#   for (j in 1:nrow(DF_appoggio)) {
#     if(DF_appoggio$category_id[i] == DF_appoggio$category_id[j]){
#      similarity_matrix[i][j] <- cosine(c(DF_matrix[i, 4], DF_matrix[i, 5], DF_matrix[i, 6], DF_matrix[i, 7]),
#                                        c(DF_matrix[j, 4], DF_matrix[j, 5], DF_matrix[j, 6], DF_matrix[j, 7]))
#      }
#     else similarity_matrix[i][j] <- 0
# 
#   }
# 
# }

#faccio vettore di similarita visto che la matrice era troppo grande di dimensioni
#prendi il video di indice i che viene scelto casualmente nell'intervallo di valori possibili


#i <- 4000
i <- floor(runif(1, min=1, max=24414)) #prendo casualmente l'indice del video target
print(i) #indice del video-target


#modello 1 cosine
#considero solo numero di likes. dislikes, commenti e views
similarity_vector_vld <- vector(, length = nrow(DF_appoggio))
for (j in 1:nrow(DF_appoggio)) {
    similarity_vector_vld[j] <- cosine(c(DF_matrix[i, 4], DF_matrix[i, 5], DF_matrix[i, 6], DF_matrix[i, 7]),
                                   c(DF_matrix[j, 4], DF_matrix[j, 5], DF_matrix[j, 6], DF_matrix[j, 7]))
}

sort_vector_vld <- sort(similarity_vector_vld, decreasing = TRUE, index.return = TRUE)
sort_vector_vld

print(DF_appoggio[i, ])  #video in analisi
print(DF_appoggio[sort_vector_vld$ix[1:6], ]) #video-target + migliori 5 video correlati


#modello 1 pearson
similarity_vector_vld_p <- vector(, length = nrow(DF_appoggio))
for (j in 1:nrow(DF_appoggio)) {
  similarity_vector_vld_p[j] <- cor(c(DF_matrix[i, 4], DF_matrix[i, 5], DF_matrix[i, 6], DF_matrix[i, 7]),
                                     c(DF_matrix[j, 4], DF_matrix[j, 5], DF_matrix[j, 6], DF_matrix[j, 7]))
}

sort_vector_vld_p <- sort(similarity_vector_vld_p, decreasing = TRUE, index.return = TRUE)
sort_vector_vld_p

print(DF_appoggio[i, ])  #video in analisi
print(DF_appoggio[sort_vector_vld_p$ix[1:6], ]) #video-target + migliori 5 video correlati








#modello 2 cosine
#considero numero di likes, dislikes, views, commenti e id della categoria
similarity_vector_ccvld <- vector(, length = nrow(DF_appoggio))
for (j in 1:nrow(DF_appoggio)) {
    similarity_vector_ccvld[j] <- cosine(c(DF_matrix[i, 3], DF_matrix[i, 4], DF_matrix[i, 5], DF_matrix[i, 6], DF_matrix[i, 7]),
                                   c(DF_matrix[j, 3], DF_matrix[j, 4], DF_matrix[j, 5], DF_matrix[j, 6], DF_matrix[j, 7]))
}

sort_vector_ccvld <- sort(similarity_vector_ccvld, decreasing = TRUE, index.return = TRUE)
sort_vector_ccvld


print(DF_appoggio[i, ])   #stampo il video in considerazione
print(DF_appoggio[sort_vector_ccvld$ix[1:6], ])   #video-target + migliori 5 video correlati


#modello 2 pearson
similarity_vector_ccvld_p <- vector(, length = nrow(DF_appoggio))
for (j in 1:nrow(DF_appoggio)) {
  similarity_vector_ccvld_p[j] <- cor(c(DF_matrix[i, 3], DF_matrix[i, 4], DF_matrix[i, 5], DF_matrix[i, 6], DF_matrix[i, 7]),
                                       c(DF_matrix[j, 3], DF_matrix[j, 4], DF_matrix[j, 5], DF_matrix[j, 6], DF_matrix[j, 7]))
}

sort_vector_ccvld_p <- sort(similarity_vector_ccvld_p, decreasing = TRUE, index.return = TRUE)
sort_vector_ccvld_p


print(DF_appoggio[i, ])   #stampo il video in considerazione
print(DF_appoggio[sort_vector_ccvld_p$ix[1:6], ])   #video-target + migliori 5 video correlati






#modello 3 cosine
#considero video della stessa categoria perche piu simili e valuto tra questi quelli di maggiore similarita
#considero numero di likes, dislikes, views, commenti
similarity_vector <- vector(, length = nrow(DF_appoggio))
for (j in 1:nrow(DF_appoggio)) {
  if(DF_appoggio$category_id[i] == DF_appoggio$category_id[j]){
    similarity_vector[j] <- cosine(c(DF_matrix[i, 4], DF_matrix[i, 5], DF_matrix[i, 6], DF_matrix[i, 7]),
                                        c(DF_matrix[j, 4], DF_matrix[j, 5], DF_matrix[j, 6], DF_matrix[j, 7]))
  }
  else similarity_vector[j] <- 0
}

sort_vector <- sort(similarity_vector, decreasing = TRUE, index.return = TRUE)
sort_vector


print(DF_appoggio[i, ])  #video in analisi
print(DF_appoggio[sort_vector$ix[1:6], ])   #video-target + migliori 5 video correlati



#modello 3 pearson
similarity_vector_p <- vector(, length = nrow(DF_appoggio))
for (j in 1:nrow(DF_appoggio)) {
  if(DF_appoggio$category_id[i] == DF_appoggio$category_id[j]){
    similarity_vector_p[j] <- cor(c(DF_matrix[i, 4], DF_matrix[i, 5], DF_matrix[i, 6], DF_matrix[i, 7]),
                                   c(DF_matrix[j, 4], DF_matrix[j, 5], DF_matrix[j, 6], DF_matrix[j, 7]))
  }
  else similarity_vector[j] <- 0
}

sort_vector_p <- sort(similarity_vector_p, decreasing = TRUE, index.return = TRUE)
sort_vector_p


print(DF_appoggio[i, ])  #video in analisi
print(DF_appoggio[sort_vector_p$ix[1:6], ])   #video-target + migliori 5 video correlati






#funzioni per la webapp
#funzioni per la webapp
similarity_cosine_fun <- function(i){
similarity_vector <- vector(, length = nrow(DF_appoggio))
for (j in 1:nrow(DF_appoggio)) {
  if(DF_appoggio$category_id[i] == DF_appoggio$category_id[j]){
    similarity_vector[j] <- cosine(c(DF_matrix[i, 4], DF_matrix[i, 5], DF_matrix[i, 6], DF_matrix[i, 7]),
                                   c(DF_matrix[j, 4], DF_matrix[j, 5], DF_matrix[j, 6], DF_matrix[j, 7]))
  }
  else similarity_vector[j] <- 0
}

sort_vector <- sort(similarity_vector, decreasing = TRUE, index.return = TRUE)

return(DF_appoggio[sort_vector$ix[1:6], ])   #video-target + migliori 5 video correlati
}


similarity_pearson_fun <- function(i){
similarity_vector_p <- vector(, length = nrow(DF_appoggio))
for (j in 1:nrow(DF_appoggio)) {
  if(DF_appoggio$category_id[i] == DF_appoggio$category_id[j]){
    similarity_vector_p[j] <- cor(c(DF_matrix[i, 4], DF_matrix[i, 5], DF_matrix[i, 6], DF_matrix[i, 7]),
                                  c(DF_matrix[j, 4], DF_matrix[j, 5], DF_matrix[j, 6], DF_matrix[j, 7]))
  }
  else similarity_vector[j] <- 0
}

sort_vector_p <- sort(similarity_vector_p, decreasing = TRUE, index.return = TRUE)

return(DF_appoggio[sort_vector_p$ix[1:6], ])   
}


#WEB APP
#WEB APP
#WEB APP

source(paste(getwd(), "Server.R", sep="/"))
source(paste(getwd(), "Ui.R", sep="/"))
shinyApp(ui = ui, server = server)


