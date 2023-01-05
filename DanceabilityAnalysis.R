#--------------------Graphs generation script-----------------#

library(tidyverse)
library(dplyr)

#1-Récupérationdu dataset contenant des musiques datées par leur année de sortie de 1921 à 2020
#ne pas oublier de modifier ici le path de récupération du dataset
yearDataset <- read.csv("./Analyse_exploratoire_Spotify_Study/spotifyTracks.csv", header=T)
str(yearDataset)

#2-Récupérationdu dataset contenant des musiques et leur genre

#ne pas oublier de modifier ici le path de récupération du dataset
genreDataset <- read.csv("./Analyse_exploratoire_Spotify_Study/datasetSpotify.csv", header=T)
str(genreDataset)
#pour pouvoir combiner les datasets
colnames(genreDataset)[colnames(genreDataset) == "track_id"] ="id"


#3-Création d'un clean dataset exploitable combinant les deux datasets précédents

cleanDataset = merge(x = spotifyDataset, y = genreDataset, by="id")

spotifyTracks <- cleanDataset[,!names(cleanDataset) %in% c("artists.y", "popularity.y", "duration_ms.y", "explicit.y", "danceability.y", "energy.y", "key.y", "loudness.y", "mode.y", "speechiness.y", "acousticness.y", "instrumentalness.y", "liveness.y", "valence.y", "tempo.y")]
colnames(spotifyTracks)[colnames(spotifyTracks) == "track_genre....................................."] ="track_genre"


#Graphique 1 : Évolution de la danceability en fonction des années de 1921 à 2020

#on garde danceability & year
dataFrame2 <- subset(spotifyTracks, select=c(year, danceability.x)) %>% group_by(year) %>% arrange(year)
yearEvolution <- dataFrame2 %>% summarise(danceability.x=mean(danceability.x))


ggplot(yearEvolution, aes(x=year, y=danceability.x)) + geom_point(color="darkgoldenrod2", size=2) + geom_line(color="coral2", size=1) + scale_x_continuous(breaks = seq(1920,2020,5))


# Graphique 2 : Moyenne de la danceability par genre de musique 

dataFrame1 <- subset(spotifyTracks, select=c(track_genre, danceability.x))  %>% group_by(track_genre) %>% arrange(track_genre)
genreMeanbis <- dataFrame1 %>% summarise(danceability.x=mean(danceability.x))

#on nettoie les noms de genre pour pouvoir les grouper
for (n in 1:nrow(genreMeanbis[1])){
  str(genreMeanbis[n,1])
  genreMeanbis[n,1] <- str_replace_all(genreMeanbis[n,1],";", "")
  
}

genreMean <- genreMeanbis %>% group_by(track_genre) %>% summarise(danceability.x=mean(danceability.x))
#On met toutes les musiques sans genre à autre
genreMean[1,1] <- "other"
genreMean <- genreMean %>% arrange(-danceability.x)

#on selectionne le top 20 en terme de danceability
genreMean20 <- genreMean[1:20,1:2]
colors = c("cadetblue", "cadetblue1", "cadetblue2", "cadetblue3", "cadetblue4", "darkseagreen", "darkseagreen1", "darkseagreen2", "darkseagreen3", "darkseagreen4", "darkgoldenrod", "darkgoldenrod1", "darkgoldenrod2", "darkgoldenrod3", "darkgoldenrod4", "coral", "coral1", "coral2", "coral3", "coral4")
genreMean20 = cbind(colors, genreMean20)

ggplot(genreMean20, aes(x=reorder(track_genre, danceability.x), y=danceability.x, group=1)) + geom_bar(position="dodge",  stat="identity", color ="aliceblue", fill=colors) + ylab("danceability") + xlab("track genre")+ coord_flip()


#Graphique 3 : Quels genres musicaux pour faire danser les populations de 1977-78 et 1990

dataFrame3 <- subset(spotifyTracks, select=c(track_genre, year))  %>% group_by(year) %>% arrange(year)

#----------1977-1978----------#
#on ne conserve que les musiques de 1977 et 1978 
genre1977e8 <- subset(dataFrame3, (year=="1977"|year=="1978"))

#on nettoie les noms de genre pour pouvoir les grouper
for (n in 1:nrow(genre1977e8[1])){
  #str(genre1977e8[n,1])
  genre1977e8[n,1] <- str_replace_all(genre1977e8[n,1],";", "")
  
}
mean7778 <-  genre1977e8 %>% group_by(track_genre) %>% arrange(track_genre) %>% count(track_genre)
mean7778 <- mean7778 %>% arrange(-n)
mean7778[16,1] <- "other"
mean7778$n <- (mean7778$n/nrow(genre1977e8[1]))*100


#top 5 des années 1977-178
top5Y77e8 <- mean7778[1:5,1:2]
colors1 = c("darkseagreen", "darkseagreen1", "darkseagreen2", "darkseagreen3", "darkseagreen4")
top5Y77e8 = cbind(top5Y77e8, colors1)
#colnames(top5Y77e8)[colnames(top5Y77e8) == "...3"] ="colors1"

ggplot(top5Y77e8, aes(x=reorder(track_genre, -n), y=n, group=1)) + geom_bar(position="dodge",  stat="identity", color ="aliceblue", fill=colors1) + ylab("Pourcentage de musiques écoutées") + xlab("track genre")

  
#------------1990------------#
#on ne conserve que les musiques de 1977 et 1978 
genre1990 <- subset(dataFrame3, year=="1990")

#on nettoie les noms de genre pour pouvoir les grouper
for (n in 1:nrow(genre1990[1])){
  #str(genre1977e8[n,1])
  genre1990[n,1] <- str_replace_all(genre1990[n,1],";", "")
  
}
mean90 <-  genre1990 %>% group_by(track_genre) %>% arrange(track_genre) %>% count(track_genre)
mean90 <- mean90 %>% arrange(-n)
mean90[16,1] <- "other"
mean90$n <- (mean90$n/nrow(genre1990[1]))*100

#on selectionne le top 20 en terme de danceability
top5Y90 <- mean90[1:5,1:2]
colors2 = c("darkgoldenrod", "darkgoldenrod1", "darkgoldenrod2", "darkgoldenrod3", "darkgoldenrod4")
top5Y90 = cbind(top5Y90, colors2)
#colnames(top5Y90)[colnames(top5Y90) == "...3"] ="colors2"

ggplot(top5Y90, aes(x=reorder(track_genre, -n), y=n, group=1)) + geom_bar(position="dodge",  stat="identity", color ="aliceblue", fill=colors2) + ylab("Pourcentage de musiques écoutées") + xlab("track genre")

# Graphe 5 : la danceability du top 25 des chansons sur Spotify

top25dance = (group_by(spotifyTracks, danceability) %>% summarise(popularity) %>% arrange(-popularity))
top25dance=head(top25dance,25)
ggplot(top25dance, aes(danceability, popularity)) + geom_point(position='jitter')

# Graphes 6 à 8 : Les facteurs qui influencent le plus sur la danceability

factors=(group_by(spotifyTracks, danceability))
ggplot(factors, aes(energy, danceability)) +geom_density_2d_filled(alpha=0.5) + geom_smooth(color='black')
ggplot(factors, aes(valence, danceability)) +geom_density_2d_filled(alpha=0.5) + geom_smooth(color='black')
ggplot(factors, aes(tempo, danceability)) +geom_density_2d_filled(alpha=0.5) + geom_smooth(color='black')

