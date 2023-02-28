library(ggplot2)
library(dplyr)
library(igraph)
library(stringr)

# GENERATION DE DONNEES ALEATOIRES ---

set.seed(123) # fixe la graine pour obtenir les mêmes résultats aléatoires à chaque exécution

#dimension de l'espace géographique simulé
x_max <- 20
y_max <- 20
# génération de données aléatoires pour les points
n <- 500 # nombre de points de données
df <- data.frame(x = runif(n, 0, x_max),
                 y = runif(n, 0, y_max))
# génération de données aléatoires pour les carrés
n_squares <- 25 # nombre de carrés
square_side_length <- 4 # longueur des carrés
square_values <- runif(n_squares, 0, 1) # valeurs aléatoires pour chaque carré

#générer une valeur aléatoire pour chaque point (individu d'enquête) dans les coordonnées
df$survey_value <- runif(n, 0, 1)
#résumer et afficher un tableau de synthèse de ces données
summary(df)

#découper les coordonnées en carrés de taille 4x4 (square_side_length=4)
square_side_length <- 4
#la fonction cut sert à découper les coordonnées en intervalles de taille prédéterminée
#la fonction seq sert à générer une séquence d'éléments dans un ordre donné
df$x_bin <- cut(df$x, breaks = seq(0, 20, by = square_side_length), labels = FALSE)
df$y_bin <- cut(df$y, breaks = seq(0, 20, by = square_side_length), labels = FALSE)

# Assignation d'un numéro ou indice à chaque carré
# en numérotant les carrés, l'identifiant de chaque carré s'obtient à partir des coordonnées x_bin et y_bin (expression modulo)
nb_cols_squares <- 5
df$square_id <- df$x_bin + nb_cols_squares * (df$y_bin-1) #petite réflexion mathématique
#ex: le carré n°6 se déduit avec x_bin=1,y_bin=2 par 6=1+2(5) on se déplace de 1 à droite et sachant qu'il faut se deplacer de 5 pas à droite avant d'arriver en bas, on s'est déplacé 2 cases en bas.

#Ajout de la colonne correspondant à la valeur de chaque carré
df$square_value <- square_values[df$square_id]

# Création du graphique avec ggplot
p <- ggplot(df, aes(x, y)) +
  geom_bin2d(bins = n_squares) + #,aes(fill = ..density..)
  #geom_point() +
  coord_fixed(ratio = 1) +
  labs(x = "", y = "") +
  theme_classic() +
  theme(plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "black",size = square_side_length/x_max,linetype = 1),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

p_numerote <- ggplot(df, aes(x, y)) +
  geom_bin2d(bins = n_squares) +
  coord_fixed(ratio = 1) +
  labs(x = "", y = "") +
  theme_classic() +
  scale_y_reverse() +
  theme(plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(color = "black",size = square_side_length/x_max,linetype = 1),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  # numérotation des carrés
  geom_text(aes(x = (x_bin - 0.5) * square_side_length, 
                y = (y_bin - 0.5) * square_side_length,
                label = square_id, size=6), 
            color = "black", 
            size = 3,
            family = "sans",
            hjust = 0.5,
            vjust = 0.5)

# Enregistrement du graphique en tant qu'image
ggsave("survey_data_square.png", plot = p, width = n_squares, height = n_squares,limitsize = FALSE)
ggsave("survey_data_square_with_ID.png", plot = p_numerote, width = n_squares, height = n_squares,limitsize = FALSE)
#Affichage
p_numerote

#Restitution du tableau final qui va être utilisé
#df$coords <- cbind(df$x, df$y)
df$coords <- paste(df$x, df$y, sep = ",")
#df <- df %>% mutate(coords = as.numeric(coords))
#Filtrer les données et renommer les colonnes
donnees <- dplyr::select(df,coords,survey_value,square_value,square_id)
colnames(donnees) <- c("coords", "individual_value","square_value", "square_ID")
#conversion en tibble des données
donnees <- as_tibble(donnees)

# CREATION DE LA MATRICE DE CONTIGUITE (QUEEN) --- 
#il s'agit plutôt ici d'une liste de paires de voisins en 2 colonnes
#on remarque que le lien de voisinage sur la numérotation est mathématique
#ce code marche pour ce cas particulier (à ne pas généraliser)
a <- b <- c()
for (i in 1:25) {
  for (j in 1:25) {
    diff <- abs(j-i)
    if (diff != 1 && diff != 4 && diff != 5 && diff != 6) {
      #ne rien faire car non-voisin 
    }
    else {
      if (i < j) {
        if (i %% 5 == 0) {
          if (j %% 5 == 1) {
            next # sautez à la prochaine itération du boucle sans ajouter i et j à a et b
          }
          else {
            a <- c(a, i)
            b <- c(b, j)
          }
        }
        else if (i %% 5 == 1) {
          if (j %% 5 == 0) {
            next # sautez à la prochaine itération du boucle sans ajouter i et j à a et b
          }
          else {
            a <- c(a, i)
            b <- c(b, j)
          }
        }
        else {
          a <- c(a, i)
          b <- c(b, j)
        }
      }
    }
  }
}

voisin <- cbind(a, b)
#suppression des lignes indésirables
voisin <- voisin[rowSums(is.na(voisin)) == 0, ]
colnames(voisin) <- c("id1","id2")
voisin <- data.frame(voisin)
#conversion des colonnes en chaine de caracteres
voisin$id1 <- as.character(voisin$id1)
voisin$id2 <- as.character(voisin$id2)
#conversion en tibble pour les fonctions d'aggrégation
contig_matrix <- as.matrix(voisin)


# MISE EN PLACE DE L'AGREGATION ---

##Comptage du nombre de répondants pour chaque carré
#Nous aggrégons les données d'enquêtes au niveau des communes pour avoir 
#le nombre de répondants par communes et les données d'enquêtes au niveau des communes

agg_indiv_to_communes <- function(data)
{data %>% group_by(square_ID,square_value) %>% summarize(donnee_enquete_commune = mean(individual_value),nb_repondants_commune = n())
}
donnees_agg <- agg_indiv_to_communes(donnees)
donnees_agg <- donnees_agg %>%rename(ID=square_ID,donnee_contexte_commune = square_value)

# CLUSTERING AVEC CONTRAINTES DE CONTIGUITE ET CRITERE D'ARRET SUR LES REPONDANTS -------

# FONCTIONS UTILITAIRES POUR LA GESTION DES CONTRAINTES ------------

groupes_voisins <- function(groups,i,j){ #fonction qui évalue si grp i et j sont voisins
  res=F
  for (k in 1:length(contig_matrix[,1])){
    for (x in groups[[i]]){
      for (y in groups[[j]]){
        if((contig_matrix[k,"id1"]==x && contig_matrix[k,"id2"]==y)|(contig_matrix[k,"id1"]==y && contig_matrix[k,"id2"]==x)){
          res=T
        }
      }
    }
  }
  return(res)
}


distance_between_groups <- function(groups,i,j) {
  # Calcul de la distance entre deux groupes pour la méthode ward.d2
  # voir "Ward’s Hierarchical Clustering Method:Clustering Criterion and Agglomerative Algorithm"
  n <- length(groups[[i]]) + length(groups[[j]])
  # Filtre des valeurs de la variable d'intérêt associés aux ID des clusters 
  data_group1 <- filter(donnees_agg,ID %in% groups[[i]])$donnee_contexte_commune
  data_group2 <- filter(donnees_agg,ID %in% groups[[j]])$donnee_contexte_commune
  mean1 <- mean(data_group1)
  mean2 <- mean(data_group2)
  mean_all <- mean(c(mean(data_group1), mean(data_group2)))
  # Calcul de la somme des carrés des écarts à la moyenne pour chaque groupe
  ssd1 <- sum((data_group1 - mean1) ^ 2)
  ssd2 <- sum((data_group2 - mean2) ^ 2)
  # Calcul de la somme des carrés des écarts à la moyenne pour tous les points
  ssd_all <- sum((c(data_group1,data_group2) - mean_all) ^ 2)
  # Calcul de la distance entre deux groupes pour la méthode ward.d2
  dist_between_groups <- ssd_all - (ssd1 + ssd2)
  return(sqrt(dist_between_groups / (n)))
}

fusion_between_groups <- function(i,j) {
  #fusion des groupes
  groups <- c(groups, list(c(groups[[i]],groups[[j]])))
  #suppression des anciens groupes ayant servi à la fusion
  groups <- groups[-c(i, j)]
}

#Fonction pour calculer l'inertie avec donnee_contexte_commune comme variable d'intérêt
inertie_inter_intra_donnee_contexte_commune <- function(data, groups) {
  K <- length(groups)
  N <- nrow(data)
  total_mean <- mean(data$donnee_contexte_commune)
  
  inter_group_var <- 0
  for (i in seq_len(K)) {
    group_mean <- mean(filter(data, ID %in% groups[[i]])$donnee_contexte_commune)
    group_size <- nrow(filter(data, ID %in% groups[[i]]))
    inter_group_var <- inter_group_var + group_size * (group_mean - total_mean)^2
  }
  
  intra_group_var <- sum((data$donnee_contexte_commune - total_mean)^2)
  
  return(list(inter = inter_group_var, intra = intra_group_var))
}

# INITIALISATION DE LA CAH SUR LES DONNEES DE CONTEXTE AVEC CONTRAINTES SUR NOMBRE DE REPONDANTS ------------

#Nombre maximum de répondants par agrégats
nb_repondants_max <- 100
#Fixation de l'écart absolu souhaité pour la stabilisation de l'inertie inter-intra
seuil_inertie <- 0.000001
inertie_prec = 100000
convergence = FALSE

# Initialisation des groupes
groups = lapply(1:25, function(x) c(as.character(x)))
# stockage du nombre de répondants
for (i in 1:length(groups)){attr(groups[[i]], "nombres_répondants") <- filter(donnees_agg,ID %in% groups[[i]])$nb_repondants_commune}
for (i in 1:length(groups)){attr(groups[[i]], "height") <- 0}
# Initialisation de la matrice de distance entre les groupes
dist_mat <- matrix(Inf, nrow = length(groups), ncol = length(groups))
for (i in 1:length(groups)) {
  for (j in i:length(groups)) {
    if (j>i){
      #dès la première étape,on exclut d'office du calcul les groupes trop éloignés et trop gros: calcul plus rapide
      #contrainte contiguite
      if (groupes_voisins(groups,i,j)){ 
        #contrainte de taille sur le nombre de répondants par agrégat
        if (sum(filter(donnees_agg,ID %in% groups[[i]])$nb_repondants_commune)
            + sum(filter(donnees_agg,ID %in% groups[[j]])$nb_repondants_commune) <= nb_repondants_max) { #contrainte de taille sur nombre de répondants
          dist_mat[i,j] <- distance_between_groups(groups,i,j)
          dist_mat[j,i] <- dist_mat[i,j]
        }
      }
    }
    
  }
}
# Initialisation de la liste des inerties inter-intra classes
liste_inertie <- c()
# Initialisation de la liste des listes des groupes pour sélectionner à la fin le groupe
#correspondant au minimum d'inertie inter-intra
liste_groups <- list()

# CAH SUR LES DONNEES DE CONTEXTE AVEC CONTRAINTES SUR NOMBRE DE REPONDANTS ------------

# Boucle de la CAH
# Nous pouvons arrêter lorsque l'inertie inter-intra se stabilise (si trop long attendre d'avoir un cluster)
# Sinon si on a les ressources pour le faire jusqu'au bout: remplacer par 'while (length(groups) > K)'
# Avec K le nombre d'agrégats qu'on estime pouvoir produire vis-à-vis du nombre de répondants et communes
# Attention si on ne peut pas avoir que K agrégats à cause de la contrainte sur les non-répondants, boucle infinie

# Nous souhaitons avoir le minimum du rapport d'inertie inter-intra classes car nous voulons les agrégats
#les plus homogènes possibles au niveau des données de contexte
#cela sera traité juste après la boucle

while (!convergence) {
  
  # Recherche des groupes les plus proches
  min_dist <- min(dist_mat)
  #tableau en 2 colonnes des couples d'indice du minimum
  min_indices <- which(dist_mat == min_dist, arr.ind = TRUE)
  i <- min_indices[1,][[1]]
  j <- min_indices[1,][[2]]
  
  #condition 
  if (groupes_voisins(groups,i,j)){ 
    #notion de voisinage entre 2 clusters à appliquer ici sur les vecteurs et non des unités
    # contient les indices de la première occurrence de la commune i dans chaque vecteur de la liste G
    nb_repondant_agregat <- sum(filter(donnees_agg,ID %in% groups[[i]])$nb_repondants_commune)
    + sum(filter(donnees_agg,ID %in% groups[[j]])$nb_repondants_commune)
    if ( nb_repondant_agregat <= nb_repondants_max) { #contrainte de taille sur nombre de répondants

      #ajouter calcul de distance et les voisins dans un des vecteurs de G.
      # Fusion des groupes
      new_group <- c(groups[[i]], groups[[j]])  
      groups[[i]] <- new_group
      
      # Stockage de la hauteur de la fusion
      height <- min_dist / 2
      
      # Stockage de la hauteur pour les deux groupes fusionnés
      attr(new_group, "height") <- height
      attr(groups[[i]], "height") <- height
      # Stockage du nombre de répondants pour les deux groupes fusionnés
      attr(new_group, "nombre_répondants") <- nb_repondant_agregat
      attr(groups[[i]], "nombres_répondants") <- nb_repondant_agregat
      cat("Nombre de répondants du cluster actuellement fusionné :",as.character(nb_repondant_agregat),"\n")
      
      # Suppression de l'ancien groupe
      groups <- groups[-j]
      # Suppression de ce cluster dans la matrice de distance des clusters
      dist_mat <- dist_mat[-j, -j]
      # Après cette suppression, i ne correpond plus au cluster designé auparavant réactualisons i
      i <- match(attr(new_group, "height"), sapply(groups, function(x) attr(x, "height")))
      
      #il peut être intéressant de stocker groups  à chaque étape
      #pour avoir un clustering proposé à chaque étape
      
      # Modification de la matrice de distance entre les groupes: nécessaire car les groupes changent 
      # On modifie la distance du nouveau cluster avec les autres existants
      # On recalcule la distance entre clusters à la ligne i et colonne i de l
      
      #le voisinage change donc aussi pour le nouveau cluster n°i fusionné
      
      for (k in 1:ncol(dist_mat)){
        #avant de recalculer les distances,on vérifie donc encore la contrainte de contiguité 
        #entre ce nouvel agrégat et les autres pour éviter les caluls inutiles
        if (groupes_voisins(groups,i,k)){
          dist_mat[i,k] <- dist_mat[k,i] <- distance_between_groups(groups,i,k)
        }
        else{dist_mat[i,k] <- dist_mat[k,i] <- Inf
        }
      }
      #par défaut, on fixe la diagonale à Inf pour ne pas 
      #associer une commune à elle-même
      dist_mat[i,i] <- dist_mat[i,i] <- Inf
      
        # Calcul de l'inertie inter-intra
        inertie_inter <- inertie_inter_intra_donnee_contexte_commune(donnees_agg, groups)$inter
        inertie_intra <- inertie_inter_intra_donnee_contexte_commune(donnees_agg, groups)$intra
        inertie = inertie_inter/inertie_intra
        # Sauvegarde du groupe et de l'inertie correspondante à chaque groupe formé/supprimé
        liste_inertie <- append(liste_inertie,inertie)
        liste_groups[[length(liste_groups) + 1]] <- groups
        }
    }
  # vérification de la convergence
  if(abs(inertie - inertie_prec) < seuil_inertie) {
    convergence = TRUE
    print("CONVERGENCE !!!")
  }
  cat("Inertie précédente:",inertie,"Inertie suivante:",inertie_prec,"\n")
  # mise à jour de la variable d'inertie précédente
  inertie_prec = inertie
}

# On retourne la plus petite inertie inter-intra classes retenue pour pouvoir la comparer après
# Récupération de l'indice du minimum d'inertie et des groupes correspondants par la même occasion
# On prend la première occurence du minimum pour avoir trop de classes inutiles
min_inertie_donnee_contexte <- min(liste_inertie)
indice_min_groupes_retenu <- which(liste_inertie == min_inertie_donnee_contexte, arr.ind = TRUE)

# Résultats finaux
agregats_donnee_contexte_repondant <- liste_groups[[indice_min_groupes_retenu]]
print('Les agrégats au niveau des données de contexte retenus sur contrainte des répondants sont :')
print(agregats_donnee_contexte_repondant)
cat("Le rapport d'inertie obtenu sur les données de contexte correspondant vaut:", min_inertie_donnee_contexte,"\n")

# Représentation dendrogramme à partir des hauteurs et des clusters (encore à développer)
# Conversion de dist_mat au bon format pour hclust en remplaçant les valeurs "Inf" par 65536
dist_mat_hclust <- dist_mat
dist_mat_hclust[is.infinite(dist_mat)] <- 100000000000000
# Construire un objet de type dendrogramme
dend <- hclust(as.dist(dist_mat_hclust))
# Extraire les clusters de la liste et les rajouter en labels des clusters
labels(dend) <- lapply(agregats_donnee_contexte_repondant, function(x) x[1:length(x)])
# Afficher le dendrogramme
plot(dend)


# Résultat final pour représenter graphiquement sur les carrés (ne marche que dans notre cas particulier)
# Conversion de la liste de vecteurs en une liste résumant l'appartenance à un groupe
clusters <- lapply(agregats_donnee_contexte_repondant, as.numeric)
cluster_indices <- rep(NA, max(unlist(clusters)))

# Parcourir chaque vecteur numérique dans la liste "clusters"
for (i in seq_along(clusters)) {
  cluster_indices[clusters[[i]]] <- i
}
# Afficher la liste d'indices de cluster
clusters <- cluster_indices 
clusters

#Visualisation du résultat

#Appariement des identifiants des carrés aux clusters
df$cluster <- clusters[df$square_id]

# Fonction pour assigner une couleur en fonction du numéro de carré
color_fun <- function(id) {
  if(id == 1) {
    return("red")
  } else if(id == 2) {
    return("blue")
  } else if(id == 3) {
    return("green")
  } else if(id == 4) {
    return("brown")
  } else if(id == 5) {
    return("orange")
  } else if(id == 6) {
    return("black")
  } else if(id == 7) {
    return("purple")
  } else if(id == 8) {
    return("yellow")
  } else {
    return("pink")
  }
}

#Fonction plus générale permettant de générer une palette de couleurs personnalisée 
# en fonction du nombre de clusters donné en entrée. 
#Cette fonction attribue une couleur différente à chaque cluster en utilisant une palette de couleurs définie
extended_color_fun <- function(id) {
  palette <- c("red", "orange", "yellow", "green", "blue", "purple", "pink", "brown", "black", "gray",
               "darkred", "darkorange", "gold", "darkgreen", "navyblue", "indigo", "violet", "saddlebrown", "darkgray", "silver",
               "maroon", "tomato", "khaki", "olive", "skyblue", "slategray", "orchid", "sienna", "dimgray", "dimgrey",
               "crimson", "coral", "lemonchiffon", "limegreen", "cornflowerblue", "mediumorchid", "mediumslateblue", "sienna", "rosybrown", "lightslategray")
  return(palette[id %% length(palette) + 1])
}

palette <- c("red", "orange", "yellow", "green", "blue", "purple", "pink", "brown", "black", "gray",
               "darkred", "darkorange", "gold", "darkgreen", "navyblue", "indigo", "violet", "saddlebrown", "darkgray", "silver",
               "maroon", "tomato", "khaki", "olive", "skyblue", "slategray", "orchid", "sienna", "dimgray", "dimgrey",
               "crimson", "coral", "lemonchiffon", "limegreen", "cornflowerblue", "mediumorchid", "mediumslateblue", "sienna", "rosybrown", "lightslategray")

# Fonction pour assigner une couleur en fonction du numéro de carré
color_fun_2 <- function(id) {return(palette[id])}

# Assigner une couleur à chaque carré
color <- sapply(clusters, color_fun_2)
# Convertir au bon format pour l'affichage
color <- as.vector(as.matrix(color)[,1])

df <- data.frame(x_bin = rep(1:5, 5), 
                 y_bin = rep(5:1, each = 5), 
                 color = color)

ggplot(df, aes(x = x_bin, y = y_bin, fill = color)) +
  geom_rect(data = df, aes(xmin = x_bin - 0.5, xmax = x_bin + 0.5,
                           ymin = y_bin - 0.5, ymax = y_bin + 0.5),fill = df$color)+
  scale_fill_identity() +
  coord_cartesian(xlim = c(0, 6), ylim = c(0, 6)) +
  theme_void() +
  ggtitle("CAH sur les données de contexte avec contraintes de contiguité et sur le nombre de répondants")



# CLUSTERING SUR LES DONNEES D'ENQUETE ---------- (EN COURS)

#Peut-on encore agrégé d'autres agrégats sur les données de contexte

#on part des clusters obtenus sur les données de contexte et on agrège encore
# en respectant une contrainte sur les répondants on comparera l'inertie inter-intra obtenu

distance_between_groups <- function(groups,i,j) {
  # Calcul de la distance entre deux groupes pour la méthode ward.d2
  # voir "Ward’s Hierarchical Clustering Method:Clustering Criterion and Agglomerative Algorithm"
  n <- length(groups[[i]]) + length(groups[[j]])
  # Filtre des valeurs de la variable d'intérêt associés aux ID des clusters 
  data_group1 <- filter(donnees_agg,ID %in% groups[[i]])$donnee_enquete_commune
  data_group2 <- filter(donnees_agg,ID %in% groups[[j]])$donnee_enquete_commune
  mean1 <- mean(data_group1)
  mean2 <- mean(data_group2)
  mean_all <- mean(c(mean(data_group1), mean(data_group2)))
  # Calcul de la somme des carrés des écarts à la moyenne pour chaque groupe
  ssd1 <- sum((data_group1 - mean1) ^ 2)
  ssd2 <- sum((data_group2 - mean2) ^ 2)
  # Calcul de la somme des carrés des écarts à la moyenne pour tous les points
  ssd_all <- sum((c(data_group1,data_group2) - mean_all) ^ 2)
  # Calcul de la distance entre deux groupes pour la méthode ward.d2
  dist_between_groups <- ssd_all - (ssd1 + ssd2)
  return(sqrt(dist_between_groups / (n)))
}

fusion_between_groups <- function(i,j) {
  #fusion des groupes
  groups <- c(groups, list(c(groups[[i]],groups[[j]])))
  #suppression des anciens groupes ayant servi à la fusion
  groups <- groups[-c(i, j)]
}

#Fonction pour calculer l'inertie avec donnee_enquete_commune comme variable d'intérêt
inertie_inter_intra_donnee_enquete_commune <- function(data, groups) {
  K <- length(groups)
  N <- nrow(data)
  total_mean <- mean(data$donnee_enquete_commune)
  
  inter_group_var <- 0
  for (i in seq_len(K)) {
    group_mean <- mean(filter(data, ID %in% groups[[i]])$donnee_enquete_commune)
    group_size <- nrow(filter(data, ID %in% groups[[i]]))
    inter_group_var <- inter_group_var + group_size * (group_mean - total_mean)^2
  }
  
  intra_group_var <- sum((data$donnee_enquete_commune - total_mean)^2)
  
  return(list(inter = inter_group_var, intra = intra_group_var))
}

# INITIALISATION DE LA CAH SUR LES DONNEES D'ENQUETE AVEC CONTRAINTES SUR NOMBRE DE REPONDANTS ------------

# On réinitialise l'état du clustering à non-convergent
convergence=FALSE
inertie_prec = 1000
# Initialisation des groupes: on part du dernier groupe obtenu sur le clustering précédent
groups = agregats_donnee_contexte_repondant
# Initialisation de la matrice de distance entre les groupes à redéfinir en remplaçant dans la
#matrice de distance les valeurs non "Inf" par la distance entre les groupes mais au niveau des données d'enquête
for (i in 1:length(groups)) {
  for (j in i:length(groups)) {
    if (!is.infinite(dist_mat[i,j])){
          #Remplacement des valeurs non_inf
          dist_mat[i,j] <- distance_between_groups(groups,i,j)
          dist_mat[j,i] <- dist_mat[i,j]
    }
    
  }
}
# Initialisation de la liste des inerties inter-intra classes
liste_inertie <- c()
# Initialisation de la liste des listes des groupes pour sélectionner à la fin le groupe
#correspondant au minimum d'inertie inter-intra
liste_groups <- list()

# CAH SUR LES DONNEES DE enquete AVEC CONTRAINTES SUR NOMBRE DE REPONDANTS ------------

# Boucle de la CAH
# Nous pouvons arrêter lorsque l'inertie inter-intra se stabilise (si trop long attendre d'avoir un cluster)
# Sinon si on a les ressources pour le faire jusqu'au bout: remplacer par 'while (length(groups) > K)'
# Avec K le nombre d'agrégats qu'on estime pouvoir produire vis-à-vis du nombre de répondants et communes
# Attention si on ne peut pas avoir que K agrégats à cause de la contrainte sur les non-répondants, boucle infinie

# Nous souhaitons avoir le minimum du rapport d'inertie inter-intra classes car nous voulons les agrégats
#les plus homogènes possibles au niveau des données de enquete
#cela sera traité juste après la boucle

while (!convergence) {
  
  # Recherche des groupes les plus proches
  min_dist <- min(dist_mat)
  #tableau en 2 colonnes des couples d'indice du minimum
  min_indices <- which(dist_mat == min_dist, arr.ind = TRUE)
  i <- min_indices[1,][[1]]
  j <- min_indices[1,][[2]]
  
  #condition 
  if (groupes_voisins(groups,i,j)){ 
    #notion de voisinage entre 2 clusters à appliquer ici sur les vecteurs et non des unités
    # contient les indices de la première occurrence de la commune i dans chaque vecteur de la liste G
    nb_repondant_agregat <- sum(filter(donnees_agg,ID %in% groups[[i]])$nb_repondants_commune)
    + sum(filter(donnees_agg,ID %in% groups[[j]])$nb_repondants_commune)
    if ( nb_repondant_agregat <= nb_repondants_max) { #contrainte de taille sur nombre de répondants
      
      #ajouter calcul de distance et les voisins dans un des vecteurs de G.
      # Fusion des groupes
      new_group <- c(groups[[i]], groups[[j]])  
      groups[[i]] <- new_group
      
      # Stockage de la hauteur de la fusion
      height <- min_dist / 2
      
      # Stockage de la hauteur pour les deux groupes fusionnés
      attr(new_group, "height") <- height
      attr(groups[[i]], "height") <- height
      # Stockage du nombre de répondants pour les deux groupes fusionnés
      attr(new_group, "nombre_répondants") <- nb_repondant_agregat
      attr(groups[[i]], "nombres_répondants") <- nb_repondant_agregat
      cat("Nombre de répondants du cluster actuellement fusionné :",as.character(nb_repondant_agregat),"\n")
      
      # Suppression de l'ancien groupe
      groups <- groups[-j]
      # Suppression de ce cluster dans la matrice de distance des clusters
      dist_mat <- dist_mat[-j, -j]
      # Après cette suppression, i ne correpond plus au cluster designé auparavant réactualisons i
      i <- match(attr(new_group, "height"), sapply(groups, function(x) attr(x, "height")))
      
      #il peut être intéressant de stocker groups  à chaque étape
      #pour avoir un clustering proposé à chaque étape
      
      # Modification de la matrice de distance entre les groupes: nécessaire car les groupes changent 
      # On modifie la distance du nouveau cluster avec les autres existants
      # On recalcule la distance entre clusters à la ligne i et colonne i de l
      
      #le voisinage change donc aussi pour le nouveau cluster n°i fusionné
      
      for (k in 1:ncol(dist_mat)){
        #avant de recalculer les distances,on vérifie donc encore la contrainte de contiguité 
        #entre ce nouvel agrégat et les autres pour éviter les caluls inutiles
        if (groupes_voisins(groups,i,k)){
          dist_mat[i,k] <- dist_mat[k,i] <- distance_between_groups(groups,i,k)
        }
        else{dist_mat[i,k] <- dist_mat[k,i] <- Inf
        }
      }
      #par défaut, on fixe la diagonale à Inf pour ne pas 
      #associer une commune à elle-même
      dist_mat[i,i] <- dist_mat[i,i] <- Inf
      
      # Calcul de l'inertie inter-intra
      inertie_inter <- inertie_inter_intra_donnee_enquete_commune(donnees_agg, groups)$inter
      inertie_intra <- inertie_inter_intra_donnee_enquete_commune(donnees_agg, groups)$intra
      inertie = inertie_inter/inertie_intra
      # Sauvegarde du groupe et de l'inertie correspondante à chaque groupe formé/supprimé
      liste_inertie <- append(liste_inertie,inertie)
      liste_groups[[length(liste_groups) + 1]] <- groups
    }
  }
  # vérification de la convergence
  if(abs(inertie - inertie_prec) < seuil_inertie) {
    convergence = TRUE
    print("CONVERGENCE !!!")
  }
  cat("Inertie précédente:",inertie,"Inertie suivante:",inertie_prec,"\n")
  # mise à jour de la variable d'inertie précédente
  inertie_prec = inertie
}

# On retourne la plus petite inertie inter-intra classes retenue pour pouvoir la comparer après
# Récupération de l'indice du minimum d'inertie et des groupes correspondants par la même occasion
# On prend la première occurence du minimum pour avoir trop de classes inutiles
min_inertie_donnee_enquete <- min(liste_inertie)
indice_min_groupes_retenu_enquete <- which(liste_inertie == min_inertie_donnee_enquete, arr.ind = TRUE)

# Résultats finaux
agregats_donnee_enquete_repondant <- liste_groups[[indice_min_groupes_retenu_enquete]]
print('Les agrégats au niveau des données de enquete retenus sur contrainte des répondants sont :')
print(agregats_donnee_enquete_repondant)
cat("Le rapport d'inertie obtenu sur les données de enquete correspondant vaut:", min_inertie_donnee_enquete,"\n")

# Représentation dendrogramme à partir des hauteurs et des clusters (encore à développer)
# Conversion de dist_mat au bon format pour hclust en remplaçant les valeurs "Inf" par 65536
dist_mat_hclust <- dist_mat
dist_mat_hclust[is.infinite(dist_mat)] <- 100000000000000
# Construire un objet de type dendrogramme
dend <- hclust(as.dist(dist_mat_hclust))
# Extraire les clusters de la liste et les rajouter en labels des clusters
labels(dend) <- lapply(agregats_donnee_enquete_repondant, function(x) x[1:length(x)])
# Afficher le dendrogramme
plot(dend)


# Résultat final pour représenter graphiquement sur les carrés (ne marche que dans notre cas particulier)
# Conversion de la liste de vecteurs en une liste résumant l'appartenance à un groupe
clusters <- lapply(agregats_donnee_enquete_repondant, as.numeric)
cluster_indices <- rep(NA, max(unlist(clusters)))

# Parcourir chaque vecteur numérique dans la liste "clusters"
for (i in seq_along(clusters)) {
  cluster_indices[clusters[[i]]] <- i
}
# Afficher la liste d'indices de cluster
clusters <- cluster_indices 
clusters

#Visualisation du résultat

#Appariement des identifiants des carrés aux clusters
df$cluster <- clusters[df$square_id]

# Fonction pour assigner une couleur en fonction du numéro de carré
color_fun <- function(id) {
  if(id == 1) {
    return("red")
  } else if(id == 2) {
    return("blue")
  } else if(id == 3) {
    return("green")
  } else if(id == 4) {
    return("brown")
  } else if(id == 5) {
    return("orange")
  } else if(id == 6) {
    return("black")
  } else if(id == 7) {
    return("purple")
  } else if(id == 8) {
    return("yellow")
  } else {
    return("pink")
  }
}

#Fonction plus générale permettant de générer une palette de couleurs personnalisée 
# en fonction du nombre de clusters donné en entrée. 
#Cette fonction attribue une couleur différente à chaque cluster en utilisant une palette de couleurs définie
extended_color_fun <- function(id) {
  palette <- c("red", "orange", "yellow", "green", "blue", "purple", "pink", "brown", "black", "gray",
               "darkred", "darkorange", "gold", "darkgreen", "navyblue", "indigo", "violet", "saddlebrown", "darkgray", "silver",
               "maroon", "tomato", "khaki", "olive", "skyblue", "slategray", "orchid", "sienna", "dimgray", "dimgrey",
               "crimson", "coral", "lemonchiffon", "limegreen", "cornflowerblue", "mediumorchid", "mediumslateblue", "sienna", "rosybrown", "lightslategray")
  return(palette[id %% length(palette) + 1])
}

# Assigner une couleur à chaque carré
color <- sapply(clusters, color_fun_2)
# Convertir au bon format pour l'affichage
color <- as.vector(as.matrix(color)[,1])

df <- data.frame(x_bin = rep(1:5, 5), 
                 y_bin = rep(5:1, each = 5), 
                 color = color)

ggplot(df, aes(x = x_bin, y = y_bin, fill = color)) +
  geom_rect(data = df, aes(xmin = x_bin - 0.5, xmax = x_bin + 0.5,
                           ymin = y_bin - 0.5, ymax = y_bin + 0.5),fill = df$color)+
  scale_fill_identity() +
  coord_cartesian(xlim = c(0, 6), ylim = c(0, 6)) +
  theme_void() +
  ggtitle("Ré-agrégation sur les données d'enquete avec contraintes de contiguité et sur le nombre de répondants")



cat("Inertie obtenue sans une agrégation supplémentaire:", min_inertie_donnee_contexte,"Inertie obtenue avec agrégation supplémentaire sur les données d'enquête:", min_inertie_donnee_enquete,"\n")
cat("Une agrégation supplémentaire sur les données d'enquête est-elle nécessaire ?",ifelse(min_inertie_donnee_contexte>min_inertie_donnee_enquete,"Oui, cela est nécessaire","Non il ne vaut mieux pas refaire une agrégation et s'en tenir aux données de contexte"))

