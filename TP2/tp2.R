getwd()
setwd("D:/School/UTC/A16-P17/P17/SY09/TD/TP1")

#setwd("/media/leminhtr/Data/School/UTC/A16-P17/P17/SY09/TD/TP1")


# classification

# matrice de distance, dissimilarités

# aftd : recherche un espace euclidien pour repréenter les données

# Si matrice distance est euclidienne (distance euclidienne entre les points) alors il existe un espace Eucld où l'on peut
# représenter les données : dimension n-1


# Simplifier la visualtion des données : résumé l'info mais conserver les spécificités du groupe d'individu

# Problème de classification optimal : sous/sur classification.


# 1) Méthodes arborescentes :
  # Construire un arbre pour visualiser les liens entre les points : 
      # calculer distance entre les points puis les ranger par grandeur dans un arbre :
      # 1-------2-------5-----------------3--4--6 => groupement dans un arbre : (1,2,5) --- (3,4,6) -> 2 paquets de points
      # 


# 2) Méthodes géométriques : 
  # Ex : k-means
    # On choisit un nombre de classe (supposé)
    # calculer des centres
    # affecter des groupes de points au centre en fonction de la distance
    # reaffecter le centre 
    # réitérer jusqu'à stabilité

    # intertie intra/inter-classe
    # => on veut minimiser l'inertie intra-classe <=> trouver classification telle que  somme variance/inertie des
    # classes est minimale
    # intertie totale supérieur à  [somme intertie intra/inter-classe supérieur]
    
    # on cherche un compromis entre petit nombre de classe et inertie limitée


    # limites : hypothèse sur le type de distance (euclidienne)...


# Qualité de la classification : indice de Rand :
  # on compte le nombre de classe de points (listes des combinaisons k parmi n  : kCn) puis on compte quelles
  # classes reviennent le plus souvent.

# AFTD : contrainte implicite : d_ij : distance euclidiene < delta_ij : dissimilarité => tous les points doivent être sous la bissectrice

# Shepard : distance entre individus= distance dissimilarité 
# vérifier qualité représentation => on représente un graphique d_ij(x,y) = distance euclidienne en fonction de delta_ij (dissimilarité)
# Sachant qu'on veut d_ij (euclidienne) = delta_ij (dissimilarité), si les points sur le diagramme de shepard sont alignés alors la représentation est bonne !
# augmente dimension espace alors plus de chances de trouver une représentation ou distance euclidienne et dissimilarité sont égales


# Ex.2 

#### Rappel : k-means :
  # Init : choix du k centres (au hasard, dans l'ensemble de données)
  # Itérations (q=1,2,...)
    # - affectation de chaque x_i au centre c_k le plus proche (distance euclidienne lié au cirtère intra-classe, 
                          #le centre de gravité est à distance minimale du nuage au sens de la distance euclidienne
                          #: d^2(x,y)=norme(x-y)^2= t((x-y)) * (x-y) = somme(j=1 à p)(x_j - y_j)^2)
    # z_ik = 1 si x_i affecté à c_k
    #      = 0 sinon
    # - mise à jour des centres : c_k= somme(i=1 à n) z_ik * x_i / somme (i=1 à n) z_ik ; k=1,...,k
  # Tant que somme(k=1 à K) norme(c(-^q)_k - c(-^q-1)_k) > espsilon (epsilon petit)

# Rmq : 
# /!\ k-means : non convexe <=> il y a des optimums locaux donc la solution trouvée n'est pas forcément la meilleure (optimum globale)
# algo k-means (donc distance euclidienne) ok avec clusters sphériques mais sinon moins performant.
# distance L1 : d_l(x,y)= somme(j=1 à p) abs(x_j-y_j)








