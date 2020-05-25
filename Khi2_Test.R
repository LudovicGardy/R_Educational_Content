# By Ludovic G.

#                                 #===================#
#                                 #   Test du khi 2   #
#                                 #   Le 5/03/2017    #
#                                 #===================#
      
## On se demande si lors d'une tache de fluence verbale passee au moment du diagnostic de patients Alzheimers 
#- (moment qu'on appelera T0), leur sujet Controle apparie cite le meme nombre de fois la ville de Paris.
#- Comme je n'ai pas le meme nombre de sujets dans mes deux groupes,les valeurs de mes tableaux sont exprimees
#- en pourcentages de sujets qui citent la ville de Paris pour chaque groupe.

#==========#===================================#
# Partie 1 # Utilisation de la fonction dans R #
#==========#===================================#

#= Creation d'une matrice: nombre de sujets Alzheimers citant Paris a T0 vs nombre de sujets Controles. =#

matriceKhi2 = matrix(0,2,2)
matriceKhi2[1,1] = 71
matriceKhi2[1,2] = 29
matriceKhi2[2,1] = 94
matriceKhi2[2,2] = 6

rownames(matriceKhi2) = c("Alzheimers","Controles")
colnames(matriceKhi2) = c("CiteParis_Oui","CiteParis_Non")

print(matriceKhi2)

## Je fais mon test chi2. Matrice 2*2 donc 1 degre de liberte soit un khi2 theorique de 3.841 pour un alpha de 0.05.
#- Si ma valeur reelle depasse ma valeur theorique, le test est significatif. Ici je test l'hypothese nulle :
#- Nombre de sujets Alzheimers qui citent Paris a T0 = nombre de sujets Controles qui citent Paris a T0.
## Sachant que mes donnees sont exprimees en pourcentage pour pouvoir etre comparees car je n'ai pas le meme nombre de 
#- sujets dans les 2 groupes.
#- Le test chi2 de R sort un X-squared de 16.762 ce qui est superieur a 3.841, je ne peux pas accepter l'hypothese 
#- nulle d'absence de difference. En effet, j'observe une p-value de 4.238e-05. 
chisq.test(matriceKhi2)

#==========#===================================#
# Partie 2 # Verifiation du resutlat a la main #
#==========#===================================#
## J'ajoute une 3eme ligne et une 3eme colonne a ma matrice, qui me permettra de stocker le total de chaque ligne,
#- de chaque colonne, et de la combinaison des 2.

#= Reprend l'ancienne matrice en ajoutant une ligne et une colonne =#
matriceKhi2_manuel = matrix(0,3,3)
matriceKhi2_manuel[1:2,1:2] = matriceKhi2[1:2,1:2]

#= Ajoute les totaux de chaque ligne et de chaque colonne =#
matriceKhi2_manuel[3,1] = sum(matriceKhi2_manuel[1,1],matriceKhi2_manuel[2,1])
matriceKhi2_manuel[3,2] = sum(matriceKhi2_manuel[1,2],matriceKhi2_manuel[2,2])
matriceKhi2_manuel[1,3] = sum(matriceKhi2_manuel[1,1],matriceKhi2_manuel[1,2])
matriceKhi2_manuel[2,3] = sum(matriceKhi2_manuel[2,1],matriceKhi2_manuel[2,2])
matriceKhi2_manuel[3,3] = sum(matriceKhi2_manuel[1,3],matriceKhi2_manuel[2,3])

rownames(matriceKhi2_manuel) = c("Alzheimers","Controles","Total Lignes")
colnames(matriceKhi2_manuel) = c("CiteParis_Oui (%)","CiteParis_Non (%)","Total Colonnes")

print(matriceKhi2_manuel)

#============#=================================#
# Partie 2.2 # Calcul des effectifs theoriques #
#============#=================================#

matriceKhi2_theorique = matrix(0,3,3)

#= Pour les totaux je reprends les memes valeurs ca ne change pas. =#
matriceKhi2_theorique[1,3] = matriceKhi2_manuel[1,3]
matriceKhi2_theorique[2,3] = matriceKhi2_manuel[2,3]
matriceKhi2_theorique[3,1] = matriceKhi2_manuel[3,1]
matriceKhi2_theorique[3,2] = matriceKhi2_manuel[3,2]
matriceKhi2_theorique[3,3] = matriceKhi2_manuel[3,3]

#= Effectif theorique = TotalLigne(i) * TotalColonne(j) / Total. =#
matriceKhi2_theorique[1,1] = (matriceKhi2_manuel[1,3] * matriceKhi2_manuel[3,1]) / matriceKhi2_manuel[3,3]
matriceKhi2_theorique[2,1] = (matriceKhi2_manuel[2,3] * matriceKhi2_manuel[3,1]) / matriceKhi2_manuel[3,3]
matriceKhi2_theorique[1,2] = (matriceKhi2_manuel[1,3] * matriceKhi2_manuel[3,2]) / matriceKhi2_manuel[3,3]
matriceKhi2_theorique[2,2] = (matriceKhi2_manuel[2,3] * matriceKhi2_manuel[3,2]) / matriceKhi2_manuel[3,3]

rownames(matriceKhi2_theorique) = c("Alzheimers","Controles","Total Lignes")
colnames(matriceKhi2_theorique) = c("CiteParis_Oui (%)","CiteParis_Non (%)","Total Colonnes")
                          
#= Calcul du khi 2 : Somme des (Oij - Tij)^2 / Tij =#                            
## On obtient un resultat pas trop eloigne de ce que trouve R, ici 18.320 pour toute a l'heure 16.762 c'est parceque
#- R effectue une correction... En tout cas on est coherant, ca reste significatif dans tous les cas.
                    
Khi2 = ((71-82.5)^2 / 82.5) +
  ((94-82.5)^2 / 82.5) +
  ((29-17.5)^2 / 17.5) +
  ((6-17.5)^2 / 17.5)

print(Khi2)



