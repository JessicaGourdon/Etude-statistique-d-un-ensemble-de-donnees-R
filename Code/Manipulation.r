#Pour pouvoir exexuter notre programme, vous devez vous placer dans le dossier de notre projet
#grace a l'onglet Session puis Set Working Directory

#On a cr?? une fonction permettant d'enlever les "," et les "$" de nos valeurs

enleverdollar<- function(x){
  gsub("\\$|,","",x)
}

#On r?cup?re tous les fichiers 
donneescode=read.csv(file = "codes.csv", na.string='NA')
donneescategories=read.csv(file = "categories.csv",na.string='NA')

#On r?cup?re les noms des pays, les cat?gories et leurs r?f?rences
nomPays<- data.frame(Name= donneescode[,'Name'])
nomCategories<- data.frame(Name= donneescategories[,'Name'],Ref= donneescategories[,'Num'])

#On commence ? remplir notre WorldFactBook avec seulement le nom des pays pour l'instant
WorldFactBook<- data.frame(Name= nomPays$Name)

for (i in 1:dim(nomCategories)[1]){

  #On r?cup?re les r?f?rences des cat?gories pour r?cup?rer le c*.csv correspondant
  r= nomCategories$Ref[i]
  #On cr?e la chaine de caract?res repr?sentant le nom du fichier ? r?cup?rer
  str=paste("./data/c",r,".csv",sep="")
  #On r?cup?re le csv correspondant
  csvCorrespondant=read.csv(file= str, na.strings = 'NA' )
  #On ne garde que les colonnes Name et Value
  el<- data.frame(Pays= csvCorrespondant[,'Name'],Valeur=csvCorrespondant[,'Value'])
  #Gr?ce ? setdiff on recup?re les pays qui sont dans WorldFactBook mais qui ne sont pas dans les donn?es r?cup?r?es du csv
  #On veut les rajouter avec une valeur NA
  manque=setdiff(WorldFactBook$Name,el$Pays)
  PaysNA <-data.frame(Pays=manque, Valeur= 'NA')
  #On ajoute les pays manquants ? nos donn?es r?cup?r?es pour la cat?gories qu'on traite
  cvs <- rbind(el,PaysNA)
  cvs[] <- lapply(cvs, as.character)
  cvs<- cvs[order(cvs$Pays),]
  #on prend soin de trier les donn?es selon l'ordre alhphabetique des pays
  tmp<-data.frame(Name=cvs$Pays,Valeur=cvs$Valeur)
  #Il peut arriver que l'on r?cup?re des donn?es dans les csv correspondant ? des pays qui ne nous int?ressent pas
  #Ces pays ne sont pas contenus dans la liste des pays que l'on souhaite ?tudier
  test<- data.frame(Pays=setdiff(tmp$Name,WorldFactBook$Name))
  #on r?cup?re le nom d'?ventuels pays qui se trouveraient dans nos donn?es r?cup?r?es mais pas dans WorldFactBook
  
  if(dim(test)[1]!=0){
    #On teste si il y a effectivement des pays dans nos donn?es qu'on ne veut pas ?tudier 
    for (i in 1:dim(test)[1]){
      #Pour chacun de ces pays on recup?re l'indice de sa position dans notre tableau de donn?es recup?r?es
      ind=which(cvs$Pays==test[i,1])
      cvs<-cvs[-ind,]
      #On enl?ve ensuite la ligne de donn?es correspondant ? ce pays
    }
  }

  #On r?cup?re les valeurs du c*.csv et on enleve les "," et les "dollars"$" si besoin
  result=cvs$Valeur
  result=enleverdollar(result)
  
  #On le rentre dans notre WorldFactBook
  WorldFactBook<- data.frame(WorldFactBook, result)
  
}
#Enfin, on souhaite renommer les colonnes de notre WorldFactBook
WorldFactBook_bis<-WorldFactBook[,-1]
#On recup?re dans une autre variable toutes nos colonnes qu'on 
#renomme pour chaque nom de cat?gorie
colnames(WorldFactBook_bis)<-nomCategories$Name
c1<- data.frame(Name= nomPays$Name)
#On refait notre WorldFactBook avec en premiere colonne les noms
#de chaque pays puis toutes les cat?gories avec leurs noms
WorldFactBook<-cbind(c1,WorldFactBook_bis)

write.csv(WorldFactBook,'WorldFactBook.csv')