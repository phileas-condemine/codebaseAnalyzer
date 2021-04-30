# Motivations

Ce projet vise à produire une visualisation de la structure de code de la pipeline de production [SIDEP](https://gitlab.com/corruss/sidep).

# Architecture du projet

Le code central est **usecase.R**. Les autres scripts sont sourcés au début et contiennent uniquement des fonctions invoquées dans usecase.R.

Ce projet a été pour [moi](https://www.linkedin.com/in/phil%C3%A9as-condemine-6a46025a/) l'occasion de découvrir le parseur de code R (fonction `parse`).

Entre les premières fonctions développées dans sources, inputs & utils et celles réalisées plus tardivement dans functions, on peut noter une progression.

L'approche consiste en plusieurs étapes : 

- A partir d'un fichier central "main" on identifier les scripts sourcés (récursivement). Eventuellement les chemins vers ces scripts peuvent être des expressions à `eval`uer. Ainsi on connait la relation entre les différents scripts.
- Ensuite on s'intéresse aux fichiers lus et écrits grâce à des fonctions (reader/writer). Grâce au parseur R et à quelques règles manuelles on peut récupéré l'argument de chaque fonction d'IO qui désigne le fichier à lire/écrire.
- Il s'agit alors d'évaluer cet argument, pour cela on remonte dans le code de manière récursive à la recherche d'assignations `<-`, `=` ou `<<-` non auto-référencées pour définir les variables permettant cette évaluation. 

Par exemple dans `filepath = paste0(path,Sys.Date(),"bonjour.csv")`, `Sys.Date` est une fonction de base de R, mais path est un objet inconnu. Il faut retrouver sa définition et la bonne ! C'est à dire la dernière avant d'évaluer filepath. Et faire attention aux définitions auto-référencées qui peuvent suivre et affiner la construction de path. Dans certains cas, path est obtenu par un `list.files(...,pattern=...,full.names=T)` puis affiné en récupérant les dates de fichier et en prenant le dernier.

Un autre problème surgit avec l'utilisation (souhaitable) de fonctions définies par l'utilisateur. Ces fonctions peuvent contenir des étapes de lecture/écriture. Il faut donc traiter les fonctions comme des sources() mais avec 2 étapes : définition et invocation.s (multiples). A chaque invocation ses paramètres... Si nécessaire (pas eu besoin pour l'instant) on devra retrouver récursivement les objets dont dépendent ces paramètres pour les évaluer. 

Exemple : `my_fun = function(path){...;fwrite(x,path)}` et plus tard `my_fun(paste0(my_path,Sys.Date(),"bonjour.csv")`. Même problème qu'avant, il faut trouver le `my_path` approprié.