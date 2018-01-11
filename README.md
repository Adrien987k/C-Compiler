Rapport : Projet de Programmation 1
======================


----------


Partie 1 : Questions
----------------------------

#### Question 1

Il est possible, lors d'un unique parcours de l'arbre de syntaxe abstraite, de stocker dans un buffer l'ensemble des chaîne de caractère à envoyer sur la sortie standard. Une fois le parcours terminé il suffit de placer en premier dans le buffer, les ligne de code correspondants aux déclarations des chaines de caractère puis d'envoyer le contenu du buffer sur la sortie standard.
Ce n'est cependant pas la méthode que j'ai choisi. 

#### Question 2

Le mot clé _let_ permet de déclarer une association entre un nom et une expression d'un certain type. Si ce type ne comporte pas le mot clé _ref_, cette association peut être vue comme un élément de sucre syntaxique permettant d'écrire des programmes plus concis. 

L'ajout du mot clé _ref_ signifie que l'on déclare une variable au même titre qu'une variable en C. Une variable est une case de mémoire unique dont le contenu peut être mis à jour partout où la variable est visible. 

Une fonction d'un programme Ocaml utilisant des variables génère donc des effets de bord difficiles à détecter.  Une fonction n'utilisant pas de variable est entièrement spécifiée et ne produit pas d'effet de bord. De plus si une variable est globale à un fichier, elle peut entrer en conflit de nom avec une autre variable globale d'un autre fichier. C'est pourquoi il est préférable de ne pas utiliser les variable dans les programmes Ocaml. 

#### Question 3

Afin de déclarer les chaines de caractères, j'effectue un premier parcours de l'arbre de syntaxe abstraite. Lors de ce parcours, je déclare également les noms des exceptions présentent après les mots clés _catch_ et _throw_, comme des chaînes de caractère et je les rajoutes dans l'environnement, à la liste des chaines de caractère. Lors de la compilation du mot clé _throw_, j'ajoute l'adresse du nom de l'exception lancée dans le registre callee-save _r14_. Ainsi, lors de la compilation d'un _try_, pour chacune des clauses _catch_, je récupère l'adresse du nom de l'exception à attraper et je la compare avec l'adresse stockée dans le registre _r14_. Ainsi si les adresses sont identiques, elles pointent sur la même chaîne de caractère. 

#### Question 4

La clause _finally_ est presque toujours exécutée, si le _try_ correspondant est exécuté. la seule exception est si un _return_ est exécuté dans un bloc catch appartement au _try_ correspondant. Dans le cas d'un _return_ dans le bloc _try_, le bloc _finally_ est exécuté. 
Dans cet exemple, la clause _finally_ n'est pas exécutée : 
```java
try {
	throw A 5;
} catch (A a) {
	return 4;
} finally {
	printf("Ce message ne s'affichera pas");
}
```
#### Question 5

Hormis l'ajout de nouveaux éléments à mon environnement, et la déclaration des exceptions dans la liste des chaines de caractères, seul le code écrit pour la compilation du _CRETURN_ et du _CALL_ a été modifié. 

Pour le _CRETURN_ : Si lors de l’exécution du premier bloc d'un _try_, si l'on tombe sur un _return_ et si la clause _try_ comporte un bloc _finally_, ce dernier bloc doit absolument être exécuté avant le retour de la fonction. 

Pour le _CALL_ : Juste après l'appel d'une fonction, il faut vérifier si une exception a été levée. Pour cela, j'utilise une variable globale assembleur _.exception\_raised_, initialisée à 0, et mise à 1 lors de l’exécution d'un _throw_. Ainsi, après l'appel d'une fonction je test si _.exception_raised_ vaut 1, et si la fonction est appelée dans un bloc _try_, auquel cas je cherche la clause _catch_ correspondante. Sinon j’exécute la sortie immédiate de la fonction.

Il serai possible, lors de la compilation du _CTRY_ de faire un parcours spécifique de l'arbre de syntaxe à la recherche de _return_ et d'appels de fonction. Cela est cependant extrêmement lourd et amène et réécrire du code déjà écrit. 

#### Question 6

L'utilisation des exceptions permet une gestion plus fine des erreurs et ainsi permet d'éviter de nombreux plantages d'un programme. En effet dans une situation ou un code peut potentiellement générer une erreur, il suffit d'entourer ce code d'une clause _try_ pour ainsi traiter l'erreur potentielle et continuer l'exécution du programme sans pour autant que ce dernier ne plante. La clause _finally_ permet par exemple en cas d'erreur irrattrapable, d'effectuer une ultime action comme la fermeture ou la sauvegarde d'un fichier en cours d'utilisation, ou encore l'enregistrement dans un fichier de log ou l'envoie vers un serveur, des informations concernant l'erreur.

----------


Partie 2 : Compilation des exceptions
------------------------------------------------

#### L'environnement

L'environnement Ocaml comporte maintenant les variables supplémentaires suivantes : 

```ocaml
type env = {
  ...
  catch_label : string;
  depth_try : int;
  finally_labels : string list;
  in_finally : bool;
}
```

* catch\_label : Lors de la compilation du code d'un bloc _try_, on génère un label assembleur vers lequel sauter pour arriver devant un code qui effectuera les comparaisons entre l'exception levée et les différentes exceptions qui peuvent potentiellement être attrapées. Ce label est stocké dans la variable _catch\_label_.

* depth\_try : il s'agit de la profondeur du bloc _try_ courant dans les imbrications de blocs _try_. Voici un exemple montrant la valeur de _depth\_try_ lors de sa compilation,
```java
depth = 0
try {
    depth = 1
    try {
        depth = 2
    } catch {
	     depth = 1
	} finally {
		depth = 1
	}
    depth = 1
}
depth = 0
```
Cela permet de vérifier si l'on se trouve dans un _try_, afin de vérifier si une exception peux être attrapée, directement après un _throw_ ou après un appel de fonction

* finally\_labels : Lors de la compilation d'un _try_, un label assembleur est généré et correspond à l'endroit où sauter afin d'aller à l’exécution de la clause _finally_. Ce label est ajouté en tête de la liste _finally\_labels_ qui contient tous les labels de tous les blocs _finally_ des clauses _try_ courantes imbriquées. Voici un exemple montrant un code et la valeur courante de _finally\_labels_ lors de sa compilation, en supposant que finally_label_n est un label qui permet d'effectuer le finally n
```java
finally\_labels = []
try {
    finally\_labels = [finally_label_2]
    try {
        finally\_labels = [finally_label_1; finally_label_2]
    } catch {
        finally\_labels = [finally_label_1; finally_label_2]
    } finally {
        //finally 1
        finally\_labels = [finally_label_2]
    }
} catch {
    finally\_labels = [finally_label_2]
} finally {
  //finally 2
  finally\_labels = []
}
finally\_labels = []
```
Cela permet, lors d'un _return_ dans un _try_ lui même imbriqué dans d'autres _try_, d'effectuer toutes les clauses _finally_ des différents _try_

* in\_finally : Un booléen indiquant si le code couramment exécuté se trouve dans une clause _finally_ ou non.

L'environnement assembleur comporte les deux variables globales suivantes, déclarées et initialisées à 0 au début de la compilation d'un programme :

* .exception\_raised : cette variable est mise à 1 après l'éxecution d'un _throw_. Elle restera à 1 jusqu'à la gestion de l'exception levée.

* .return\_label\_set : cette variable est mise à 1 juste avant l’exécution d'un _return_ se trouvant dans un bloc _try_. Elle permet à ce que juste après l’exécution d'un _finally_ n'atteignant pas de _return_, de savoir s'il reste un _return_ à exécuter dans le bloc _try_ correspondant. Si c'est le cas, un label aura été stocké dans le registre _r13_ qui est callee-save. Il suffit de sauter vers ce label en utilisant l'étoile :
	```
	jmp   *%r13
	```
De plus, la valeur à retourner est stockée dans le registre callee-save _r15_. En effet, si c'est une variable qui est retournée, le bloc _finally_ peut potentiellement la modifier alors qu'il faut retourner sa valeur au moment de l'arrivée sur le _return_

#### Stratégie de compilation

La première étape consiste à déclarer et initialiser les deux variables assembleur présentées plus haut. Ensuite, les noms des exceptions sont déclarés comme des chaînes de caractères et stockés dans l'environnement, comme expliqué dans la réponse à la question 3.

* Compilation de CTHROW :
    * La variable _.exception\_raised_ est mise à 1
    * Le nom de l'exception est cherchée dans l’environnement afin de récupérer  l'adresse de sa chaîne de caractère qui est ensuite stockée dans le registre callee-save _r12_
    * L'expression associée au _throw_ est exécutée et sa valeur est stockée dans le registre callee-save _r14_
    * Si le _throw_ se trouve dans un bloc _try_, on saute directement vers le label stocké dans la variable _env.catch\_label_. On peut vérifier cela grâce à la variable _env.depth_try_.
    * Sinon on quitte la fonction avec _leave_ et _ret_

* Compilation de CRETURN :
    * Si on se trouve dans un bloc _finally_, on remet 0 dans _.exception\_raised_ et _.return\_label\_set_. En effet, dans ce cas on quitte la fonction normalement, toutes les exceptions ont été gérées
    * On exécute l'expression à retourner et on stocke sa valeur dans le registre _r15_
    * Si on se trouve dans un bloc _try_, et que ce _try_ comporte un bloc _finally_ (c'est le cas si _env.finally\_labels_ est différent de la liste vide), pour chacun des labels dans _env.finally\_labels_, on effectue les opérations suivantes.
      * On génère un label de retour, _return\_label_ que l'on stocke dans le registre _r13_
      * on met la variable _.return\_label\_set_ à 1.
      * On exécute le bloc _finally_ correspondant en sautant sur le label
      * On produit le code
      ```java
      return_label:
      ```
      Ainsi les différents bloc _finally_ vont être éxectutés. Si l'un d'eux comporte un _return_ les autres seront éxectués de manière récursive et c'est la valeur du bloc _finnaly_ contenant un _return_ le plus à l'extérieur qui sera renvoyée.
    * Si on a sauvegardé une valeur dans _r15_, on retourne cette valeur. Sinon on compile l'expression _return_ comme dans la première partie du projet

* Compilation du CTRY :
    * On génère un label _label\_end_ permettant de sauter à la fin des catchs
    * S'il y a un bloc _fianlly_, on génère un label permettant de sauter au _finally_, que l'on ajoute en tête de la liste _finally\_labels_
    * On incrémente la variable _env.depth_try_ de 1
    * On génère une liste de labels pour chacun des catchs afin de sauter vers les codes correspondants si une exception levée est attrapée
    * On stocke le contenu de la variable _env.catch\_label_ et l'on génère un nouveau label permettant d'accéder aux comparaisons entre la listes des exceptions attrapées et l'exception courante levée. On stocke ce label dans la variable _env.catch\_label_
    * On compile le code du bloc _try_ puis on restaure la variable _env.catch\_label_
    * On produit le code effectuant les comparaisons comme expliqué dans la réponse à la question 3
    * Pour chacun des catchs, on commence par produire le code :
    ```java
    label:
    ```
    Ou _label_ est le label généré plus haut, et correspondant au catch
    * Pour une clause (_catch E e_) On alloue sur la pile pour ce catch uniquement, une variable locale de nom _e_ où l'on met la valeur du registre _r12_
    * On compile le code correspondant et on saute sur le label de fin générer en première étape
    * On restaure l'ancienne valeur de _env.finally\_labels_ en supprimant son premier élement
    Les dernières étapes ne s’effectuent que dans le cas où il y a un bloc _finally_
    * On compile le code associé en passant dans l’environnement la variable _env.in_finally_ à vrai
    * Si l'on arrive à la fin, c'est à dire qu'aucun _return_ n'a été rencontré :
    On vérifie la présence d'un _return_ rencontré dans le bloc _try_ grâce à la variable _.return\_label\_set_. Si cette variable vaut 1, on exécute le _return_ comme expliqué plus haut
    * Sinon on vérifie si une exception a été levée dans le bloc _finally_ auquel cas on quitte la fonction avec _leave_ et _ret_

* Compilation du CALL
    * Après avoir éxecuté l'instruction _call_, on vérifie si la variable _.exception\_raised_ est à 1 auquel cas une exception a été levée dans la fonction
    Si c'est le cas,
        * Si on se trouve dans un bloc _try_, on saute sur le label stocké dans _env.catch_label_
        * Si on ne se trouve pas dans un bloc _finally_ on quitte la fonction
        (en effet dans le cas d'un bloc _finally_ il faut continuer et terminer l’exécution du bloc)

Le reste du code n'est pas modifié.

#### Avantages et inconvéniants

* L'utilisation d'un type enregistrement permet d'avoir un unique argument _env_ à passer en paramètre des fonctions. Cependant une modification d'un champ de l'environnement nécessite de faire une copie de celui-ci, ce qui engendre une syntaxe lourde

* Maintenir dans l'environnement la liste des labels vers les différents bloc _finnaly_ des clauses _try_ courantes est également très lourd. Pour gérer les cas de clauses _try_ imbriqués_, il existe surement des façons plus naturelles de respecter la sémantique pour gérer les cas de clauses _try_ imbriqués. En pensant de manière plus récursive notemment.

----------




















