**French version - English below**

# Mon manuscrit de thèse

Formalisation et étude de problématiques de scoring en risque de crédit
Inférence de rejet, discrétisation de variables et interactions, arbres de régression logistique

Thèse de Doctorat en Mathématiques et leurs interactions de l'Université de Lille, Spécialité Statistique  
Soutenue le ...  
Préparée au sein du Laboratoire Paul Painlevé, de l'Inria Lille  
Nord-Europe et de Crédit Agricole Consumer Finance.  
École doctorale Sciences pour l’Ingénieur  
Unité de recherche Équipe-projet MODAL


Les codes nécessaires à l'obtention de la majorité des résultats sur données simulées / UCI sont disponibles dans ce repo.  
Pour compiler la thèse, il est nécessaire d'avoir [git](https://git-scm.com/), [arara](https://github.com/cereda/arara) (inclus dans la plupart des distributions TeX), et une [distribution TeX](https://www.latex-project.org/get/).

```bash
git clone https://github.com/adimajo/manuscrit_these.git
cd manuscrit_these
arara these -v
```

Le fichier `these.pdf` est ainsi obtenu.

**English version - French above**

# My PhD thesis

Formalization and study of statistical problems in Credit Scoring
Reject inference, discretization and pairwise interactions, logistic regression trees

PhD of Applied Mathematics from the University of Lille, Speciality Statistics  
Thesis defended on ...  
Prepared at Laboratoire Paul Painlevé, Inria Lille Nord-Europe and Crédit Agricole Consumer Finance.  
Doctoral School Sciences pour l’Ingénieur  
University Department Équipe-projet MODAL

The code to obtain all results on simulated / UCI data are made available in this repo.  
To compile the manuscript, [git](https://git-scm.com/), [arara](https://github.com/cereda/arara) (included in lots of TeX distributions) and a [TeX distribution](https://www.latex-project.org/get/) are needed.

```bash
git clone https://github.com/adimajo/manuscrit_these.git
cd manuscrit_these
arara these -v
```

The file `these.pdf` should be visible in the `manuscrit_these` folder.

## Credits

This PhD is built on the [yathesis LaTeX class](https://ctan.org/pkg/yathesis) (under license LPPL Ver­sion 1.3c).  
To build the pdf file, [arara](https://github.com/cereda/arara) is used (BSD Licence).
To make sure LaTeX packages are not loaded but not used, I used the Python [LaTeXpkges script](https://github.com/TarasKuzyo/LaTeXpkges) (MIT Licence).  
Feel free to clone / fork / star for your own work!
