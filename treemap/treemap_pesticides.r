###########################################################
#                                                         #
#                                                         #
#       Pesticides categories dynamic treemap             #
#                                                         #
#                                                         #
###########################################################

## FIST TIME ONLY : install and load devtools, required to load d3treeR
library(devtools)
install_github("timelyportfolio/d3treeR")

## set your own working dir
setwd('/home/jc/Bureau/GreenTech_Challenge/treemap/')

# load libraries
library(treemap)
library(d3treeR)

# load csv file
pests <- read.csv('ma_tot_bypests_2012.csv',sep=',', header=TRUE, na.strings=c("–", "-",""))
str(pests)

## OPTIONNEL: repartition taille des fonctions
f=pests$CODE_FONCTION
fct_size=c()
for (i in 1:length(levels(f))){
  fct_size=c(fct_size,length(which(f==levels(f)[i])))
}
names(fct_size) <- levels(f)


## plot dynamic treemap
d3tree2(
    treemap(pests,
            index=c("CODE_FONCTION", "LB_PARAMETRE"),
            vSize="ma_tot",
            #vColor="CD_PARAMETRE",
            type="index")
    , rootname = "Pests"
)

##########################

## piste d'amélioration
library(data.tree)

## build a tree
pests$CODE_FONCTION <- as.character(pests$CODE_FONCTION)
pests$LB_PARAMETRE <- as.character(pests$LB_PARAMETRE)
pests$pathString <- paste("pesticides", pests$CODE_FONCTION, pests$LB_PARAMETRE, sep = "/")
tree <- as.Node(pests[,])
print(tree, pruneMethod = "dist", limit = 20)
