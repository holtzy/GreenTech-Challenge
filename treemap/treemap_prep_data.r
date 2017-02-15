### prepare data for treemap

## set your own working dir
setwd('/home/jc/Bureau/GreenTech_Challenge/')

# load csv file
ma <- read.csv('DATA/Moyennes_analyses_pesticides\ dans\ eaux\ souterraines_HISTORIQUE/fichiers\ csv/ma_qp_fm_ttres_pesteso_2012_utf.csv',sep=';', header=TRUE, na.strings=c("–", "-",""))
str(ma)
pests <- read.csv('DATA/Pesticides/pesticides_utf.csv',sep=';', header=TRUE, na.strings=c("–", "-",""))
str(pests)

# change data type
ma$MA_MOY <- as.numeric(sub("," , ".",ma$MA_MOY))[ma$MA_MOY]

## aggregate values
agg <- aggregate(ma[,"MA_MOY"], by=list(ma$LB_PARAMETRE), "sum")
names(agg) <- c("LB_PARAMETRE","ma_tot")

# match pests
matchvec<- which(pests$LB_PARAMETRE%in%agg$LB_PARAMETRE)
agg<- cbind(agg, pests[matchvec,c("CODE_FAMILLE","CODE_FONCTION")])

write.csv(agg, "treemap/ma_tot_bypests_2012.csv")
