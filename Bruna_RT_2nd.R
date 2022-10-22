setwd("~/Box/Manuscripts/alfalfa/Bruna_RootData")
library(openxlsx)
sheetNames2=getSheetNames("Secondary root lengths_ALL REPS.xlsx")

df_33_34= data.frame()
for (i in 1:4){
df2= read.xlsx("Secondary root lengths_ALL REPS.xlsx", sheet =sheetNames2[i], startRow = 6 , na.strings = c(" ", "."))
print(sheetNames2[i])
first_position_3233= head(grep("3233", df2[1,]), n=1)
last_position_3233=  tail(grep("3233", df2[1,]), n=1)
first_position_3234= head(grep("3234", df2[1,]), n=1)
last_position_3234= tail(grep("3234", df2[1,]), n=1)

L33= df2[2:nrow(df2), first_position_3233:last_position_3233]
L33= apply(L33, 2, as.numeric)
#L33$cultivar = "L3233"






no_of_roots_L33 = apply(L33, 2, function (x) {sum(!is.na(x))})
mean_of_Root_L33_all= apply(L33, 2, function(x) {mean(as.numeric( x), na.rm = T)})

mean_of_Root_L33_gt1= apply(L33, 2, function(x) {mean(x[x>=1], na.rm = T)})
mean_of_Root_L33_gt2= apply(L33, 2, function(x) {mean(x[x>=2], na.rm = T)})
mean_of_Root_L33_gt25= apply(L33, 2, function(x) {mean(x[x>=2.5], na.rm = T)})
mean_of_Root_L33_gt3= apply(L33, 2, function(x) {mean(x[x>=3], na.rm = T)})
mean_of_Root_L33_gt35= apply(L33, 2, function(x) {mean(x[x>=3.5], na.rm = T)})
mean_of_Root_L33_gt4= apply(L33, 2, function(x) {mean(x[x>=4], na.rm = T)})
mean_of_Root_L33_lt1= apply(L33, 2, function(x) {mean(x[x<=1], na.rm = T)})
mean_of_Root_L33_lt05= apply(L33, 2, function(x) {mean(x[x<=0.8], na.rm = T)})
mean_of_Root_L33_lt1= apply(L33, 2, function(x) {mean(x[x<=1], na.rm = T)})
mean_of_Root_L33_lt15= apply(L33, 2, function(x) {mean(x[x<=1.5], na.rm = T)})
mean_of_Root_L33_lt2= apply(L33, 2, function(x) {mean(x[x<=2], na.rm = T)})
max_of_Root_L33_lt2= apply(L33, 2, function(x) {max(x, na.rm = T)})
min_of_Root_L33_lt2= apply(L33, 2, function(x) {min(x, na.rm = T)})
df33= data.frame(No_roots= no_of_roots_L33,
                 mean_all= mean_of_Root_L33_all,
                 mean_gt1= mean_of_Root_L33_gt1,
                 mean_gt2= mean_of_Root_L33_gt2,
                 mean_gt25= mean_of_Root_L33_gt25,
                 mean_gt3= mean_of_Root_L33_gt3,
                 mean_gt35= mean_of_Root_L33_gt35,
                 mean_gt4= mean_of_Root_L33_gt4,
                 mean_lt05= mean_of_Root_L33_lt05,
                 mean_lt1= mean_of_Root_L33_lt1,
                 mean_lt15= mean_of_Root_L33_lt15,
                 mean_lt2= mean_of_Root_L33_lt2,
                 maxValue =max_of_Root_L33_lt2,
                 minValue= min_of_Root_L33_lt2
                 
                 
)

df33$cultivar= "L33"
df33$sheetNames= sheetNames2[i]





mean_of_Root_L33_all= apply(L33, 2, function(x) {mean(as.numeric( x), na.rm = T)})
L34 =df2[2:nrow(df2),first_position_3234:last_position_3234]
L34= apply(L34, 2, as.numeric)
#L34$cultivar= "L3234"
no_of_roots_L34 = apply(L34, 2, function (x) {sum(!is.na(x))})
mean_of_Root_L34_all= apply(L34, 2, function(x) {mean(as.numeric( x), na.rm = T)})

mean_of_Root_L34_gt1= apply(L34, 2, function(x) {mean(x[x>=1], na.rm = T)})
mean_of_Root_L34_gt2= apply(L34, 2, function(x) {mean(x[x>=2], na.rm = T)})
mean_of_Root_L34_gt25= apply(L34, 2, function(x) {mean(x[x>=2.5], na.rm = T)})
mean_of_Root_L34_gt3= apply(L34, 2, function(x) {mean(x[x>=3], na.rm = T)})
mean_of_Root_L34_gt35= apply(L34, 2, function(x) {mean(x[x>=3.5], na.rm = T)})
mean_of_Root_L34_gt4= apply(L34, 2, function(x) {mean(x[x>=4], na.rm = T)})
mean_of_Root_L34_lt1= apply(L34, 2, function(x) {mean(x[x<=1], na.rm = T)})
mean_of_Root_L34_lt05= apply(L34, 2, function(x) {mean(x[x<=0.8], na.rm = T)})
mean_of_Root_L34_lt1= apply(L34, 2, function(x) {mean(x[x<=1], na.rm = T)})
mean_of_Root_L34_lt15= apply(L34, 2, function(x) {mean(x[x<=1.5], na.rm = T)})
mean_of_Root_L34_lt2= apply(L34, 2, function(x) {mean(x[x<=2], na.rm = T)})
max_of_Root_L34_lt2= apply(L34, 2, function(x) {max(x, na.rm = T)})
min_of_Root_L34_lt2= apply(L34, 2, function(x) {min(x, na.rm = T)})

df34= data.frame(No_roots= no_of_roots_L34,
                  mean_all= mean_of_Root_L34_all,
                 mean_gt1= mean_of_Root_L34_gt1,
                 mean_gt2= mean_of_Root_L34_gt2,
                 mean_gt25= mean_of_Root_L34_gt25,
                 mean_gt3= mean_of_Root_L34_gt3,
                 mean_gt35= mean_of_Root_L34_gt35,
                 mean_gt4= mean_of_Root_L34_gt4,
                 mean_lt05= mean_of_Root_L34_lt05,
                 mean_lt1= mean_of_Root_L34_lt1,
                 mean_lt15= mean_of_Root_L34_lt15,
                 mean_lt2= mean_of_Root_L34_lt2,
                 maxValue =max_of_Root_L34_lt2,
                 minValue= min_of_Root_L34_lt2

)

df34$cultivar= "L34"
df34$sheetNames= sheetNames2[i]

print(((dim(df33))))
print(dim(df34))
df_temp= rbind(df33, df34)
if (nrow(df_33_34)==0){
df_33_34=df_temp

}else{
  df_33_34= rbind(df_33_34, df_temp)
}
print(dim(df_33_34))

}
sum(is.na(df_33_34))
df_33_34
dim(df_33_34)
table(df_33_34$sheetNames)
tail(df_33_34, n=2)
table(df_33_34$cultivar)
df_33_34$phenotype = rownames(df_33_34)
df_33_34$phenotype[grep("Branch", df_33_34$phenotype)]="B"
df_33_34$phenotype[grep("Tap", df_33_34$phenotype)]="T"
df_33_34$phenotype[grep("TB", df_33_34$phenotype)]="TB"
table(df_33_34$cultivar,df_33_34$phenotype)

df_33_34$Lines= paste0( df_33_34$phenotype, df_33_34$cultivar, seq(1:nrow(df_33_34)))
df_33_34_numberOnly=df_33_34[1:(ncol(df_33_34)-4)]
rownames(df_33_34_numberOnly)= df_33_34$Lines
# https://www.youtube.com/watch?v=0Jp4gsfOLMs
pcaAlfalfa= prcomp(df_33_34_numberOnly, scale=T, center = T)

summary(pcaAlfalfa)

plot(pcaAlfalfa$x[,1], pcaAlfalfa$x[,2])
pcaAlfalfa.var = pcaAlfalfa$sdev^2
pcaAlfalfa.var.Per= round(pcaAlfalfa.var/sum(pcaAlfalfa.var)*100, 2)
barplot(pcaAlfalfa.var.Per, main = "PCA component importance", xlab = "Pricipal Component", ylab = "Percent variation (%)", col="blue")

pca_df= data.frame(lines= rownames(pcaAlfalfa$x),
                   X= pcaAlfalfa$x[, 1],
                   Y= pcaAlfalfa$x[,2],
                   cultivar= df_33_34$cultivar,
                   phenotype= df_33_34$phenotype)

head(pca_df, n=2)

library(ggplot2)
ggplot(pca_df, aes(x=X, y=Y, label= lines, colour= phenotype))+
  geom_text(size=5) +
  theme_bw() +
  xlab(paste("PC1 - ", pcaAlfalfa.var.Per[1], "%", sep=""))+
  ylab(paste("PC2 - ", pcaAlfalfa.var.Per[2], "%", sep = ""))+
  ggtitle("Alfalfa Root Type PCA analysis")


ggplot(pca_df, aes(x=X, y=Y, shape= phenotype, col= phenotype))+
 # geom_text(size=5) +
  geom_point(size=5)+
  theme_bw() +
  xlab(paste("PC1 - ", pcaAlfalfa.var.Per[1], "", sep=""))+
  ylab(paste("PC2 - ", pcaAlfalfa.var.Per[2], "", sep = ""))+
  ggtitle("")+
  theme(legend.position = "")

ggplot(pca_df, aes(x=X, y=Y, 
                   col= phenotype))+
  # geom_text(size=5) +
  geom_point(size=5)+
  theme_bw() +
  xlab(paste("PC1 - ", pcaAlfalfa.var.Per[1], "%", sep=""))+
  ylab(paste("PC2 - ", pcaAlfalfa.var.Per[2], "%", sep = ""))+
  ggtitle("Alfalfa Root Type PCA analysis")



importance_varianles = sort(pcaAlfalfa$rotation[,1], decreasing = T)
importance_varianles


library(pca3d); library(rgl)

scores_alfalfa =data.frame( pcaAlfalfa$x[,1:3])
scores_alfalfa$culitvar_genotype= paste0(df_33_34$cultivar, df_33_34$phenotype)
scores_alfalfa$culitvar_genotypeC= paste0(df_33_34$cultivar, df_33_34$phenotype)
table(scores_alfalfa$culitvar_genotype)

scores_alfalfa$culitvar_genotype[scores_alfalfa$culitvar_genotype=="L33B"]=1
scores_alfalfa$culitvar_genotype[scores_alfalfa$culitvar_genotype=="L33T"]=2
scores_alfalfa$culitvar_genotype[scores_alfalfa$culitvar_genotype=="L33TB"]=3
scores_alfalfa$culitvar_genotype[scores_alfalfa$culitvar_genotype=="L34B"]=4
scores_alfalfa$culitvar_genotype[scores_alfalfa$culitvar_genotype=="L34T"]=5
scores_alfalfa$culitvar_genotype[scores_alfalfa$culitvar_genotype=="L34TB"]=6


#scores_alfalfa$culitvar_genotype= df_33_34$groups
plot3d(x=scores_alfalfa$PC1,y= scores_alfalfa$PC2, z=scores_alfalfa$PC3, type="p",size=6,  col=as.numeric(scores_alfalfa$culitvar_genotype),
       xlim = c(-5,5), ylim=c(-5,5), zlim=c(-5,5))

text3d(scores_alfalfa[,1]+0.2, scores_alfalfa[,2]+0.2, scores_alfalfa[,3]+0.2,
       texts=c((scores_alfalfa$culitvar_genotypeC), cex= 0.7, pos=3))



# use princomp to check the differences
# youtube link: https://www.youtube.com/watch?v=NLrb41ls4qo
pca2= princomp(df_33_34_numberOnly, cor = T, scores = T)
summary(pca2)
plot(pca2, col="blue")

biplot(pca2)
importance_var2= sort(abs(pca2$loadings[,1]), decreasing = T)
importance_var2
barplot(importance_var2, col = "blue", las=2,  horiz =F, ylim = c(0, 0.4), main = "Importance of variables")

library(cluster)
km.res <- kmeans(df_33_34_numberOnly, 2, nstart = 50)
# Visualize
library("factoextra")
fviz_cluster(km.res, data = df_33_34_numberOnly, frame.type = "norm", ellipse = TRUE, frame.level=0.7, labelsize=14)+
  theme_minimal()+
  xlab("PC1 and percentage 50.6% ")+
  ylab("Pc2 and percenate 21.1%")


# 3rd method for PCA

library("FactoMineR")
pca3 <- PCA(df_33_34_numberOnly, graph = T)
fviz_eig(pca3, addlabels = T, ylim=c(0, 55))
fviz_pca_var(pca3, col.var = "black")
library(corrplot)
var = get_pca_var(pca3)
head(var$contrib)
corrplot(var$cos2, is.corr = F)
fviz_cos2(pca3, choice = "var", axis=1:2)


fviz_pca_ind(pca3, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

str(df_33_34)
df_33_34$groups= paste0(df_33_34$cultivar, df_33_34$phenotype)
fviz_pca_ind(pca3,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = df_33_34$phenotype, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)




#pca.idc_steven <- prcomp(SSPdata[,-1], scale.=TRUE)
df_33_34$cluster= paste0(df_33_34$cultivar, df_33_34$phenotype)
position_cluter= grep("phenotype", names(df_33_34))
pca3d(pcaAlfalfa, components = 1:3, bg = "white", fancy = F, group = df_33_34[, position_cluter], 
      show.group.labels =  T, axes.color= "black", new= F,
      show.shapes=T, show.plane =T, radius = 3, labels.col=30, legend = "right"
)
axes3d(edges=c("x--", "y--", "z"), lwd=1, axes.len=1, labels=FALSE)
grid3d("x")
grid3d("y")
grid3d("z")





# Finding the right citation information is sometimes complicated. In R, this process is made quite easy, 
# you simply run citation("packagename"). For instance, 
citation("dplyr")
citation("pca3d")
citation("stats")

















# based on the new data, conduct t.test and darw figure
PValues_RT= data.frame(trait=matrix(nrow = ncol(df_33_34_numberOnly)),
                       pvalue=matrix(nrow = ncol(df_33_34_numberOnly)))

library(ggpubr)
par(mfrow=c(3,5))
plots_list=list()
for (i in (1:ncol(df_33_34_numberOnly))){
  PValues_RT[i,1]=print(names(df_33_34)[i])
  df05_L33= df_33_34[df_33_34$cultivar=="L33",]; print(nrow(df05_L33))
  df05_L34= df_33_34[df_33_34$cultivar=="L34",]; print(nrow(df05_L34))
  
  tResults =t.test(df05_L33[, names(df_33_34)[i]], df05_L34[,names(df_33_34)[i]], alternative = "greater")
  print(tResults)
  PValues_RT[i,2]=tResults[3]
  plotNames= paste0("plot_", i)
  plotNames= ggboxplot(df_33_34, x= "cultivar", y=names(df_33_34)[i] , add = "jitter", fill = "cultivar")+
    stat_compare_means(method = "t.test", method.args =list(alternative= "less"))+ 
   ggtitle(paste0("The trait tested is: ", names(df_33_34)[i]))
  #print(plotNames)
  Y_min1_L34= min(df_33_34[,names(df_33_34)[i]])
  Y_max1_L34= max(df_33_34[,names(df_33_34)[i]])
  
  plots_list[[i]]=ggboxplot(df_33_34, x= "phenotype", y= names(df_33_34)[i], 
                            fill = "phenotype", facet.by = "cultivar", add = "jitter")+
    stat_compare_means(comparisons = list(c("B", "T"), c("T", "TB"), c("B", "TB")), method = "t.test")+
    stat_summary(fun.data = function(x) data.frame(y=(Y_min1_L34 +1), label = paste("Mean=",round(mean(x),2))), geom="text") +
    theme(legend.position="none")+
    ggtitle(paste0("Measurements: ", names(df_33_34)[i]))
  

}



plot_numbers_roots_L33 = ggboxplot(df_33_34, x= "phenotype", y= "No_roots", ylim=c(9,40),
                                   fill = "phenotype", facet.by = "cultivar", add = "jitter")+
  stat_compare_means(comparisons = list(c("B", "T"), c("T", "TB"), c("B", "TB")), method = "t.test")+
  stat_summary(fun.data = function(x) data.frame(y=10, label = paste("µ=",round(mean(x),2))), geom="text") +
  theme(legend.position="none")+
  ggtitle(paste0("Measurements: ", "Number of roots"))



plot_mean_all_L33 = ggboxplot(df_33_34, x= "phenotype", y= "mean_all", ylim=c(1,8),
                                   fill = "phenotype", facet.by = "cultivar", add = "jitter")+
  stat_compare_means(comparisons = list(c("B", "T"), c("T", "TB"), c("B", "TB")), method = "t.test")+
  stat_summary(fun.data = function(x) data.frame(y=1, label = paste("µ=",round(mean(x),2))), geom="text") +
  theme(legend.position="none")+
  ggtitle(paste0("Measurements: ", "Mean from all data"))


plot_mean_gt1_L33 = ggboxplot(df_33_34, x= "phenotype", y= "mean_gt1", ylim=c(1,8),
                                   fill = "phenotype", facet.by = "cultivar", add = "jitter")+
  stat_compare_means(comparisons = list(c("B", "T"), c("T", "TB"), c("B", "TB")), method = "t.test")+
  stat_summary(fun.data = function(x) data.frame(y=1, label = paste("µ=",round(mean(x),2))), geom="text") +
  theme(legend.position="none")+
  ggtitle(paste0("Measurements: ", "Mean of >1.0cm"))

plot_mean_gt2_L33 = ggboxplot(df_33_34, x= "phenotype", y= "mean_gt2", ylim=c(2,9.5),
                                   fill = "phenotype", facet.by = "cultivar", add = "jitter")+
  stat_compare_means(comparisons = list(c("B", "T"), c("T", "TB"), c("B", "TB")), method = "t.test")+
  stat_summary(fun.data = function(x) data.frame(y=3, label = paste("µ=",round(mean(x),2))), geom="text") +
  theme(legend.position="none")+
  ggtitle(paste0("Measurements: ", "Mean of >2.0cm"))


plot_mean_gt25_L33 = ggboxplot(df_33_34, x= "phenotype", y= "mean_gt25", ylim=c(2,9.5),
                              fill = "phenotype", facet.by = "cultivar", add = "jitter")+
  stat_compare_means(comparisons = list(c("B", "T"), c("T", "TB"), c("B", "TB")), method = "t.test")+
  stat_summary(fun.data = function(x) data.frame(y=3, label = paste("µ=",round(mean(x),2))), geom="text") +
  theme(legend.position="none")+
  ggtitle(paste0("Measurements: ", "Mean of >2.5cm"))


plot_mean_gt3_L33 = ggboxplot(df_33_34, x= "phenotype", y= "mean_gt3", ylim=c(2,9.5),
                              fill = "phenotype", facet.by = "cultivar", add = "jitter")+
  stat_compare_means(comparisons = list(c("B", "T"), c("T", "TB"), c("B", "TB")), method = "t.test")+
  stat_summary(fun.data = function(x) data.frame(y=3, label = paste("µ=",round(mean(x),2))), geom="text") +
  theme(legend.position="none")+
  ggtitle(paste0("Measurements: ", "Mean of >3.0cm"))



ggarrange(plot_numbers_roots_L33, plot_mean_all_L33, plot_mean_gt1_L33, plot_mean_gt2_L33,plot_mean_gt25_L33, plot_mean_gt3_L33,
          labels = c("A", "B", "C", "D", "E", "F"), nrow = 2, ncol = 3)


ggarrange(bxp, dp, bp + rremove("x.text"), 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)
library(tidyverse)
PValues_RT%>%
arrange(pvalue)%>%
  mutate(pvalue=round(pvalue,5))



plot_L33_34_gt3 = ggboxplot(df_33_34, x= "cultivar", y= "mean_gt3", ylim=c(3,7.5),
                              fill = "cultivar", add = "jitter")+
  stat_compare_means(comparisons = list(c("L33", "L34")), method = "t.test",method.args = list(alternative="greater") )+
  stat_summary(fun.data = function(x) data.frame(y=3.5, label = paste("µ=",round(mean(x),2))), geom="text") +
  theme(legend.position="none")+
  ggtitle(paste0("Measurements: ", "Mean of >3.0cm"))


plot_L33_34_gt25 = ggboxplot(df_33_34, x= "cultivar", y= "mean_gt25", ylim=c(2.5,7.5),
                            fill = "cultivar", add = "jitter")+
  stat_compare_means(comparisons = list(c("L33", "L34")), method = "t.test",method.args = list(alternative="greater") )+
  stat_summary(fun.data = function(x) data.frame(y=3.0, label = paste("µ=",round(mean(x),2))), geom="text") +
  theme(legend.position="top")+
  ggtitle(paste0("Measurements: ", "Mean of >2.5cm"))

plot_L33_34_No_roots = ggboxplot(df_33_34, x= "cultivar", y= "No_roots", ylim=c(6,35),
                            fill = "cultivar", add = "jitter")+
  stat_compare_means(comparisons = list(c("L33", "L34")), method = "t.test",method.args = list(alternative="greater") )+
  stat_summary(fun.data = function(x) data.frame(y=8, label = paste("µ=",round(mean(x),2))), geom="text") +
  theme(legend.position="none")+
  ggtitle(paste0("Measurements: ", "Number of Roots"))

plot_L33_34_mean_all  = ggboxplot(df_33_34, x= "cultivar", y= "mean_all", ylim=c(0.8,6),
                            fill = "cultivar", add = "jitter")+
  stat_compare_means(comparisons = list(c("L33", "L34")), method = "t.test",method.args = list(alternative="greater") )+
  stat_summary(fun.data = function(x) data.frame(y=1, label = paste("µ=",round(mean(x),2))), geom="text") +
  theme(legend.position="none")+
  ggtitle(paste0("Measurements: ", "Mean length of all roots"))





ggarrange(plot_L33_34_gt25, plot_mean_gt25_L33, plot_L33_34_gt3,plot_mean_gt3_L33,plot_L33_34_mean_all, plot_L33_34_No_roots, 
          labels = c("A", "B", "C", "D", "E", "F"), nrow = 3, ncol = 2)

ggarrange(plot_L33_34_mean_all, plot_L33_34_No_roots,plot_L33_34_gt25,  plot_L33_34_gt3, 
          labels = c("A", "B", "C", "D"), nrow = 2, ncol = 2)


# show only the roor length > 2.5 and 3.0cm
df_33_34_cultivar= df_33_34
df_33_34_cultivar$cultivar[df_33_34_cultivar$cultivar=="L33"] = "UMN3233"
df_33_34_cultivar$cultivar[df_33_34_cultivar$cultivar=="L34"] = "UMN3234"

plot_L33_34_gt25_Fig3 = ggboxplot(df_33_34_cultivar, x= "cultivar", y= "mean_gt25", ylim=c(2.5,7.5),
                             fill = "cultivar", add = "jitter")+
  stat_compare_means(comparisons = list(c("UMN3233", "UMN3234")), method = "t.test",method.args = list(alternative="greater") )+
  stat_summary(fun.data = function(x) data.frame(y=3.0, label = paste("µ=",round(mean(x),2))), geom="text") +
  theme(legend.position="top")+
  ylab("2nd root length longer") +
  xlab("")+
  ggtitle(paste0("The 2nd root length longer than 2.5cm"))

plot_L33_34_gt3_Fig3 = ggboxplot(df_33_34_cultivar, x= "cultivar", y= "mean_gt3", ylim=c(3,7.5),
                            fill = "cultivar", add = "jitter")+
  stat_compare_means(comparisons = list(c("UMN3233", "UMN3234")), method = "t.test",method.args = list(alternative="greater") )+
  stat_summary(fun.data = function(x) data.frame(y=3.5, label = paste("µ=",round(mean(x),2))), geom="text") +
  theme(legend.position="top")+
  ylab("2nd root length longer") +  xlab("")+
  ggtitle(paste0( "The 2nd root length longer than 3.0cm"))

ggarrange(plot_L33_34_gt25_Fig3,  plot_L33_34_gt3_Fig3, 
          labels = c("A", "B" ), nrow = 1, ncol = 2)



# random forest to find the best paramaters to differentiate the the two cultivars

#  write.csv(df_33_34, "df_33_34_combined_2nd_data.csv", row.names = F)

# perform the added t test for the Tap between 3233 and 3234
dim(df_33_34)
head(df_33_34, 2)
df_33_34_tap= df_33_34[df_33_34$phenotype=="T",]
dim(df_33_34_tap)
table(df_33_34_tap$groups)

df_33_34_tap_3233= df_33_34_tap[df_33_34_tap$groups=="L33T",]
df_33_34_tap_3234= df_33_34_tap[df_33_34_tap$groups=="L34T",]
t.test(df_33_34_tap_3233$mean_all, df_33_34_tap_3234$mean_all, alternative = "two.sided")
