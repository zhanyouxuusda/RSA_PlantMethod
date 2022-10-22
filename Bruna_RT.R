setwd("~/Box/Manuscripts/alfalfa/Bruna_RootData")
library(openxlsx); library(tidyverse); library(ggpubr)

sheet_names=getSheetNames("Primary root data_ALL REPS.xlsx")

allD= data.frame(matrix(, ncol = 6))
names(allD) = c("primaryLengt", "lengthlonger",      "Per", "phenotype",          "sheetNames","cultivar" )
for (i in 1: 4){
#  print(sheet_names[i])
jun= read.xlsx("Primary root data_ALL REPS.xlsx", sheet =sheet_names[i], startRow = 7 , na.strings = c("", ""))
july= read.xlsx("Primary root data_ALL REPS.xlsx", sheet = "1rt - July 3233_3234")
Aug = read.xlsx("Primary root data_ALL REPS.xlsx", sheet = "1rt - Aug 3233_3234")
Feb16= read.xlsx("Primary root data_ALL REPS.xlsx", sheet = "1rt - Feb 3233_3234")
head(jun)
last_position_3233=grep("3233", jun[1,]); last_position_3233
first_position_3234= grep("3234", jun[1,])
jun_Branch = jun[2:4, 2:tail(last_position_3233, n=1)]
jun_Branch=rbind(jun_Branch, names(jun_Branch))
head(jun_Branch)
dim(jun_Branch)
jun_Branch_T=t(jun_Branch)%>%data.frame()%>%rename(
  primaryLengt="X2",lengthlonger= "X3", Per="X4", phenotype="X41"
  )%>%
  summarise(primaryLengt= as.numeric(primaryLengt),
              lengthlonger= as.numeric(lengthlonger),
              Per= as.numeric(Per), phenotype= phenotype)
jun_Branch_T$sheetNames= sheet_names[i]
jun_Branch_T$cultivar = "3233"

jun_tap= jun[2:4, head(first_position_3234, n=1):ncol(jun)]
jun_tap= rbind(jun_tap, names(jun_tap))

jun_tap_T = t(jun_tap)%>%data.frame()%>%rename(
  primaryLengt="X2",lengthlonger= "X3", Per="X4",  phenotype= "X41"
)%>%summarise(primaryLengt= as.numeric(primaryLengt),
             lengthlonger= as.numeric(lengthlonger),
             Per= as.numeric(Per), 
             phenotype= phenotype)

jun_tap_T$sheetNames= sheet_names[i]
jun_tap_T$cultivar = "3234"

print((jun_Branch_T))
print(paste0("June Branch row: ", nrow(jun_Branch_T), " ", "and columns: ", ncol(jun_Branch_T)))
print(paste0("June tap row: ", nrow(jun_tap_T), " ", "and columns: ", ncol(jun_tap_T)))
print((jun_tap_T))

print("__________*********")
allD= rbind(allD, jun_Branch_T, jun_tap_T)

}
table(allD$cultivar,allD$phenotype)
table(allD$cultivar,allD$sheetNames)


allD$phenotype[grep("Branch", allD$phenotype)] ="Branch"
allD$phenotype[grep("Tap", allD$phenotype)] ="Tap"
allD$phenotype[grep("TB", allD$phenotype)] ="TB"

allD$sheetNames[allD$sheetNames =="1rt - June 3233_3234"] = "1: 1rt-Jun 3233_34"
allD$sheetNames[allD$sheetNames =="1rt - July 3233_3234"] = "2: 1rt-Jul 3233_34"
allD$sheetNames[allD$sheetNames =="1rt - Aug 3233_3234"] = "3: 1rt-Aug 3233_34"
allD$sheetNames[allD$sheetNames =="1rt - Feb 3233_3234"] =  "4: 1rt-Feb 3233_34"

table(allD$sheetNames)

# t.test(jun_Branch_T$primaryLengt, jun_tap_T$primaryLengt, alternative = "greater", conf.level = 0.9)
# t.test(jun_Branch_T$lengthlonger, jun_tap_T$lengthlonger, alternative = "greater", conf.level = 0.9)
allD= allD[complete.cases(allD),]
ggplot(allD, aes(x= cultivar, y=primaryLengt, fill= cultivar  ))+ geom_boxplot()+
  facet_grid(.~sheetNames, scales = "free" )

ggplot(allD, aes(x= phenotype, y=primaryLengt, fill= cultivar  ))+ geom_boxplot()+
  facet_grid(.~cultivar, scales = "free" )


ggplot(allD, aes(x= phenotype, y=primaryLengt, fill= cultivar  ))+ geom_boxplot()+
  geom_jitter(width = 0.2)

ggplot(allD, aes(x= cultivar, y=primaryLengt, fill= cultivar  ))+ geom_boxplot()+
  facet_grid(phenotype~sheetNames, scales = "free" )
comparison_Bruna = list(c("3233", "3234"),
                        c("3233", "3234"),
                        c("3233", "3234"))
allD_no_TB= allD[allD$phenotype!= "TB",]

ggboxplot(allD, x= "cultivar", y= "primaryLengt",fill = "cultivar", add="jitter",
          palette = c("#00AFBB", "#E7B800"),
          facet.by = "phenotype")+
  stat_compare_means(label = "p.format", method = "t.test", label.x = 1)

          
ggboxplot(allD, x= "phenotype", y= "primaryLengt", fill="phenotype",ylim=c(4.5, 25),
           add = "jitter" ,
          facet.by = "cultivar")+
  stat_compare_means(comparisons = list(c("Branch", "Tap"), c("Tap", "TB"), c("Branch", "TB")),label.y = c(20, 22, 24),
    label = "p.format", method = "t.test", label.x = 1)+

    stat_summary(fun.data = function(x) data.frame(y=6, label = paste("Mean=",round(mean(x),3))), geom="text") +
  theme(legend.position="none")+
  ggtitle("Measurements:primaryLength")


ggboxplot(allD, x= "phenotype", y= "primaryLengt", fill="phenotype",ylim=c(4.5, 25),
          add = "jitter")+
  stat_compare_means(comparisons = list(c("Branch", "Tap"), c("Tap", "TB"), c("Branch", "TB")),label.y = c(20, 22, 24),
                     label = "p.format", method = "t.test", label.x = 1)+
  stat_summary(fun.data = function(x) data.frame(y=6, label = paste("Mean=",round(mean(x),3))), geom="text") +
  theme(legend.position="none")+
  ggtitle("Measurements:primaryLength")





ggboxplot(allD_no_TB, x= "cultivar", y= "lengthlonger",
          fill = c("green", "yellow"), add = "jitter" )+
  stat_compare_means(label = "p.format", method = "t.test", label.x = 1)

####  overall between 3233 and 3234

ggboxplot(allD, x= "cultivar", y= "primaryLengt",
          fill = c("green", "yellow"), add = "jitter", ylim=c(5, 20) )+
  stat_compare_means(label = "p.format", method = "t.test", label.x = 0.6)+
  stat_summary(fun.data = function(x) data.frame(y=6, label = paste("Mean=",round(mean(x),3))), geom="text") +
  theme(legend.position="none")+
  ggtitle("Measurements:primaryLength")


ggboxplot(allD, x= "cultivar", y= "lengthlonger",
          fill = c("green", "yellow"), add = "jitter", ylim=c(1.5, 9) )+
  stat_compare_means(label = "p.format", method = "t.test", label.x = 0.6)+
  stat_summary(fun.data = function(x) data.frame(y=1.8, label = paste("Mean=",round(mean(x),3))), geom="text") +
  theme(legend.position="none")+
  ggtitle("Measurements:lengthlonger")


ggboxplot(allD, x= "cultivar", y= "Per",
          fill = c("green", "yellow"), add = "jitter", ylim=c(20, 80) )+
  stat_compare_means(label = "p.format", method = "t.test", label.x = 0.6)+
  stat_summary(fun.data = function(x) data.frame(y=20, label = paste("Mean=",round(mean(x),3))), geom="text") +
  theme(legend.position="none")+
  ggtitle("Measurements:% Primary rt >3cm 2nd rt")



ggboxplot(allD, x= "cultivar", y= "primaryLengt",fill="cultivar",
          add = "jitter", ylim=c(5, 21),
          facet.by = "sheetNames")+
  stat_compare_means(label = "p.format", method = "t.test", label.x = 0.6)+
  stat_summary(fun.data = function(x) data.frame(y=6, label = paste("Mean=",round(mean(x),3))), geom="text") +
  theme(legend.position="none")+
  ggtitle("Measurements:primaryLength")

Bruna_comparions_2= list(c("Branch", "Tap"), c("Tap", "TB"), c("Branch", "TB"))

ggboxplot(allD, x= "phenotype", y= "primaryLengt", fill = "phenotype",
          add = "jitter", ylim=c(5, 25),
          facet.by = "sheetNames")+
  stat_compare_means(comparisons = Bruna_comparions_2,label = "p.format", method = "t.test", label.x = 0.6)+
  stat_summary(fun.data = function(x) data.frame(y=6, label = paste("Mean=",round(mean(x),3))), geom="text") +
  theme(legend.position="none")+
  ggtitle("Measurements:primaryLength")


ggboxplot(allD_no_TB, x= "cultivar", y= "lengthlonger",fill="cultivar",
          add = "jitter", ylim=c(1.5, 9),
          facet.by = "sheetNames", scales="free")+
  stat_compare_means(label = "p.format", method = "t.test", label.x = 0.6)+
  stat_summary(fun.data = function(x) data.frame(y=2, label = paste("Mean=",round(mean(x),3))), geom="text") +
  theme(legend.position="none")+
  ggtitle("Measurements:lengthlonger> 3cm")


ggboxplot(allD, x= "cultivar", y= "Per",fill="cultivar",
          add = "jitter", ylim=c(1, 80),
          facet.by = "sheetNames", scales="free")+
  stat_compare_means(label = "p.format", method = "t.test", label.x = 0.6)+
  stat_summary(fun.data = function(x) data.frame(y=2, label = paste("Mean=",round(mean(x),3))), geom="text") +
  theme(legend.position="none")+
  ggtitle("Measurements:% of primary comprising >3cm")




ggboxplot(allD, x= "phenotype", y= "primaryLengt",fill="cultivar",
           ylim=c(5, 30),
          facet.by = "cultivar")+
  stat_compare_means(comparisons = list(c("Branch", "Tap"), c("Tap", "TB"), c("Branch", "TB")),label = "p.format", method = "t.test", label.x = 0.6)+
  stat_summary(fun.data = function(x) data.frame(y=6, label = paste("Mean=",round(mean(x),3))), geom="text") +
  theme(legend.position="none")+
  ggtitle("Measurements:primaryLength")



ggboxplot(allD, x= "phenotype", y="primaryLengt", facet.by = "cultivar" , ylim=c(5, 27), fill="cultivar")+
  stat_compare_means(
    comparisons = list(c("Branch", "Tap"), c("Tap", "TB"), c("Branch", "TB"))
  )+
  ggtitle("Measurement: primaryLengt")








ggboxplot(allD, x= "cultivar", y= "primaryLengt",fill = "cultivar", add="jitter",
          palette = c("#00AFBB", "#E7B800"),
          facet.by = "phenotype")+
  stat_compare_means(label = "p.format", method = "t.test", label.x = 1)

head(allD, 2)
dim(allD)
table(allD$phenotype, allD$cultivar)





write.csv(allD, "combined_data_primary_root.csv", row.names = F)
my_comparisons= list(c("Logistic", "KNN"),c("Logistic", "SVM") ,
                     c("rrBLUP", "KNN"), c("rrBLUP", "SVM"),
                     c("rrBLUP", "NB"), c("rrBLUP", "ANN"),
                     c("rrBLUP", "GBM"),c("rrBLUP", "RF"))


# ggboxplot(Rocs, x= "Models", y = "Values", panel.labs =list(c("")),xlab = "",ylab = "AUC",
#           fill =  "Models", 
#           order = c("rrBLUP", "Logistic", "KNN","SVM","NB","ANN", "GBM",  "RF"),
#           font.label = list(size = 19, face = "bold", color ="red")) +
#   stat_compare_means(comparisons =my_comparisons,method = "t.test",paired = F,size =5,
#                      label.y =  c(0.6,0.83,0.89, 0.93, 0.98,1.02, 1.06, 1.1 ))+
#   theme(legend.position = "none") +
#   stat_compare_means(label.y = 1.22, label.x = "Logistic", size=6) + 
#   font("title", size = 14, color = "red", face = "bold.italic")+
#   font("subtitle", size = 10, color = "black", face = "bold")+
#   font("caption", size = 10, color = "orange")+
#   font("xlab" , size = 18, color = "black", face = "bold")+
#   rotate_x_text(angle = 20)+
#   font("ylab", size = 12, color = "black", face = "bold")+
#   font("xy.text", size = 13, color = "black", face = "bold")
