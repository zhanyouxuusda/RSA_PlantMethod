# setwd("~/Box/Proposal/Digestibility_Exp_2021"), this is for the GSDIG
setwd("~/Box/Manuscripts/alfalfa/Bruna_RootData")

# generate the 120 plants
line120= rep(c("B3233","T3234", "B4561", "B4563", "Saranac", "Agate"), 20)

# rep 1
set.seed(2021);Rep_1=sample(line120, 120, replace = F); table(Rep_1)
set.seed(20211);Rep_2=sample(line120, 120, replace = F); table(Rep_2)
set.seed(20212);Rep_3=sample(line120, 120, replace = F); table(Rep_3)
set.seed(20213);Rep_4=sample(line120, 120, replace = F); table(Rep_4)


rep1_df= data.frame(matrix(Rep_1, nrow = 12, ncol = 10)); names(rep1_df) =c("R1C1", "R1C2","R1C3","R1C4","R1C5","R1C6","R1C7","R1C8","R1C9","R1C10")
rep2_df= data.frame(matrix(Rep_2, nrow = 12, ncol = 10)); names(rep2_df) =c("R2C1", "R2C2","R2C3","R2C4","R2C5","R2C6","R2C7","R2C8","R2C9","R2C10")
rep3_df= data.frame(matrix(Rep_3, nrow = 12, ncol = 10)); names(rep3_df) =c("R3C1", "R3C2","R3C3","R3C4","R3C5","R3C6","R3C7","R3C8","R3C9","R3C10")
rep4_df= data.frame(matrix(Rep_4, nrow = 12, ncol = 10)); names(rep4_df) =c("R4C1", "R4C2","R4C3","R4C4","R4C5","R4C6","R4C7","R4C8","R4C9","R4C10")

# genearte the 4 reps
set.seed(2021031); (rep_layout= sample(1:4, 4, replace = F))

# put all replicates into one data frame

all_4_resp = cbind(rep3_df, rep2_df, rep4_df, rep1_df)
dim(all_4_resp)

all_4_resp[1:2, 1:15]

file_names_to_save_N_fixation= paste0("N_fixzation_comparison_Branch_Tap", gsub(" |:", "_", Sys.time()), ".csv")

# save the layout to a file


# rep 2
set.seed(20215);pop1_rep2=sample(1:100, 100, replace = F)
set.seed(20216);pop2_rep2=sample(1:100, 100, replace = F)
set.seed(20217);pop3_rep2=sample(1:100, 100, replace = F)
set.seed(20218);pop4_rep2=sample(1:100, 100, replace = F)
set.seed(20219);pop5_rep2=sample(1:100, 100, replace = F)

pop1_rep2_df= data.frame(matrix(pop1_rep2, nrow = 50, ncol = 2)); names(pop1_rep2_df) =c("R2P1C1", "R2P1C2")
pop2_rep2_df= data.frame(matrix(pop2_rep2, nrow = 50, ncol = 2)); names(pop2_rep2_df) =c("R2P2C1", "R2P2C2")
pop3_rep2_df= data.frame(matrix(pop3_rep2, nrow = 50, ncol = 2)); names(pop3_rep2_df) =c("R2P3C1", "R2P3C2")
pop4_rep2_df= data.frame(matrix(pop4_rep2, nrow = 50, ncol = 2)); names(pop4_rep2_df) =c("R2P4C1", "R2P4C2")
pop5_rep2_df= data.frame(matrix(pop5_rep2, nrow = 50, ncol = 2)); names(pop5_rep2_df) =c("R2P5C1", "R2P5C2")



# rep 3
set.seed(202110);pop1_rep3=sample(1:100, 100, replace = F)
set.seed(202111);pop2_rep3=sample(1:100, 100, replace = F)
set.seed(202112);pop3_rep3=sample(1:100, 100, replace = F)
set.seed(202113);pop4_rep3=sample(1:100, 100, replace = F)
set.seed(202114);pop5_rep3=sample(1:100, 100, replace = F)

pop1_rep3_df= data.frame(matrix(pop1_rep3, nrow = 50, ncol = 2)); names(pop1_rep3_df) =c("R3P1C1", "R3P1C2")
pop2_rep3_df= data.frame(matrix(pop2_rep3, nrow = 50, ncol = 2)); names(pop2_rep3_df) =c("R3P2C1", "R3P2C2")
pop3_rep3_df= data.frame(matrix(pop3_rep3, nrow = 50, ncol = 2)); names(pop3_rep3_df) =c("R3P3C1", "R3P3C2")
pop4_rep3_df= data.frame(matrix(pop4_rep3, nrow = 50, ncol = 2)); names(pop4_rep3_df) =c("R3P4C1", "R3P4C2")
pop5_rep3_df= data.frame(matrix(pop5_rep3, nrow = 50, ncol = 2)); names(pop5_rep3_df) =c("R3P5C1", "R3P5C2")




# randomize the 5 populations
set.seed(15); (rand_5_pops_rep1= sample(1:5, 5, replace = F))
set.seed(25); (rand_5_pops_rep2= sample(1:5, 5, replace = F))
set.seed(35); (rand_5_pops_rep3= sample(1:5, 5, replace = F))

# randomize the 3 reps
set.seed(333); (three_reps_layout= sample(1:3, 3, replace = F))

# the layout of the 3 reps are 
# rep2, 1, and 3


# rep 2 for 5 populations, the layout is pop 5,4,1,3,2
rep2_5pops_df= cbind(pop5_rep2_df, pop4_rep2_df, pop1_rep2_df, pop3_rep2_df, pop2_rep2_df)


# rep 1 for 5 populations, the layout is pop 5 3 2 4 1
rep1_5pops_df= cbind(pop5_rep1_df, pop3_rep1_df, pop2_rep1_df,pop4_rep1_df , pop1_rep1_df)


# rep 3 for 5 populations, the layout is pop 2 5 1 3 4
rep3_5pops_df= cbind(pop2_rep3_df, pop5_rep3_df, pop1_rep3_df, pop3_rep3_df, pop4_rep3_df)

# combine all the 3 reps together
all_df= cbind(rep2_5pops_df,rep1_5pops_df,rep3_5pops_df )
dim(all_df)
# write.csv(all_df, "all_df_digestibility_2021.csv", row.names = T)
getwd()
