setwd("C:/Users/athee/Documents/NTX_Hack_Music_EEG_2023/02_EEG_OutPut/Band_Power_CSA")

cur_file <- read.table("~/NTX_Hack_Music_EEG_2023/02_EEG_OutPut/Band_Power_CSA/cur_file2.txt", quote="\"", comment.char="")
samples_list <- as.list(cur_file$V1)


#Create table

new_list= matrix(c(1:2), ncol=3, byrow=TRUE)

colnames(new_list) = c('Sample', 'alpha_ratio','theta_beta_ratio')

new_list <- as.data.frame(new_list)


for (i in samples_list){
  
  alpha <- read.delim(paste(i,"_C10_Alpha.csv",sep = ""))
  beta <- read.delim(paste(i,"_C10_Beta.csv",sep = ""))
  theta <- read.delim(paste(i,"_C10_Theta.csv",sep = ""))
  
  alpha_avergae <- median(alpha$E1-alpha$E123)
  
  beta$theta_beta_sum  <- beta$E1 + beta$E123
  theta$theta_beta_sum <- theta$E1 + theta$E123
  beta$ratio <-  theta$theta_beta_sum / beta$theta_beta_sum 
  theta_beta_ratio <- median(beta$ratio)
  
  new_list[nrow(new_list)+1,] <- c(i,alpha_avergae,theta_beta_ratio)
  

}

new_list <- new_list[-1,]

new_list <- new_list %>% mutate(valence = case_when(Sample %in% grepl("ses-01") ~"0.211"))

new_list$valence <- ifelse(grepl("ses-01", new_list$Sample), "0.211", 
                           ifelse(grepl("ses-02", new_list$Sample), "0.964"), no = NA)  

new_list$valence <- ifelse(grepl("ses-01", new_list$Sample), "0.447",
       ifelse(grepl("ses-02", new_list$Sample), "0.272",
              ifelse(grepl("ses-03", new_list$Sample), "0.612",
                     ifelse(grepl("ses-04", new_list$Sample), "0.0734",
                            ifelse(grepl("ses-05", new_list$Sample), "0.325",
                                   ifelse(grepl("ses-06", new_list$Sample), "0.193",
                                          ifelse(grepl("ses-07", new_list$Sample), "0.149",
                                                 ifelse(grepl("ses-08", new_list$Sample), "0.504",
                                                        ifelse(grepl("ses-09", new_list$Sample), "0.741",
                                                               ifelse(grepl("ses-10_", new_list$Sample), "0.346",
                                                                      ifelse(grepl("ses-11_", new_list$Sample), "0.696",
                                                                             ifelse(grepl("ses-12_", new_list$Sample), "0.224",
              
              new_list$valence ))))))))))))



write.csv(new_list, "~/NTX_Hack_Music_EEG_2023/02_EEG_OutPut/Band_Power_CSA/final.csv",quote = F)
