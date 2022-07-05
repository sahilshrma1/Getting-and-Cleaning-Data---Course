
###########################################################################################
###### THis is a script for final assignment in "Getting and Cleaning Data" course ########
###########################################################################################


library(dplyr)
library(tidyr)
library(plyr)
library(readr)


  # Reading-in the data files
            
          test_x <- read.table("R for Data Science/Getting and Cleaning Data/UCI HAR Dataset/test/X_test.txt",
                               ) # Data
          test_y <- read.table("R for Data Science/Getting and Cleaning Data/UCI HAR Dataset/test/y_test.txt",
                               col.names = "Label_Code") #Test Set Labels
          
          
          train_x <- read.table("R for Data Science/Getting and Cleaning Data/UCI HAR Dataset/train/X_train.txt",
                                ) # Data
          train_y <- read.table("R for Data Science/Getting and Cleaning Data/UCI HAR Dataset/train/y_train.txt",
                                col.names = "Label_Code") # Train Set Labels
     # attach Labels to Data
          
          test_data <- cbind(test_y, test_x)
          train_data <- cbind(train_y, train_x)
          

#############        
#### 1 ###### Merges training and test data to create one single data file
#############
          
          data_full <- rbind(train_data, test_data)
          
          
#############
#### 2 ###### Extracts only the measurements on the mean and standard deviation for each measurement. 
#############          
          
          features <- read.table("R for Data Science/Getting and Cleaning Data/UCI HAR Dataset/features.txt")
          
          # because we have 562 variables in the full data set so we'll 1 row at the top so that numbering match
          
          features <- add_row(features, .after = FALSE)
          
          all_means <- grep("mean()", features$V2)
          all_std <- grep("std()", features$V2)
          
          data_m_std <- data_full %>% select(data_full$Label_Code,grep("mean()", features$V2),grep("std()", features$V2))
          
          data_m_std <- as_tibble(data_m_std)
          
          # Bring label_code to first position of variables
          data_m_std <- data_m_std %>% relocate(Label_Code)
          
          
#############
#### 3 ###### Use descriptive activity names to name the activities in the data set
#############
          
              data_m_std$Label_Code[data_m_std$Label_Code == 1] <- "WALKING"
              data_m_std$Label_Code[data_m_std$Label_Code == 2] <- "WALKING_UPSTAIRS"
              data_m_std$Label_Code[data_m_std$Label_Code == 3] <- "WALKING_DOWNSTAIRS"
              data_m_std$Label_Code[data_m_std$Label_Code == 4] <- "SITTING"
              data_m_std$Label_Code[data_m_std$Label_Code == 5] <- "STANDING"
              data_m_std$Label_Code[data_m_std$Label_Code == 6] <- "LAYING"
                                      
               # Now print the changed activity names 
              
              data_m_std$Label_Code
 
             
                           
#############
#### 4 ###### Appropriately label the data set with descriptive variable names 
#############
              
              # This requirement has been fulfilled already, in other words, all descriptive label names are already
              # attached to the main data 
              
              # See Line 25-28, 51 and 63 - 68
              
              # see results
              data_m_std$Label_Code

#############
##### 5 ##### From the data set in step 4, creates a second, independent tidy data set with the average of
############# each variable for each activity and each subject.
#############
              
              table(is.na(data_m_std))
              
        # Create a second, independent tidy data set
              
              second_data <- data_m_std
              
        # Average for each variable
              
              walking <- second_data[second_data$Label_Code == "WALKING",]
              walking_avg <- sapply(walking[,2:81], mean)
              
              walking_upstairs <- second_data[second_data$Label_Code == "WALKING_UPSTAIRS",]
              walking_upstairs_avg <- sapply(walking_upstairs[,2:81], mean)
              
              walking_downstairs <- second_data[second_data$Label_Code == "WALKING_DOWNSTAIRS",]
              walking_downstairs_avg <- sapply(walking_downstairs[,2:81], mean)
              
              sitting <- second_data[second_data$Label_Code == "SITTING",]
              sitting_avg <- sapply(sitting[,2:81], mean)
              
              standing <- second_data[second_data$Label_Code == "STANDING",]
              standing_avg <- sapply(standing[,2:81], mean)
              
              laying <- second_data[second_data$Label_Code == "LAYING",]
              laying_avg <- sapply(laying[,2:81], mean)
              
         # Calculating Averages for Each subject (total = 30)
              
              #Import subject data
              
              subject_test <- read.table("R for Data Science/Getting and Cleaning Data/UCI HAR Dataset/test/subject_test.txt") 
              subject_train <- read.table("R for Data Science/Getting and Cleaning Data/UCI HAR Dataset/train/subject_train.txt")
              
              subjects <- rbind(subject_train, subject_test)
              
                  # Add subjects variable to main data set
              
                        subjects_2_data <- cbind("subjects" = subjects$V1, second_data)
                        
                        subjects_2_data <- subjects_2_data %>% relocate(subjects, .after = 1)
                        
                  # Calculate averages for each subject
                        
                       sub_1 <-  subjects_2_data[subjects_2_data$subjects == 1,]
                       sub_1_avg <- sapply(sub_1[,3:82], mean)
                       
                       sub_2 <-  subjects_2_data[subjects_2_data$subjects == 2,]
                       sub_2_avg <- sapply(sub_2[,3:82], mean)
                       
                       sub_3 <-  subjects_2_data[subjects_2_data$subjects == 3,]
                       sub_3_avg <- sapply(sub_3[,3:82], mean)
                       
                       sub_4 <-  subjects_2_data[subjects_2_data$subjects == 4,]
                       sub_4_avg <- sapply(sub_4[,3:82], mean)
                       
                       sub_5 <-  subjects_2_data[subjects_2_data$subjects == 5,]
                       sub_5_avg <- sapply(sub_5[,3:82], mean)
                       
                       sub_6 <-  subjects_2_data[subjects_2_data$subjects == 6,]
                       sub_6_avg <- sapply(sub_6[,3:82], mean)
                       
                       sub_7 <-  subjects_2_data[subjects_2_data$subjects == 7,]
                       sub_7_avg <- sapply(sub_7[,3:82], mean)
                       
                       sub_8 <-  subjects_2_data[subjects_2_data$subjects == 8,]
                       sub_8_avg <- sapply(sub_8[,3:82], mean)
                       
                       sub_9 <-  subjects_2_data[subjects_2_data$subjects == 9,]
                       sub_9_avg <- sapply(sub_9[,3:82], mean)
                       
                       sub_10 <-  subjects_2_data[subjects_2_data$subjects == 10,]
                       sub_10_avg <- sapply(sub_10[,3:82], mean)
             
                       sub_11 <-  subjects_2_data[subjects_2_data$subjects == 11,]
                       sub_11_avg <- sapply(sub_11[,3:82], mean)
                       
                       sub_12 <-  subjects_2_data[subjects_2_data$subjects == 12,]
                       sub_12_avg <- sapply(sub_12[,3:82], mean)
                       
                       sub_13 <-  subjects_2_data[subjects_2_data$subjects == 13,]
                       sub_13_avg <- sapply(sub_13[,3:82], mean)
                       
                       sub_14 <-  subjects_2_data[subjects_2_data$subjects == 14,]
                       sub_14_avg <- sapply(sub_14[,3:82], mean)
                       
                       sub_15 <-  subjects_2_data[subjects_2_data$subjects == 15,]
                       sub_15_avg <- sapply(sub_15[,3:82], mean)
                       
                       sub_16 <-  subjects_2_data[subjects_2_data$subjects == 16,]
                       sub_16_avg <- sapply(sub_16[,3:82], mean)
                       
                       sub_17 <-  subjects_2_data[subjects_2_data$subjects == 17,]
                       sub_17_avg <- sapply(sub_17[,3:82], mean)
                       
                       sub_18 <-  subjects_2_data[subjects_2_data$subjects == 18,]
                       sub_18_avg <- sapply(sub_18[,3:82], mean)
                       
                       sub_19 <-  subjects_2_data[subjects_2_data$subjects == 19,]
                       sub_19_avg <- sapply(sub_19[,3:82], mean)
                       
                       sub_20 <-  subjects_2_data[subjects_2_data$subjects == 20,]
                       sub_20_avg <- sapply(sub_20[,3:82], mean)
                      
                       sub_21 <-  subjects_2_data[subjects_2_data$subjects == 21,]
                       sub_21_avg <- sapply(sub_21[,3:82], mean)
                       
                       sub_22 <-  subjects_2_data[subjects_2_data$subjects == 22,]
                       sub_22_avg <- sapply(sub_22[,3:82], mean)
                       
                       sub_23 <-  subjects_2_data[subjects_2_data$subjects == 23,]
                       sub_23_avg <- sapply(sub_23[,3:82], mean)
                       
                       sub_24 <-  subjects_2_data[subjects_2_data$subjects == 24,]
                       sub_24_avg <- sapply(sub_24[,3:82], mean)
                       
                       sub_25 <-  subjects_2_data[subjects_2_data$subjects == 25,]
                       sub_25_avg <- sapply(sub_25[,3:82], mean)
                       
                       sub_26 <-  subjects_2_data[subjects_2_data$subjects == 26,]
                       sub_26_avg <- sapply(sub_26[,3:82], mean)
                       
                       sub_27 <-  subjects_2_data[subjects_2_data$subjects == 27,]
                       sub_27_avg <- sapply(sub_27[,3:82], mean)
                       
                       sub_28 <-  subjects_2_data[subjects_2_data$subjects == 28,]
                       sub_28_avg <- sapply(sub_28[,3:82], mean)
                       
                       sub_29 <-  subjects_2_data[subjects_2_data$subjects == 29,]
                       sub_29_avg <- sapply(sub_29[,3:82], mean)
                       
                       sub_30 <-  subjects_2_data[subjects_2_data$subjects == 30,]
                       sub_30_avg <- sapply(sub_30[,3:82], mean)
                       
                      
                       
              # Creating a final tidy data frame
              
              new_data <- as_tibble(data.frame("Variable" = (names(second_data[,2:81])), walking_avg,walking_upstairs_avg,
                         walking_downstairs_avg, standing_avg, sitting_avg, standing_avg, laying_avg,
                         sub_1_avg,sub_2_avg,sub_3_avg,sub_4_avg,sub_5_avg,sub_6_avg,sub_7_avg,sub_8_avg,sub_9_avg,sub_10_avg,
                         sub_11_avg,sub_12_avg,sub_13_avg,sub_14_avg,sub_15_avg,sub_16_avg,sub_17_avg,sub_18_avg,sub_19_avg,sub_20_avg,
                         sub_21_avg,sub_22_avg,sub_23_avg,sub_24_avg,sub_25_avg,sub_26_avg,sub_27_avg,sub_28_avg,sub_29_avg,sub_30_avg))


          
         
