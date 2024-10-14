# ------ 02.10.2024
# Ai Vinh Pham-Le, Appendices - Dissertation 
# English Linguistics Dept., Univerisity of Rostock (UR)
# ------ series of scripts which are used for processing data
# triadic files for analysis, takin areas which has no overlap (verwendbar)
# preparing parameters for importing to ELAN

rm(list=ls(all=TRUE))
setwd("/Users/phamlevinhai")

library(soundgen)
require(dplyr)
require(readtextgrid)

# export syllable tier from ELAN & read
F01_syllable <-   read_textgrid("/Users/phamlevinhai/Documents/P_Fx/Reg_Dar/F01_Reg_Dar/F01_syllables_ABC_TCU_import_Ros.TextGrid", file = NULL)
F01_A_syllable <- F01_syllable %>% filter(F01_syllable$tier_name =="syllable A")

# read syllable measurements
F01_A_semitones <-read.table("/Users/phamlevinhai/Documents/P_Fx/Reg_Dar/F01_Reg_Dar/F01_syllable_A_pitch_semitones.txt", header=TRUE)

# solving 02 questions: 
#     (1) what happens when a syllable is repeated more than 02 times
#     (2) last syllable lengthening (if any)

# For each row in F01_A_syllables, find matching semitones and hertz values in F01_A_semitones
F01_A_syllable <- F01_A_syllable %>%
  rowwise() %>%
  mutate(
    # Store semitones and hertz values in lists based on time intervals
    semitones_in_range = list(F01_A_semitones$semitones[F01_A_semitones$time >= xmin & F01_A_semitones$time <= xmax]),
    hertz_in_range = list(F01_A_semitones$hertz[F01_A_semitones$time >= xmin & F01_A_semitones$time <= xmax]),
    
    # Compute mean of semitones and hertz, assign NA if no values
    mean_semitones = ifelse(length(unlist(semitones_in_range)) > 0, mean(unlist(semitones_in_range)), NA),
    mean_hertz = ifelse(length(unlist(hertz_in_range)) > 0, mean(unlist(hertz_in_range)), NA),
    
    # Handle empty cases and combine semitones and hertz into strings
    semitones_in_range = ifelse(length(unlist(semitones_in_range)) == 0, NA, paste(unlist(semitones_in_range), collapse = ",")),
    hertz_in_range = ifelse(length(unlist(hertz_in_range)) == 0, NA, paste(unlist(hertz_in_range), collapse = ","))
  )


# filter each column of semitones_in_range; hertz_in_range; mean_semitones; mean_hertz into 04 data frame for importing to ELAN
F01_syllable_A  <- F01_A_syllable %>% 
  filter(!text== "") %>%
  select (tier_name, xmin, xmax, text) %>%
  rename(parameter = text)

F01_smt_A <- F01_A_syllable %>%
  filter(!text== "") %>%
  select (tier_name, xmin, xmax, semitones_in_range) %>%
  mutate(tier_name = ifelse(tier_name == "syllable A", "semitone_A_ROS", tier_name)) %>%
  rename(parameter = semitones_in_range)

F01_hzt_A <- F01_A_syllable %>%
  filter(!text== "") %>%
  select (tier_name, xmin, xmax, hertz_in_range) %>%
  mutate(tier_name = ifelse(tier_name == "syllable A", "herzt_A_ROS", tier_name)) %>%
  rename(parameter = hertz_in_range)

F01_mean_smt_A <- F01_A_syllable %>%
  filter(!text== "") %>%
  select (tier_name, xmin, xmax, mean_semitones) %>%
  mutate(tier_name = ifelse(tier_name == "syllable A", "mean_smt_A_ROS", tier_name)) %>%
  rename(parameter = mean_semitones)

F01_mean_hzt_A <- F01_A_syllable %>%
  filter(!text== "")  %>%
  select (tier_name, xmin, xmax, mean_hertz) %>%
  mutate(tier_name = ifelse(tier_name == "syllable A", "mean_hz_A_ROS", tier_name)) %>%
  rename(parameter = mean_hertz)

# saved all together # the syllable has to be there, deleted later
F01_A_syl_parameter <-rbind(F01_syllable_A, F01_smt_A, F01_mean_smt_A,F01_hzt_A, F01_mean_hzt_A, keep.all=F)
F01_A_syl_parameter<-F01_A_syl_parameter[-nrow(F01_A_syl_parameter),]

write.table(F01_A_syl_parameter, "/Users/phamlevinhai/Documents/P_Fx/Reg_Dar/F01_Reg_Dar/F01_parameter_A.txt", row.names = FALSE, sep = "\t", quote = FALSE)
# or individually
write.table(F01_smt_A, "/Users/phamlevinhai/Documents/P_Fx/Reg_Dar/F01_Reg_Dar/F01_smt_A.txt", row.names = FALSE, sep = "\t", quote = FALSE)
