# analyze VR data - exp 1 (singles)
# cube: 30 CCW rotation (polar co-ordinates)
# sphere: -30 CW rotation
# data was normalized by flipping CW

library(dplyr)
library(ggplot2)
library(svglite)
library(gginnards)
library(Hmisc)
library(ggbeeswarm)
library(tidyr)
library(lsr)
library(OneR)

# LOAD DATA AND MAKE BINS FOR PLOTTING
df <- read.csv('data/all_reaches_single.csv', header = TRUE)
df <- tbl_df(df) 
df <- df %>%
  filter(block_num > 5) %>% # exclude practice blocks
  group_by(ppid) %>%
  mutate(trialn = 1:n(),
         allbinno = bin(trialn, # bins for ALL trials, ignore task
                        nbins = (max(trialn))/3,
                        labels = c(1:(max(trialn)/3)))) %>%
  group_by(block_num) %>%
  mutate(binno = bin(trial_num_in_block, # bins per task 
                     nbins = (max(trial_num_in_block))/3,
                     labels = c(1:(max(trial_num_in_block)/3))))

## PLOT BLOCKED LEARNING CURVES ##
  taskmeans <- df %>%
    group_by(block_num) %>%
    group_by(allbinno) %>%
    mutate(Mean_th = mean(theta, na.rm = TRUE),
              SD_th = sd(theta, na.rm = TRUE),
              SEM_th = SD_th/sqrt(length(unique(ppid)))) %>%
    distinct(allbinno, .keep_all = TRUE)
  
  taskplot <- ggplot(taskmeans, 
                     aes(x = as.numeric(allbinno),
                         y = as.numeric(Mean_th),
                         color = factor(type),
                         shape = factor(obj_shape))) +
    geom_point(group = 1) +
    scale_shape_manual(values=c(0, 1)) +
    scale_size_manual(values=c(1,2)) +
    geom_errorbar(aes(ymin=Mean_th-SEM_th, ymax=Mean_th+SEM_th),
                alpha=0.4) +
    coord_fixed(ratio = 1.5) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.title = element_blank(),
          legend.position = "none") +
    scale_x_continuous(name="Block",
                       limits=c(0, 138),
                       breaks=seq(0,138,9)) +
    scale_y_continuous(name="Hand angle (Degrees)",
                       limits=c(-30, 30),
                       breaks=seq(-30,30,10)) +
    geom_hline(yintercept = 0,  color = "grey") +
    geom_vline(xintercept = 31,  color = "grey") +
    geom_hline(yintercept = -30,  linetype = "dashed", color = "red")

  print(taskplot)
  


## PLOT CLAMPED FB TRIALS

  ### BAR PLOT #1: ALL CLAMPED CONDITIONS ###
  # RIGHT HAND, TRAINED OBJECT
  rt_means <- df %>% 
    filter(type == "clamped") %>%
    filter(block_num > 16) %>% # clamped trials after training
    filter(hand == "r") %>% # trained right hand
    filter(obj_shape == 1) %>% # trained object
    group_by(ppid) %>%
    mutate(pp_th = mean(theta, na.rm = TRUE)) %>%
    distinct(ppid, .keep_all = TRUE) %>%
    ungroup() %>%
    mutate(condition = 'rt', # right hand, trained obj
           Mean_th = mean(pp_th, na.rm = TRUE),
            SD_th = sd(pp_th, na.rm = TRUE),
            SEM_th = SD_th/sqrt(length(unique(ppid))))
  
  # LEFT HAND, TRAINED OBJECT
  lt_means <- df %>% 
    filter(type == "clamped") %>%
    filter(block_num > 16) %>% # clamped trials after training
    filter(hand == "l") %>% # untrained left hand
    filter(obj_shape == 1) %>% # trained object
    group_by(ppid) %>%
    mutate(pp_th = mean(theta, na.rm = TRUE)) %>%
    distinct(ppid, .keep_all = TRUE) %>%
    ungroup() %>%
    mutate(condition = 'lt', # left hand, trained obj
           Mean_th = mean(pp_th, na.rm = TRUE),
           SD_th = sd(pp_th, na.rm = TRUE),
           SEM_th = SD_th/sqrt(length(unique(ppid))))
  
  # RIGHT HAND, UNTRAINED OBJECT
  ru_means <- df %>% 
    filter(type == "clamped") %>%
    filter(block_num > 16) %>% # clamped trials after training
    filter(hand == "r") %>% # trained right hand
    filter(obj_shape == 2) %>% # untrained object
    group_by(ppid) %>%
    mutate(pp_th = mean(theta, na.rm = TRUE)) %>%
    distinct(ppid, .keep_all = TRUE) %>%
    ungroup() %>%
    mutate(condition = 'ru', # right hand, untrained obj
           Mean_th = mean(pp_th, na.rm = TRUE),
           SD_th = sd(pp_th, na.rm = TRUE),
           SEM_th = SD_th/sqrt(length(unique(ppid))))
  
  # LEFT HAND, UNTRAINED OBJECT
  lu_means <- df %>% 
    filter(type == "clamped") %>%
    filter(block_num > 16) %>% # clamped trials after training
    filter(hand == "l") %>% # untrained left hand
    filter(obj_shape == 2) %>% # untrained object
    group_by(ppid) %>%
    mutate(pp_th = mean(theta, na.rm = TRUE)) %>%
    distinct(ppid, .keep_all = TRUE) %>%
    ungroup() %>%
    mutate(condition = 'lu', # left hand, untrained obj
           Mean_th = mean(pp_th, na.rm = TRUE),
           SD_th = sd(pp_th, na.rm = TRUE),
           SEM_th = SD_th/sqrt(length(unique(ppid))))
  
  # COMBINE ALL DATA
  allclamps <- bind_rows(rt_means, lt_means, ru_means, lu_means)
    
  # PLOT ALL CLAMP CONDITIONS
  bar1 <- ggplot(allclamps,
         aes(x = condition, y = pp_th,
             color = factor(hand),
             fill = factor(hand))) +
    stat_summary(fun = mean, geom = "bar", na.rm = TRUE) +
    geom_errorbar(data = allclamps,
                  aes(x = condition, y = pp_th,
                      ymin = Mean_th - SEM_th,
                      ymax = Mean_th + SEM_th),
                  width = 0.1, size = 0.5, color = "black") +
    geom_beeswarm(data = allclamps, 
                  aes(x = condition, y = pp_th),
                  alpha = 1/7,
                  dodge.width = 2, cex = 3,
                  stroke = 0.3, color = "black") +
    coord_fixed(ratio = 1/30) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.title = element_blank(),
          legend.position = "none") +
    scale_y_continuous(name="pp_th",
                       limits=c(-40, 10),
                       breaks=seq(-40,10,10))
  print(bar1)
  ggsave(file = 'bar1.svg',
         plot = bar1,
         height = 10, dpi = 96, units = "cm")
  ### BAR PLOT #2: COLLAPSED ACROSS OBJECTS (IGNORE OBJ) ###
  # LEFT HAND, EITHER OBJECT
  le_means <- df %>% 
    filter(type == "clamped") %>%
    filter(block_num > 16) %>% # clamped trials after training
    filter(hand == "l") %>% # untrained left hand
    group_by(ppid) %>%
    mutate(pp_th = mean(theta, na.rm = TRUE)) %>%
    distinct(ppid, .keep_all = TRUE) %>%
    ungroup() %>%
    mutate(condition = 'le', # left hand, either object
           Mean_th = mean(pp_th, na.rm = TRUE),
           SD_th = sd(pp_th, na.rm = TRUE),
           SEM_th = SD_th/sqrt(length(unique(ppid))))
  
  # RIGHT HAND, EITHER OBJECT
  re_means <- df %>% 
    filter(type == "clamped") %>%
    filter(block_num > 16) %>% # clamped trials after training
    filter(hand == "r") %>% # trained right hand
    group_by(ppid) %>%
    mutate(pp_th = mean(theta, na.rm = TRUE)) %>%
    distinct(ppid, .keep_all = TRUE) %>%
    ungroup() %>%
    mutate(condition = 're', # right hand, either object
           Mean_th = mean(pp_th, na.rm = TRUE),
           SD_th = sd(pp_th, na.rm = TRUE),
           SEM_th = SD_th/sqrt(length(unique(ppid))))
  
  delta_obj <- bind_rows(le_means, re_means)
  
  bar2 <- ggplot(delta_obj,
         aes(x = condition, y = pp_th,
             color = factor(hand),
             fill = factor(hand))) +
    stat_summary(fun = mean, geom = "bar", na.rm = TRUE) +
    geom_errorbar(data = delta_obj,
                  aes(x = condition, y = pp_th,
                      ymin = Mean_th - SEM_th,
                      ymax = Mean_th + SEM_th),
                  width = 0.1, size = 0.5, color = "black") +
    geom_beeswarm(data = delta_obj, 
                  aes(x = condition, y = pp_th),
                  alpha = 1/7,
                  dodge.width = 2, cex = 3,
                  stroke = 0.3, color = "black") +
    coord_fixed(ratio = 1/30) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.title = element_blank(),
          legend.position = "none") +
    scale_y_continuous(name="pp_th",
                       limits=c(-40, 10),
                       breaks=seq(-40,10,10))
  print(bar2)
  ggsave(file = 'bar2.svg',
         plot = bar2,
         height = 10, dpi = 96, units = "cm")
  
  ### BAR PLOT #3: COLLAPSED ACROSS OBJECTS (IGNORE OBJ) ###
  # TRAINED OBJECT, EITHER HAND
  te_means <- df %>% 
    filter(type == "clamped") %>%
    filter(block_num > 16) %>% # clamped trials after training
    filter(obj_shape == 1) %>% # trained object
    group_by(ppid) %>%
    mutate(pp_th = mean(theta, na.rm = TRUE)) %>%
    distinct(ppid, .keep_all = TRUE) %>%
    ungroup() %>%
    mutate(condition = 'te', # trained object, either hand
           Mean_th = mean(pp_th, na.rm = TRUE),
           SD_th = sd(pp_th, na.rm = TRUE),
           SEM_th = SD_th/sqrt(length(unique(ppid))))
  
  # UNTRAINED OBJECT, EITHER HAND
  ue_means <- df %>% 
    filter(type == "clamped") %>%
    filter(block_num > 16) %>% # clamped trials after training
    filter(obj_shape == 2) %>% # untrained object
    group_by(ppid) %>%
    mutate(pp_th = mean(theta, na.rm = TRUE)) %>%
    distinct(ppid, .keep_all = TRUE) %>%
    ungroup() %>%
    mutate(condition = 'ue', # untrained object, either hand
           Mean_th = mean(pp_th, na.rm = TRUE),
           SD_th = sd(pp_th, na.rm = TRUE),
           SEM_th = SD_th/sqrt(length(unique(ppid))))
  
  delta_hand <- bind_rows(te_means, ue_means)
  
  bar3 <- ggplot(delta_hand,
                 aes(x = condition, y = pp_th)) +
    stat_summary(fun = mean, geom = "bar", na.rm = TRUE) +
    geom_errorbar(data = delta_hand,
                  aes(x = condition, y = pp_th,
                      ymin = Mean_th - SEM_th,
                      ymax = Mean_th + SEM_th),
                  width = 0.1, size = 0.5, color = "black") +
    geom_beeswarm(data = delta_hand, 
                  aes(x = condition, y = pp_th),
                  alpha = 1/7,
                  dodge.width = 2, cex = 3,
                  stroke = 0.3, color = "black") +
    coord_fixed(ratio = 1/30) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.title = element_blank(),
          legend.position = "none") +
    scale_y_continuous(name="pp_th",
                       limits=c(-40, 10),
                       breaks=seq(-40,10,10))
  print(bar3)
  ggsave(file = 'bar3.svg',
         plot = bar3,
         height = 10, dpi = 96, units = "cm")