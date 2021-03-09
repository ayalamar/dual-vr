# analyze VR data - exp 2 (duals)
# cube: 30 or 60 / CCW rotation (polar co-ordinates)
# sphere: -30 or -60 / CW rotation
# data was normalized by flipping CW

plot_single_errors <- function(){
  
  library(dplyr)
  library(ggplot2)
  library(ggbeeswarm)
  library(tidyr)
  library(reprex)
  library(lsr)
  
  single <- read.csv('data/all_reaches_single.csv', header = TRUE)
  
  groupnames <- unique(single$experiment)
  
  cube <- single %>% # un-normalize errors for now so we can make directional
    filter(experiment == groupnames[1]) %>% 
    mutate(theta = ifelse(block_num %in% c(16, 7, 17), theta*-1, theta))
  
  single <- single %>% 
    filter(experiment == groupnames[2]) %>%
    bind_rows(cube)
  
  # analyze and visualize learning
  singles <- NA
  for (groupname in groupnames) {
    
    ppdf <- single %>% # get individual means so can display individual learning curves
      filter(experiment == groupname) %>%
      group_by(ppid) %>%
      filter(block_num == 16) %>% # this rotated training block needs to change PER EXPERIMENT
      filter(trial_num_in_block %in% c(1:6,178:180)) %>%
      mutate(firstlast = ifelse(trial_num_in_block %in% c(1:3), 1,
                                ifelse(trial_num_in_block %in% c(4:6), 2, 7))) %>% # last block is labeled "7" for plot aesthetics
      ungroup() %>%
      group_by(ppid, firstlast) %>% # get block means per ppid
      summarise(pptheta = mean(theta, na.rm = TRUE),
                group = groupname, rot = unique(cursor_rotation)) 
    
    groupdf <- ppdf %>% # get block means per group, for plots
      group_by(firstlast) %>%
      summarise(grouptheta = mean(pptheta, na.rm = TRUE),
                groupsd = sd(pptheta, na.rm = TRUE),
                groupsem = groupsd/sqrt(length(unique(ppid))), 
                group = groupname, rot = unique(rot))
    
    if (groupname == groupnames[2]){ # make LCs from sphere dashed
      linetypeset = "dashed" 
    } else {
      linetypeset = "solid"
    }
    
    groupplot <- ggplot(data = groupdf,
                        aes(x = firstlast, y = grouptheta)) +
      geom_point() +
      geom_line(data = groupdf, 
                aes(x = firstlast, y = grouptheta)) +
      geom_ribbon(data = groupdf,
                  aes(ymin = grouptheta - groupsem, 
                      ymax = grouptheta + groupsem),
                  alpha=0.4) +
      geom_line(data = ppdf, 
                aes(x = firstlast, y = pptheta,
                    color = as.factor(ppid)), 
                linetype = linetypeset,
                alpha = 0.3) +
      coord_fixed(ratio = 1/7) +
      scale_y_continuous(name="Hand angle (Degrees)",
                         limits=c(-60, 60),
                         breaks=seq(-60,60,10)) +
      xlim(0,8) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.title = element_blank(), 
            legend.position = "none",
            axis.text.x = element_blank()) +
      ggtitle(groupname) +
      geom_hline(yintercept = -30,  linetype = "dashed", color = "red")  +
      geom_hline(yintercept = 30,  linetype = "dashed", color = "red")
    
    print(groupplot)
    
    if (is.na(singles)){
      singles <- ppdf
    } else {
      singles <- rbind(singles, ppdf)
    }
    
  } 
  
  # print some stats 
  library(ez)
  # mod <- ezANOVA(data = singles,
  #                dv = pptheta, 
  #                wid = ppid,
  #                within = firstlast, 
  #                between = .(rot), 
  #                detailed = TRUE,
  #                return_aov = TRUE)
  # print(mod)

  # analyze sphere only - first vs. last block
  singles_sphere <- singles %>%
    filter(group == "moveObject_R_Sphere_1") %>%
    filter(firstlast %in% c(1,7))
  
  t.test(pptheta ~ firstlast, data = singles_sphere, paired = TRUE, alternative = "greater")
  cohensD(pptheta ~ firstlast, data = singles_sphere)
  
  # analyze cube only - first vs. last block
  singles_cube <- singles %>%
    filter(group == "moveObject_R_Cube_1") %>%
    filter(firstlast %in% c(1,7))
  
  t.test(pptheta ~ firstlast, data = singles_cube, paired = TRUE, alternative = "less")
  cohensD(pptheta ~ firstlast, data = singles_cube)
  
  # analyze and visualize reach AEs
  for (groupname in groupnames) {
    
    ppdf <- single %>% 
      filter(experiment == groupname) %>%
      group_by(ppid) %>%
      filter(block_num %in% c(7, 17)) %>% # blocks with baseline clamp and clamp following rotated training
      filter(trial_num %in% c(66:68, 319:321)) %>%
      mutate(clampblock = ifelse(trial_num %in% c(66:68), 1, 2)) %>%
      ungroup() %>%
      group_by(ppid, clampblock) %>% # get block means per ppid
      summarise(pptheta = mean(theta, na.rm = TRUE), group = groupname) %>%
      spread(clampblock, pptheta) %>%
      mutate(ae = `2` - `1`) 
    
    groupdf <- ppdf %>%
      group_by(group) %>%
      mutate(groupae = mean(ae, na.rm = TRUE),
             groupsd = sd(ae, na.rm = TRUE),
             groupsem = groupsd/sqrt(length(unique(ppid))))
    
    aebars_explicit <- ggplot(data = groupdf,
                              aes(x = unique(group), y = unique(groupae))) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(data = groupdf, 
                    aes(x = unique(group), y = unique(groupae), 
                                                  ymin = unique(groupae) - unique(groupsem), ymax =unique(groupae) + unique(groupsem)),
                    width = 0.2, size = 0.5, color = "black",
                    position = position_dodge(width = 0.9)) +
      geom_beeswarm(data = ppdf, aes(x = unique(group), y = ae),
                    alpha = 1/7,
                    dodge.width = .9, cex = 3,
                    stroke = 0.3) +
      ylab("theta") +
      ggtitle(groupname) +
      coord_fixed(ratio = 1/13) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            legend.title = element_blank(), legend.position = "none") +
      scale_y_continuous(name="Hand angle (Degrees)",
                         limits=c(-60, 60),
                         breaks=seq(-60,60,10))
    
    print(aebars_explicit)
    
    # print some stats 
    ae_stats(groupdf)
    
  }
  
}

plot_dual_errors <- function(){
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggbeeswarm)
  library(ez)
  
  dual30 <- read.csv('data/all_reaches_dual.csv', header = TRUE)
  dual60 <- read.csv('data/all_reaches_dual_60.csv', header = TRUE)
  
  dual30 <- dual30 %>%
    mutate(dualgroup = 'dual30')
  
  dual60 <- dual60 %>%
    mutate(dualgroup = 'dual60')
  
  dual <- rbind(dual30, dual60)
  
  dualgroups <- unique(dual$dualgroup)
  
  rots <- unique(dual$obj_shape)
  
  # analyze and visualize learning
  duals <- NA
  for (group in dualgroups){
    
    for (rot in rots){
      
      ppdf <- dual %>% # get individual means so you can display individual learning curves
        filter(dualgroup == group) %>%
        filter(obj_shape == rot) %>%
        group_by(ppid) %>%
        filter(block_num %in% c(7,10)) %>% # rotated training blocks
        mutate(trialn = 1:n()) %>% # rewrite new trial numbers because rotations are interleaved
        filter(trialn %in% c(1:6,178:180)) %>%
        mutate(firstlast = ifelse(trialn %in% c(1:3), 1,
                                  ifelse(trialn %in% c(4:6), 2, 7))) %>% # last block is labeled "7" for plot aesthetics
        ungroup() %>%
        group_by(ppid, firstlast) %>% # get block means per ppid
        summarise(pptheta = mean(theta, na.rm = TRUE),
                  group = paste('dual', rot, sep = ""), rot = unique(dual_rotation))
      
      groupdf <- ppdf %>% # get block means per group, for plots
        group_by(firstlast) %>%
        summarise(grouptheta = mean(pptheta, na.rm = TRUE),
                  groupsd = sd(pptheta, na.rm = TRUE),
                  groupsem = groupsd/sqrt(length(unique(ppid))), 
                  group = unique(group), rot = unique(rot))
      
      if(group == 'dual30'){
        limits_y = c(-30, 30)
      } else { #dual60 group
        limits_y = c(-60, 60)
      }
      
      if(rot == "sphere") {
        linetypeset = "dashed"
      } else {
        linetypeset = "solid"
      }
      
      groupplot <- ggplot(data = groupdf,
                          aes(x = firstlast, y = grouptheta)) +
        geom_point() +
        geom_line(data = groupdf,
                  aes(x = firstlast, y = grouptheta)) +
        geom_ribbon(data = groupdf,
                    aes(ymin = grouptheta - groupsem,
                        ymax = grouptheta + groupsem),
                    alpha = 0.4) +
        geom_line(data = ppdf, 
                  aes(x = firstlast, y = pptheta, 
                      color = as.factor(ppid)),
                  linetype = linetypeset, alpha = 0.3) +
        coord_fixed(ratio = 1/7) +
        xlim(0,8) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.title = element_blank(), 
              legend.position = "none",
              axis.text.x = element_blank()) +
        ggtitle(paste('dual', rot)) +
        scale_y_continuous(name="Hand angle (Degrees)",
                           limits=c(-60, 60),
                           breaks=seq(-60,60,10)) +
        geom_hline(yintercept = limits_y[1],
                   linetype = "dashed",
                   color = "red")  +
        geom_hline(yintercept = limits_y[2],
                   linetype = "dashed",
                   color = "red")
      
      print(groupplot)
      
      if (is.na(duals)){
        duals <- ppdf
      } else {
        duals <- rbind(duals, ppdf)
      }
      
    }
    
  }
  
  # print some stats
  dual30 <- duals %>%
    filter(rot %in% c(-30,30)) %>%
    filter(firstlast %in% c(1,7))
  
  mod30 <- ezANOVA(data = dual30,
                 dv = pptheta, 
                 wid = ppid,
                 within = .(firstlast, rot),
                 #between = group,
                 detailed = TRUE,
                 return_aov = TRUE)
  
  print(mod30)
  
  dual60 <- duals %>%
    filter(rot %in% c(-60,60)) %>%
    filter(firstlast %in% c(1,7))
  
  mod60 <- ezANOVA(data = dual60,
                   dv = pptheta, 
                   wid = ppid,
                   within = .(firstlast, rot),
                   #between = group,
                   detailed = TRUE,
                   return_aov = TRUE)
  
  print(mod60)
  
  # analyze and visual dual AEs
  for (group in dualgroups){
    
    for (rot in rots){
      
      ppdf <- dual %>% 
        filter(dualgroup == group) %>%
        filter(obj_shape == rot) %>%
        group_by(ppid) %>%
        filter(block_num %in% c(5, 11, 14)) %>% # clamp blocks: baseline, include-strategy, exclude-strategy
        mutate(trialn = 1:n()) %>% # rewrite new trial numbers because rotations are interleaved
        filter(trialn %in% c(4:6, 7:9, 13:15)) %>%
        mutate(clampblock = ifelse(trialn %in% c(4:6), 'baseline',
                                   ifelse(trialn %in% c(7:9), 'include', 'exclude'))) %>%
        ungroup() %>%
        group_by(ppid, clampblock) %>% # get block means per ppid
        summarise(pptheta = mean(theta, na.rm = TRUE), groupname = group, rotation = rot) %>%
        spread(clampblock, pptheta) %>%
        mutate(ae = include - baseline, implicit_ae = exclude - baseline) 
      
      groupdf <- ppdf %>%
        group_by(groupname) %>%
        mutate(groupae = mean(ae, na.rm = TRUE),
               groupsd = sd(ae, na.rm = TRUE),
               groupsem = groupsd/sqrt(length(unique(ppid))),
               groupimpae = mean(implicit_ae, na.rm = TRUE),
               groupimpsd = sd(implicit_ae, na.rm = TRUE),
               groupimpsem = groupimpsd/sqrt(length(unique(ppid)))) 
      
      aebars_explicit <- ggplot(data = groupdf, aes(x = unique(groupname), y = unique(groupae))) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(data = groupdf, mapping = aes(x = unique(groupname), y = unique(groupae), 
                                                    ymin = unique(groupae) - unique(groupsem), ymax =unique(groupae) + unique(groupsem)),
                      width = 0.2, size = 0.5, color = "black",
                      position = position_dodge(width = 0.9)) +
        geom_beeswarm(data = ppdf, aes(x = unique(groupname), y = ae),
                      alpha = 1/7,
                      dodge.width = .9, cex = 3,
                      stroke = 0.3) +
        ylab("theta") +
        ggtitle(paste(group, rot, ' - with strategy')) +
        coord_fixed(ratio = 1/13) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.title = element_blank(), 
              legend.position = "none") +
        scale_y_continuous(name="Hand angle (Degrees)",
                           limits=c(-60, 60),
                           breaks=seq(-60,60,10)) 
      
      print(aebars_explicit)
      
      aebars_implicit <- ggplot(data = groupdf, aes(x = unique(groupname), y = unique(groupimpae))) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_errorbar(data = groupdf, mapping = aes(x = unique(groupname), y = unique(groupimpae), 
                                                    ymin = unique(groupimpae) - unique(groupimpsem), ymax =unique(groupimpae) + unique(groupimpsem)),
                      width = 0.2, size = 0.5, color = "black",
                      position = position_dodge(width = 0.9)) +
        geom_beeswarm(data = ppdf, aes(x = unique(groupname), y = implicit_ae),
                      alpha = 1/7,
                      dodge.width = .9, cex = 3,
                      stroke = 0.3) +
        ylab("theta") +
        ggtitle(paste(group, rot, ' - without strategy')) +
        coord_fixed(ratio = 1/13) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.title = element_blank(),
              legend.position = "none") +
        scale_y_continuous(name="Hand angle (Degrees)",
                           limits=c(-60, 60),
                           breaks=seq(-60,60,10))
      print(aebars_implicit)
      
      # stats - directional comparison to 0 depending on rotation
      print(paste(groupname, rot))
      
      ae_stats(groupdf)
      
      
      
    }
    
  }
  
}

ae_stats <- function(groupdf){
  
  if(ncol(ppdf) == 5){ # it's a single group
    
    print(unique(ppdf$group))
    
    if(unique(ppdf$group) == 'moveObject_R_Sphere_1'){
      t.test(ppdf$ae, mu = 0, alternative = "less") # reach ae only
      cohensD(ppdf$ae, mu=0)
    } else { # cube
      t.test(ppdf$ae, mu = 0, alternative = "greater") # reach ae only
      cohensD(ppdf$ae, mu=0)
    }
    
  } else { # it's a dual group
    
    print(unique(groupdf$groupname))
    print(unique(groupdf$rotation))
    
    if(unique(groupdf$rotation) == 'sphere'){
      #y <- t.test(ppdf$ae, mu = 0, alternative = "less") # reach ae only
      y <- t.test(ppdf$implicit_ae, mu = 0, alternative = "less") # implicit ae
      print(y)
      cohensD(ppdf$ae, mu=0)
    } else { # cube
      #y <- t.test(ppdf$ae, mu = 0, alternative = "greater") # reach ae only
      y <- t.test(ppdf$implicit_ae, mu = 0, alternative = "greater") # implicit ae
      print(y)
      cohensD(ppdf$ae, mu = 0)
    }
    
  }
  
}

analyzed_dual_collapsed <- function(){ 
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggbeeswarm)
  library(ez)
  
  dual30 <- read.csv('data/all_reaches_dual.csv', header = TRUE)
  dual60 <- read.csv('data/all_reaches_dual_60.csv', header = TRUE)
  
  dual30 <- dual30 %>%
    mutate(dualgroup = 'dual30')
  
  dual60 <- dual60 %>%
    mutate(dualgroup = 'dual60')
  
  dual <- rbind(dual30, dual60)
  
  dualgroups <- unique(dual$dualgroup)
  
  # make a column that has normalized errors
  dual_n <- dual %>% 
    mutate(theta_n = ifelse(dual_rotation %in% c(30, 60), theta*-1, theta*1))
  
  duals_n <- NA # collapsed across rotations
  
  for (group in dualgroups){
    
    ppdf <- dual_n %>% 
      filter(dualgroup == group) %>%
      group_by(ppid) %>%
      filter(block_num %in% c(7,10)) %>% # rotated training blocks
      mutate(trialn = 1:n()) %>% # rewrite new trial numbers because rotations are interleaved
      filter(trialn %in% c(1:3,178:180)) %>%
      mutate(firstlast = ifelse(trialn %in% c(1:3), 1, 7)) %>% # last block is labeled "7" 
      ungroup() %>%
      group_by(ppid, firstlast) %>% # get block means per ppid
      summarise(pptheta_n = mean(theta_n, na.rm = TRUE))
    
    groupdf <- ppdf %>% # get block means per group, for plots
      group_by(firstlast) %>%
      summarise(grouptheta = mean(pptheta_n, na.rm = TRUE),
                groupsd = sd(pptheta_n, na.rm = TRUE),
                groupsem = groupsd/sqrt(length(unique(ppid))))
    
    
    if (is.na(duals_n)){
      duals_n <- ppdf
    } else {
      duals_n <- rbind(duals_n, ppdf)
    }
    
    
    # compare first and last block for each dual group - collapsed rotations
    t.test(duals_n$pptheta_n[which(duals_n$firstlast==1)],
           duals_n$pptheta_n[which(duals_n$firstlast==7)],
           alternative = "less",
           paired = TRUE)
    
  }
  
  # compare first and last block for each dual group - collapsed rotations
  t.test(duals_n$pptheta_n[which(duals_n$firstlast==1)],
         duals_n$pptheta_n[which(duals_n$firstlast==7)],
         alternative = "less",
         paired = TRUE)
  
  cohensD(duals_n$pptheta_n[which(duals_n$firstlast==1)], duals_n$pptheta_n[which(duals_n$firstlast==7)])
  
}

plot_dual_full_LC <- function(){
  
  library(dplyr)
  library(ggplot2)
  library(svglite)
  library(gginnards)
  library(Hmisc)
  library(ggbeeswarm)
  library(tidyr)
  library(lsr)
  library(OneR)
  
  dual30 <- read.csv('data/all_reaches_dual.csv', header = TRUE)
  dual60 <- read.csv('data/all_reaches_dual_60.csv', header = TRUE)
  
  dual30 <- dual30 %>%
    mutate(dualgroup = 'dual30')
  
  dual60 <- dual60 %>%
    mutate(dualgroup = 'dual60')
  
  dual <- rbind(dual30, dual60)
  
  dualgroups <- unique(dual$dualgroup)
  
  rots <- unique(dual$obj_shape)
  
  dual <- tbl_df(dual) 
  
  for (group in dualgroups){

    dfcube <- dual %>%
      filter(dualgroup == group) %>%
      filter(block_num > 3) %>% # exclude practice blocks
      group_by(ppid) %>%
      filter(obj_shape == "cube") %>%
      mutate(trialn = 1:n(),
             allbinno = bin(trialn, # bins for ALL trials, ignore task
                            nbins = (max(trialn))/3,
                            labels = c(1:(max(trialn)/3)))) %>%
      group_by(block_num) %>%
      mutate(binno = bin(trial_num_in_block, # bins per task 
                         nbins = (max(trial_num_in_block))/3,
                         labels = c(1:(max(trial_num_in_block)/3))))
    
    taskmeanscube <- dfcube %>%
      group_by(block_num) %>%
      group_by(allbinno) %>%
      mutate(Mean_th = mean(theta, na.rm = TRUE),
             SD_th = sd(theta, na.rm = TRUE),
             SEM_th = SD_th/sqrt(length(unique(ppid)))) %>%
      distinct(allbinno, .keep_all = TRUE)
    
    dfsphere <- dual %>%
      filter(dualgroup == group) %>%
      filter(block_num > 3) %>% # exclude practice blocks
      group_by(ppid) %>%
      filter(obj_shape == "sphere") %>%
      mutate(trialn = 1:n(),
             allbinno = bin(trialn, # bins for ALL trials, ignore task
                            nbins = (max(trialn))/3,
                            labels = c(1:(max(trialn)/3)))) %>%
      group_by(block_num) %>%
      mutate(binno = bin(trial_num_in_block, # bins per task 
                         nbins = (max(trial_num_in_block))/3,
                         labels = c(1:(max(trial_num_in_block)/3))))
  
    taskmeanssphere <- dfsphere %>%
      group_by(block_num) %>%
      group_by(allbinno) %>%
      mutate(Mean_th = mean(theta, na.rm = TRUE),
             SD_th = sd(theta, na.rm = TRUE),
             SEM_th = SD_th/sqrt(length(unique(ppid)))) %>%
      distinct(allbinno, .keep_all = TRUE)
    
    taskmeans <- bind_rows(taskmeanscube, taskmeanssphere)
    
    ## PLOT FULL LC ##
    if (group == "dual30"){ # for plotting the rotation size horizontal lines 
      rotsize <- c(-30, 30)
    } else { 
      rotsize <- c(-60,60)
    }
    
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
                         limits=c(0, 72),
                         breaks=seq(0,72,9)) +
      scale_y_continuous(name="Hand angle (Degrees)",
                         limits=c(-60, 60),
                         breaks=seq(-60,60,10)) +
      geom_hline(yintercept = 0,  color = "grey") +
      geom_vline(xintercept = 8,  color = "grey") +
      geom_hline(yintercept = rotsize[1],  linetype = "dashed", color = "red")+
      geom_hline(yintercept = rotsize[2],  linetype = "dashed", color = "red")
    
    print(taskplot)

  
}
  
}
