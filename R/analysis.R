# analyze dual VR data
# cube: 30 or 60 / CCW rotation (polar co-ordinates)
# sphere: -30 or -60 / CW rotation
# data was normalized by flipping CW

plot_single_errors <- function(){
  
  library(dplyr)
  library(ggplot2)
  library(ggbeeswarm)
  
  single <- read.csv('data/all_reaches_single.csv', header = TRUE)
  
  groupnames <- unique(single$experiment)
  
  # analyze and visualize learning
  singles <- NA
  for (groupname in groupnames) {
    
    ppdf <- single %>% # get individual means so you can display individual learning curves
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
    
    groupplot <- ggplot(data = groupdf, aes(x = firstlast, y = grouptheta)) +
      geom_point() +
      geom_line(data = groupdf, aes(x = firstlast, y = grouptheta)) +
      geom_ribbon(data = groupdf,
                  aes(ymin = grouptheta - groupsem, ymax = grouptheta + groupsem),
                  alpha=0.4) +
      geom_line(data = ppdf, aes(x = firstlast, y = pptheta, color = as.factor(ppid)), alpha = 0.1) +
      coord_fixed(ratio = 1/7) +
      ylim(-45,45) +
      xlim(0,8) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            legend.title = element_blank(), legend.position = "none",
            axis.text.x = element_blank()) +
      ggtitle(groupname)
    
    print(groupplot)
    
    if (is.na(singles)){
      singles <- ppdf
    } else {
      singles <- rbind(singles, ppdf)
    }

  } 
  
  # print some stats 
  library(ez)
  mod <- ezANOVA(data = singles,
                  dv = pptheta, 
                  wid = ppid,
                  within = firstlast, 
                  between = .(rot), 
                  detailed = TRUE,
                  return_aov = TRUE)
  
  print(mod)
  
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
    
    aebars <- ggplot(data = groupdf, aes(x = unique(group), y = unique(groupae))) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(data = groupdf, mapping = aes(x = unique(group), y = unique(groupae), 
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
      scale_y_continuous(breaks = seq(-45, +45, 15), limits = c(-45, 45))
    
    print(aebars)
    
    # print some stats 
    t.test(ppdf$ae, mu = 0, alternative = "less")
    
  }

}

plot_dual_errors <- function(){
  
  library(dplyr)
  library(ggplot2)
  library(ggbeeswarm)
  
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
      
      groupplot <- ggplot(data = groupdf, aes(x = firstlast, y = grouptheta)) +
        geom_point() +
        geom_line(data = groupdf, aes(x = firstlast, y = grouptheta)) +
        geom_ribbon(data = groupdf,
                    aes(ymin = grouptheta - groupsem, ymax = grouptheta + groupsem),
                    alpha = 0.4) +
        geom_line(data = ppdf, aes(x = firstlast, y = pptheta, color = as.factor(ppid)), alpha = 0.1) +
        coord_fixed(ratio = 1/7) +
        ylim(-45,45) +
        xlim(0,8) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              legend.title = element_blank(), legend.position = "none",
              axis.text.x = element_blank()) +
        ggtitle(paste('dual', rot, sep = ""))
      
      print(groupplot)
      
      if (is.na(duals)){
        duals <- ppdf
      } else {
        duals <- rbind(duals, ppdf)
      }
      
    }
    
  }
  
  # print some stats
  mod <- ezANOVA(data = duals,
                 dv = pptheta, 
                 wid = ppid,
                 within = .(firstlast, rot),
                 between = group,
                 detailed = TRUE,
                 return_aov = TRUE)
  
  print(mod)
  
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
               groupsem = groupsd/sqrt(length(unique(ppid))))
      
      aebars <- ggplot(data = groupdf, aes(x = unique(groupname), y = unique(groupae))) +
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
        ggtitle(paste(group, rot)) +
        coord_fixed(ratio = 1/13) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              legend.title = element_blank(), legend.position = "none") +
        scale_y_continuous(breaks = seq(-45, +45, 15), limits = c(-45, 45))
      
      print(aebars)
      
      
      # print some stats 
      t.test(ppdf$ae, mu = 0, alternative = "less") # reach ae only
      t.test(ppdf$implicit_ae, mu = 0, alternative = "less") # implicit ae
      
      
    }
    
  }
  
}




## next steps -- PI maybe
