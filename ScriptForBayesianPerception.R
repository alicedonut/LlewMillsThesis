rm(list = ls())


bDensPlot <- function(priorM, priorSD, lM, lSD, postM, postSD, gTitle, plotName) {

# generate data
a <- rnorm(100000, priorM, priorSD)
b <- rnorm(100000, lM, lSD)
c <- rnorm(100000, postM, postSD)
group <- rep(c("Prior/Expectancy", "Likelihood/Sensory Data", "Posterior/Perception"), each = 100000)
dv <- c(a,b,c)
df <- data.frame(group, dv)


# now we plot it
library(ggplot2)

d <- ggplot(df, aes(x = dv, fill = group)) +
  geom_density(alpha = 0.4) +
  xlab("Score") +
  ylab("") +
  ylim(c(0,1)) +
  ggtitle(substitute(gTitle)) +
  scale_fill_manual(values = c("forestgreen", "purple1", "blue"), breaks = c("Prior/Expectancy", "Likelihood/Sensory Data", "Posterior/Perception")) +
  scale_y_continuous(breaks=NULL) +
  scale_x_continuous(limits = c(0,10), breaks = pretty) +
  theme_bw() # this gets rid of the grey background


specs <- theme(plot.title = element_text(face = "bold", color = "black", size = 16, vjust = 2, hjust = 1),
               axis.title = element_text(face = "bold", color = "black", size = 15, vjust = 1.5),
               axis.title.x = element_text(face = "bold", color = "black", size = 15, vjust = 0.01),
               axis.text.y =  element_blank(),
               axis.text.x = element_text(face = "plain", color = "black", size = 14, angle = 0, vjust = 1, hjust = 1),
               legend.title = element_blank(),
               legend.text = element_text(face = "plain", color = "black", size = 12),
               panel.grid.minor = element_blank(),
               panel.grid.major = element_blank())


d1 <- d + specs

ggsave(paste("/Users/Mills/Dropbox/PhD/Placebo/LlewMillsThesis/bayesianDensityPlots/", plotName, sep = ""), plot = d1, device = "png", height = 5, width = 7)

return(d1)

}


bDensPlot(3,2,7,2.5,8,3,"h","hiIncongruentImprecise")
