
library(ggplot2)
library(tikzDevice)

n <- 1000

pairwise_dist = matrix(0, nrow = 499500, ncol = 7)

for (d in c(2,5,10,20,50,100,1000)) {
    x <- matrix(runif(n*d),nrow=n,ncol=d)
    
    pairwise_dist[,which(c(2,5,10,20,50,100,1000) == d)] <- as.vector(dist(x))
}

tikz(file=paste0("freq_distance",which(c(2,5,10,20,50,100,1000)==d),".tex"), width = 8, height = 4)
the_plot <- ggplot(data.frame(dist = pairwise_dist[,1]), aes(dist)) +
    geom_histogram(aes(y=..count../sum(..count..), fill = "2"), guide = "legend", binwidth = ((max(pairwise_dist[,1])-min(pairwise_dist[,1]))/30), fill = "red", alpha = 0.4) +
    geom_histogram(data = data.frame(dist = pairwise_dist[,2]), guide = "legend", aes(y=..count../sum(..count..), fill = "5"), binwidth = ((max(pairwise_dist[,2])-min(pairwise_dist[,2]))/30), fill = "blue", alpha = 0.6) +
    geom_histogram(data = data.frame(dist = pairwise_dist[,3]), guide = "legend", aes(y=..count../sum(..count..), fill = "10"), binwidth = ((max(pairwise_dist[,3])-min(pairwise_dist[,3]))/30), fill = "green", alpha = 0.6) +
    geom_histogram(data = data.frame(dist = pairwise_dist[,4]), guide = "legend", aes(y=..count../sum(..count..), fill = "20"), binwidth = ((max(pairwise_dist[,4])-min(pairwise_dist[,4]))/30), fill = "orange", alpha = 0.6) +
    geom_histogram(data = data.frame(dist = pairwise_dist[,5]), guide = "legend", aes(y=..count../sum(..count..), fill = "50"), binwidth = ((max(pairwise_dist[,5])-min(pairwise_dist[,5]))/30), fill = "violet", alpha = 0.6) +
    geom_histogram(data = data.frame(dist = pairwise_dist[,6]), guide = "legend", aes(y=..count../sum(..count..), fill = "100"), binwidth = ((max(pairwise_dist[,6])-min(pairwise_dist[,6]))/30), fill = "#E69F00", alpha = 0.6) +
    geom_histogram(data = data.frame(dist = pairwise_dist[,7]), guide = "legend", aes(y=..count../sum(..count..), fill = "1000"), binwidth = ((max(pairwise_dist[,7])-min(pairwise_dist[,7]))/30), fill = "#CC79A7", alpha = 0.6) +
    labs(fill="Dimension") +
    scale_y_continuous(name ="Frequence",limits = c(0,0.15), breaks=seq(0,0.15,0.05)) +
    scale_x_continuous(name ="Distance",limits = c(0,15),breaks=seq(0,15,3)) +
    # scale_fill_manual(labels = c(2,5,10,20,50,100,1000), 
    #                   values = c("red", "blue", "green", "orange", "violet", "#E69F00", "#CC79A7"), 
    #                   guide = guide_legend(fill = c("red", "blue", "green", "orange", "violet", "#E69F00", "#CC79A7"),
    #                                        colour = c("red", "blue", "green", "orange", "violet", "#E69F00", "#CC79A7")))
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    guides(fill=FALSE)

print(the_plot)
dev.off()
