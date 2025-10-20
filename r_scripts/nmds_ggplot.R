
ib<-read.csv("./data/Atlantic_Caribbean.csv",header=T)

ib2<-ib[,-1:-4]


rownames(ib2)<-ib[,4]


ac<-vegdist(ib2, "jaccard")


nmdsBird<-metaMDS(ac,k=2, trace=T, trymax=100)
nmdsBird

stressplot(nmdsBird)



#ggplot
data.scores <- as.data.frame(scores(nmdsBird))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- ib$Island.Name  # create a column of island names, from the original data frame "ib"
data.scores$Ocean <- ib$Ocean  #  create a column of ocean names, from the original data frame "ib"
data.scores$Archipelago <- ib$Archipelago # create a column of archipelago names, from the original data frame "ib"
head(data.scores)  #look at the data

ggplot(data.scores, aes(x= NMDS1, y= NMDS2, col=Archipelago, shape=Ocean)) + #denote groupings by color "col" and shape
  geom_point() +#adds points
  geom_text(aes(label=rownames(data.scores)),hjust=0, vjust=0)+#adds island names
  stat_ellipse() +#adds ellipses
  theme_bw() +
  xlim(-.75, .75)+
  ylim(-.75,.75)+
  labs(title = "NMDS Plot")