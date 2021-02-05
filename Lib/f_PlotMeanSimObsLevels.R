# Function : Plots the graph between mean observed and simulated mean levels at piezometers


f_PlotMeanSimObsLevels<-function(df, plotName, piezMin, piezMax, layerNames){
  
	# Opening pdf output file
	pdf(plotName, height = 7, width = 10)

	figTitle <- "Mean observed vs. mean simulated levels (1970-2020)"

	# First bissector line
    bissecXY <- seq(piezMin, piezMax)

	# Correlation coeffcient (Pearson)
	Cpearson <- cor(df[,15], df[,14], use="pairwise.complete", method = "pearson")
	print(paste("Pearson linear correlation coefficient on mean levels :",Cpearson,sep=" "))

	pl =  	ggplot(df) +
			geom_point(aes(x = df[,14], y = df[,15], group=ID_LAY, colour = layerNames[ID_LAY+1]), size = 2.0, alpha = 0.7) + 
         	geom_line(aes(x = df[,1], y = df[,1], color = 'X=Y'), size = 0.4, alpha = 0.4, colour = "black", linetype = "dashed")  +  # Rustine méga foireuse car tous les vecteurs x et y doivent avoir la meme longeur ! 
			ggtitle(label = figTitle) + 
			labs(x = 'Mean observed water level [m NGF]', y = 'Mean simulated water level [m NGF]') +
			geom_label(label=paste("Cp = ",signif(Cpearson,4)), x=25, y=piezMax-25, label.padding = unit(0.6, "lines"), label.size = 0.45, color = "black", fill="#FFFFFF")

	# Aesthetic settings
	myGraphOptions <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust = 0.5),
	plot.subtitle = element_text(hjust = 0.5, size = (13)),
	legend.title = element_text(colour = "black",  face = "bold.italic", family = "Helvetica"),
	legend.text = element_text(face = "italic", colour="black",family = "Helvetica"),
	legend.position="top", legend.direction="horizontal",
	axis.title = element_text(family = "Helvetica", size = (13), colour = "black"),
	axis.text.y = element_text(family = "Helvetica", face="bold", colour = "black", size = (13)),
	axis.text.x = element_text(family = "Helvetica", face="bold", colour = "black", angle = 0, hjust = 1, size = (13)),
	plot.margin = unit(c(1,1,1,1), "cm"))
     

    print(pl + myGraphOptions + scale_color_manual(values=c("#CBEB6B", "#FFFF73","#FFDC78","#FFAA00","#A87000","#553900","#55FF00")) +
			labs(color = "Layers : ") +
			scale_x_continuous(limits=c(piezMin, piezMax)) + scale_y_continuous(limits=c(piezMin, piezMax)) + ggtitle(label = figTitle))

	dev.off()

	return(0)
}
