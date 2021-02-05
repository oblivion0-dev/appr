# Function : Plots the graph between mean observed and simulated discharges at gauging stations


f_PlotMeanSimObsDischarges<-function(df, plotName, Qmin, Qmax){
  
	# Opening pdf output file
	pdf(plotName, height = 7, width = 10)

	figTitle <- "Mean observed vs. mean simulated discharges (1970-2020)"

	# First bissector line
    bissecXY <- seq(Qmin, Qmax)

	# Correlation coeffcient (Pearson)
	Cpearson <- cor(df[,4], df[,3], use="pairwise.complete", method = "pearson")
	print(paste("Pearson linear correlation coefficient on mean discharges :",Cpearson,sep=" "))

	pl =  	ggplot(df) +
			geom_point(aes(x = df[,3], y = df[,4]), size = 2.0, alpha = 0.7) + 
      #   	geom_line(aes(x = seq(0,Qmax), y = seq(0,Qmax), color = 'X=Y'), size = 0.4, alpha = 0.4, colour = "black", linetype = "dashed")  +  
			ggtitle(label = figTitle) + 
			labs(x = 'Mean observed discharge [m3/s]', y = 'Mean simulated discharge [m3/s]') +
			geom_label(label=paste("Cp = ",signif(Cpearson,4)), x=1, y=500, label.padding = unit(0.6, "lines"), label.size = 0.45, color = "black", fill="#FFFFFF")

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
     

    print(pl + myGraphOptions + scale_color_manual(values=c("#1B019B")) +
			labs(color = "Layers : ") +
			scale_x_log10(limits=c(Qmin, Qmax), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
			scale_y_log10(limits=c(Qmin, Qmax), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
			ggtitle(label = figTitle))

	dev.off()

	return(0)
}
