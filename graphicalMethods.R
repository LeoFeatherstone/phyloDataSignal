library(tidyverse)
library(latex2exp)
library(ggpmisc)
library(viridis)

path <- getwd()
figPath <- paste0(path, '/figures/')

## data fig
fullData <- 'Full Data (F)\n>Sample1_01-01-2020\nACGTATCGTATCATACGTG...\n>Sample2_02-02-2020\nAGGTATCGTATCATACGTG...\n>Sample3_03-03-2020\nATCACATCGTATATACGTG...'
seqData <- 'Sequence Only (S)\n>Sample1_??-??-????\nACGTATCGTATCATACGTG...\n>Sample2_??-??-????\nAGGTATCGTATCATACGTG...\n>Sample3_??-??-????\nATCACATCGTATATACGTG...'
dateData <- 'Dates Only (D)\n>Sample1_01-01-2020\nNNNNNNNNNNNNNNN...\n>Sample2_02-02-2020\nNNNNNNNNNNNNNNN...\n>Sample3_03-03-2020\nNNNNNNNNNNNNNNN...'
noData <- 'Marginal Prior (N)\n>Sample1_??-??-????\nNNNNNNNNNNNNNNN...\n>Sample2_??-??-????\nNNNNNNNNNNNNNNN...\n>Sample3_??-??-????\nNNNNNNNNNNNNNNN...'

# dummy data
set.seed(1234)
dat <- as_tibble(rbind(cbind(rnorm(2,0.2,n=10000), rep('Full Data', length.out=10000)),
					cbind(rnorm(2.3,0.2,n=10000), rep('Sequence Only', length.out=10000)),
					cbind(rnorm(1.2,0.2,n=10000), rep('Dates Only', length.out=10000)),
					cbind(abs(rnorm(0.9,0.5,n=10000)), rep('No Data', length.out=10000))))
colnames(dat) <- c('R', 'type')
dat$R <- as.numeric(dat$R)

label <- data.frame(
  x = c(2.25, 3.25, 1.25, 0.25), 
  y = c(3, 3, 3, 3), 
  label = c(fullData, seqData, dateData, noData)
)
# plane
trsup <- data.frame(x=c(0,0,1.2),y=c(0,1.2,1.2))
trinf <- data.frame(x=c(0,1.2,1.2),y=c(0,0,1.2))
p1 <- ggplot() +
		xlim(0,1.2) + ylim(0,1.2) +
		# shade quad 1
		geom_polygon(aes(x=x, y=y), data=trsup, fill=alpha("grey", 0.5)) +
		geom_polygon(aes(x=x, y=y), data=trinf, fill=alpha("white", 0.5)) +
		geom_quadrant_lines(linetype = "solid") +
		coord_fixed(ratio = 1) +
			ylab(TeX('W_{S}')) + xlab(TeX('W_{D}')) +	
			annotate("text", 	x = c(0.2,1.1), 
							y = c(1.1,0.2), 
			label = c("atop(Date-Driven, (W[S]>W[D]))", 
				"atop(Seq-Driven, (W[S]<W[D]))"), size=5, parse=T) +
		# point example 
		annotate('segment', x=0, y=0, xend=0.5, yend=0.1) +
		annotate('segment', x=0.5, y=0.1, xend=0.5, yend=0.5) +
		annotate('point', x=0.5, y=0.1, size=1.5) +
		annotate('text', parse=T, x=0.65, y=0.1, label='paste("(", 0.5,",", ~0.1,")")') +
		annotate('text', parse=T, x=0.25, y=0.1, size=4, angle=180*(atan(0.1/0.5)/pi), label="r[SD]==0.51") +
		annotate('text', parse=T, x=0.65, y=0.25, size=4, label="d[SD]==0.4") +
		annotate("label", x = 0.825, y = 0.85, colour = "black", label='r[SD]==sqrt(W[S]^2+W[D]^2)', parse=T, size=5) +
		annotate("label", x = 0.825, y = 0.75, colour = "black", label='d[SD]==abs(W[S]-W[D])', parse=T, size=5) +
			theme_minimal() +
			theme(legend.position='bottom', 
				legend.title=element_text(size=14),
				legend.text=element_text(size=14),
				axis.title=element_text(size=14),
				axis.text=element_text(size=12))


### new arrangement trying
label <- data.frame(
  x = c(0, 0.33, 0.66, 1), 
  y = c(1, 1, 1, 1), 
  label = c(fullData, seqData, dateData, noData),
  fill=alpha(viridis::viridis(4)[c(2,4,1,3)],0.2)
)

dataPlot <- ggplot() +
  annotate(x=0, y=1, geom='label', label=noData, fill=alpha(viridis(4)[3],0.5), hjust = 0) +
  annotate(x=1, y=1, geom='label', label=dateData, fill=alpha(viridis(4)[1],0.5), hjust = 0) +
  annotate(x=2, y=1, geom='label', label=fullData, fill=alpha(viridis(4)[2],0.5), hjust = 0) +
  annotate(x=3, y=1, geom='label', label=seqData, fill=alpha(viridis(4)[4],0.5), hjust = 0) +
  xlim(-0.25,3.5) +
  theme_void() 

dists <- ggplot(data = data.frame(x = c(0, 4)), aes(x)) +
# datasets
  #annotate("segment", x = 2.5, xend = 2.5, y = 3, yend = 2.05,
  #         colour = "black", size = 1, arrow = arrow()) +
  #annotate("segment", x = 1.5, xend = 1.5, y = 3, yend = 2.05,
  #         colour = "black", size = 1, arrow = arrow()) +
  #annotate("segment", x = 3.75, xend = 3.05, y = 3, yend = 2.05,
  #         colour = "black", size = 1, arrow = arrow()) +
  #annotate("segment", x = 0.5, xend = 1, y = 3, yend = 0.65,
  #         colour = "black", size = 1, arrow = arrow()) +
  # date data
  stat_function(fun = dnorm, n = 10000, args = list(mean = 1.5, sd = 0.2), 
  	fill=alpha(viridis::viridis(4)[1], 0.5), geom='density') +
  # fulldata
  stat_function(fun = dnorm, n = 10000, args = list(mean = 2.5, sd = 0.2), 
  	fill=alpha(viridis::viridis(4)[2], 0.5), geom='density') +
  # no data
  stat_function(fun = dgamma, n = 10000, args = list(shape = 3.5, rate = 2.5), 
  	fill=alpha(viridis::viridis(4)[3], 0.5), geom='density') + ylab("") +
  # seq data
  stat_function(fun = dnorm, n = 10000, args = list(mean = 3, sd = 0.2), 
  	fill=alpha(viridis::viridis(4)[4], 0.5), geom='density') +

  # wasserstein
  annotate("segment", x = 2.51, xend = 3, y = 1.5, yend = 1.5,
           colour = "black", size = 1, arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("label", x = 2.75, y = 1.5, colour = "black", label='W[S]==0.5', parse=T) +

  annotate("segment", x = 1.5, xend = 2.49, y = 1.5, yend = 1.5,
           colour = "black", size = 1, arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("label", x = 2, y = 1.5, colour = "black", label='W[D]==1.0', parse=T) +

  annotate("segment", x = 1, xend = 2.49, y = 0.4, yend = 0.4,
           colour = "black", size = 1, arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("label", x = 1.75, y = 0.4, colour = "black", label='W[N]==1.15', parse=T) +

  annotate("label", x = 0.6, y = 1, colour = "black", label='W[X]==integral(paste("| ", F[X]^-1*(u)-F[F]^-1*(u), " |")*du, 0, 1)', parse=T, size=4) +
  annotate("label", x = 3.5, y = 1, colour = "black", label='W[D]>W[S]%->%Seq~Driven', parse=T) +
  xlab(TeX('Posterior $R_{0}$')) + ylab('Density') + 
  coord_fixed() +
  theme_minimal() +
		theme(legend.position='bottom', 
			legend.title=element_text(size=14),
			legend.text=element_text(size=14), 
			axis.title=element_text(size=14))

tmp <- cowplot::plot_grid(dists, p1, labels=c('B', 'C'), ncol=2, rel_widths=c(1.3,1))


pdf(paste0(figPath, 'graphicalMethods.pdf'), useDingbats=F, width=16, height=8)
	cowplot::plot_grid(dataPlot, tmp, labels=c("A", NA), ncol=1, rel_heights=c(1,3.5))
dev.off()





