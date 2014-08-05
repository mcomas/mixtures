library(plyr)
require(abind)
require(gtools)
require(ggplot2)
require(grid)
require(gridExtra)
require(ggtern)
load('data/selected-glass-data.RData')

lbs=c('Al', 'Ca', 'Si')
df = X
df$glasses = y

df.win = data.frame(
          Ca=c(0.2, 0.0, 0.0),
          Si=c(0.8, 0.8, 1.0 ),
          Al=c(0.0, 0.2, 0.0),
          glasses=c(NA,NA,NA))
p0 = ggtern(data=df, aes(Ca, Al, Si, col=glasses))+geom_point(size=1.1) + 
  theme_rgbw() +
  scale_color_manual(values=c("black", "#CC6666", "#66CC99"), 
                   breaks=c("Con", "Head", "Veh"),
                   labels=c("container", "headlamp", "vehicle window"))

pa = p0 + geom_polygon(data=df.win, color="red",alpha=0.25,size=1,linetype=1) +
  scale_T_continuous(breaks=seq(0.2,0.8,0.2),minor_breaks=seq(0,1,0.05)) + 
  scale_L_continuous(breaks=seq(0.2,0.8,0.2),minor_breaks=seq(0,1,0.05)) + 
  scale_R_continuous(breaks=seq(0.2,0.8,0.2),minor_breaks=seq(0,1,0.05)) +
  guides(col=FALSE)

pb = p0 + geom_point(size=3) + coord_tern(Tlim=c(0,.2), Llim=c(0.0,0.2), Rlim=c(0.8,1.0)) +
  scale_T_continuous(breaks=seq(0.05,0.15,0.05),minor_breaks=seq(0,1,0.05)) + 
  scale_L_continuous(breaks=seq(0.05,0.15,0.05),minor_breaks=seq(0,1,0.05)) + 
  scale_R_continuous(breaks=seq(0.85,0.95,0.05),minor_breaks=seq(0,1,0.05)) + 
  ggtitle("Forensic Glass data set") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size = 12),
        legend.position="bottom",
        legend.key = element_blank())

pa = pa+theme_classic() + scale_color_grey()
pb = pb+theme_classic() + scale_color_grey()

pdf(file='figures//main_df.pdf', width=9.5, height=4.4, pointsize=10)
grid.newpage()
vpb_ <- viewport(width = 0.46, height = 0.46, x = 0.15, y = 0.74)  # the larger map
vpa_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.49 )  # the inset in upper right
print(pb, vp = vpa_)
print(pa, vp = vpb_)
dev.off()
