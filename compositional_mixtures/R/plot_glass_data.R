library(plyr)
library(abind)
library(gtools)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggtern)
load('data/selected-glass-data.RData')

lbs=c('Al', 'Ca', 'Si')
df = X
df$glasses = y

df.win = data.frame(
          Ca=c(0.2, 0.0, 0.0),
          Si=c(0.8, 0.8, 1.0 ),
          Al=c(0.0, 0.2, 0.0),
          glasses=c(NA,NA,NA))
p0 = ggtern(data=df, aes(Ca, Al, Si, shape=glasses))+
  theme_rgbw() +
  scale_shape_manual(name="Type of glasses",
                     values=c(1, 2, 0),
                     breaks=c("Con", "Head", "Veh"),
                     labels=c("container", "headlamp", "vehicle window") )
#   scale_color_manual(values=c("black", "#CC6666", "#66CC99"), 
#                    breaks=c("Con", "Head", "Veh"),
#                    labels=c("container", "headlamp", "vehicle window"))

pa = p0 + geom_polygon(data=df.win, color="red",alpha=0.25,size=1,linetype=1) + geom_point(size=1.2) +
  scale_T_continuous(breaks=seq(0.2,0.8,0.2),minor_breaks=seq(0,1,0.05)) + 
  scale_L_continuous(breaks=seq(0.2,0.8,0.2),minor_breaks=seq(0,1,0.05)) + 
  scale_R_continuous(breaks=seq(0.2,0.8,0.2),minor_breaks=seq(0,1,0.05)) +
  theme_bw() + 
  guides(shape=FALSE)

pb = p0 + geom_point(size=4) + coord_tern(Tlim=c(0,.2), Llim=c(0.0,0.2), Rlim=c(0.8,1.0)) +
  scale_T_continuous(breaks=seq(0.05,0.15,0.05),minor_breaks=seq(0,1,0.05)) + 
  scale_L_continuous(breaks=seq(0.05,0.15,0.05),minor_breaks=seq(0,1,0.05)) + 
  scale_R_continuous(breaks=seq(0.85,0.95,0.05),minor_breaks=seq(0,1,0.05)) + 
#  ggtitle("Forensic Glass data set") + 
  theme_bw() + 
  theme(plot.title = element_text(lineheight=.8, face="bold"),
        legend.title = element_text(colour="black", size = 15, face='bold'),
        legend.text = element_text(colour="black", size = 15),
        #legend.position="bottom",
        legend.key = element_blank())

pa = pa + scale_color_hue()
pb = pb +  scale_color_hue()

pdf(file='figures//main_df.pdf', width=11.72, height=6.4, colormodel = 'gray')
grid.newpage()
vpb_ <- viewport(width = 0.46, height = 0.46, x = 0.2, y = 0.7)  # the larger map
vpa_ <- viewport(width = 1, height = 1, x = 0.6, y = 0.49 )  # the inset in upper right
print(pb, vp = vpa_)
print(pa, vp = vpb_)
dev.off()
