# Supplementary Figure 3: redraw the saved density curves.
# Open replication.Rproj, then run this file from top to bottom (or click Source).
# Paths are relative to code/; this script does not change the working directory.

if (!dir.exists("../data/intermediate"))
  stop("Open replication.Rproj or set the working directory to code/ first.")

# 1. Packages and output folders ----
library(data.table)
library(dplyr)
library(ggplot2)
library(R.utils)

dir.create("../output/figures", recursive = TRUE, showWarnings = FALSE)
set.seed(123)

# 2. Data and analysis ----
# Figure style.
base_theme <- theme_bw(base_size=14) + theme(
  panel.grid.minor=element_blank(), panel.grid.major=element_line(colour='grey93',linewidth=.3),
  legend.position='bottom', legend.title=element_blank(),
  legend.text=element_text(size=14), legend.key.width=grid::unit(1.0,'cm'),
  legend.margin=margin(0,0,0,0), legend.box.spacing=grid::unit(3,'pt'),
  axis.title=element_text(size=15), plot.margin=margin(10,12,8,8))

curves <- fread('../data/intermediate/supplementary/proof_density_curves.csv')
order <- c('Ground truth','Machine-learning crosswalk','Area-weighted crosswalk')
curves[,method:=factor(method,levels=order)]
p <- ggplot(curves,aes(weight,density,colour=method,linetype=method)) +
  geom_line(linewidth=.75) +
  scale_colour_manual(labels=c('Ground truth'='Ground truth','Machine-learning crosswalk'='Machine learning','Area-weighted crosswalk'='Area weighted'),values=c('Ground truth'='#bdd7e7','Machine-learning crosswalk'='#2171b5','Area-weighted crosswalk'='#6baed6')) +
  scale_linetype_manual(labels=c('Ground truth'='Ground truth','Machine-learning crosswalk'='Machine learning','Area-weighted crosswalk'='Area weighted'),values=c('Ground truth'='solid','Machine-learning crosswalk'='dashed','Area-weighted crosswalk'='dotdash')) +
  scale_x_continuous(breaks=seq(0,1,.25)) + scale_y_continuous(limits=c(0,NA)) +
  labs(x='Allocation weight (stocks)',y='Density') + base_theme +
  guides(colour=guide_legend(nrow=1,byrow=TRUE),linetype=guide_legend(nrow=1,byrow=TRUE))
ggsave('../output/figures/FigureS3.png',p,width=7,height=3.8,dpi=300,bg='white')

