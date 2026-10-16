# Supplementary Figure 4: diversity over time.
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

project <- "."
# Same join, sample, summary statistics, and loess smoothing as original script.
datacrop <- fread(file.path(project,'../data/intermediate/panels/datacrop.csv.gz'))[year>1880]
alt <- fread(file.path(project,'../data/intermediate/panels/datacrop_altxwalk.csv.gz'))[,.(base_fips,year,ginisimpson_alt=ginisimpson)]
datacrop <- as.data.table(left_join(datacrop,alt,by=c('base_fips','year')))
s <- datacrop[,.(mean=mean(ginisimpson,na.rm=T),mean2=mean(ginisimpson_alt,na.rm=T),
 quantile9=quantile(ginisimpson,.9,na.rm=T),quantile92=quantile(ginisimpson_alt,.9,na.rm=T),
 quantile1=quantile(ginisimpson,.1,na.rm=T),quantile12=quantile(ginisimpson_alt,.1,na.rm=T)),by=year][year<2010]
g <- ggplot(datacrop,aes(year,ginisimpson)) + geom_jitter(colour='grey',alpha=.05,size=.5)
for (v in c('mean','quantile9','quantile1','mean2','quantile92','quantile12')) {
 method <- if(v %in% c('mean','quantile9','quantile1')) 'Machine-learning crosswalk' else 'Area-weighted crosswalk'
 ds <- data.frame(year=s$year,value=s[[v]],method=method)
 g <- g + geom_smooth(data=ds,aes(x=year,y=value,colour=method),inherit.aes=FALSE,
   method='loess',formula=y~x,se=FALSE,linewidth=.55,linetype=if(grepl('mean',v)) 'solid' else 'dashed')
}
g <- g + scale_colour_manual(values=c('Machine-learning crosswalk'='#2171b5','Area-weighted crosswalk'='#6baed6')) +
 labs(x='Year',y='Gini-Simpson index') + base_theme +
 guides(colour=guide_legend(nrow=1,override.aes=list(linetype='solid')))
ggsave('../output/figures/FigureS4.png',g,width=7,height=4.0,dpi=300,bg='white')

