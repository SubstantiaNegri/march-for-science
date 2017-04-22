#  Load Essential Libraries----
# Load the plyr package to enable use of the 'rename' function
library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
# Load the gglot2 package to enable ggplot graphing functions
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
# Load the data.table package to enable extended dataframe functions
library("data.table", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library(scales)
library(ggrepel)

#  Values
US <- "United States"
PR <- "Puerto Rico"
VI <- "Virgin Islands"
DC <- "District of Columbia"
GU <- "Guam"

#  State Abbreviations----
state.abbr <- read.csv("state.abbrv.csv")

#  STEM Employment----
STEM.Occ.Codes <- data.table(
  read.csv("STEM_OccCodes.csv")
  )

Occ.stats <- data.table(
  read.csv("BLS_OES_state_May_2016.csv")
)
Occ.stats[,TOT_EMP:=as.numeric(TOT_EMP)]

STEM.Occ.stats <- Occ.stats[OCC_CODE %in% STEM.Occ.Codes$OCC_CODE,
  .(ST, STATE, OCC_CODE, OCC_TITLE, OCC_GROUP, TOT_EMP, EMP_PRSE, JOBS_1000, LOC_Q)
]

state.stats.employ <- STEM.Occ.stats[!STATE %in% c(DC,GU,PR,VI)][
  ,.(STEM.emp.per.1000=sum(JOBS_1000, na.rm = TRUE)),by=STATE]


#  GDP----
State.GDP.percapita <- data.table(
  read.csv("BEA_realGDP_per-capita_states.csv")
)
State.GDP.percapita[,`:=`(GeoFIPS=NULL,Region=NULL,ComponentName=NULL)]

State.GDP.percapita <- melt.data.table(
  State.GDP.percapita,id.vars = "GeoName", 
  variable.name = "year", value.name = "per.capita.GDP")

# convert year to numeric
State.GDP.percapita[,year:=as.numeric(substring(as.character(year),2))]
setnames(State.GDP.percapita,"GeoName","STATE")

state.stats.GDP <- State.GDP.percapita[
  !STATE %in% c(US, DC, "New England", "Mideast", "Great Lakes",
                "Plains", "Southeast", "Southwest", "Rocky Mountain", "Far West")][
                  year==2015, .(STATE,per.capita.GDP)]

#  election 2016----
election.2016 <- data.table(
  read.csv("election_prez_2016.csv")
)

#  state level statistics----
state.stats <- merge(state.stats.employ,state.stats.GDP,by="STATE")
state.stats <- merge(state.stats,election.2016,by="STATE")
state.stats <- merge(state.stats,state.abbr,by="STATE")

#  GDP ~ STEM model----
STEM.GDP.model <- lm(per.capita.GDP ~ STEM.emp.per.1000, state.stats)

plot(state.stats$STEM.emp.per.1000,state.stats$per.capita.GDP);
abline(a=STEM.GDP.model$coefficients[1],b=STEM.GDP.model$coefficients[2])

#  GDP ~ STEM plot----
state.stat.plot <- ggplot(state.stats,
                          aes(x=STEM.emp.per.1000,y=per.capita.GDP,
                              color=Election_Pres_2016,
                              fill=Election_Pres_2016,
                              group=STATE))

pdf(paste(Sys.Date(),"state.stat.plot.pdf",sep = "_"),useDingbats = FALSE)
print(
state.stat.plot+
  geom_point(aes(size=15))+
  geom_text_repel(aes(label = state.abb),
                  point.padding = unit(0.05, "lines"))+
  geom_abline(slope = STEM.GDP.model$coefficients[2],
              intercept = STEM.GDP.model$coefficients[1],
              size=1)+
  scale_color_manual(values = c("#1f78b4","#e41a1c"))+
  #scale_fill_manual(values = alpha(c("#1f78b4","#e41a1c"), 0.6))+
  theme_bw()+
  theme(legend.position = "none")
)
dev.off()
#ggsave(paste(Sys.Date(),"state.stat.plot.pdf",sep = "_"), width = 13.5,
#       height = 13.5, units = "in", device = "pdf")
