##---
# R code for Data Vislization with R

## -----
library(ggplot2)
library(GGally)
theme_set(theme_bw())
library(magrittr)
library(tidyverse)
library(scales)
library(ggiraph)
library(ggiraphExtra)
library(plotly)
## ----results='asis'-------------------------------------------------------------------------------
dat<- mpg%>%dplyr::count(drv)%>% mutate(prop=n/sum(n)*100)
dat

mpg %>% count(drv) %>% mutate(prop=n/sum(n)*100, prop=paste0(round(prop,2),'%'))



g<-ggplot(dat,aes(x = drv, tooltip = paste0(round(prop,2),'%'),
                  data_id = drv, y=prop/100))+
  geom_bar_interactive(stat='identity',fill = "cornflowerblue", color="black")+
  scale_y_continuous(labels = scales::percent)+
  labs(y='Percent',title='Percent of cars in each level of drv')
girafe(print(g))
## ----results='asis'-------------------------------------------------------------------------------
dat2<-mpg%>%dplyr::count(class)
g<-ggplot(dat2,aes(x = class, tooltip = n,
        data_id =class, y=n))+
  geom_bar_interactive(stat='identity',fill = "cornflowerblue", color="black")+
  labs(y='Count',title='Count of cars in each class')
girafe(print(g))

## -------------------------------------------------------------------------------------------------
# create a pie chart with slice labels
g<-ggPie(mpg,aes(pies=drv))
girafe(print(g))


## -------------------------------------------------------------------------------------------------
# create a pie chart with slice labels
g<-ggPie(mpg,aes(pies=class))
girafe(print(g))


## -------------------------------------------------------------------------------------------------
# create a pie chart with slice labels
g<-ggDonut(mpg,aes(donuts=drv))
girafe(print(g))

## ----out.width = '80%'----------------------------------------------------------------------------
library(treemapify)
# create a treemap of marriage officials
plotdata <- mpg %>%  dplyr::count(class)
ggplot(plotdata, 
       aes(fill = class, 
           area = n, 
           label = class))+
  geom_treemap() + 
  geom_treemap_text(colour = "white", 
                    place = "centre") +
  labs(title = "Cars by class") +
  theme(legend.position = "none")


## ----results='asis'-------------------------------------------------------------------------------
# plot the histogram with 20 bins
g<-ggplot(mpg, aes(x = hwy)) +
  geom_histogram_interactive( aes(tooltip = ..count..,
                             data_id = hwy),fill = "cornflowerblue", 
                 color = "white", 
                 bins = 20) + 
  labs(title="Cars by Highway MPG", 
       subtitle = "number of bins = 20",
       x = "Highway MPG")+scale_x_continuous(breaks=seq(10,50,by=2))
girafe(print(g))

## -------------------------------------------------------------------------------------------------
g<-ggplot(mpg, aes(x = hwy)) +
  geom_density_interactive(tooltip="Kernal Density", data_id="Kernal Density",color='red')+
  geom_histogram_interactive(aes(y= ..density..,tooltip = round(..density..,2),
                             data_id = hwy),
                 fill = "cornflowerblue",color = "white",bins = 20,alpha=0.5)+xlab("Highway MPG")
girafe(print(g))


## -------------------------------------------------------------------------------------------------
Y=mpg$hwy
max(Y)-min(Y)
BY=4  # Decide BY
breaks=seq(min(Y)-2*BY,max(Y),by=BY)
Cut=cut(mpg$hwy,breaks,right=TRUE,include.lowest=FALSE)
Cut
freq = table(Cut)
y=as.vector(freq)
dat=data.frame(breaks=breaks[-1],interval=names(freq),Freq=y/sum(y),cum_freq=cumsum(y)/sum(y))
g<-ggplot(dat,aes(breaks,cum_freq)) +
  geom_point() + geom_line_interactive(tooltip="Cumulative Freq.", data_id="Cumulative Freq.",color='red') + 
  labs(title = "Cumulative Freq. of age",
       y = "Cumulative Freq.",
       x = "Highway MPG")+
  geom_hline(yintercept =c(0.25,0.5,0.75),col='gray30',linetype=2)
girafe(print(g))


## ----results='asis'-------------------------------------------------------------------------------
Titandat<-as.data.frame(Titanic)
dat<-Titandat%>%group_by(Class,Survived)%>%
  summarise(Freq=sum(Freq))
dat2<- dat%>%  pivot_wider(id_cols = Class,names_from = 2,values_from = 3)
dat2


## -------------------------------------------------------------------------------------------------
dat=transform(dat,tooltip=paste0('Class=',Class,', ','Survived=',Survived,', ','n=',Freq))
dat
ggBar(dat,aes(x=Class,fill=Survived,y=Freq),stat="identity",addlabel=TRUE,horizontal=FALSE,width=0.5,interactive = TRUE)


## -------------------------------------------------------------------------------------------------
ggBar(dat,aes(x=Class,fill=Survived,y=Freq),stat="identity",position='dodge',addlabel=TRUE,horizontal=FALSE,width=0.5,interactive = TRUE)


## -------------------------------------------------------------------------------------------------
ggBar(dat,aes(x=Class,fill=Survived,y=Freq),stat="identity",position='fill',addlabel=TRUE,horizontal=FALSE,width=0.5,interactive = TRUE)

## -----
library(moonBook)
ggPieDonut(acs,aes(pies=Dx,donuts=smoking),interactive=TRUE)

## -------------------------------------------------------------------------------------------------
library(vcd)
mosaic(~Class+Survived,Titanic,direction='v',shade=T,legend=FALSE)
## ----out.width = '90%',fig.width=8----------------------------------------------------------------
mosaic(~Class+Survived,Titanic,direction='v',shade=T,legend=FALSE,type='expected')


## -------------------------------------------------------------------------------------------------
data(mtcars)
mtcars

## -------------------------------------------------------------------------------------------------
mtcars$Car=rownames(mtcars)
g <- ggplot(mtcars, aes(wt, mpg))+geom_point_interactive(aes(tooltip = Car, data_id=Car ))
ggPoints(mtcars, aes(wt, mpg),smooth = FALSE,interactive = TRUE)

## -------------------------------------------------------------------------------------------------

data(gapminder, package="gapminder")
plotdata <- filter(gapminder, 
                   country == "United States")
# simple line plot
g<-ggplot(plotdata, 
       aes(x = year, 
           y = lifeExp)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_point(size = 3, 
             color = "steelblue") +
  labs(y = "Life Expectancy (years)", 
       x = "Year",
       title = "Life expectancy changes over time",
       subtitle = "United States (1952-2007)",
       caption = "Source: http://www.gapminder.org/data/")
g


## -------------------------------------------------------------------------------------------------
#plot mean salaries
load('Salaries.Rdata')
g<-ggplot(data=plotdata,
        aes(x = Rank,y = Mean_Salary)) +
   geom_bar_interactive(aes(tooltip = scales::dollar(Mean_Salary),
        data_id = rank),stat = "identity",
            fill = "cornflowerblue") +
   #geom_text(aes(label = scales::dollar(Mean_Salary)),vjust = -0.25) +
   scale_y_continuous(breaks = seq(0, 130000, 20000),
                      label = dollar) +
   labs(title = "Mean Salary by Rank",
        subtitle = "9-month academic salary for 2008-2009",
        x = "",
        y = "")
g

## --------------------------------------------------------------
data(gapminder, package="gapminder")
# subset Asian countries in 2007
plotdata <- gapminder %>%
  filter(continent == "Americas" & 
         year == 2007) %>% mutate(country=reorder(country, lifeExp))%>%
  droplevels()%>%mutate(tooltip=paste0(country,', ',round(lifeExp,1)))
g2<-ggplot(plotdata, 
       aes(x=lifeExp, 
           y=country)) +
  geom_point(color="blue", 
             size = 2) +
  geom_segment(aes(x = 40, 
               xend = lifeExp, 
               y = country, 
               yend = country),
               color = "lightgrey") +
  labs (x = "Life Expectancy (years)",
        y = "",
        title = "Life Expectancy by Country",
        subtitle = "GapMinder data for Americas - 2007") 
g2


## ----out.width = '80%',cache=TRUE-----------------------------------------------------------------
load('Salaries.Rdata')
p<-ggplot(Salaries, 
       aes( x = rank,y=salary,color=rank))+
  labs(title = "Academic Salary by Rank",
       subtitle = " 9-month salary for 2008-2009",
       x = "",
       y = "")+
  theme(legend.position = "none")+  
  scale_y_continuous(label = dollar)

g<-p+geom_boxplot(aes(fill=rank),notch = TRUE,alpha = .7)
g


## ----out.width = '80%',cache=TRUE-----------------------------------------------------------------
g<-p+geom_violin(color='Purple')
g

## ----out.width = '80%',cache=TRUE-----------------------------------------------------------------
g<-ggplot(Salaries,aes(x =salary, color = rank,fill=rank,data_id=rank,tooltip=rank))+
  geom_density_interactive(alpha = 0.4) +
  theme(legend.position = "bottom",axis.title.y = element_text(angle=0))
g

## ----out.width = '80%',cache=TRUE-----------------------------------------------------------------
library(ggridges)
g<-ggplot(Salaries,aes(x =salary,y =rank, color = rank,fill=rank))+
  geom_density_ridges2(alpha = 0.4) +
  theme_ridges() +
  theme(legend.position = "none",axis.title.y = element_text(angle=0))
g
## ----out.width = '80%',cache=TRUE-----------------------------------------------------------------
# plot the distribution of salaries 
# by rank using strip plots
g<-p+ geom_point()
g


## ----out.width = '80%',cache=TRUE-----------------------------------------------------------------
g<-p+geom_jitter(alpha = 0.7,
              size = 1.5) 
g


## ----out.width = '90%',cache=TRUE-----------------------------------------------------------------
g<-p+geom_boxplot(size=1,
               outlier.shape = 1,
               outlier.color = "black",
               outlier.size  = 2) +
  geom_jitter(alpha = 0.7,width=0.2) 
g


## ----out.width = '90%',cache=TRUE-----------------------------------------------------------------
library(ggpol)
g<-p+geom_boxjitter(errorbar.draw = FALSE)
g


## ----out.width = '80%',cache=TRUE-----------------------------------------------------------------
library(ggbeeswarm)
g<-p+geom_quasirandom(alpha = 0.7,
                   size = 1.5)
g


## ----out.width = '80%',cache=TRUE-----------------------------------------------------------------
load('Salaries.Rdata')
g<-ggplot(Salaries, aes(x = yrs.since.phd, 
                     y = salary, 
                     color=rank)) +
  geom_point() + scale_y_continuous(label = dollar)+
  labs(title = "Academic salary by rank and years since degree")
g


## ----out.width = '80%',cache=TRUE-----------------------------------------------------------------
g<-ggplot(Salaries, aes(x = yrs.since.phd, 
                     y = salary, 
                     color=rank,shape= sex)) +
  geom_point(size = 3,alpha = .5)+
  scale_y_continuous(label = dollar)+
  labs(title = "Academic salary by rank and years since degree")
g


## ----out.width = '80%',cache=TRUE-----------------------------------------------------------------
g<-ggplot(mtcars, 
       aes(x = wt, y = mpg, size = hp)) +
  geom_point(alpha = .5, 
             fill="cornflowerblue", 
             color="black", 
             shape=21) +
  scale_size_continuous(range = c(1, 14)) +
  labs(title = "Auto mileage by weight and horsepower",
       subtitle = "Motor Trend US Magazine (1973-74 models)",
       x = "Weight (1000 lbs)",
       y = "Miles/(US) gallon",
       size = "Gross horsepower") 
g


## ----out.width = '90%',cache=TRUE-----------------------------------------------------------------
g<-ggplot(Salaries, aes(x = salary/1000)) +
  geom_histogram(fill = "cornflowerblue",
                 color = "white") +
  facet_wrap(~rank, ncol = 1) +
  labs(title = "Salary histograms by rank",x = "Salary ($1000)")
g


## ----out.width = '90%',cache=TRUE-----------------------------------------------------------------
g<-ggplot(Salaries, aes(x = salary / 1000)) +
  geom_histogram(color = "white",
                 fill = "cornflowerblue") +
  facet_grid(sex ~ rank) +
  labs(title = "Salary histograms by sex and rank",
       x = "Salary ($1000)")
g


## ----out.width = '90%',cache=TRUE-----------------------------------------------------------------
library(corrplot)
data(mtcars)
corr_matrix <- cor(mtcars)
corrplot(corr_matrix)


## ----out.width = '90%',cache=TRUE-----------------------------------------------------------------
# with numbers and lower
corrplot.mixed(corr_matrix)


## ----out.width = '90%',cache=TRUE-----------------------------------------------------------------
psych_variables <- attr(psychademic, "psychology")
academic_variables <- attr(psychademic, "academic")
g<-ggpairs(psychademic, psych_variables, title = "Within Psychological Variables",lower = list(continuous = wrap("points", alpha = 0.5, size=1,color='cornflowerblue')),upper=list(continuous = wrap("cor", size=10,color='cornflowerblue')))
g


## ----out.width = '90%',cache=TRUE-----------------------------------------------------------------
g<-ggpairs(psychademic, academic_variables, title = "Within Academic Variables",lower = list(continuous = wrap("points", alpha = 0.5, size=1,color='cornflowerblue')),upper=list(continuous = wrap("cor", size=10,color='cornflowerblue')))
g


## -------------------------------------------------------------------------------------------------
data(iris)

## ----out.width = '90%',cache=TRUE-----------------------------------------------------------------
g<-ggpairs(iris,title = "Iris",mapping=ggplot2::aes(colour =Species),lower = list(continuous = wrap("points", alpha = 0.5,size=1)),legend =c(3,5))
g


## -------------------------------------------------------------------------------------------------
dat<-iris%>%group_by(Species)%>%summarise_all(mean)
dat


## ----out.width='100%',fig.height=6,fig.width=12,cache=TRUE----------------------------------------
dat2<-dat%>%pivot_longer(cols = 2:5)
g<-ggplot(dat2,aes(Species,value,fill=Species))+geom_bar(stat='identity')+facet_wrap(~name,nrow = 2)
print(g)


## ----out.width='70%',fig.width=8,cache=TRUE-------------------------------------------------------
g<-ggRadar(data=iris,aes(color=Species))+theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),legend.position = 'right')
print(g)


## ----fig.width=6,fig.height=4,out.width='90%',cache=TRUE------------------------------------------
library(ggbiplot)
iris_pca <- prcomp(iris[,1:4], scale=FALSE)
ggbiplot(iris_pca,groups=iris$Species,ellipse = TRUE, circle = TRUE,obs.scale = 0.5, var.scale = 1)


## ----out.width='90%',cache=TRUE-------------------------------------------------------------------
library(scales)
g<-ggplot(economics, aes(x = date, y = psavert)) +
  geom_line(color = "indianred3", 
            size=0.5 ) +
  geom_smooth() +
  scale_x_date(date_breaks = '5 years', 
               labels = date_format("%b-%y")) +
  labs(title = "Personal Savings Rate",
       subtitle = "1967 to 2015",
       x = "",
       y = "Personal Savings Rate")
g


## ----out.width='90%',cache=TRUE-------------------------------------------------------------------
library(ggalt)
data(gapminder, package = "gapminder")
# subset data
plotdata_long <- filter(gapminder,
                        continent == "Americas" &
                        year %in% c(1952, 2007)) %>%
  select(country, year, lifeExp) 
# convert data to wide format
plotdata_wide <- plotdata_long%>% 
  tidyr::pivot_wider(id_cols = country,names_from = year,values_from = lifeExp,names_prefix = 'y')
# create dumbbell plot
g<-ggplot(plotdata_wide, 
       aes(y = reorder(country, y1952),
           x = y1952,
           xend = y2007)) +  
  geom_dumbbell(colour="#a3c4dc", size=1.5, colour_xend="#0e668b", 
                         dot_guide=TRUE, dot_guide_size=0.15) +
  labs(title = "Change in Life Expectancy",
       subtitle = "1952 to 2007",
       x = "Life Expectancy (years)",
       y = "")
g
## ----out.width='90%',cache=TRUE-------------------------------------------------------------------
library(CGPfunctions)
df <- gapminder %>%
  filter(year %in% c(1992, 1997, 2002, 2007) &
           country %in% c("Panama", "Costa Rica", 
                          "Nicaragua", "Honduras", 
                          "El Salvador", "Guatemala",
                          "Belize")) %>%
  mutate(year = factor(year),
         lifeExp = round(lifeExp)) 
# create slope graph
g<-newggslopegraph(df, year, lifeExp, country) +
  labs(title="Life Expectancy by Country", 
       subtitle="Central America", 
       caption="source: gapminder")
g


## -------------------------------------------------------------------------------------------------
library(maps)
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
states_map <- ggplot2::map_data("state")
g<-ggChoropleth(crimes,aes(fill=Murder,map_id=state),map=states_map,interactive=TRUE)
g
## ----results='asis'-------------------------------------------------------------------------------
mtcars%<>% mutate(cyl=factor(cyl))
fit1<-lm(mpg~wt,mtcars)
ggPredict(fit1,interactive = TRUE)
## ----results='asis'-------------------------------------------------------------------------------
fit2<-lm(mpg~wt+cyl,mtcars)
ggPredict(fit2,interactive = TRUE)
## ----results='asis'-------------------------------------------------------------------------------
horseshoecrabs<-read.table('data/horseshoecrabs.txt')
horseshoecrabs %<>% mutate(has_satellites=as.numeric(Satellites>0))
crabs.fit1 <- glm(has_satellites ~ Weight,family=binomial, data=horseshoecrabs)
#visreg(crabs.fit1,ylab="Log odds(has satellites)",gg = TRUE)
#visreg(crabs.fit1,ylab="P(has satellites)",gg = TRUE,scale = 'response')
ggPredict(crabs.fit1)+labs(y='P(has satellites)',interactive=TRUE)
## ----results='asis'-------------------------------------------------------------------------------
horseshoecrabs %<>% mutate(Dark=factor(Color==4))
crabs.fit2 <- glm(has_satellites ~ Weight+Dark,family=binomial, data=horseshoecrabs)
ggPredict(crabs.fit2)+labs(y='P(has satellites)',interactive=TRUE)



## ----out.width='90%',cache=TRUE-------------------------------------------------------------------
library(survival)
library(survminer)
data(lung)
sfit <- survfit(Surv(time, status) ~  1, data=lung)
ggsurvplot(sfit,
            title="Kaplan-Meier curve for lung cancer survival")


## ----out.width='90%',cache=TRUE-------------------------------------------------------------------
sfit <- survfit(Surv(time, status) ~  sex, data=lung)
ggsurvplot(sfit, 
           conf.int=TRUE, 
           pval=TRUE,
           legend.labs=c("Male", "Female"), 
           legend.title="Sex",  
           palette=c("cornflowerblue", "indianred3"), 
           title="Kaplan-Meier Curve for lung cancer survival",
           xlab = "Time (days)")


## -------------------------------------------------------------------------------------------------
p <- ggplot(gapminder::gapminder, 
            aes(gdpPercap, lifeExp, size = pop, color = continent, frame = year)) +
  geom_point() + scale_x_log10(label = dollar) + theme_bw()
plotly::ggplotly(p,height=500)


