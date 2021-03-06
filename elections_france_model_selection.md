Elections France 2017 Models analysis
================

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
library(stringr)
library(ggplot2)
library(ggcorrplot)
library(ggthemes)
```

### Load file

``` r
df<- read.csv('data/DOI/elections_2017_First_round.csv')
```

### Add new features :

``` r
df<-df %>% mutate(Combined_ratio=log(NoJob_1524_ratio+1)*log(education_no_diplom_ratio+1)*log(family_monoparental_ratio+1))
```

### Set features

``` r
candidats<-c("ARTHAUD","ASSELINEAU","CHEMINADE","DUPONT.AIGNAN","FILLON","HAMON","LASSALLE","LE.PEN","MACRON","MÉLENCHON","POUTOU")
job_indicators<- grep('Job|ratio',colnames(df),value = TRUE)
fiscal<-c('Q213','Q3_Q1','Population')

all_features<-c(job_indicators,fiscal)
```

### Filter on numerical values

``` r
# %>% mutate_if(is.factor, funs(as.numeric(levels(.))[.])) 
```

### Correlations

``` r
df_numeric<- df %>% select(one_of(c(all_features,'LE.PEN')))
cor.matrix<-cor(df_numeric %>% filter(!is.na(LE.PEN))%>% filter(!is.na(Q3_Q1)))
df_corr<- as.data.frame(cor.matrix) %>% select(LE.PEN) 
df_corr$feature<-rownames(df_corr)

df_corr<-df_corr %>% mutate(corr_abs=abs(LE.PEN)) %>% top_n(10,corr_abs) %>% arrange(desc(corr_abs))

highest.corr.features<- as.vector(df_corr$feature)

corr <- round(cor(df_numeric %>% select(one_of(highest.corr.features)) %>% na.omit()),2)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 2, 
           method="circle", 
           colors = c("cyan4", "white", "red3"), 
           title="Correlogram for LE PEN",
           show.legend=FALSE,
           show.diag = FALSE,
           ggtheme=theme_bw)
```

![](elections_france_model_selection_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
df_numeric<- df %>% select(one_of(c(all_features,'MACRON')))
cor.matrix<-cor(df_numeric %>% filter(!is.na(MACRON))%>% filter(!is.na(Q3_Q1)))
df_corr<- as.data.frame(cor.matrix) %>% select(MACRON) 
df_corr$feature<-rownames(df_corr)

df_corr<-df_corr %>% mutate(corr_abs=abs(MACRON)) %>% top_n(10,corr_abs) %>% arrange(desc(corr_abs))

highest.corr.features<- as.vector(df_corr$feature)

corr <- round(cor(df_numeric %>% select(one_of(highest.corr.features)) %>% na.omit()),2)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 2, 
           method="circle", 
           colors = c("cyan4", "white", "red3"), 
           title="Correlogram for MACRON",
           show.legend=FALSE,
           show.diag = FALSE,
           ggtheme=theme_bw)
```

![](elections_france_model_selection_files/figure-markdown_github/unnamed-chunk-7-1.png) \#\#\# Multiplot function

``` r
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(grid)


grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {

  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)

  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)

}
```

### Generic function to plot

``` r
plot_corr<-function(df,feature,ratio,max,pop_threshold,candidate,xlab){
  df<-  df[df[,feature]>0,] 
 df<-  df[df[,feature]<max,]  %>%filter(Population>pop_threshold) %>% filter(REG %in% c(53,93,22,31))
  
  x_data<-df[,feature] * ratio
  
  
  p<-ggplot(df,aes_string(x=x_data,y=candidate,col="NOM_REG",size="Population"))+ geom_jitter(alpha=.7)  +theme_fivethirtyeight() +
    theme(axis.title = element_text())+ylab(paste0('1st Round - ',candidate,' (%)'))+ xlab(xlab)+theme(legend.direction='vertical',legend.position='top',legend.box='horizontal')+ggtitle(paste0('Cities with population>',pop_threshold))+labs(caption = "©Clément Lefèvre 2017 (Sources : Ministère de l'Intérieur, INSEE)")+scale_colour_manual(values = c("#e41a1c", "#377eb8", "#4daf4a"))+labs(size = "Population",color=' ')+ theme(legend.title=element_text(size=8))+ylim(0,0.5)
  return(p)
}

p0<-plot_corr(df,"Job_OUVRIER_ratio",100,100,1000,'LE.PEN',"Workers in active population(%)")
p1<-plot_corr(df,"Job_OUVRIER_ratio",100,100,1000,'MACRON',"Workers in active population(%)")

p2<-plot_corr(df,"education_no_diplom_ratio",100,100,1000,'LE.PEN',"% of adult without degree")
p3<-plot_corr(df,"education_no_diplom_ratio",100,100,1000,'MACRON',"% of adult without degree")

p4<-plot_corr(df,"family_monoparental_ratio",100,100,1000,'LE.PEN',"% monoparental families")
p5<-plot_corr(df,"family_monoparental_ratio",100,100,1000,'MACRON',"% monoparental families")

p6<-plot_corr(df,"NoJob_1524_ratio",100,100,1000,'LE.PEN',"% Unemployment <24 years old")
p7<-plot_corr(df,"NoJob_1524_ratio",100,100,1000,'MACRON',"% Unemployment <24 years old")

p8<-plot_corr(df %>% filter(Q213>0),"Q213",100,max(df$Q213),1000,'LE.PEN',"Median yearly Income per household (Euros)")
p9<-plot_corr(df,"Q213",100,max(df$Q213),1000,'MACRON',"Median yearly Income per household (Euros)")

p10<-plot_corr(df %>% filter(Q3_Q1>0),"Q3_Q1",100,max(df$Q3_Q1),1000,'LE.PEN',"Yearly household incomes IQR (Euros)")
p11<-plot_corr(df %>% filter(Q3_Q1>0),"Q3_Q1",100,max(df$Q3_Q1),1000,'MACRON',"Yearly household incomes IQR (Euros)")
p0
```

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](elections_france_model_selection_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
p1
```

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](elections_france_model_selection_files/figure-markdown_github/unnamed-chunk-9-2.png)

``` r
p2
```

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](elections_france_model_selection_files/figure-markdown_github/unnamed-chunk-9-3.png)

``` r
p3
```

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](elections_france_model_selection_files/figure-markdown_github/unnamed-chunk-9-4.png)

``` r
p4
```

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](elections_france_model_selection_files/figure-markdown_github/unnamed-chunk-9-5.png)

``` r
p5
```

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](elections_france_model_selection_files/figure-markdown_github/unnamed-chunk-9-6.png)

``` r
p6
```

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](elections_france_model_selection_files/figure-markdown_github/unnamed-chunk-9-7.png)

``` r
p7
```

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](elections_france_model_selection_files/figure-markdown_github/unnamed-chunk-9-8.png)

``` r
p8
```

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](elections_france_model_selection_files/figure-markdown_github/unnamed-chunk-9-9.png)

``` r
p9
```

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](elections_france_model_selection_files/figure-markdown_github/unnamed-chunk-9-10.png)

``` r
p10
```

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](elections_france_model_selection_files/figure-markdown_github/unnamed-chunk-9-11.png)

``` r
p11
```

    ## Warning: Removed 4 rows containing missing values (geom_point).

![](elections_france_model_selection_files/figure-markdown_github/unnamed-chunk-9-12.png)

``` r
# grid_arrange_shared_legend(p2,p3,nrow =1, ncol = 2)
# grid_arrange_shared_legend(p4,p5,nrow =1, ncol = 2)
# grid_arrange_shared_legend(p6,p7,nrow =1, ncol = 2)
# grid_arrange_shared_legend(p8,p9,nrow =1, ncol = 2)
# grid_arrange_shared_legend(p10,p11,nrow =1, ncol = 2)
```

``` r
df$Population_cat <- factor(df$Population_cat, levels = c("<100","[100,300]","[300,500]","[500,1000]" ,"[1000,2000]","[2000,5000]",">10000" ))


groupy_pop<- df %>% group_by(Population_cat) %>% summarise(LE.PEN=mean(LE.PEN*100,na.rm=TRUE),MACRON=mean(MACRON*100,na.rm=TRUE),FILLON=mean(FILLON*100,na.rm=TRUE),MELENCHON=mean(MÉLENCHON*100,na.rm=TRUE))
groupy_pop<- groupy_pop %>% gather(candidate,score,-Population_cat)
groupy_pop$candidate<-  factor(groupy_pop$candidate, levels = c("MACRON","LE.PEN","FILLON","MELENCHON"))

ggplot(groupy_pop,aes(x=Population_cat,y=score,fill=candidate)) + geom_bar(stat='identity', position = "dodge")+ylab('1st round (%)')+scale_fill_manual(values = c("#ecad31", "#767374", "#1d88be",'#ec6a56'))+ggtitle('Per city population')+labs(caption = "©Clément Lefèvre 2017 (Sources : Ministère de l'Intérieur, INSEE)") + theme_fivethirtyeight()+theme(axis.title = element_text())+xlab("")+ theme(legend.title=element_blank())
```

![](elections_france_model_selection_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
p1<-ggplot(df%>% filter(!is.na(LE.PEN))%>% filter(Q3_Q1.Data.Available) %>% filter(REG %in% c(53,93,22,31)) ,aes(Q3_Q1,LE.PEN, col=as.factor(NOM_REG),size=Population))+ geom_jitter(alpha=.6)+ scale_colour_brewer(palette = "Set1")+theme(legend.title=element_blank())+ylab('1st Round - LE PEN (%)')+ xlab('Yearly Income per household - Inter Quartile Range')+theme(aspect.ratio=1/2)+ggtitle('For cities with Household>50 or Population>100')

p2<-ggplot(df%>% filter(!is.na(LE.PEN))%>% filter(Q3_Q1.Data.Available) %>% filter(REG %in% c(53,93,22,31)) ,aes(Q3_Q1,MACRON, col=as.factor(NOM_REG),size=Population))+ geom_jitter(alpha=.6)+ scale_colour_brewer(palette = "Set1")+theme(legend.title=element_blank())+ylab('1st Round - MACRON (%)')+ xlab('Yearly Income per household - Inter Quartile Range')+theme(aspect.ratio=1/2)

grid_arrange_shared_legend(p1, p2,nrow = 2, ncol = 1)
```

![](elections_france_model_selection_files/figure-markdown_github/unnamed-chunk-11-1.png)
