library ( tidyverse ) 
library(ggExtra)
install.packages(‘ggplot2movies’)
library(ggcorrplot)
library(pastecs)
library(psych) #psych::describe
library(gmodels)
library(Hmisc)
library(car)
library(ggmosaic)
library(ISLR)
library ( lubridate )
library(ggplot2)
library(lsr)
install.packages("lsr")
# df1 <- readRDS ( "GSS_SM" ) 


## hisztogram
# h1 <- ggplot(data= dat_2007) +
#   geom_histogram(aes(gdpPercap, y = ..density.., fill = continent))+
#   geom_vline( xintercept = weighted.mean(dat_2007$gdpPercap, dat_2007$pop)) +
#   stat_function(fun = dnorm, args = c(mean=mean(dat_2007$gdpPercap),
#                                       sd = sd(dat_2007$gdpPercap)),
#                 color = "red")

# ggplot ( data = x, aes ( x = x ) ) +
#   geom_histogram( aes ( y = ..density.. , color = "empirikus_eloszlás" ) , 
#                   fill = "white" ,
#                   binwidth = 0.1 ) +
#   stat_function ( fun = dt , aes ( color = "sűrűségfüggvény" ), 
#                   args = list ( df ) ) +
#   stat_function ( fun = pt , aes ( color = "eloszlásfüggvény" ), 
#                   args = list ( df ) ) +
#   geom_vline ( xintercept = qt ( prob , df ) , 
#                color = "red" ) +
#   geom_vline ( xintercept = t , 
#                color = "black" ,
#                linetype = "dashed" ) +
#   scale_x_continuous ( breaks = seq ( -3 , +3 , 1 ) ,
#                        limits = c ( -4 , +4 ) ) +
#   scale_colour_manual ( name = "Függvény" ,
#                         values = c ( empirikus_eloszlás = "black" , 
#                                      sűrűségfüggvény = "blue" , 
#                                      eloszlásfüggvény = "green" ) ) +
#   labs ( y = NULL ,
#          title = "Student eloszlás" ) +
#   theme_minimal( ) 



# ggplot( data = mpg , mapping = aes( x = displ, y = hwy, color = class ) ) +
# geom_point( ) +
#  geom_smooth(   color = "blue"  ) +
# facet_grid( drv~. )

# ggplot ( ) +
#   geom_boxplot( aes ( x = reorder ( continent, lifeExp ) , # sorbarendezve
#                       y = lifeExp ),
#                 varwidth = T )

# dat_2007    <- filter(gapminder, gapminder$year == 2007)
# dat_arranged <- arrange (dat_2007, desc(dat_2007$gdpPercap))
# view(dat_arranged)
# dat_popmill <- mutate ( dat_arranged, pop_mill = pop / 1000000)
# head(dat_popmill, n = 10)

# gapminder_yearcontinent <-  gapminder %>%
#   group_by ( year , continent ) %>%
#   summarise ( nepesseg = sum ( pop ) ) %>%
#   mutate ( nepesseg = nepesseg / 1000000 ) %>% 
#   ggplot( mapping = aes ( x = year , y = nepesseg , color = continent) ) +
#   geom_line (  ) +
#   geom_point(  )



a = read.csv("C:/Users/Pelis Peti/Desktop/adatelemzes/ZH2_adat")
library(data.table)
a = data.table(a)

unique(a$Year)
a[, ':='( Year = factor(Year, ordered = TRUE, levels = c("2014", "2015")),
          onallo = factor(onallo),
          likes_dislikes = Likes+Dislikes)]

a[, ':='( Year = factor(Year, ordered = TRUE, levels = c("2014", "2015")),
          onallo = factor(onallo),
          likes_dislikes = Likes+Dislikes,
          rel_tetszett = Likes/likes_dislikes)]
is.factor(a$Year)
 min(a$Year)
 is.character(a$Movie)
class(a$rel_tetszett) # numeric

nrow(a[rel_tetszett<0.5]) # 1 db olyan film van

a[Year == 2014][Likes == max(Likes)][, Movie] #The_Fault_in_Our_Stars

summary(a$likes_dislikes)

describe(a$likes_dislikes)
psych::describe(a$rel_tetszett)
 #ertelmezes

r = a[sorozat == 1][order(-Likes)][1:10, rel_tetszett]
m = a[sorozat == 1][order(-Likes)][1:10, Movie]
rm = cbind(r, m)
legvitatottabb = rm[r == min(r)]
legvitatottabb


filmatlag = mean(a[onallo==1, rel_tetszett])
sorozatatlag = mean(a[onallo==0, rel_tetszett])
filmatlag
sorozatatlag

table(a$Year)

ggplot(a, aes(Dislikes))+
  geom_boxplot() +
  facet_wrap(~Year, nrow = 2)

ggplot(a, aes(Gross, Likes, color = onallo))+
  geom_point() +
  geom_smooth()

ggplot(a[Likes<100000], aes(Gross, Likes, color = onallo))+
  geom_point() +
  geom_smooth()

ggplot(a, aes(Gross, Likes, color = onallo))+
  geom_point() +
  geom_smooth() +
  scale_y_continuous(0:100000)


a1 = a[, sorozat := as.factor(ifelse(onallo==1,0,1))]


dev.off()

ggplot(a[, sorozat := as.factor(ifelse(onallo==1,0,1))],aes(Screens, Ratings, color = sorozat)) +
         geom_point() +
         geom_smooth(method='lm', se = F) +
  facet_wrap(~Year)+
  theme_classic()

#elofeltetelek H0: normális eloszlású
library ( pastecs ) 
shapiro.test ( a[onallo==0, Ratings] )
shapiro.test ( a[onallo==1, Ratings] )
#nem tudjuk elutasitani h0-t h1 javara, tehat a normalis eloszlas feltetelezheto
hist(a[onallo==0, Ratings])
hist(a[onallo==1, Ratings])

library ( car )  #H0: szórások egyenlőek -> nem tudjuk elutasitani mert 0.61es a p ertekunk, tehat a szorasok egyeloek
leveneTest( Ratings ~ onallo , data = a , 
            center = "mean" )

# H0: mu ( sorozat ) - mu ( onallo ) <= 0
# H1: mu ( sorozat ) - mu ( onallo ) < 0
t.test(a[onallo==0, Ratings], a[onallo==1, Ratings], alternative = "greater")  
# p-value = 0.367, tehat nem tudjuk elutasitani a null hipotezist a h1 javara,

t  <- 0.34111
df <- 69.139
# hatásméret
r <- sqrt ( t^2/ ( t^2 + df ) ) 
round ( r , 3 ) 

## különbség ábrázolása megfelelő ábrával
# # Mit mond az ábra így?
ggplot ( a , aes ( x = Ratings , y = onallo )  ) + 
  stat_summary ( fun.data = mean_cl_normal ,
                 geom = "errorbar" , width = 0.1 ) +
  theme_light (  ) +
  theme ( panel.grid.major.x = element_blank (  )  ) 





#elofeltetelek H0: normális eloszlású
library ( pastecs ) 
shapiro.test ( a$Genre )
shapiro.test ( a$onallo )
#nem tudjuk elutasitani h0-t h1 javara, tehat a normalis eloszlas feltetelezheto
hist(a$Genre])
hist(a$onallo])

library ( car )  #H0: szórások egyenlőek -> nem tudjuk elutasitani mert 0.61es a p ertekunk, tehat a szorasok egyeloek
leveneTest( Genre ~ onallo , data = a , 
            center = "mean" )

table ( a$Genre ,a$onallo )
# H0: független / nincs kapcsolat



gmodels :: CrossTable (a$Genre , a$onallo , 
                        chisq = T, sresid = T )
# p<0.05 szignifikáns kapcsolat van a Genre es onallo végzettsége között

t  <- 20.55921 
df <- 9 

# hatásméret
r <- sqrt ( t^2/ ( t^2 + df ) ) 
round ( r , 3 ) 

## különbség ábrázolása megfelelő ábrával
# # Mit mond az ábra így?
ggplot ( a , aes ( x = Genre , y = onallo )  ) + 
  stat_summary ( fun.data = mean_cl_normal ,
                 geom = "errorbar" , width = 0.1 ) +
  theme_light (  ) +
  theme ( panel.grid.major.x = element_blank (  )  ) 

# fill bar
ggplot ( na.omit ( a ) ) +
  geom_bar ( aes ( x = Genre , 
                   fill = factor ( onallo ) ) ,
             position = 'fill' )

mosaicplot( Genre ~ onallo , data=a )
