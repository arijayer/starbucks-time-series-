attach(SBUX)
dt=SBUX
View(dt)
str(dt)
dim(dt)
colnames(dt)
nrow(dt) # notre daataset contient 1006 lignes 
#notre dataset contient la variables date open high low close adj.close et volume 
# on doit comprendre chaque variable 
#date:date
#Open :Repr??sente le prix d'ouverture de l'action pour une journ??e donn??e. 
#High :Repr??sente le prix le plus ??lev?? atteint par l'action au cours de la journ??e. 
#Low :Repr??sente le prix le plus bas atteint par l'action au cours de la journ??e. 
#cmose : Repr??sente le prix de cl??ture de l'action pour une journ??e donn??e. 
#Adj.Close :Repr??sente le "prix de cl??ture ajust??". C'est le prix de cl??ture corrig?? pour tenir compte des ??v??nements tels que les dividendes, les fractionnements d'actions (stock splits), ou les fusions.
#Volume :Indique le nombre total d'actions ??chang??es au cours de la journ??e.


summary(dt)
#changer le type de la variable date en type date
library(lubridate)
dt$Date <- as.Date(dt$Date)

#affichage de la matrice de correlation 
correlation_matrix <- cor(dt[ , -1], use = "complete.obs")
correlation_matrix
# Charger ggplot2 pour cr??er un heatmap
library(ggplot2)
 library(reshape2)  # Pour transformer la matrice en format long

# Transformer la matrice de corr??lation en format long
cor_long <- melt(correlation_matrix)

# Cr??er un heatmap pour la matrice de corr??lation
ggplot(data = cor_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  labs(title = "Matrice de Corr??lation", x = "Variable 1", y = "Variable 2") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
  theme_minimal()
#d'apr??es la matrice de correlation que toutes les variables ont une forte correlation entre eux a l'exception de la 
#variable volume qui est faiblement corrol?? avec le reste des variables  
#Le volume repr??sente le nombre total d'actions ??chang??es pendant une p??riode donn??e.
#Contrairement aux autres indicateurs financiers (prix d'ouverture, de cl??ture, etc.),
# qui sont des mesures de prix, le volume repr??sente l'activit?? commerciale. Il n'a pas
# n??cessairement une relation lin??aire avec les mouvements de prix, ce qui peut expliquer
# la faible corr??lation.
# on va choisir la varaible low pour faire l 'analyse de la serie temporelle et faire la pr??diction
#Le prix "low" d'une journ??e est g??n??ralement un point plus stable que le prix "high" ou 
#"close", qui peuvent ??tre plus volatiles. Cela peut rendre la s??rie temporelle plus pr??visible
#et plus facile ?? mod??liser.
#Dans le contexte des march??s financiers, le prix "low" peut repr??senter un point 
#d'achat id??al. Ainsi, mod??liser cette s??rie temporelle peut aider 
#les traders ou investisseurs ?? identifier des opportunit??s d'achat.
data <- SBUX
data$Date <- ymd(data$Date)
data <- data %>%
  select(Date,Low) %>%
  arrange(Date)

time_series <- ts(data$Low, frequency = 1)
plot(time_series, main = "Starbucks low Stock Prices", xlab = "Time", ylab = "low price ", col="green")




t = 1:length(time_series)
t
time_series.lin = lm(time_series ~ t)
summary(time_series.lin)

x = time_series.lin$fitted.values
x
#  l 'output nous donne l a fonction suivante y=94.646572+0.004098*t
#Cr??er la s??rie temporelle 
lin.fit<- ts(x, start = 1, frequency = 1)
lin.fit
##Tracer la courbe lin??aire 
plot(time_series)
lines(lin.fit, lwd = 2 , col = 'yellow')
abline(lin.fit[1],0, lwd = 2 , col = "blue" ) 


#une r??gression polynomiale
t1 = t
t2 = t^2
temp.quad = lm(time_series ~ t1 + t2)
# la fonction est 
summary(temp.quad)

#Cr??er la s??rie temporelle 
quad.fit = ts(temp.quad$fitted.values, start= 1 , frequency = 1)

##Tracer la courbe 
lines(quad.fit, lwd = 2 , col = 'green')

# m??thode moyene mobile 
window_length <- 7
moving_average <- stats::filter(time_series, 
                                filter = rep(1 / window_length, window_length), 
                                method = "convolution", 
                                sides = 2)

temp.fit.mvav = ts(moving_average , start = 1, frequency = 1)

lines(temp.fit.mvav, lwd = 2 , col= 'red')
#on a la serie est non saisonniere 
decomposed <- decompose(time_series)

residual(time_series)
## tester la normalit?? de la serie 
shapiro.test(time_series)
#h0 les donn??es suivent une distrubution normale
#h1 les donn??es  ne suivent pas une distrubution normale 
#p-value =p-value = 1.745e-12<0.05 donc  le donn??es ne suivent pas la loi normale 
#tester la stationnairt?? de la serie 
library(tseries)
adf.test(time_series)
#h0 n 'est pas stationnaire 
#h1 stationnaire 
#p-value = p-value = 0.6715>0.05 donc la serie n 'est pas stionnaire
kpss.test(time_series)
#h0 est stationnaire 
#h1 la serie  n'est  pas stationnaire 
#p-value = 0.01 <0.05 donc on rejete h0 donc la serie n'est  pas stationnaire 
#pour une premiere etape est pour rendre la serie stationnaire on applique le log
# pour reduire de la variance et faire la linearisation de la tendance 
#Box-Cox est une techniques utilis??es pour stabiliser la variance et rendre les donn??es plus proches d'une distribution normale
library(forecast)
lambda <- BoxCox.lambda(time_series)
transformed_series <- BoxCox(time_series, lambda = lambda)
plot(transformed_series , main = "BoxCox-Transformed Time Series")

#on va tester la stationnarit?? apr??es avoir appliquer le boxcox.
adf.test(transformed_series)  #p-value = 0.6701>0 meme  pour le test de adf pour transformed_series  la serie reste non stationnaire 
#donc on fait appliquer la differenciation 
difTransformedSeries=diff(transformed_series)
adf.test(difTransformedSeries)
#p-value = 0.01<0.05 donc la serie devient  stationnaire
kpss.test(difTransformedSeries)
#p-value = 0.1>0.05 donc la serie apr??es differenciation est stationnaire 
# alores on fait aapliquer le modele arima qui pour parametre p et d et q arima(p,d,q)
#ACF :
#Si l'ACF montre un arr??t net apr??s un certain nombre de d??calages (lags), cela indique un mod??le MA (Moving Average). Le premier d??calage o?? l'ACF devient insignifiant peut indiquer le param??tre q.
#Si l'ACF d??cro??t progressivement ou pr??sente des oscillations, cela peut indiquer un mod??le AR (AutoRegressive).
#PACF :
#Si le PACF montre un arr??t net apr??s un certain nombre de d??calages, cela sugg??re un mod??le AR. Le d??calage o?? le PACF devient insignifiant indique le param??tre p.
#Si le PACF d??cro??t progressivement ou montre des oscillations, cela peut indiquer un mod??le MA.
serie_acf=acf(difTransformedSeries)
serie_acf
#q=1 d 'apr??es le graph de acf  le dans le premier lags la autocorrelation depasse l 'intervalle de confiance 
serie_pacf=pacf(difTransformedSeries)
serie_pacf
#p=1 d ' apr??es le graphe de pacf dans le premier lags la autocorrelation depasse l 'intervalle de confiance  


library(forecast)
model1=Arima(difTransformedSeries,order=c(1,0,1))
summary(model1)
#Le mod??le ARIMA(1,0,1) peut ??tre repr??sent?? par l'??quation suivante
#yt=0.0390???0.091*y(t???1)+0.1611*??(t???1)+??t 
#Les statistiques sugg??rent que le mod??le a un ajustement raisonnable, mais avec des erreurs
#de variance mod??r??es ?? ??lev??es (sigma^2 = 15.1). Les crit??res AIC et BIC sont coh??rents avec 
#ce niveau d'ajustement. Les mesures d'erreur indiquent que le mod??le a une performance acceptable
#, mais avec des points ?? am??liorer, comme les valeurs ??lev??es de RMSE et MAE. Les mesures ACF1 et 
#ME indiquent un faible biais et une faible autocorr??lation, ce qui est un bon signe.
#on va visualiser les residu pour savoir sil sont de bruit blanc ou pas 
residuals <- residuals(model1)
plot(residuals, type = "l", col = "blue", lwd = 2)


shapiro.test(residuals) # p-value < 2.2e-16 <0.05 rejet de h0 donc les 
#r??sidus ne suivent une lois normale 
library(e1071)
skewness(residuals) #different de 0  donc pas d'asymetrie
kurtosis(residuals) # different de 3 donc non aplatie 
qqnorm(residuals)
qqline(residuals, col = "red", lwd = 1.5)
#comme les autres indicateurs l 'interpretation la comparison de qqplot avec qqline indique que les r??sidus ne suivent une loi normale 
jarque.bera.test(residuals) #p-value < 2.2e-16<0.05 il ya obsence de normalit?? des reidus 
acf(residuals) # d'apr??es l acf il n aya pas de depassement de l intervalle de confiance 
Box.test(residuals)
Box.test(residuals, type="Ljung-Box")

#p-value = 0.9887>0.05 Cela signifie que le mod??le a captur?? la structure sous-jacente des
#donn??es et que les r??sidus ressemblent ?? un bruit blanc (white noise)
#toutes les indicateurs montre que les residus ne sont pas bruit blanc 

#pour trouver les parametre optimale du modele arima on utilise la fonction auto.arima
best_model=auto.arima(difTransformedSeries)
best_model
#le modele auto arima a proposer un ajustement de p=2 et q=2 et d=0 ce modele donne 
#likelihood = -2788.07>-2788.64 : le likelihood  de mon premier modele 
#visalisation des residu du best_model
#l'equation donn?? par best_model est 
#:yt=-0.4499*y(t???1)-0.4355*y(t???2)+0.5202*??(t???1)+0.4623*??(t???2)+??t 
residuals1=residuals(best_model)
acf(residuals1)     
#les correlation ne depasse pas le intervalle de confiance 

#alors  on va faire une pr??diction avec le modele arima avec p=2 q=2; et d=1  
auto.arima(time_series)
the_model=arima(time_series,order=c(3,1,3))
forecast(the_model, h =) %>%
  autoplot() + labs(title = "Forecast of Stock volume", x = "Time", y = "Forecasted Stock volume",col="yellow")


the_model=arima(time_series,order=c(2,1,2))
forecast(the_model, h =60) %>%
  autoplot() + labs(title = "Forecast of Stock volume", x = "Time", y = "Forecasted Stock volume",col="yellow")





nnetar_model <- nnetar(time_series, lambda = 0)  # lambda = 0 indicates no transformation

# Predict the next 60 days
forecast_result <- forecast(nnetar_model, h = 28)

# Plot the predictions
autoplot(forecast_result) +
  labs(title = "Neural Network Forecast for the Next 60 Days",
       x = "Time",
       y = "Forecasted Values") +
  theme_minimal()










# pour rendre la serie stationnaire en fait log en premier etape puis tester la stationnarite 
#si elle stationnaire en applique modele arma
#sinon on applique differenciation puis en test par adf on applique arima(p,d,q  sur la serie differenci?? )
#sil elle non sationnaire on fait une on fait la differenciation ine autre fois 
#analyse de residu : bruit blanc 
#box.test() ljungbox() normalit?? ~
#qqplot      jarque bera ind??pendance 
#acf r??sidu 

#pour  connaitre le nombre de p d q obtimale par le code auto.arimma
#apr??es ca on fait partie entrainnement et test pour valider mon model 
#sarima pour degager les parametre  on fait on differenciation sur la saisonnalit?? 
#diif= yt-y(t-12)

