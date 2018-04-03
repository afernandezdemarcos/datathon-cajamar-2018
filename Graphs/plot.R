library(plotly)
library(readr)
setwd("~/cajamar")
train = read_csv("./trainMercado.csv")
train = as.data.frame(train)
train$Mercado = as.factor(train$Mercado)
p = plot_ly(data=train, x=~Poder_Adquisitivo, color=~Mercado,type="histogram")%>%
  layout(xaxis=list(type = "log"))
p = plot_ly(data=train, x=~Poder_Adquisitivo,type="histogram")%>%
layout(xaxis=list(type = "log"))

train$Count = 1
dinero = aggregate(Poder_Adquisitivo~Mercado, data = train, sum)
gente = aggregate(Count~Mercado, data = train, sum)
levels(gente$Mercado) = c("95-600K","3-17K","17-35K","35-95K",">600K")
levels(dinero$Mercado) = c("95-600K","3-17K","17-35K","35-95K",">600K")

piegente = plot_ly(data = gente, values=~Count, labels = ~Mercado, type = "pie",
                   # textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   marker=list(colors = colors,
                               line = list(color = '#FFFFFF', width = 1)),
                   showlegend=F)%>%
  layout(title="#Gente")
piedinero = plot_ly(data = dinero, values=~Poder_Adquisitivo, labels = ~Mercado, type = "pie",
                   # textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   marker=list(colors = colors,
                               line = list(color = '#FFFFFF', width = 1)),
                   showlegend=F)%>%
  layout(title="Poder Adquisitivo respecto al global del banco")

histograma = plot_ly(alpha=0.0)%>%
  add_histogram(data=train, x=~Poder_Adquisitivo, type="histogram")%>%
  layout(xaxis=list(type="log"))

# Agrupamos el dataset de train con las siguientes variables
# Ind_Cons_06
# Socio_Demo_02 
# Total_Cons
# Total_Sal
# Indice2
# Poder_Adquisitivo
# Modificamos la tenencia a 0 - No tiene y 1 - Tiene (El 2 no afecta demasiado)
for (i in 40:63){
  train[train[,i]==2,i]=1
}
# Creamos un indice que representa el numero de productos "ricos" (que tienen los ricos pero apenas los pobres)
indice = 0
for (i in c(39+1,39+4,39+13,39+15,39+17,39+22,39+23)){
  indice = indice+train[,i]
}
train$indice = indice
# Este otro indice representa el numero de productos "medios" (que tienen los medios, pero apenas pobres)
indice = 0
for (i in c(39+2,39+10,39+11,39+14,39+16,39+21)){
  indice = indice+train[,i]
}
train$indice2 = indice

train$indice2[train$indice==5]=11
train$indice2[train$indice==4]=10
train$indice2[train$indice==3]=9
train$indice2[train$indice==2]=8
train$indice2[train$indice==1]=7

trainmodified = train[(train$indice2==0 & train$Poder_Adquisitivo<=22550) | (train$indice2==1 & train$Poder_Adquisitivo<=29870) | (train$indice2==2 & train$Poder_Adquisitivo<=39460)
                      | (train$indice2==3 & train$Poder_Adquisitivo<=45950) | (train$indice2==4 & train$Poder_Adquisitivo<=48380) | (train$indice2==5 & train$Poder_Adquisitivo<=69710)
                      | (train$indice2==6 & train$Poder_Adquisitivo<=138630) | (train$indice2==7 & train$Poder_Adquisitivo<=259470) | (train$indice2==8 & train$Poder_Adquisitivo<=302000),]
indicePlot = plot_ly(data = train, x=~indice2, y=~Poder_Adquisitivo,type="box")
sexo = plot_ly(data = train, x=~Socio_Demo_02, y=~Poder_Adquisitivo,type="box",showlegend=F,color = as.factor(train$Socio_Demo_02))
# Variable suma de consumos
suma=0
VectorConsumo = c(1,2,3,4,6,9,12,15,16)
for (i in (1+VectorConsumo)){suma=suma+train[,i]}
train$total_cons = suma
# Variable suma de saldos clave (los saldos 8,9,19,21)
suma=0
VectorSaldo = c(8,9,16,19,20,21)
for (i in (1+VectorSaldo)){suma=suma+train[,i]}
train$total_sal = suma
train = train[,c(7,85,92:94,90)]

operaciones_saldo = train[,c(19:39,64:83,89)]
correlacion = rcorr(as.matrix(operaciones_saldo))
MatrizCorrelacion = correlacion$r
corrplot(MatrizCorrelacion, type="lower")

train$Mercado[train$Mercado=="2A" | train$Mercado=="2B" | train$Mercado=="2C"] = 2
train$Mercado = as.factor(train$Mercado)
for (producto in 1:24){
  prod=data.frame()
  for(mercado in 1:3){
    Producto = train[train$Mercado==mercado,producto+39]
    NoTiene = Producto[Producto==0]
    Tiene = Producto[Producto==1]
    Tuvo = Producto[Producto==2]
    prod[mercado,1] = length(NoTiene)/length(Producto)*100
    prod[mercado,2] = length(Tiene)/length(Producto)*100
    prod[mercado,3] = length(Tuvo)/length(Producto)*100
    variable = paste0("producto",producto)
    variableplot = paste0("plotprod",producto)
    p = plot_ly(prod, x=c("1 - 3-95K","2 - 95-600K","3 - >600K"), y = ~V3, type = 'bar', name = 'Tuvo',marker=list(color="#4490E3")) %>%
      add_trace(y = ~V2, name = 'Tiene',marker=list(color=c("#3DAE4A")), showlegend=F) %>%
      add_trace(y = ~V1, name = 'No tiene',marker=list(color=c("#E85B2C")), showlegend=F) %>%
      layout(yaxis = list(title = 'Count'), barmode = 'stack')
    if(mercado==3){
      assign(variable, prod)
      assign(variableplot, p)
      }
  }
}

s1 <- subplot(plotprod1,plotprod2,plotprod3,plotprod4, nrows = 1, shareX = TRUE, shareY = TRUE)
s2 <- subplot(plotprod5,plotprod6,plotprod7,plotprod8, nrows = 1, shareX = TRUE, shareY = TRUE)
s3 <- subplot(plotprod9,plotprod10,plotprod11,plotprod12, nrows = 1, shareX = TRUE, shareY = TRUE)
S1 = subplot(s1,s2,s3,nrows=3)

s1 <- subplot(plotprod13,plotprod14,plotprod15,plotprod16, nrows = 1, shareX = TRUE, shareY = TRUE)
s2 <- subplot(plotprod17,plotprod18,plotprod19,plotprod20, nrows = 1, shareX = TRUE, shareY = TRUE)
s3 <- subplot(plotprod21,plotprod22,plotprod23,plotprod24, nrows = 1, shareX = TRUE, shareY = TRUE)
S2 = subplot(s1,s2,s3,nrows=3)
