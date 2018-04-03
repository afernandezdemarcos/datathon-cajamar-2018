# DATATHON · University Hack 2018
## Salesforce Predictive Modelling
### Team: #SCAHaters

## EL OBJETIVO
Finalistas de la Primera Fase del Datathon organizado por Cajamar UniversityHack 2018.

Mediante el uso de las distintas técnicas de Machine Learning disponibles actualmente se busca desarrollar el mejor modelo de regresión mediante el desarrollo de un modelo predictivo que defina con precisión el poder adquisitivo del cliente.

## ORGANIZACIÓN DEL PROYECTO
En el presente proyecto puedes encontrar los siguientes archivos:
* *Test_Mission.txt* Contiene el test con los resultados del último modelo.
* *Code#SCAHaters.R* Código de construcción y evaluación del modelo.
* *Descripcion.txt* Contiene una breve descripción del método seguido.
* *Graphs* Contiene gráficas finales del análisis exploratorio de las variables del dataset.
* *Resultados.pptx* Presentación de los resultados del modelo una vez testeado.
* *Presentacion#SCAHaters.* Video presentación del análisis y modelo del equipo.

## DESARROLLO DEL PROYECTO
### Resumen
   La capacidad que tienen los bancos para conseguir nuevos clientes y las areas del mercado en las que se enfocan vienen en gran medidad determinadas por las características de sus clientes.
   Siendo el poder adquisitivo, el factor determinante en la adquisición de unos productos u otros, es lógico que la estimación de esta variable sea una prioridad cualquier negocio.
   En este documento se presentan las variables recogidas por una plataforma CRM (Salesforce), su analisis y el posterior desarrollo de un algoritmo de estimación del poder adquisitivo de los clientes.

### Analisis de las variables
   Las variables a las que se ha tenido acceso son:
   - Sociodemográficas [ Socio_Demo_01-05 ]
   - Importe de consumos habituales [Imp_Cons_01-17]
   - Importe de saldos de distintos productos financieros [Imp_Sal_01-21]
   - La tenencia de los distintos productos financieros  [Ind_Prod_01-24]
   - Número de operaciones de los distintos productos financieros [Num_Oper_01-20]
   - El poder adquisitivo [Poder_Adquisitivo]

   En primer lugar, dada la variedad del poder adquisitivo de los clientes se ha llevado a cabo un analisis de clustering, aplicando el algortimo de K-means.
   Se ha llegado a la conclusión de que el número de clusters más representativo es 3 y por tanto en todo el analisis posterior se ha tenido en cuenta 3 tipos de mercado atendiendo al poder adquisitivo de los clientes:
   Mercado 1 -  Clientes cuyo poder adquisitivo es menor a 94000
   Mercado 2 -  Clientes cuyo poder adquisitivo está entre 94000 y 675000
   Mercado 3 -  Clientes cuyo poder adquisitivo es  mayor a 675000
Dentro del grupo 2, que es el mayoritario, es conveniente subdividir tres grupos
   Mercado2A - Clientes cuyo poder adquisitivo es menor a 17000
   Mercado2B - Clientes cuyo poder adquisitivo está entre 17000 y 35000
   Mercado2C - Clientes cuyo poder adquisitivo está entre 35000 y 94000

   Las muestras de ciertos grupos (aquellos con mayor poder adquisitivo) son muy inferiores a las de los grupos más bajos, por lo que es necesario para que a continuación el modelo entrene de forma adecuada que los grupos estén equilibrados. Para ello se hace uso del algoritmo SMOTE.

   En segundo lugar se han analizado los importes de consumo habituales. Se han hallado las correlaciones de las variables con el poder adquisitivo y entre sí mismas.
   Se ha llegado a la conclusión de que las correlaciones más fuertes con el poder adquisitivo se dan en las variables Imp_Cons_02, Imp_Cons_06, Imp_Cons_09,Imp_Cons_12 y la suma de todas las Imp_Cons.
   Tras un analisis más exhaustivo, se ha observado que las correlaciones entre las parejas Imp_Cons_06-Imp_Cons_12 y las Imp_Cons_02 e Imp_Cons_09, son altas.
   Por este motivo, se ha tomado la decisión de realizar un analisis PCA (Principal Component Analysis) entre las parejas reduciendo las dimensiones del modelo final.
   
   Tras esto, se han analizado los importes de los saldos de los distintos productos financieros. Se han hallado las correlaciones entre estas variables y el número de operaciones de los distintos productos financieros, así como con el Poder Adquisitivo.
   Se ha llegado a la conclusión de que el Imp_Sal_15 se relacionaba directamente con el Num_Oper_02,07,08 y 18, a la vez que entre dichos Num_Oper existían relaciones fuertes, del orden de una correlación cercana a la unidad en algunos casos.
   Ocurre lo mismo para las variables Imp_Sal_16 y 17 con otros Num_Oper, pero debido a la baja correlación de estos saldos con el Poder_Adquisitivo, se decidió no tenerlas en cuenta.
   En cambio, focalizando el análisis en el Mercado 3, los saldos no nulos corresponden a Imp_Sal_08, Imp_Sal_09, Imp_Sal_19 y Imp_Sal_21, cuya combinación lineal (ajustada a través de técnica de optimización) ofrece una correlación del 0.5 con el Poder Adquisitivo.
   
   A continuación, se han analizado la tenencia de los distintos productos financieros. En primer lugar se realizó un análisis de cada producto en cada uno de los tres mercados, calulando el porcentaje de personas de cada mercado que tenía o no el producto.
   Se ha llegado a la conclusión de que Ind_Prod_01, Ind_Prod_04, Ind_Prod_13, Ind_Prod_15, Ind_Prod_17, Ind_Prod_22 y Ind_Prod_23 son productos que la gran parte del Mercado 3 posee, pero no es así en el Mercado 1, por lo que marcará la diferencia entre ambos mercados.
   En cuanto a Ind_Prod_02, Ind_Prod_10, Ind_Prod_11, Ind_Prod_14, Ind_Prod_16 y Ind_Prod_21, son productos que el Mercado 2 posee normalmente, pero no tanto el Mercado 3.
   Por ello se han agrupado estas variables en dos indices, el primero corresponde a la cantidad de productos de la primera sección (los del Mercado 3) y el segundo a la cantidad de productos del segundo conjunto.
   Mediante una visualización de estos indices frente al Poder_Adquisitivo, se ha comprobado que cuanto mayor es el numero de productos contratados, mayor es el Poder Adquisitivo del cliente. 
   Además, se ha visto que ambos indices se podian agrupar, de manera que las personas con mayor poder adquisitivo se encuentran en dos situaciones:
   - Bien han adquirido muchos productos ricos
   - O bien, han adquirido mucho producto medio y algún rico.
   Con este hallazgo,a aquellos clientes del Mercado 3 que sólo habían consumido 1 o 2 productos ricos se les rebajó el índice.
   Finalmente se ha obtenido un índice de 0 a 8, que se relaciona directamente con el Poder_Adquisitivo.
   
   En cuanto a las variables socio demográficas, no se ha podido obtener relaciones con el poder adquisitivo excepto en Socio_Demo_02, que ofrece una división en dos categorías. El grupo 1 presenta un Poder_Adquisitivo mayor al 2.
   Mediante un análisis de la varianza (ANOVA) se ha comprobado que la diferencia entre ambos grupos era significativa (p-valor<0.05).
   
   El dataset de train modificado consiste en 5 variables explicativas:
   - Imp_Cons_06: (Numérica continua)
   - Socio_Demo_02: (Numérica discreta)
   - indice2: Índice de tenencia de productos que aportan un mayor poder adquisitivo(Numérica discreta)
   - Sal_Total: Suma del importe de saldos que aportan un mayor poder adquisitivo(Numérica continua)
   - Cons_Total: Suma de importes de consumo(Numérica continua)
   
### Justificación del modelo
   Habiendo entendido la estructura del espectro de los clientes, se ha decidido realizar el modelo en dos partes:
	- Clasificación del cliente según las variables en uno de los cinco grupos descritos anteriormente.
	- Regresión particularizada en cada uno de los grupos.
   Random Forest es un algoritmo sencillo y a la vez potente para realizar regresión y clasificación con diferentes tipos de variable (continuas y discretas).
   También permite la optimización de dos parámetros, número de árboles y número de características exploradas en cada split, haciendo uso del Cross-Validation dataset.
   Finalmente se escogió aquel modelo cuyos parámetros ofrecian menor MSE (Mean Squar.ed Error) en el cross-validation, y se evaluó el comportamiento a través del Mean Absolute Error y el coeficiente R^2.
