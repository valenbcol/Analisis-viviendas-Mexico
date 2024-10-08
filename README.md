# Analisis de viviendas usando microdatos de México

Este repositorio contiene archivos que trabajan con datos para México, en principio con la encuesta de hogares (microdatos correspondientes al año 2016 desde la
página oficial del instituto de Estadísticas). Más específicamente: 
- Características de las viviendas que habitan los integrantes del hogar
- Características sociodemográficas de los integrantes del hogar

Los pasos seguidos para el análisis se encuentran detallados en el archivo R. La primera parte consiste en cargar y seleccionar variables clave de las bases de viviendas e individuos de manera eficiente.  y unir las bases mediante un atributo común, creando una variable "rango_edad" categórica. Generar y ajustar tablas con la población por edad y sexo, considerando el ponderador, y calcular proporciones totales y por sexo en formato adecuado. Finalmente, crear un gráfico de torta o dona para representar la estructura poblacional por género.

En la segunda parte se trabaja con un análisis a nivel de estado o entidad federativa de México. Siendo ahora el objetivo crear un gráfico con círculos que representen el share de propiedad por entidad usando  la renta promedio. Primero se debe extraer el código de la entidad de "ubicageo" con substr() según su longitud y calcular estadísticas de renta  sin missing. Luego unir la tabla con entidades, regiones, ingreso promedio y share de propietarios usando un join relevante y finalmente replicar un gráfico con círculos que representen el share de propiedad por entidad usando  la renta promedio. Recordar filtrar hogares rentados y calcular el alquiler por entidad con "renta" y "tenencia".

Como ejercicio final, se procede a descargar y manipular datos de cobertura de alumbrado público para el estado de Querétaro, verificando correspondencias con table(). Automatizar el proceso de descarga para los estados usando patrones de URL y un bucle que descargue y procese datos para los primeros 5 estados, imprimiendo un mensaje por iteración. Extender el bucle para limpiar y guardar los data frames modificados de los primeros 9 estados en una lista, optimizando el tiempo. Usar list.files() para iterar por las bases descargadas, cargarlas, transformarlas, y hacer un append de data frames. Calcular la media de "alumbrado" a nivel municipal en el bucle, filtrando valores NA, y obtener un df con columnas de identificación y promedio; explicar por qué varían las filas respecto al ejercicio anterior.



