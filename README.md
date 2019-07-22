# El neoliberalismo a vuelo de pájaro

Este repositorio contiene los datos y el código con el cuál se elaboró el artículo *El noeliberalismo a vuelo de pájaro: un análisis bibliométrico*. La idea detrás de este artículo es contribuir con un contexto académico la recurrente discusión sobre el *neoliberalismo* como concepto rector en la definición de políticas públicas y de la discusión política.

## Datos

Este artículo tiene como insumo principal una serie de consultas de la base de datos [*SCOPUS*](https://www.elsevier.com/solutions/scopus).<sup>1</sup> En esta plataforma se hizo una consulta sencilla que recoletace todos los artículos académicos cuyo título tuviera la palabra *neoliberalismo*.

La búsqueda, en términos de *SCOPUS* fue ejecutada de la siguiente forma:

```TITLE(neoliberalism OR neoliberalimo OR néolibéralisme OR neoliberalismus) AND (LIMIT-TO (DOCTYPE , "ar"))```

Además, para limitar las búquedas a artículos que tuviesen *al menos a uno de sus autores* como perteneciente a una institución mexicana se utilizó la siguiente consulta:

```TITLE(neoliberalism OR neoliberalimo OR néolibéralisme OR neoliberalismus) AND (LIMIT-TO (DOCTYPE , "ar")) AND  (LIMIT-TO ( AFFILCOUNTRY ,  "Mexico"))```

Dichas consultas fueron hechas el 22 de julio de 2019 al medio día en formatos `.csv` y `.bib`, disponibles en la carpeta **01_Datos** en este repositorio.

## Código

La base fue tratada en **R** con base en los fundamentos teóricos y los comandos del paquete [`bibliometrix`](https://cran.r-project.org/web/packages/bibliometrix/vignettes/bibliometrix-vignette.html)<sup>2</sup>. El archivo *neoliberalismo_code.R* contiene todo el código necesario para constuir las visualizaciones, a su vez guardadas en la carpeta **03_Graficas**, y los análisis correspondientes. Aunado a lo anterior, el archivo *neoliberalismo_art.Rmd* contiene el código de *Markdown* para producir el artículo. 

<sup>1</sup> Aprovecho para agrader enormemente a mis colegas Alicia Berceinas y Miguel Toro por ayudarme a conseguir las bases.

<sup>2</sup> Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, Journal of Informetrics, 11(4), pp 959-975, Elsevier.
