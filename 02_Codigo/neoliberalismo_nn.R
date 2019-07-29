library(tidyverse)
library(hrbrthemes)
library(readxl)
library(ggrepel)
library(extrafont)
#font_import()
loadfonts(device = "win")
library(bibliometrix)
library(igraph)
library(simcausal)
#detach(package:statnet)
library(statnet)
library(intergraph)
library(network)

### DATOS

B <-  read_csv("neolibe_comp.csv")
journals <- read_excel("CiteScore_Metrics_2011-2018_download.xlsx", 
                       sheet = "2018 All")

neolib <- readFiles("neolib_comp.bib")
M <- convert2df(neolib, dbsource = "scopus", format = "bibtex")
kw <- M$DE
results <- biblioAnalysis(M, sep = ";")

B <- cbind(B, kw)

options(width=100)
S <- summary(object = results, k = 20, pause = FALSE)

plot(x = results, k = 20, pause = FALSE)



  # TIEMPO

B %>% 
  mutate(var = Year>2018) %>% 
  # filter(Year >= 1990) %>% 
  count(Year, var) %>% 
  ungroup() %>% 
  ggplot(aes(Year, n)) +
  geom_line(aes(colour=as.factor(var)))+
  scale_color_manual(guide=FALSE, values=c("black", "white"))+ 
  geom_segment(aes(x = 2018, y = 188, xend =2019, yend = 144), linetype="dashed")+
  geom_point()+
  theme_ipsum(grid="Y")+
  scale_x_continuous(breaks = seq(1960,2019,4))+
  labs(title="Artículos académicos sobre «neoliberalismo»",
       subtitle="de 2010 a 2019", x="", y="Total de Artículos",
       caption="Fuente: SCOPUS | @jmtoralc")+
  ylim(0, 200)

ggsave("yr.png", width = 10, height = 8)



## Países

pais <- as.data.frame(results$CountryCollaboration) %>% 
  gather(variable, value, -Country) %>% 
  arrange(-value) %>% 
  filter(value >= 10)%>% 
  mutate(Country = recode(Country, "USA"="Estados Unidos",
                         "UNITED KINGDOM"="Reino Unido",
                         "CANADA"="Canadá",
                         "AUSTRALIA"="Australia",
                         "NEW ZEALAND"="Nueva Zelanda",
                         "SPAIN"="EspaÃ±a",
                         "IRELAND"="Irlanda",
                         "FRANCE"="Francia",
                         "ITALY"="Italia",
                         "NETHERLANDS"="Países Bajos",
                         "GERMANY"="Alemania",
                         "CHILE"="Chile",
                         "SOUTH AFRICA"="Sudáfrica",
                         "ARGENTINA" = "Argentina",
                         "FINLAND"="Finlandia",
                         "BRAZIL" = "Brasil",
                         "SINGAPORE"="Singapur",
                         "MEXICO"="México",
                         "BELGIUM"="Bélgica",
                         "TURKEY"="Turquía",
                         "AUSTRIA"="Austria")) 


ggplot(pais, aes(x=reorder(Country,value), y=value,
                 fill=variable)) +
  geom_col() +
  coord_flip() +
  theme_ipsum(grid="X")+
  ylim(0, 600) +
  scale_fill_manual(name="Múltiples autores",
                    values=c("red","blue"),
                    labels=c("Sí","No")) +
  labs(title="Países que produjeron más de 10 Artículos",
       subtitle="por tipo de colaboración",
       x="Número de Artículos",
       y="País de afiliación",
       caption="Fuente: SCOPUS | @jmtoralc")

ggsave("paises.png", width = 10, height = 8)



B %>% 
  filter(Year >= 2010) %>% 
  select(Year, idioma = `Language of Original Document`) %>% 
  mutate(eng = grepl("English",idioma)) %>%
  count(Year, eng) %>% 
  group_by(Year) %>%
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(as.factor(Year), prop, order=rev(eng),
             fill= eng)) +
  geom_col() +
  scale_fill_manual(name="",
                    values = c("indianred1", "royalblue"),
                    labels = c("En otro idioma","Inglés"))+
  scale_y_percent()+
  theme_ipsum(grid="Y") +
  labs(title="Proporción de Artículos académicos sobre «neoliberalismo»",
       subtitle = "escritos en Inglés o en otro idioma, de 2010 a 2019",
       y="Proporción",
       x="",
       caption="Fuente: SCOPUS | @jmtoralc")

ggsave("langeng.png", width = 10, height = 8)

# Otros idiomas

B %>% 
  filter(!grepl("English",`Language of Original Document`)) %>%
  filter(Year >= 2010) %>% 
  count(Year, idioma= `Language of Original Document`) %>% 
  mutate(idioma = recode(idioma, "Bosnian"="Bosnio",
                         "Czech"="Checo",
                         "Dutch"="Holandés",
                         "French"="Francés",
                         "German"="Alemán",
                         "Italian"="Italiano",
                         "Polish"="Polaco",
                         "Portuguese"="Portugués",
                         "Russian"="Ruso",
                         "Slovenian"="Esloveno",
                         "Spanish"="EspaÃ±ol")) %>% 
  ggplot(aes(as.factor(Year), n, 
             fill= idioma)) +
  geom_col() +
  theme_ipsum(grid="Y") +
  labs(title="Artículos académicos sobre el «neoliberalismo»",
       subtitle = "escritos en un idioma diferente al Inglés",
       y="Número de Artículos",
       x="",
       caption="Fuente: SCOPUS | @jmtoralc")+
  scale_fill_brewer(palette="Set3", name="Idioma del artículo")

ggsave("lang.png", width = 10, height = 8)




# Inglás vs. otros

B %>% 
  filter(Year >= 2010) %>% 
  select(Year, idioma = `Language of Original Document`) %>% 
  mutate(eng = grepl("English",idioma)) %>%
  count(Year, eng) %>% 
  group_by(Year) %>%
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(as.factor(Year), prop, order=rev(eng),
             fill= eng)) +
  geom_col() +
  scale_fill_manual(name="",
                    values = c("indianred1", "royalblue"),
                    labels = c("En otro idioma","Inglés"))+
  scale_y_percent()+
  theme_ipsum(grid="Y") +
  labs(title="Proporción de Artículos académicos sobre «neoliberalismo»",
       subtitle = "escritos en Inglés o en otro idioma, de 2010 a 2019",
       y="Proporción",
       x="",
       caption="Fuente: SCOPUS | @jmtoralc")

ggsave("langeng.png", width = 8, height = 4)




## Universidades
universidades <- as_tibble(results$Affiliations)

universidades %>% 
  filter(n > 10) %>% 
  ggplot(aes(reorder(AFF,n), n, fill=n)) +
  geom_col() +
  coord_flip()+
  theme_ipsum(grid="X") +
  labs(title="Artículos académicos sobre el «neoliberalismo»",
       subtitle = "por institución de origen",
       y="Número de Artículos",
       x="",
       caption="Fuente: SCOPUS | @jmtoralc") +
  guides(fill=FALSE) +
  scale_fill_distiller(palette="Blues", direction = 1)

ggsave("univers.png", width = 10, height = 8)





#JOURNAL

B %>% 
  count(`Source title`) %>% 
  arrange(-n) %>% 
  head(20) %>% 
  ggplot(aes(reorder(`Source title`,n), n, fill=n)) +
  geom_col() +
  coord_flip()+
  theme_ipsum(grid="X") +
  labs(title="Artículos académicos sobre el «neoliberalismo»",
       subtitle = "por publicación",
       y="Número de Artículos",
       x="",
       caption="Fuente: SCOPUS | @jmtoralc") +
  guides(fill=FALSE) +
  scale_fill_distiller(palette="Spectral", direction = -1)

ggsave("journal.png", width = 10, height = 8)





### FUENTE


B %>% 
  count(Title=`Source title`) %>% 
  arrange(-n) -> fuente

#quantile(merge$CiteScore, na.rm = T, probs = .99)[4]
#quantile(merge$n, na.rm = T, probs = .99)[4]

fuente %>% 
  left_join(journals) %>% 
  select(Title, n, CiteScore) %>%
  mutate(CiteScore = as.numeric(CiteScore)) %>% 
  group_by(Title, n) %>% 
  summarise(CiteScore = mean(CiteScore, na.rm = T))-> merge

ggplot(merge, aes(n, CiteScore)) +
  geom_point(color="grey60") +
  geom_text_repel(data=merge %>% filter(CiteScore > 5.51 |
                                          n > 13.48), 
                  aes(n, CiteScore, label=Title)) +
  geom_point(data=merge %>% filter(CiteScore > 5.51 |
                                     n > 13.48), 
             aes(n, CiteScore), color="red")+
  theme_ipsum() +
  labs(title="Número de articulos sobre «neoliberalismo» con respecto a la influencia de la revista",
       subtitle="medido con el CiteScore de SCOPUS",
       y="Número de Artículos",
       x="",
       caption="Fuente: SCOPUS | @jmtoralc\nNota: En rojo los de los últimos deciles de cada variable.") +
  geom_hline(yintercept = 5.51, linetype="dashed", color="red") +
  geom_vline(xintercept = 13.48, linetype ="dashed", color="red")

ggsave("scatters.png", width = 10, height = 6)





# CITAS

B %>% 
  arrange(-`Cited by`) %>% 
  head(10) %>% 
  select(Authors,Title, Year, `Source title` ,`Cited by`)  %>% 
  write.csv("cited.csv")




## Autores más productivo

source("topaues.R")

topAU <- authorProdOverTime2(M, k = 10, graph = TRUE)


ggsave("topau.png", width = 10, height = 6)



# Autores
png("red_autores.png", width = 2000, height = 2000)
NetMatrix <- biblioNetwork(M, analysis = "co-citation", 
                           network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "Red de referencias conjuntas", 
                type = "fruchterman",
                size.cex=TRUE, size=20, remove.multiple=FALSE,
                labelsize=2,edgesize = 5, edges.min=2, curved = T)
dev.off()

NetMatrix.ig <- sparseAdjMat.to.igraph(NetMatrix, mode = "directed")
NetMatrix.net <- asNetwork(NetMatrix.ig)

gden(NetMatrix.net)


detach(package:statnet)

grados <-  degree(NetMatrix.net)




# Keywords

# Create keyword co-occurrences network

Mc <- M %>% 
  mutate(ID = gsub("NEOLIBERALISM|NEOLIBERALISM;", "", ID))

png("red_palabras.png", width = 2000, height = 2000)

NetMatrix <- biblioNetwork(Mc, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, normalize="jaccard", weighted=T, n = 80, 
                Title = "Keyword Co-occurrences", type = "fruchterman", 
                size=T,edgesize = 5,labelsize=2, curved = T)

dev.off()




















B %>% 
  count(kw) %>% 
  mutate(coma = str_count(kw, ";")) %>% 
  arrange(-coma) %>% 
  select(-n,-coma) %>% 
  separate(kw,paste0("p",1:58), "; ") %>% 
  gather(variable, kw) %>% 
  na.omit() %>% 
  count(kw) %>% 
  arrange(-n) -> kwcount
  
kwcount %>% 
  filter(!kw %in% c("NEOLIBERALISM", "ARTICLE")) %>% 
  head(20) %>% 
  ggplot(aes(reorder(kw,n), n,
             fill=n)) +
  geom_col() +
  coord_flip()+
  theme_ipsum(grid="X") +
  scale_fill_distiller(palette="Spectral") +
  guides(fill=FALSE)+
  labs(title="20 palabras clave más frecuentes",
       subtitle = "en Artículos sobre «neoliberalismo»",
       y="Número de Artículos",
       x="",
       caption="Fuente: SCOPUS | @jmtoralc\nNota: Se eliminaron las palabras 'Neoliberalism' y 'Article'.")


ggsave("kw.png", width = 10, height = 6)




B %>% 
  arrange(-`Cited by`) %>% 
  head(10) %>% 
  select(Title, Year, `Source title` ,`Cited by`) %>% 
  knitr::kable()













CR <- citations(M, field = "article", sep = ";")

