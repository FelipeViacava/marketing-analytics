# Aula 5 - Grupos
# Pacotes ----
library(readxl)
library(devtools)
install_github("kassambara/factoextra")
library(factoextra)

#Leitura da base ----
insper <- read_excel("Insper.xlsx")insper %>% filter(Finished == 1)
insper <- insper[,12:102]
colunas <- colnames(insper)
colunas <- sub(".*6- Usando uma escala de 1 a 7, onde 1/ discordo fortemente e 7 / concordo fortemente, classifique...- ", "",colunas)
colunas <- sub(".*6- Usando uma escala de 1 a 7, onde 1/ discordo fortemente e 7 / concordo fortemente, classifique...-", "",colunas)
colunas <- sub(".*Usando uma escala de 1 a 7, onde 1/ prefiro pouco e 7 prefiro / muito, indique qual o seu grau...-", "", colunas)
colnames(insper) <- colunas


reduzida<- insper %>% 
  mutate_all(as.character) %>% 
  pivot_longer(`tradicional-FEI`:`Grau de preferencia-USP-Sao Carlos`, names_to = "nomes", values_to = "valores")

reduzida <- as.data.frame(reduzida)
reduzida$nomes <- gsub("POLI - USP", "POLIUSP", reduzida$nomes)
reduzida$nomes <- gsub("USP-Sao Carlos", "USPSao Carlos", reduzida$nomes)

reduzida<- reduzida%>% 
  separate(nomes, into = c("atributo","faculdade"), sep="-") %>% 
  filter(valores != "x") %>% 
  mutate(valores = as.numeric(valores))

detach(package:plyr)

reduzida <- reduzida%>%
  group_by(faculdade, atributo) %>% 
  summarize(media = mean(valores, na.rm=TRUE)) %>% 
  pivot_wider(names_from = atributo, values_from = media)

reduzida <- as.data.frame(reduzida)
rownames(reduzida)<- reduzida$faculdade
reduzida <- reduzida[,-1]

pc.cr <- reduzida %>% prcomp(cor = TRUE)
library(ggthemes)
fviz_pca_biplot(pc.cr, repel = TRUE)+
  labs(title = "Proximidade de atributos entre as Faculdades(MultiDimensional Scaling)",
       caption='Fonte:Aula de Marketing Analítico-Insper') +
  theme_few()+
  theme(panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust=0,
                                    vjust=-0.5,size=8))
