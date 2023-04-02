
#########################################################
####               Ejercicio: Bar plots              ####
####               Diapositivas: 7- 10               ####
#########################################################
# data
# library(gcookbook)
### crear datos "cabbage_exp"
# cabbage_exp --> Es el recopilado de un experimento de un cultivo de "Col o repollo (cabbage)". Fueron 2 condiciones C39 y C52; y se cortaron las coles a los dias 16, 20 y 21; se pesaron 10 coles de la condicion y se obtuvo la desviacion estandar y el error estandar de la media del peso.

cabbage_exp <- data.frame(Cultivar=c(rep("c39", 3), rep("c52", 3)), 
                          Date=rep(c("d16", "d20", "d21"),2), 
                          Weight=c(3.18, 2.80, 2.74, 2.26, 3.11, 1.47), 
                          sd= c(0.9566144, 0.2788867, 0.9834181, 0.4452215, 0.7908505, 0.2110819), 
                          n=rep(10,6), 
                          se=c(0.30250803, 0.08819171, 0.31098410, 0.14079141, 0.25008887, 0.06674995))
# Explorar datos
cabbage_exp

# Grafico de Bar plots basico
ggplot(data= cabbage_exp, aes(x=Date, y= Weight)) + 
  geom_bar(stat="identity")

# Cambio de color de las barras
ggplot(data= cabbage_exp, aes(x=Date, y= Weight)) + 
  geom_bar(stat="identity", fill="dodgerblue")

# Cambio de tema
ggplot(data= cabbage_exp, aes(x=Date, y= Weight)) + 
  geom_bar(stat="identity", fill="dodgerblue") + 
  theme_bw()

### Grafico con barras apiladas coloreado por condicion
ggplot(data= cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + 
	geom_bar(stat="identity")

### Añadir texto
ggplot(data= cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=0.4)

### Grafico con barras separadas por condicion
ggplot(data= cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + 
  geom_bar(stat="identity", position = "dodge")

### Usa otra paleta de colores
ggplot(data= cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + 
	geom_bar(stat="identity", position="dodge") +
	scale_fill_brewer(palette="Set1")

## Añadir texto Posicion "dodge"
ggplot(data= cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + 
	geom_bar(stat="identity", position="dodge") +
	scale_fill_brewer(palette="Set1") +
	geom_text(aes(label=Weight), vjust=-0.4, 
	          position=position_dodge(0.9), size=3, colour="green4")

### Añadir barras de error y cambiat tema
ggplot(data= cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + 
	geom_bar(stat="identity", position="dodge") +
	scale_fill_brewer(palette="Set1") +
  geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), 
                width=0.2, position=position_dodge(0.9)) +
	theme_bw()

### Cambiar los titulos de los ejes
ggplot(data= cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + 
  geom_bar(stat="identity", position="dodge") +
  scale_fill_brewer(palette="Set1") +
  geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), 
                width=0.2, position=position_dodge(0.9)) +
  labs(y="Day", x="Weight (lb)", fill="Experiment") +
  theme_bw()

ggsave("figuras/barplot_cultivos.png", device = "png")

#########################################################
####           Ejercicio: Scatter plots              ####
####              Diapositivas: 36-39                ####
#########################################################

## Usaremos un set de datos simulado "antibodies". 
#Datos que provienen de pacientes recuperados de Dengue
# Explorar los datos
antibodies <- read.delim("tablas/antibodies.txt", header=T)
View(antibodies)
summary(antibodies)
dim(antibodies)

#### Scatter plots basicos
ggplot(data= antibodies, aes(x= CDR3.length, y= VJ.identity)) +
	geom_point()
	
# Color por condicion
ggplot(data= antibodies, aes(x= CDR3.length, y= VJ.identity, colour=V.GENE)) +
	geom_point()

# Forma por condicion
ggplot(data= antibodies, aes(x= CDR3.length, y= VJ.identity, shape=V.GENE)) +
	geom_point()

# Forma y color por condicion
ggplot(data= antibodies, 
       aes(x= CDR3.length, y= VJ.identity, shape=V.GENE, colour=V.GENE)) +
	geom_point() 

# Cambiar los valores de forma
ggplot(data= antibodies, 
       aes(x= CDR3.length, y= VJ.identity, shape=V.GENE, colour=V.GENE)) +
	geom_point() +
	scale_shape_manual(values=c(1,2,3))

# Cambiar colores manuales
# Con nombre de color
ggplot(data= antibodies, aes(x= CDR3.length, y= VJ.identity, shape=V.GENE, colour=V.GENE)) +
	geom_point() +
	scale_shape_manual(values=c(1,2,3)) +
	scale_colour_manual(values=c("deepskyblue", "seagreen2", "salmon")) +
	theme_bw()

# Con codigo HEX
ggplot(data= antibodies, aes(x= CDR3.length, y= VJ.identity, shape=V.GENE, colour=V.GENE)) +
  geom_point() +
  scale_shape_manual(values=c(15,16,17)) +
  scale_colour_manual(values=c("#24761A", "#9B79D9", "#2468E6")) +
  theme_bw()

### Quitar leyenda, agregar texto y modificar ejes
ggplot(data= antibodies, aes(x= CDR3.length, y= VJ.identity, shape=V.GENE, colour=V.GENE)) +
  geom_point(show.legend = FALSE) +
  scale_shape_manual(values=c(1,2,3)) +
  scale_colour_manual(values=c("#24761A", "#9B79D9", "#2468E6")) +
  annotate("text", x=75, y=85, label="IGHV1-69", colour="#24761A") +
  annotate("text", x=35, y=88, label="IGHV2-5", colour="#9B79D9") +
  annotate("text", x=60, y=82, label="IGHV3-73", colour="#2468E6") +
  labs(x="CDR3 length (bp)", y= "VJ identity (%)", colour="")+
  theme_bw()

ggsave("figuras/scatterplot_antibodies.png", device = "png")

#########################################################
####              Ejercicio: Histograms              ####
####               Diapositivas: 40-41               ####
#########################################################

## Histograma basico
ggplot(data= antibodies, aes(x= VJ.identity)) +
	geom_histogram()

# Elegir un mejor ancho de barra (binwidth)
ggplot(data= antibodies, aes(x= VJ.identity)) +
	geom_histogram(binwidth=0.3)
	
# Divide el rango x en 20 barras (bins)
summary(antibodies$VJ.identity)
binsize <- diff(range(antibodies$VJ.identity))/20

# Cambiar el color de las barras
ggplot(data= antibodies, aes(x= VJ.identity)) +
	geom_histogram(binwidth= binsize, colour="black", fill="royalblue4")

# Varios histogramas con diferentes colores de relleno
ggplot(data= antibodies, aes(x= VJ.identity, fill=V.GENE)) +
	geom_histogram(binwidth= binsize, colour="black")
	
# Cambiar el valor de "alpha" (transparencia)
ggplot(data= antibodies, aes(x= VJ.identity, fill=V.GENE)) +
	geom_histogram(binwidth= binsize, position="identity", alpha=0.4)

# Añadir un Density plot -- Plot de Densidad
ggplot(data= antibodies, aes(x= VJ.identity, fill=V.GENE, y= ..density..)) +
	geom_histogram(binwidth= binsize, position="identity", alpha=0.4, colour="gray") +	
	geom_density(alpha=0.4)

# Cambiar la apariencia
ggplot(data= antibodies, aes(x= VJ.identity, fill=V.GENE, y= ..density..)) +
	geom_histogram(binwidth= binsize, position="identity", alpha=0.4, colour="gray") +	
	geom_density(alpha=0.4) + 
	scale_fill_manual(values=c("deepskyblue", "seagreen2", "salmon")) +
  labs(x="VJ identity (%)", y= "Density", fill="") +
	theme_bw() +
  theme(legend.position = "top")

ggsave("figuras/histogram_antibodies.png", device = "png")


#########################################################
####               Ejercicio: Boxplots               ####
####               Diapositivas: 42-44               ####
#########################################################

# Boxplot basico
ggplot(data=antibodies, aes(y= V.Nb.mutations, x= V.GENE)) +
	geom_boxplot()

# Con muescas (notch)
ggplot(data=antibodies, aes(y= V.Nb.mutations, x= V.GENE)) +
	geom_boxplot(notch= TRUE)
	
# Cambiar la apariencia
ggplot(data=antibodies, aes(y= V.Nb.mutations, x= V.GENE, fill=V.GENE)) +
	geom_boxplot(notch= TRUE) +
	scale_fill_manual(values=c("deepskyblue", "seagreen2", "salmon")) +
	theme_bw()

## Cambiar las etiquetas de los ejes
ggplot(data=antibodies, aes(y= V.Nb.mutations, x= V.GENE, fill=V.GENE)) +
	geom_boxplot(notch= TRUE) +
	scale_fill_manual(values=c("deepskyblue", "seagreen2", "salmon")) +
	labs (title= " Boxplot V.GENE", y= "VH mutations (nt)", x= "", fill= "") +
	theme_bw()

# Grafico de Violin
ggplot(data=antibodies, aes(y= V.Nb.mutations, x= V.GENE,  fill=V.GENE)) +
	geom_violin() +
	scale_fill_manual(values=c("deepskyblue", "seagreen2", "salmon")) +
	labs (title= " Boxplot V.GENE", y= "VH mutations (nt)", x= "", fill= "") 

# Quitar leyenda y mejorar apariencia
ggplot(data=antibodies, aes(y= V.Nb.mutations, x= V.GENE,  fill=V.GENE)) +
	geom_violin(show.legend = FALSE, trim = FALSE) +
	scale_fill_manual(values=c("deepskyblue", "seagreen2", "salmon")) +
	labs (title= " Boxplot V.GENE", y= "VH mutations (nt)", x= "", fill= "") +
  theme_linedraw()

# Añadir una linea de tendencia
# en este caso añadiremos una linea de la media de V.Nb.mutations
v_media <- mean(antibodies$V.Nb.mutations)

ggplot(data=antibodies, aes(y= V.Nb.mutations, x= V.GENE,  fill=V.GENE)) +
  geom_violin(show.legend = FALSE, trim = FALSE) +
  scale_fill_manual(values=c("deepskyblue", "seagreen2", "salmon")) +
  labs (title= " Boxplot V.GENE", y= "VH mutations (nt)", x= "", fill= "") +
  geom_hline(yintercept = v_media, col="gold", linetype="dashed") +
  annotate("text", x = 0.7, y=30, label="Mean", colour="gold") +
  theme_linedraw()

ggsave("figuras/boxplot_antibodies.png", device = "png")

####################################################################
####  Ejercicio Extra: Diagramas de caja con ajuste de facetas  ####
####                     Diapositivas: 46-47.                   ####
####################################################################

#### Cargar libreria extra
library(reshape2)
cdrs <- antibodies[,1:4]
View(cdrs)
tmp <- melt(cdrs)

# Explorar los datos ¿Que hace la funcion melt?
View(tmp)
dim(tmp)
summary(tmp)

# boxplot con ajuste de faceta (facet wrap)
ggplot(data= tmp, aes(x= V.GENE, y= value)) +
	geom_violin() +
	facet_wrap(~variable)

# Cambiar la apariencia
ggplot(data= tmp, aes(x= V.GENE, y= value, fill= V.GENE)) +
  geom_violin(trim = FALSE) +
	facet_wrap(~variable) +
	scale_fill_manual(values=c("deepskyblue", "seagreen2", "salmon")) +
	labs(y="length (nt)", x="", fill="") +
  theme_linedraw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        legend.position = "bottom")

ggsave("figuras/extra_violin_antibodies.png", device = "png")
