library(readr)
library(readxl)
library(git2r)
library(ggplot2)

##Datos sacados de https://www.kaggle.com/datasets/malingarajapaksha/dataset

##Cargamos los datos desde nuestro repositorio de GitHub
datos <-read.csv("https://raw.githubusercontent.com/JuanferCalle/ProyectoFinalE/main/Dataset.csv")


# Filtramos los datos donde work_year es diferente a 2023
añoDescartable <- subset(datos, work_year != 2023)


##Eliminamos los datos que tengan un año diferente a 2023, dado a los fines de investigación del proyecto
datos_filtrados <- subset(datos, !(work_year %in% c(2020, 2021, 2022)))
# Eliminar filas donde el título del trabajo contenga "salary" en cualquier posición


# Calculamos el número de valores faltantes en cada columna
valores_faltantes <- colSums(is.na(datos))

# Identificamos las columnas que tienen valores faltantes
columnas_con_faltantes <- names(valores_faltantes[valores_faltantes > 0])

# Mostramos las columnas con valores faltantes
print(columnas_con_faltantes)

print(datos_filtrados)  

# Contar la frecuencia de cada tipo de empleo
frecuencia_empleos <- table(datos_filtrados$job_tittle)

# Contar la cantidad de datos en la columna "job_tittle"
cantidad_datos <- nrow(datos_filtrados)
print(cantidad_datos)

# Contar la frecuencia de cada tipo de empleo
frecuencia_empleos <- table(datos_filtrados$job_tittle)

attach(datos_filtrados)

table(job_title)
table(job_category)
table(employee_residence)
# Mostrar la frecuencia de cada tipo de empleo específico
print(frecuencia_empleos[c("Data Engineer", "Data Scientist", "Data Analyst", "Machine Learning Engineer", "Applied Scientist")])

colores <- c("blue", "green", "red", "purple", "orange", "yellow", "cyan", "magenta", "brown", "gold")
barplot(table(job_category), 
        main = "Frecuencia de categoria de Trabajo",
        xlab = "Título de Trabajo",
        ylab = "Frecuencia",
        col = colores,
        las = 2)  # Rotar etiquetas en el eje x si es necesario
# Crear el diagrama de torta
# Crear el diagrama de torta
# Crear el diagrama de torta sin las etiquetas de las categorías de trabajo
# Crear el diagrama de torta sin las etiquetas de las categorías de trabajo
# Crear el diagrama de torta sin las etiquetas de las categorías de trabajo
# Crear el diagrama de torta sin las etiquetas de las categorías de trabajo
pie(table(job_title),
    main = "Frecuencia de Títulos de Trabajo",
    col = rainbow(length(table(job_title))),  # Colores del arco iris para cada sector
    labels = NA)  # No mostrar etiquetas dentro del gráfico de torta

# Obtener los colores del arco iris
colores <- rainbow(length(table(job_title)))

# Crear recuadros para las categorías de trabajo
for (i in 1:length(table(job_title))) {
  rect(xleft = 1.2, ybottom = 1.1 - i * 0.1, xright = 1.3, ytop = 1.0 - i * 0.1, col = colores[i])
  text(1.35, 1.05 - i * 0.1, names(table(job_title))[i], pos = 4, cex = 0.8)
}


