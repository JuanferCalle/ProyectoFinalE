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


names(datos_filtrados)

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
table(work_setting)

# Crear una nueva columna llamada "salary_in_usd_thousands" con los valores de "salary_in_usd" divididos por 1000
datos_filtrados$salary_in_usd_thousands <- datos_filtrados$salary_in_usd / 1000


# Mostrar las primeras filas del conjunto de datos para verificar los cambios



# Mostrar la frecuencia de cada tipo de empleo específico
print(frecuencia_empleos[c("Data Engineer", "Data Scientist", "Data Analyst", "Machine Learning Engineer", "Applied Scientist")])

###Las tablas por job_category
datos_data_engineering <- subset(datos_filtrados, job_category == "Data Engineering")
datos_Architecture_and_Modeling <- subset(datos_filtrados, job_category == "Data Architecture and Modeling")
datos_Data_Science_and_Research <- subset(datos_filtrados, job_category == "Data Science and Research")
datos_BI_and_Visualization <- subset(datos_filtrados, job_category == "BI and Visualization")
datos_Data_Quality_and_Operations <- subset(datos_filtrados, job_category == " Data Quality and Operations")
datos_Machine_Learning_and_AI <- subset(datos_filtrados, job_category == "Machine Learning and AI")
datos_Cloud_and_Database  <- subset(datos_filtrados, job_category == "Cloud and Database ")
datos__Data_Analysis  <- subset(datos_filtrados, job_category == "Data Analysis ")
datos_Data_Management_and_Strategy  <- subset(datos_filtrados, job_category == "Data Management and Strategy ")
datos_Leadership_and_Management  <- subset(datos_filtrados, job_category == "Leadership and Management ")



# Seleccionar solo los salarios
salarios_data_engineering <- datos_filtrados$salary_in_usd_thousands[datos_filtrados$job_category == "Data Engineering"]
salarios_Architecture_and_Modeling <- datos_filtrados$salary_in_usd_thousands[datos_filtrados$job_category == "Data Architecture and Modeling"]
salarios_Data_Science_and_Research <- datos_filtrados$salary_in_usd_thousands[datos_filtrados$job_category == "Data Science and Research"]
salarios_BI_and_Visualization <- datos_filtrados$salary_in_usd_thousands[datos_filtrados$job_category == "BI and Visualization"]
salarios_Data_Quality_and_Operations <- datos_filtrados$salary_in_usd_thousands[datos_filtrados$job_category == "Data Quality and Operations"]
salarios_Machine_Learning_and_AI <- datos_filtrados$salary_in_usd_thousands[datos_filtrados$job_category == "Machine Learning and AI"]
salarios_Cloud_and_Database <- datos_filtrados$salary_in_usd_thousands[datos_filtrados$job_category == "Cloud and Database"]
salarios_Data_Analysis <- datos_filtrados$salary_in_usd_thousands[datos_filtrados$job_category == "Data Analysis"]
salarios_Data_Management_and_Strategy <- datos_filtrados$salary_in_usd_thousands[datos_filtrados$job_category == "Data Management and Strategy"]
salarios_Leadership_and_Management <- datos_filtrados$salary_in_usd_thousands[datos_filtrados$job_category == "Leadership and Management"]







#####Histogramas del salario


# Crear histogramas para cada categoría de trabajo


hist(salarios_data_engineering, main = "Salarios en Data Engineering", xlab = "Salario en USD", col = "skyblue", freq = FALSE)
lines(density(salarios_data_engineering), col = "red", lwd = 2)

hist(salarios_Architecture_and_Modeling, main = "Salarios en Data Architecture and Modeling", xlab = "Salario en USD", col = "skyblue", freq = FALSE)
lines(density(salarios_Architecture_and_Modeling), col = "red", lwd = 2)

hist(salarios_Data_Science_and_Research, main = "Salarios en Data Science and Research", xlab = "Salario en USD", col = "skyblue", freq = FALSE)
lines(density(salarios_Data_Science_and_Research), col = "red", lwd = 2)

hist(salarios_BI_and_Visualization, main = "Salarios en Data Engineering", xlab = "Salario en USD", col = "skyblue", freq = FALSE) 
lines(density(salarios_BI_and_Visualization), col = "red", lwd = 2)

hist(salarios_Data_Quality_and_Operations, main = "Salarios en Data Engineering", xlab = "Salario en USD", col = "skyblue", freq = FALSE)
lines(density(salarios_Data_Quality_and_Operations), col = "red", lwd = 2)

hist(salarios_Machine_Learning_and_AI, main = "Salarios en Data Engineering", xlab = "Salario en USD", col = "skyblue", freq = FALSE)
lines(density(salarios_Machine_Learning_and_AI), col = "red", lwd = 2)

hist(salarios_Cloud_and_Database, main = "Salarios en Data Engineering", xlab = "Salario en USD", col = "skyblue", freq = FALSE)
lines(density(salarios_Cloud_and_Database), col = "red", lwd = 2)

hist(salarios_Data_Analysis, main = "Salarios en Data Engineering", xlab = "Salario en USD", col = "skyblue", freq = FALSE)
lines(density(salarios_Data_Analysis), col = "red", lwd = 2)

hist(salarios_Data_Management_and_Strategy, main = "Salarios en Data Engineering", xlab = "Salario en USD", col = "skyblue", freq = FALSE)
lines(density(salarios_Data_Management_and_Strategy), col = "red", lwd = 2)

hist(salarios_Leadership_and_Management, main = "Salarios en Data Engineering", xlab = "Salario en USD", col = "skyblue", freq = FALSE)
lines(density(salarios_Leadership_and_Management), col = "red", lwd = 2)







#############Gráficos de torta

# Contar la frecuencia de cada tipo de configuración de trabajo
frecuencia_work_setting <- table(datos_filtrados$work_setting)


# Calcular porcentajes
porcentajes <- round(prop.table(frecuencia_work_setting) * 100, 2)

# Crear el diagrama de pastel
pie(frecuencia_work_setting,
    main = "Distribución de Configuraciones de Trabajo",
    labels = paste(names(porcentajes), "\n", porcentajes, "%"),
    col = rainbow(length(frecuencia_work_setting)))  # Colores del arco iris para cada sector


# Tamaño de la compañía

# Calcular porcentajes
porcentajes_larges_company <- round(prop.table(frecuencia_larges_company) * 100, 2)

# Crear el diagrama de pastel
pie(frecuencia_larges_company,
    main = "Distribución del Tamaño de la Compañía",
    labels = paste(names(porcentajes_larges_company), "\n", porcentajes_larges_company, "%"),
    col = rainbow(length(frecuencia_larges_company)))  # Colores del arco iris para cada sector











# Crear diagramas de cajas y alambres para cada categoría de trabajo y su salario correspondiente
porcentajes_larges_company <- round(prop.table(frecuencia_larges_company) * 100, 2)


# Crear un gráfico de caja para los salarios por categoría de trabajo
p1 <- ggplot(data = datos_filtrados, aes(x = job_category, y = salary_in_usd_thousands)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Salarios por Categoría de Trabajo",
       x = "Categoría de Trabajo",
       y = "Salario en miles de USD") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas del eje x si es necesario

# Mostrar el gráfico de caja
print(p1)


##Caja de alambre global

boxplot(salary_in_usd_thousands)





summarytools::descr(salary_in_usd_thousands)



