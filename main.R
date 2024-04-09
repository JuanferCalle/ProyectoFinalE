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

attach(datos_filtrados)
names(datos_filtrados)

# Contar la frecuencia de cada tipo de empleo
frecuencia_empleos <- table(datos_filtrados$job_tittle)

# Contar la cantidad de datos en la columna "job_tittle"
cantidad_datos <- nrow(datos_filtrados)
print(cantidad_datos)

# Contar la frecuencia de cada tipo de empleo
frecuencia_empleos <- table(datos_filtrados$job_tittle)


table(job_title)
table(job_category)
table(employee_residence)
table(work_setting)
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
salarios_data_engineering <- datos_filtrados$salary_in_usd[datos_filtrados$job_category == "Data Engineering"]
salarios_Architecture_and_Modeling <- datos_filtrados$salary_in_usd[datos_filtrados$job_category == "Data Architecture and Modeling"]
salarios_Data_Science_and_Research <- datos_filtrados$salary_in_usd[datos_filtrados$job_category == "Data Science and Research"]
salarios_BI_and_Visualization <- datos_filtrados$salary_in_usd[datos_filtrados$job_category == "BI and Visualization"]
salarios_Data_Quality_and_Operations <- datos_filtrados$salary_in_usd[datos_filtrados$job_category == "Data Quality and Operations"]
salarios_Machine_Learning_and_AI <- datos_filtrados$salary_in_usd[datos_filtrados$job_category == "Machine Learning and AI"]
salarios_Cloud_and_Database <- datos_filtrados$salary_in_usd[datos_filtrados$job_category == "Cloud and Database"]
salarios_Data_Analysis <- datos_filtrados$salary_in_usd[datos_filtrados$job_category == "Data Analysis"]
salarios_Data_Management_and_Strategy <- datos_filtrados$salary_in_usd[datos_filtrados$job_category == "Data Management and Strategy"]
salarios_Leadership_and_Management <- datos_filtrados$salary_in_usd[datos_filtrados$job_category == "Leadership and Management"]





# Crear un gráfico de dispersión de los salarios
plot(x = 1:length(salarios_data_engineering), 
     y = salarios_data_engineering,
     main = "Salarios de Data Engineering",
     xlab = "Data Engineering",
     ylab = "Salario en USD")

plot(x = 1:length(salarios_Architecture_and_Modeling), 
     y = salarios_Architecture_and_Modeling,
     main = "Salarios de Architecture and modeling",
     xlab = "Architecture and modeling",
     ylab = "Salario en USD")

plot(x = 1:length(salarios_Data_Science_and_Research), 
     y = salarios_Data_Science_and_Research,
     main = "Salarios de Data science and research",
     xlab = "Data science and research",
     ylab = "Salario en USD")

plot(x = 1:length(salarios_BI_and_Visualization), 
     y = salarios_BI_and_Visualization,
     main = "Salarios BI and visualization",
     xlab = "BI and visualization",
     ylab = "Salario en USD")

plot(x = 1:length(salarios_Data_Quality_and_Operations), 
     y = salarios_Data_Quality_and_Operations,
     main = "Salarios de Data Quality and operations",
     xlab = "Data quality and operations",
     ylab = "Salario en USD")

plot(x = 1:length(salarios_Machine_Learning_and_AI), 
     y = salarios_Machine_Learning_and_AI,
     main = "Salarios Machine learing and AI",
     xlab = "Machine learning and AI",
     ylab = "Salario en USD")

plot(x = 1:length(salarios_Cloud_and_Database), 
     y = salarios_Cloud_and_Database,
     main = "Salarios de Cloud and database",
     xlab = "Cloud and data base",
     ylab = "Salario en USD")

plot(x = 1:length(salarios_Data_Analysis), 
     y = salarios_Data_Analysis,
     main = "Salarios de Data analysis",
     xlab = "Data analysis",
     ylab = "Salario en USD")

plot(x = 1:length(salarios_Data_Management_and_Strategy), 
     y = salarios_Data_Management_and_Strategy,
     main = "Salarios de Data management and strategy",
     xlab = "Data management and strategy",
     ylab = "Salario en USD")

plot(x = 1:length(salarios_Leadership_and_Management), 
     y = salarios_Leadership_and_Management,
     main = "Salarios de Leadership and management",
     xlab = "Leadership and management",
     ylab = "Salario en USD")



#####Gráfico de dispersión de todos los job_category con su salario en usd




plot(x = 1:length(salary_in_usd),
     salary_in_usd,
     main = "disperisión de los salarios",
     xlab = "Cantidad de datos",
     ylab = "salario en dolrares")










#############Gráficos de torta

# Contar la frecuencia de cada tipo de configuración de trabajo
frecuencia_work_setting <- table(datos_filtrados$work_setting)

# Crear el diagrama de pastel, tipo de trabajo
pie(frecuencia_work_setting,
    main = "Distribución de Configuraciones de Trabajo",
    labels = paste(names(frecuencia_work_setting), "\n", frecuencia_work_setting),
    col = rainbow(length(frecuencia_work_setting)))  # Colores del arco iris para cada sector

# Tamaño de la compañía

frecuencia_larges_company <- table(datos_filtrados$company_size)

pie(frecuencia_larges_company,
    main = "distribución del tamaño de la compañía",
    labels = paste(names(frecuencia_larges_company), "\n", frecuencia_larges_company),
    col =rainbow(length(frecuencia_larges_company))
    )











# Crear diagramas de cajas y alambres para cada categoría de trabajo y su salario correspondiente
p1 <- ggplot(data = datos_filtrados, aes(x = job_category, y = salary_in_usd)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Salarios por Categoría de Trabajo",
       x = "Categoría de Trabajo",
       y = "Salario en USD") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas del eje x si es necesario

# Mostrar los diagramas de cajas y alambres
print(p1)






prop.table(Tabla)

hist(salary_in_usd, col=c("#0000CD", "#FF4040", "#76EEC6", "#FFD39B", "#8EE5EE", "#7FFF00", "#EE7621", "#FFB90F"))



hist(salary_in_usd, freq = FALSE)
lines(density(salary_in_usd), col = "blue")

summary(salary_in_usd)

summarytools::descr(salary_in_usd)

boxplot(salary_in_usd)

