# 📘 Índice de Digitalización Comunal de NUDOS (IDC) – 2025

### Descripción general

Este repositorio contiene los procedimientos, scripts y documentación necesarios para procesar y analizar el Índice de Digitalización Comunal (IDC) correspondiente al año 2025. El IDC es una herramienta desarrollada por el Núcleo Milenio NUDOS para evaluar el nivel de digitalización en las comunas de Chile, considerando variables como conectividad, infraestructura tecnológica municipal y uso de herramientas digitales en educación.

### Estructura del repositorio

El repositorio está organizado en las siguientes carpetas y archivos:

```
idc/
│
├── codebooks/                  # Documentos técnicos y manuales
│   └── codebook_idc_full.pdf   # Libro de códigos completo
│
├── data/                       # Datos procesados y fuentes
│   ├── proc_data/              # Datos procesados listos para análisis
│   └── raw_data/               # Datos originales sin procesar
│
├── documents/                  # Documentación adicional
│
├── plots_tables_images/        # Gráficos, tablas e imágenes generadas o importadas
│
├── reports/                    # Reportes generados para documentar decisiones
│
├── scripts/                    # Scripts de procesamiento y análisis
│   ├── 01_data_processing.R    # Script principal de procesamiento de datos
│   └── 02_analysis.R           # Script de análisis de datos
│
├── .gitignore                  # Archivos y directorios ignorados por Git
├── README.md                   # Este archivo
└── idc.Rproj                   # Proyecto de RStudio
```

### Descripción de las carpetas

codebooks/: Contiene el libro de códigos que describe las variables, su origen y las transformaciones realizadas. Es esencial para comprender la estructura y el significado de los datos.

data/:

  proc_data/: Almacena los datos procesados listos para su análisis. Incluye archivos en formatos como .rds, .sav y .xlsx.
  
    private_data/: Datos para uso interno de NUDOS (Información en bruto).
    
    public_data/: Datos depurados para publicar y compartir.

  raw_data/: Contiene los datos originales sin procesar, utilizados como base para el procesamiento.

documents/: Incluye documentación adicional relevante para el proyecto, como manuales y validaciones de datos.

plots_tables_images/: Carpeta donde se guardan los gráficos, tablas e imágenes generadas o importadas durante el análisis.

reports/: Contiene los reportes sobre la toma de decisiones para la conformación de cada una de las dimensiones del índice, disponibles en formato HTML y PDF.

scripts/:
  
  a1-proc-data.R: Procesamiento de datos de conectividad.
  
  b1-proc-data.R: Procesamiento de datos de municipio digital.
  
  c1-proc-individual-level.R: Procesamiento de nivel individual de datos SIMCE 2025.
  
  c2-proc-communal-level.R: Procesamiento de nivel comunal de datos SIMCE 2025.
  
  c3-merge-long-data.R: Fundición de bases 2024 y 2025 de datos educativos.
  
  d1-proc-full-idc.R: Conformación de la base final IDC 2025 (incluyendo resultados de 2024).

### Archivos clave

idc.Rproj: Proyecto de RStudio que incluye la configuración y los scripts necesarios para ejecutar el análisis.

README.md: Este archivo, que proporciona una visión general del repositorio y su estructura.

data/proc_data/public_data/2025_idc_v2.sav: Base de datos pública IDC 2025

### Flujo de procesamiento de datos

Obtención de datos: Los datos originales se encuentran en la carpeta data/raw_data/. Estos datos provienen de diversas fuentes, como encuestas y registros administrativos.

Procesamiento de datos: Los scripts en scripts/ se encargan de limpiar y transformar los datos, generando nuevas variables y asegurando la calidad de la información. El resultado se guarda en la carpeta data/proc_data/. Los scripts están especificados por letra para cada dimensión, y por número para indicar la secuencia del procesamiento.

Generación de reportes: Una vez procesados, los datos se analizan en los reportes hechos en .qmd y .pdf alojados en la carpeta reports/. Este script genera estadísticas descriptivas, índices y visualizaciones sobre la muestra de comunas, los puntajes y otras informaciones varias. Además se señalan las justificaciones de las decisiones tomadas para confeccionar la versión final de los datos.

### Reportes

[Reporte conectividad](https://github.com/milenio-nudos/idc/blob/main/reports/2025-a-acceso-report.pdf)

[Reporte municipio digital](https://milenio-nudos.github.io/idc/reports/2025-b-politico-report.html)

[Reporte adopción digital educativa](https://milenio-nudos.github.io/idc/reports/2025-c-educative-report.html)

### Descargar Base final

[Versión excel](https://github.com/milenio-nudos/idc/blob/main/data/proc_data/public_data/2025_idc_v2.xlsx)
[Versión spss](https://github.com/milenio-nudos/idc/blob/main/data/proc_data/public_data/2025_idc_v2.sav)
[Versión excel](https://github.com/milenio-nudos/idc/blob/main/data/proc_data/public_data/2025_idc_v2.rds)
