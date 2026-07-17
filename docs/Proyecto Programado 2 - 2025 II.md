

Instituto Tecnológico de Costa Rica
Ingeniería en Computación
Lenguajes de Programación
Semestre II, 2025
## Profesor: Allan Rodríguez Dávila

## Proyecto Programado #2
Análisis de Ventas
## Introducción
En la actualidad, el análisis funcional de datos permite modelar información compleja de manera
declarativa y expresiva. El Sistema de Análisis de Datos de Ventas en Haskell se presenta como una
solución orientada al procesamiento y análisis de datos de ventas a partir de archivos en formato
JSON. El sistema estará diseñado bajo el paradigma funcional, haciendo uso de tipos algebraicos,
funciones de orden superior, procesamiento con listas y persistencia de datos en archivos.

Se espera que el proyecto siga los principios del paradigma funcional, como la inmutabilidad de
datos y el uso de funciones de orden superior.

Además, el proyecto busca someter al estudiante a entornos y realidades lo más cercano a la
realidad, con el objetivo de generar un aprendizaje efectivo.
Proyecto a desarrollar
El código deberá implementarse en Haskell utilizando librerías estándar y aeson para el manejo de
JSON. El programa funcionará en consola, presentando un menú interactivo.

El programa deberá desplegar un menú al usuario con las opciones que dispone el sistema (el
menú debe mostrarse recurrentemente después de cada opción realizada -a excepción del salir- y
la opción de salir.
Se habilitará un menú donde se colocarán las siguientes funcionalidades:
- Importación de datos
- Procesamiento de datos
- Análisis de datos
- Análisis temporal

## • Estadísticas
## • Salir

Importación de Datos
El sistema debe permitir dar mantenimiento a la información de los datos de ventas, se debe
permitir incluir en lote, leyendo un archivo json; la información será: identificador de venta, fecha
(formato yyyy-mm-dd), identificador de producto, descripción de producto, categoría de producto,
cantidad, precio unitario y total de línea. El usuario debe indicar una ruta de archivo que
contendrá los registros por incluir, con la siguiente forma:

## [
## {
## "venta_id": 1,
## "fecha": "2023-01-04",
## "producto_id": 109,
"producto_nombre": "C\u00e1mara",
"categoria": "Electr\u00f3nica",
## "cantidad": 2,
## "precio_unitario": 250.0,
## "total": 500.0
## },
## {
## "venta_id": 2,
## "fecha": "2023-01-12",
## "producto_id": 106,
"producto_nombre": "Tel\u00e9fono",
"categoria": "Electr\u00f3nica",
## "cantidad": 1,
## "precio_unitario": 500.0,
## "total": 500.0
## }
## ]
Los datos deben ser cargados en memoria utilizando estructuras adecuadas para su posterior
análisis. Se pueden hacer múltiples cargas, que se consideran como un “append” a la información
preexistente. Se debe reportar las líneas que no se incluyeron, en caso de que falte algún atributo
(identificador de venta, fecha (formato yyyy-mm-dd), identificador de producto, descripción de
producto o categoría de producto).

Se debe utilizar la librería aeson.






Procesamiento de datos
El sistema debe permitir funciones para procesar y limpiar los datos cargados, realizando lo
siguiente:
a. Completar datos faltantes en cantidad y precio unitario. Se utiliza diferentes técnicas para
los atributos como la moda, la media (promedio) o mediana.
b. Eliminar datos duplicados.

Para cada uno de los 2 procesos se debe indicar los registros alterados por cada técnica
(identificador de venta).

Utilizar funciones de orden superior.

Análisis de datos
El sistema deberá permitir al usuario realizar diferentes análisis estadísticos sobre los datos. El
sistema dispondrá de las siguientes opciones:
- Total de ventas (suma de los importes de todas las ventas).
- Total de ventas mensuales y anuales.
- Promedio de ventas por categoría por año.

## Análisis Temporal
El sistema deberá permitir al usuario visualizar un análisis temporal sobre los datos y podrá
visualizar alguna de las siguientes opciones:
- Mes con mayor venta (total) y día de la semana más activo (transacciones diarias,
cantidad).
- Calcular la tasa de crecimiento o decrecimiento de las ventas en un trimestre específico
(investigar fórmula).
- Generar un resumen de ventas por trimestre.

Búsqueda específica
Debe disponer de búsqueda de ventas por rango de fechas.

## Estadísticas
Se deberán mostrar las siguientes estadísticas (indican el código o letra de esta):
A. Top 5 de categorías con mayores ventas (monto).
B. Producto más vendido (por cantidad).
C. Categoría con menor participación (cantidad).
D. Resumen general:
- Cantidad de ventas por categoría.
- Venta más alta y más baja.
- Categoría con mayor variedad de productos vendidos.
Cada reporte de estadísticas debe exportarse en formato csv o json.

Otros aspectos
a. Validar que precios y cantidades sean mayores a cero.
b. Reportar las inconsistencias.
## Salir
Los datos del sistema no son persistentes, toda la información indicada anteriormente se libera, se
hace un manejo de estructuras en memoria. Al ser información volátil, se valorará la usabilidad
del sistema, libre de fallas.

## Puntos Extra
Se darán 2.5 puntos adicionales al entregar a más tardar el miércoles 01 de octubre a las 11:55:55
PM el Documento de Requerimientos, ver plantilla suministrada en el Tec Digital. Debe subirse en
la documentación llamada “Proyecto Programado II (archivos adicionales)” debajo de la carpeta de
“Proyectos”.

Se darán 7.5 puntos adicionales si se maneja persistencia de datos, es decir que al salir del Menú
Principal y volver a entrar al Menú Principal se mantengan los datos. La persistencia debe
realizarse por medio de un medio físico (archivo o base de datos). Con la carga de datos, si se
carga por segunda vez una información, se anexará lo nuevo y se mantendrá lo anterior, sin
permitir repetidos.
Aspectos técnicos
El proyecto deberá estar escrito en el lenguaje de programación Haskell. En caso de requerir
librerías adicionales para compilar y ejecutar el programa, deberán validarlo con el profesor, ya
que de lo contrario se descontarán puntos en la evaluación.

Deberán utilizar el sistema de control de versiones GitHub, el repositorio deberá ser público o
incluir al profesor en el control de acceso de este. Se debe demostrar un desarrollo incremental de
la solución.

Se valorará el aporte generado por cada estudiante, la gestión del tiempo, considerando, entre
otras cosas, los commit generados por cada uno. Por lo que el puntaje obtenido por cada uno de
los estudiantes puede ser diferenciado.
## Documentación
La documentación es un aspecto de gran importancia en el desarrollo de programas,
especialmente en tareas relacionadas con el mantenimiento de estos.

Para la documentación interna, deberán incluir comentarios descriptivos para cada función, con
sus entradas, salidas, restricciones y objetivo.

La documentación externa deberá incluir:
## 1. Portada.
- Manual de usuario: instrucciones de compilación, ejecución y uso.
- Pruebas de funcionalidad: incluir screenshots.
- Descripción del problema.
- Diseño del programa: decisiones de diseño, algoritmos más importantes creados.
- Librerías usadas: manejo entradas-salidas, archivos, etc.
- Análisis de resultados: objetivos alcanzados, objetivos no alcanzados, y razones por las
cuales no se alcanzaron los objetivos (en caso de haberlos).
- Justificación de toma de decisiones:
a. Explicar las funciones de orden superior utilizadas.
b. Explicar cómo se encadenan las funciones.
- Bitácora (autogenerada en git, commit por usuario incluyendo comentario)
Forma de trabajo
El trabajo se debe realizar en grupos de tres personas. No se permite el trabajo individual.
## Evaluación
La evaluación se va a centrar en dos elementos: programación y documentación.

El proyecto programado tiene un valor de 10% de la nota final, en el rubro de Proyectos.

Desglose de la evaluación del proyecto programado:
- Documentación interna 2 ptos.
- Documentación externa 8 ptos.
- Funcionalidad 80 ptos (ver detalle en Proyecto a Desarrollar)
- Revisión del proyecto (según completitud del proyecto y gestión del tiempo) 5 ptos.
- Hora de Entrega 5 ptos.

Aspectos administrativos
Debe crear un archivo .zip (“PP2_Integrante1_Integrante2.zip”) que contenga únicamente un
archivo info.txt y 2 carpetas llamadas documentacion y programa, en la primera deberá incluir el

documento de word o pdf solicitado y en la segunda los archivos y carpetas necesarias para la
implementación de este proyecto programado, y/o link en git del repositorio. El archivo info.txt
debe contener la siguiente información (cualidades):
a. Nombre del curso
b. Número de semestre y año lectivo
c. Nombre de los Estudiantes
d. Número de carnet de los estudiantes
e. Número de proyecto programado
f. Fecha de entrega
g. Estatus de la entrega (debe ser CONGRUENTE con la solución entregada):
[Deplorable|Regular|Buena|MuyBuena|Excelente|Superior]
## Entrega
Deberá subir el archivo antes mencionado al TEC Digital en el curso de LENGUAJES DE
PROGRAMACIÓN GR 60, en la asignación llamada “P2” debajo del rubro de “Proyectos”.  En la
evaluación del Proyecto el rubro de “Hora de Entrega” valdrá por 5 puntos de la nota total del
proyecto, según la siguiente escala:
a. Si se entrega antes de las 11:55:55 PM del viernes 17 de octubre de 2025, 5 puntos.
b. Si se entrega antes de las 11:55:55 AM del sábado 18 de octubre de 2025 2.5 puntos.
c. Si se entrega antes de las 11:55:55 PM del sábado 18 de octubre de 2025, 0 puntos.
NO SE ACEPTARÁN trabajos que contengan “commits” posterior a esta fecha.

Cada estudiante deberá participar en la revisión del proyecto, demostrando tanto la funcionalidad
como su autoría. Durante esta revisión, se podrán solicitar ajustes en tiempo real; la incapacidad
de realizarlos se considerará como una falta de dominio del programa (código).

