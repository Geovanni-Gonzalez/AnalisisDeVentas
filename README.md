# Sistema de Análisis de Ventas (Haskell)

![Console App](https://img.shields.io/badge/Type-Console%20Application-blue?style=for-the-badge)
![Haskell](https://img.shields.io/badge/Language-Haskell-purple?style=for-the-badge)
![Stack](https://img.shields.io/badge/Tool-Stack-blueviolet?style=for-the-badge)
![Status](https://img.shields.io/badge/Status-Completed-success?style=for-the-badge)

Aplicación profesional de consola para el análisis, procesamiento y visualización de datos de ventas. Desarrollada en **Haskell**, esta herramienta está diseñada para manejar grandes volúmenes de datos con robustez y eficiencia, ofreciendo una experiencia de usuario moderna y limpia.

![Screenshot](https://raw.githubusercontent.com/Geovanni-González/AnalisisDeVentas/main/screenshots/principalImage.jpg)

## 🚀 Características Principales

### 📊 Análisis de Datos

* **KPIs Financieros**: Cálculo inmediato de totales, promedios anuales y ventas mensuales.
* **Inteligencia de Negocio**: Identificación de productos estrella y categorías con menor participación.
* **Análisis Temporal**: Detección de tendencias de crecimiento trimestral y días de la semana con mayor actividad ("Días Pico").

### 🛠️ Limpieza y Procesamiento

* **Imputación Estadística**: Relleno inteligente de datos faltantes (precios o cantidades a cero) utilizando técnicas de Media, Mediana o Moda.
* **Deduplicación**: Algoritmos eficientes para eliminar registros duplicados garantizando la integridad de la base de datos.
* **Importación Segura**: Sistema de carga de lotes (`Lote.json`) con validación automática de duplicados por ID.

### 🎨 Experiencia de Usuario (CLI)

* **Interfaz Moderna**: Menús interactivos con navegación fluida y limpieza de pantalla automática.
* **Visualización Mejorada**: Uso de códigos de color ANSI y bordes ASCII seguros para una presentación nítida en cualquier terminal Windows.
* **Feedback Inmediato**: Mensajes de éxito y error claramente diferenciados.

## 💻 Tech Stack

* **Lenguaje**: Haskell (GHC 9.10.3)
* **Gestor de Proyectos**: Stack
* **Testing**: Hspec (Suite de pruebas unitarias robusta)
* **Dependencias Clave**:
  * `aeson` & `aeson-pretty`: Manejo eficiente de JSON.
  * `ansi-terminal`: Control de colores y cursor en consola.
  * `time`: Manejo preciso de fechas y horas.

## ⚙️ Instalación y Ejecución

### Prerrequisitos

* [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) instalado.

### Pasos

1. **Clonar el repositorio**:

    ```bash
    git clone https://github.com/Geovanni-Gonzalez/AnalisisDeVentas.git
    cd AnalisisDeVentas/programa
    ```

2. **Compilar el proyecto**:

    ```bash
    stack build
    ```

3. **Ejecutar la aplicación**:

    ```bash
    stack run
    ```

4. **Ejecutar Pruebas**:

    ```bash
    stack test
    ```

## 📁 Estructura del Proyecto

* `app/Main.hs`: Punto de entrada y lógica de navegación (UI).
* `src/`:
  * `Venta.hs`: Modelo de datos y lógica de negocio core.
  * `Procesamiento.hs`: Módulo de limpieza e imputación estadística.
  * `Estadisticas.hs`: Cálculos de agregación y reportes.
  * `Interfaz.hs`: Capa de presentación (Colores, ASCII Art).
  * `Utilidades.hs`: Manejo de archivos I/O y validaciones.
* `test/`: Suite de pruebas Hspec.

---
© 2026 Sistema de Análisis de Ventas - Haskell Project.
