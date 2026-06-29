# 🌲 Taller de R

¡Bienvenidas al repositorio de nuestro taller de estudio de R! Este espacio fue creado por **Rena y Fran** (ambas ingenieras forestales) con el objetivo de aprender a programar juntas, automatizar análisis y dominar la ciencia de datos.

Nuestro mapa de ruta principal es leer y resolver el libro **"R for Data Science"** (2da Edición) de Hadley Wickham, Mine Çetinkaya-Rundel y Garrett Grolemund.

---

## 💻 Configuración de nuestro Entorno

Para trabajar de forma colaborativa y mantener nuestro historial de código a salvo en GitHub, configuramos nuestras computadoras en la **Sesión 1**:

*   **Rena (Windows):** Trabaja con [Git Bash](https://gitforwindows.org/) para el control de versiones y [VSCode](https://code.visualstudio.com/) como editor de código.
*   **Fran (Mac):** Utiliza la terminal nativa de macOS (con Git integrado) y [VSCode](https://code.visualstudio.com/) como editor principal.

---

## 📈 Bitácora de Sesiones

### 📑 Sesión 1: El "Whole Game" (Visualización y Transformación)
En nuestra primera sesión nos saltamos directo al ecosistema de **Tidyverse** (¡adiós R base por ahora!). Repasamos los fundamentos de manipulación y exploración de datos a través de los primeros capítulos del libro.

#### 1. Visualización de Datos (`ggplot2`)
Aprendimos que `ggplot2` funciona por **capas** (`+`). Practicamos con el set de datos de `palmerpenguins`:
*   Uso de `glimpse()` para reconocer filas, columnas y tipos de datos.
*   Mapeo de estéticas (`aes`) de color y forma según variables categóricas.
*   Inclusión de líneas de tendencia con `geom_smooth(method = "lm")`.
*   Paletas accesibles con `scale_color_colorblind()`.
*   Creación de sub-gráficos con `facet_wrap(~variable)`.
*   Guardar reportes visuales directo en el proyecto usando `ggsave()`.

> 💡 **Recurso extra:** Conocimos la iniciativa [TidyTuesday](https://github.com/rfordatascience/tidytuesday) para sacar sets de datos reales y seguir practicando.

#### 2. Fundamentos del flujo de trabajo
*   Reglas para nombrar variables (solo letras, números y `_` o `.`, pero nunca empezar con números).

#### 3. Transformación de Datos (`dplyr`)
Para este apartado usamos los datos de vuelos de Nueva York (`nycflights13`) y dominamos los atajos del **Pipe** (`%>%`):
*   💻 *Atajo Mac:* `Shift + Command + M`
*   💻 *Atajo Windows:* `Shift + Control + M`

**Herramientas clave de `dplyr` dominadas:**
*   `filter()`: Filtrar filas usando condiciones lógicas (`&`, `|`, `%in%`).
*   `arrange()`: Ordenar filas de menor a mayor (o de mayor a menor con `desc()`).
*   `distinct()`: Remover duplicados y quedarse con filas únicas (`.keep_all = TRUE` para no perder columnas).
*   `mutate()`: Crear nuevas columnas basadas en operaciones de columnas existentes (`.keep = "used"` para limpiar el reporte).
*   `select()`: Elegir o descartar columnas (usando rangos `year:day` o ayudantes como `where(is.character)`).
*   `rename()`: Cambiar el nombre de las variables.
*   `relocate()`: Mover columnas de posición usando `.before` o `.after`.
*   `group_by()` y `summarize()`: Agrupar datos y colapsarlos en estadísticas de resumen (¡clave usar `na.rm = TRUE` para evitar los valores perdidos!).
*   `slice_*()`: Extraer filas específicas por grupos (como `slice_max()` para buscar los retrasos más altos).
*   `.by`: El nuevo argumento para agrupar directamente dentro de `summarize()` sin tener que usar `group_by()` y luego `ungroup()`.

---

## 📦 Paquetes de Tidyverse utilizados hoy
Para verificar actualizaciones de estas herramientas usamos `tidyverse_update()`. Los 9 paquetes base que cargamos hoy mediante `library(tidyverse)` son:
1. `ggplot2` (Gráficos)
2. `dplyr` (Manipulación de datos)
3. `tidyr` (Ordenamiento de datos)
4. `readr` (Lectura de datos)
5. `purrr` (Programación funcional)
6. `tibble` (Dataframes modernos)
7. `stringr` (Cadenas de texto)
8. `forcats` (Factores/Variables categóricas)
9. `lubridate` (Fechas y horas)

---

## 🚀 Próximos pasos
*   [ ] Completar los ejercicios pendientes de transformación de datos.
*   [ ] Definir fecha para la **Sesión 2**.
