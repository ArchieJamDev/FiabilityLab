# FiabilityLab – Documento Maestro v1.0
## Integración Library + Bibliografía + Estructura del Proyecto

---

## 1. Propósito del documento

Este documento consolida la versión 1.0 del sistema FiabilityLab aplicado a
Jamovi, integrando:

- Estructura conceptual del proyecto
- Sistema de Library (biblioteca de contenidos de confiabilidad)
- Sistema de Bibliografía unificada
- Flujo pedagógico orientado a la evaluación de la confiabilidad de
  instrumentos de medición
- Mapa de módulos de análisis, presentes y planificados
- Diseño modular para uso académico

El objetivo es servir como fuente única de referencia para estudiantes,
docentes e investigadores, centralizando contenido teórico, práctico y
fuentes externas sobre confiabilidad e instrumentos de medición.

FiabilityLab es el segundo módulo de la suite Lab, hermano de
AssumptionsLab, y hereda de él su filosofía pedagógica y su arquitectura de
cuatro capas (`a.yaml` / `u.yaml` / `r.yaml` / `b.R`).

---

## 2. Filosofía del sistema

FiabilityLab se basa en tres principios fundamentales.

### 2.1 Aprendizaje basado en comprensión
El investigador no solo obtiene un coeficiente de confiabilidad, sino que
comprende qué tipo de consistencia está midiendo (interna, temporal, entre
jueces, de constructo) y por qué ese coeficiente —y no otro— es el
apropiado para su diseño.

### 2.2 Transparencia de supuestos
Cada coeficiente de confiabilidad está acompañado de:
- Supuestos (tau-equivalencia, unidimensionalidad, independencia entre
  jueces, invarianza factorial, etc.)
- Condiciones de uso (nivel de medida, número de jueces/ocasiones,
  estructura factorial)
- Interpretación correcta
- Riesgos de mal uso (p. ej., usar Alfa de Cronbach en una escala
  multidimensional, o interpretar Kappa sin revisar la prevalencia)

### 2.3 Integración teoría-práctica
El aprendizaje ocurre mediante la conexión directa entre:
- Conceptos psicométricos (Teoría Clásica de los Tests, Teoría de la
  Generalizabilidad, Teoría de Respuesta al Ítem)
- Ejecución en Jamovi
- Interpretación aplicada de resultados

---

## 3. Estructura del sistema

El sistema se organiza en tres capas:

### 3.1 Library (Biblioteca de contenidos)
La Library contiene todos los recursos educativos del sistema. No calcula:
explica.

Incluye:
- Conceptos fundamentales de confiabilidad
- Marcos teóricos (TCT, Teoría de la Generalizabilidad, TRI)
- Explicaciones de supuestos por coeficiente
- Guías de interpretación
- Ejemplos aplicados
- Errores comunes

Cada entrada en la Library debe incluir:
- Título
- Descripción clara
- Nivel de dificultad
- Relación con otras entradas
- Referencias bibliográficas asociadas

**Contrato de admisión:** ningún coeficiente se implementa en un módulo de
análisis sin que exista antes su ficha completa en Library y sus citas en
Bibliography. La Library y la Bibliografía no son un anexo que se completa
al final: son la condición de entrada de cada nueva pieza de código.

---

### 3.2 Módulos de análisis (Jamovi)

Cada módulo se organiza por **diseño de confiabilidad** (qué tiene el
investigador entre manos), no por familia estadística suelta, y dentro de
cada uno se cubren los niveles de medida aplicables. Cada módulo incluye:

- Objetivo del análisis
- Diseño de datos requerido (ítems, jueces, ocasiones, estructura factorial)
- Supuestos estadísticos
- Procedimiento en Jamovi
- Panel de discordancia entre coeficientes (cuando aplica)
- Interpretación de resultados
- Errores frecuentes

**Mapa de módulos:**

| Módulo | Estado | Contenido |
|---|---|---|
| **Internal Consistency** (CTT) | Implementado | Alfa, Alfa ordinal, Omega, Omega jerárquico (bifactor), GLB, Split-half, Guttman λ, KR-20/21, análisis de ítems |
| **Advanced Reliability (SEM)** | Implementado | Análisis de menú propio (`advancedReliability`), no una pestaña dentro de Internal Consistency como se planeó originalmente. AVE, Confiabilidad Compuesta (CR), Coeficiente H (Hancock & Mueller), Omega de 2do orden/jerárquico, HTMT — vía `lavaan`/`semTools` |
| **Measurement Invariance** | Implementado | Análisis de menú propio (`measurementInvariance`). Secuencia configural/métrica/escalar/estricta multi-grupo, criterios LRT y ΔCFI, sobre el mismo modelo de medida confirmatorio que Advanced Reliability |
| **Inter-Rater Agreement** | Implementado | Kappa (Cohen/Fleiss), Gwet AC1/AC2, ICC (formas 1,1 / 2,1 / 3,1), Alfa de Krippendorff, W de Kendall |
| **Temporal Stability** | Fase 2 (no iniciado) | Correlación test-retest, ICC longitudinal, límites de acuerdo (Bland-Altman), Error Estándar de Medición (SEM), Índice de Cambio Confiable (RCI) |
| **Classification Consistency** | Fase 4 (exploratorio, sin fecha) | Consistencia y precisión de decisiones de corte (Livingston-Lewis, Subkoviak) |
| **Generalizability Theory** | Fase 4 (exploratorio, sin fecha) | Descomposición de varianza G-study/D-study |
| **Fiability Library** | Crece con cada fase | Glosario metodológico completo |
| **Bibliography** | Crece con cada fase | Base de citas por tema |

Ver `ARCHITECTURE.md` para el mapa completo y siempre actualizado de
módulos, su estado y su arquitectura interna.

---

### 3.3 Bibliografía unificada
La Bibliografía respalda cada entrada de la Library y cada coeficiente
implementado con fuentes primarias verificables, organizadas por tema
(TCT, acuerdo entre jueces, Teoría de la Generalizabilidad, TRI, y los
temas que se añadan en Fases 2–4: estabilidad temporal, confiabilidad
compuesta, consistencia de clasificación).

---

## 4. Sistema de Bibliografía

### 4.1 Estructura
Las referencias se organizan por tema metodológico (`ctt`, `irr`,
`gtheory`, `irt`, y los temas nuevos que abran las Fases 2–4), no por
módulo de Jamovi, para que una misma fuente pueda respaldar varias
entradas de Library y varios módulos de análisis.

### 4.2 Integración con Library
Cada entrada de Library enlaza a las referencias de su tema. Ninguna
afirmación metodológica en Library o en un reporte de análisis debe quedar
sin fuente citable.

### 4.3 Objetivo pedagógico
Enseñar al investigador a leer la literatura psicométrica original, no
solo a confiar en el número que produce el software.

---

## 5. Flujo pedagógico del investigador

1. El investigador identifica su diseño de datos (¿un instrumento con
   varios ítems? ¿varios jueces? ¿varias ocasiones? ¿una estructura
   factorial de 2do orden?).
2. Library le indica qué módulo y qué coeficiente(s) corresponden a ese
   diseño, y qué supuestos debe verificar.
3. El módulo de análisis calcula el/los coeficiente(s) y, cuando hay más
   de uno disponible, muestra el panel de discordancia.
4. El motor de interpretación responde las cuatro preguntas: ¿qué tan
   confiable es la medida?, ¿por qué?, ¿qué implica?, ¿qué debe hacer
   ahora?
5. Bibliography respalda cada afirmación con fuentes primarias.

---

## 6. Principios de diseño del contenido

- Ningún coeficiente se presenta sin sus supuestos explícitos.
- Toda discordancia entre coeficientes se explica, nunca se oculta.
- El contenido de Library se escribe primero en español e inglés a la vez;
  no hay versión "provisional" en un solo idioma.
- Cada afirmación metodológica lleva cita en Bibliography.
- Todo estimador nuevo debe tener, antes de considerarse estable, un
  registro de validación Monte Carlo en `validation/` (sesgo, cobertura,
  robustez bajo condiciones realistas: distribuciones no normales,
  datos faltantes, muestras pequeñas, diseños de jueces desbalanceados).

---

## 7. Versión del sistema

**v1.0 — estado actual (2026-09):**
- Internal Consistency: implementado.
- Inter-Rater Agreement: implementado.
- Advanced Reliability (SEM): implementado, como análisis de menú propio
  (`advancedReliability`), no como pestaña dentro de Internal Consistency.
- Measurement Invariance: implementado, como análisis de menú propio
  (`measurementInvariance`).
- Fiability Library / Bibliography: esqueleto mínimo, en expansión activa
  siguiendo el contrato de admisión.
- Temporal Stability: planificado, no iniciado (Fase 2).
- Classification Consistency, Generalizability Theory: exploratorios, sin
  fecha comprometida (Fase 4).

Cada nueva fase debe actualizar este documento y `ARCHITECTURE.md` como
parte de su definición de "hecho" — no al final, sino antes de escribir el
primer `.yaml`.

---

## 8. Nota final

FiabilityLab no es una calculadora de coeficientes de confiabilidad: es un
sistema pedagógico que usa el cálculo de esos coeficientes como vehículo
para enseñar psicometría aplicada. La Library y la Bibliografía son el
núcleo del sistema; los módulos de análisis son su superficie ejecutable.
