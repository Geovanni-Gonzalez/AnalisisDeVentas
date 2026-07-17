# IMPROVEMENT_ROADMAP — AnalisisDeVentas

Backlog priorizado. Impacto/Esfuerzo: Alto/Medio/Bajo.

## Quick Wins

| # | Mejora | Impacto | Esfuerzo | Prioridad |
|---|---|---|---|---|
| 1 | Commitear el untracking de `programa/build_log.txt` + `.gitignore` ampliado (aplicado en esta revisión) | Medio | Bajo | P0 |
| 2 | GitHub Topics: `haskell`, `functional-programming`, `stack`, `aeson`, `data-analysis` + descripción | Medio | Bajo | P1 |
| 3 | Corregir typos del README ("funciónes") y añadir badge de CI si falta | Bajo | Bajo | P2 |

## Mejoras técnicas

| # | Mejora | Impacto | Esfuerzo | Prioridad |
|---|---|---|---|---|
| 4 | Ampliar `Spec.hs` a `Procesamiento`/`Utilidades` (parsing de fechas inválidas, ventas vacías, empates en top-N) | Alto | Bajo | P1 |
| 5 | Property testing con QuickCheck (p. ej. `totalDeVentas xs + totalDeVentas ys == totalDeVentas (xs++ys)`) — evidencia funcional distintiva | Medio | Medio | P2 |
| 6 | `readMaybe`/`Either` para entradas corruptas en el parsing | Medio | Bajo | P2 |

## Mejoras arquitectónicas

| # | Mejora | Impacto | Esfuerzo | Prioridad |
|---|---|---|---|---|
| 7 | Dividir `Utilidades.hs` (288 LOC) por cohesión (formato, IO, fechas) | Bajo | Bajo | P3 |

## Mejoras de GitHub

Ya presentes: CI con `stack test`, LICENSE, enunciado en docs, lock file. Faltan: Topics (item 2).
