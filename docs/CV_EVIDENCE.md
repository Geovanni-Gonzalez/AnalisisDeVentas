# CV_EVIDENCE — AnalisisDeVentas

Primary Haskell evidence in the portfolio (larger and better-tooled than GestionDeFincaAgricola). Tests run in CI (`stack test`, GHC 9.10.2).

## Resume bullets (pick & adapt)

- Built a sales-analytics console application in Haskell (~1,600 LOC) that processes sales records into business statistics (totals, quarterly grouping, top-N categories/products) and emits reproducible JSON reports using aeson.
- Structured the codebase into pure domain/statistics/processing modules with an Hspec test suite executed in GitHub Actions CI via a pinned Stack/GHC toolchain.

## Skills matrix

| Skill | Evidence | Depth | Confidence |
|---|---|---|---|
| Haskell (records, JSON instances, module design) | `src/Venta.hs`, 5 modules | Medium | High |
| Functional data transformation & aggregation | `Estadisticas.hs`, `Procesamiento.hs` (grouping, top-N) | Medium | High |
| Haskell ecosystem (aeson, containers, time, ansi-terminal) | `package.yaml` | Medium | High |
| Stack build tooling + reproducibility | `stack.yaml` + lock, CI with pinned GHC 9.10.2 | Medium | High |
| Testing (Hspec) run in CI | `test/Spec.hs` (6 cases), `ci.yml` → `stack test` | Basic-Medium | High |

## What this project proves

- First appearance of: Stack, aeson/JSON serialization in Haskell, Hspec, functional data analytics.
- Reinforces: functional paradigm (with GestionDeFincaAgricola), CI with executed tests.
- Use THIS repo (not Finca) as the Haskell reference in the Master Resume.

## ATS keywords

Haskell, functional programming, Stack, GHC, aeson, JSON, Hspec, data analysis, data transformation, algebraic data types, GitHub Actions, test automation.
