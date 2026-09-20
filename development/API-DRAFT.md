# Draft public API for msma 4.0

The API is provisional until it is reconciled with the submitted S4PCA
manuscript and implementation.

```r
msma(
  X,
  Y = NULL,
  structureX = NULL,
  structureY = NULL,
  structure.method = c("none", "soft"),
  lambdaStructureX = 0,
  lambdaStructureY = 0,
  ...
)
```

## Dispatch rule

- No structure and zero structural tuning parameters: use the Version 3.2
  legacy or extended engine unchanged.
- Any explicit soft-structure request: use the new soft engine.
- In 4.0.0, reject unsupported combinations explicitly rather than silently
  changing their meanings.

## Structure input

- Single matrix X: one square numeric structure matrix.
- Multiblock X: list of square numeric matrices, one per block.
- Dimensions and names must align with the corresponding data variables.


