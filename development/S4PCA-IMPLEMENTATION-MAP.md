# S4PCA theory-to-code map

Complete this document before integrating the new engine.

## Model specification

- Objective function:
- Constraints:
- Sparse penalty:
- Structural penalty:
- Meaning and scale of every tuning parameter:

## Algorithm

- Initialization:
- Weight update:
- Score update:
- Normalization:
- Convergence criterion:
- Maximum iterations:
- Deflation:
- Multiple components:

## Software mapping

| Mathematical object | Manuscript location | Current source function | Version 4.0 function | Test |
|---|---|---|---|---|
| Objective | TBD | TBD | `.soft_objective()` | objective decreases/stabilizes |
| Structure validation | TBD | TBD | `.validate_structure()` | invalid input rejected |
| One component | TBD | TBD | `.fit_soft_one_component()` | zero penalty equivalence |
| Deflation | TBD | TBD | `.deflate_soft_component()` | multiple-component test |

## Open decisions

- Public names for the structure and tuning parameters.
- Whether the input is an adjacency, similarity, penalty, or Laplacian matrix.
- Required symmetry and diagonal convention.
- Scaling or normalization of the supplied structure.
- Supported combinations with supervision and NMF-based super analysis.


