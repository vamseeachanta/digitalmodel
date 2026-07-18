# Issue 1619 parent compatibility check — round 2

Decision: **PARENT AMENDMENT REQUIRED**

`complex_RAO_payload_sha256` is motion response and propagates to the vessel-motion boundary. `hydrodynamic_matrix_payload_sha256` is restricted to added mass, radiation damping, and restoring. `diffraction_execution_evidence_sha256` may retain native excitation as evidence but cannot represent a canonical normalized public payload. The parent revision must add a dedicated excitation commitment and extend allowed response kinds, row keys, payload projection, and release-set derivation. Excitation need not propagate into the vessel-motion boundary.
