# AQWA offline damping basis, revision 2

This calculation is a single-mode, motion-only, harmonic approximation. It does
not reproduce the solver's correlated two-dimensional stochastic drag model.
The declared wave amplitude is a peak amplitude in m. Heading quadrature is a
representative amplitude across headings, not a sea-state response statistic.
Member areas are effective projected areas for the respective motion; general
oriented tube coupling and relative fluid velocity require a separate model.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | Not recorded | Initial offline calculation basis. |
| 2 | 2026-10-17 | Correct E2/E3 rounding and document structure selection and evidence bindings. |

## E1. Drag force and energy equivalence

The starting drag law is F = (1/2) rho Cd A |u|u. For u = U cos(omega t),
the cycle means are mean(cos²) = 1/2 and mean(|cos|³) = 4/(3 pi).
Equating mean(B u²) to mean(F u) gives B = (4/(3 pi)) rho Cd A U.
The 1/2 in the drag law is retained. This is an independent integral derivation;
the governing drag law is also identified in the AQWA Theory Manual 2024 R2,
[section 6.1, equation 6-1](https://ansyshelp.ansys.com/public/Views/Secured/corp/v242/en/aqwa_thy/aqwathy_morison_equation.html).
The distinction from stochastic linearisation is described in
[section 6.4](https://ansyshelp.ansys.com/public/Views/Secured/corp/v242/en/aqwa_thy/aqwathy_morison_drag_lin.html).

## E2. Translational motion

X = wave_amplitude_m * RAO_m_per_m; U = omega X.
B33 = (4/(3 pi)) rho Cd A omega X, in N s/m.
For rho = 1025 kg/m³, Cd = 1, A = 10 m², omega = 0.5 rad/s and X = 1 m,
B33 = 2175.1 N s/m.

## E3. Rotational motion

theta = wave_amplitude_m * RAO_rad_per_m, with rad = deg pi/180.
Local velocity is r omega theta. A further r² converts linear damping to
rotational damping: B44 = (4/(3 pi)) rho Cd A |r|³ omega theta.
For rotation about x, r = sqrt((y-y_CoG)² + (z-z_CoG)²); for rotation about y,
r = sqrt((x-x_CoG)² + (z-z_CoG)²). This radial model assumes effective drag area
normal to local motion. Two identical members at ±10 m, theta = 0.1 rad and
the E2 parameters give 435023.5 N m s/rad.

## E4. Resonance

The undamped single-DOF equation (I+A) q'' + C q = 0 gives
omega_n² (I+A(omega_n)) = C. A is interpolated linearly within the supplied
grid; one positive in-grid root is required. No extrapolation or selection
among multiple roots is permitted. Constant A uses the same equation.

## E5. Critical damping and additions

The repeated-root condition of (I+A) s² + B s + C = 0 gives
B_critical = 2 (I+A(omega_n)) omega_n, zeta = B(omega_n)/B_critical,
T_n = 2 pi/omega_n. For I = 1e9, A = 2e8, C = 2e8 and B = 1e7 in consistent
rotational SI units, omega_n = 0.40824829 rad/s, T_n = 15.39059796 s and
zeta = 0.0102062073. Additional damping is max(0, B*(target/current - 1)),
with both ratios explicitly in percent; 5% versus 2% and B = 1000 gives 1500.
An unlabelled target below 1 is rejected as ambiguous. Explicit percent permits
sub-percent targets. Zero current damping cannot determine a scale and raises.

## DAT conventions

AQWA Reference Manual 2025 R1 [section 10.3](https://ansyshelp.ansys.com/public/Views/Secured/corp/v251/en/aqwa_ref/aqwaref_deck6_freq_perd_hrtz.html)
defines HRTZ as Hz, FREQ as rad/s and PERD as s. Conversion uses omega = 2 pi f
and T = 2 pi/omega. Section 10.4 defines DIRN separately in degrees.
The package writer's deck-1 layout is read by its structure and node columns;
it is not parsed as one concatenated integer. Missing rotational RAO units
require an explicit configuration declaration; direction degrees do not establish
rotation units. No commercial solver result is established by these tests.

## Structure selection and evidence records

Deck-6 FDRn blocks retain separate frequency and direction grids.
Inspection exposes all grids through `frequency_grids`; `frequency_grid` and
`_parse_deck6` default to structure 1. The latter accepts `structure=n`.
FILE, CSTR and CPDB cards reference external database data; they do not supply
an inline grid. The LIS displacement parser requires `structure=n` when multiple
bodies are present. The configured-search reader uses the selected keyword range
and extraction settings, carries period/frequency across RAO continuation rows,
and rejects an extraction with no rows.

AQ-01 through AQ-03 retain individual checks, measured evidence and input digests.
Native completion evidence includes the input DAT digest, the fresh LIS digest,
and the observed body count from hydrodynamic structure headings. The expected
count is declared in `execution.expected_body_count`. Missing or stale listing
output cannot satisfy AQ-04. Launch-only execution requires `execution.allow_solver`
and returns a process handle; it supplies no completion qualification.

AQ-05 through AQ-10 require a `signature` mapping with `identity`, `role`,
`signed_at`, `manifest_sha256` and `gates_sha256`. The role is `maintainer` and
the date is an ISO timestamp with a timezone. SHA-256 binds canonical JSON
(sorted keys, compact separators, UTF-8, no nonfinite values). The manifest
binding covers `input_sha256`; the gate binding covers the evidence record
without `signature`, with the artifact represented by its basename.
AQ-11 uses the same signature fields, binding the complete input manifest and
AQ-01 through AQ-10 evidence. Per-record signatures can be supplied through
`quality_signatures`. Bare role strings are rejected. These are retained manual
attestations: reviewer identity and authority require independent confirmation;
the records do not provide cryptographic identity authentication.
