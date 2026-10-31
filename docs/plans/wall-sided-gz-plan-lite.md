# Plan-lite: wall-sided righting lever from section moments

## Scope

Establish one synthetic rectangular section for `gz_wall_sided`: beam 4 m,
upright draft 2 m, depth 4 m and KG 1 m, with waterline slope 1/2.
The waterline remains between 1 m and 3 m above the bottom, so neither bilge
exposure nor deck-edge immersion occurs. The production implementation is
unchanged. The test covers this section, its reflection and its upright limit.

## Independent oracle

Integrate the submerged area and its first moments directly over the section.
The stipulated geometry gives area 8 m2 and buoyancy-centre coordinates
x_B = 1/3 m and z_B = 13/12 m. Project the separation from G to buoyancy onto
the horizontal to obtain GZ = 3/(4*sqrt(5)) m. Reflection gives the opposite
signed lever; upright geometry gives zero. Production helpers supply neither
the centroids nor the expected levers.

## Verification

The complete derivation and two test functions are in
[the geometric GZ tests](../../tests/naval_architecture/test_loading_computer_geometric_gz.py).
Run that file together with `test_loading_computer.py` and
`test_loading_computer_values.py` in the same directory: 34 cases are expected,
with 32 passed and the two existing damage-stability expected failures.
Use absolute tolerance 1e-12 m and zero relative tolerance for arithmetic
rounding only. Check repository structure, engineering writing register,
whitespace, publication restrictions, ignore status and LFS attributes.

## Qualification limits

This independent geometric case does not qualify a textbook edition or
establish compliance with a stability acceptance code. Physical model error,
other hull forms and the existing damage-stability approximation remain outside
this scope. Broader source qualification remains open under issue #3352.
