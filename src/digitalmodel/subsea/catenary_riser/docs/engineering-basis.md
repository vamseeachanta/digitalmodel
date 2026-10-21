# Analytical audit basis

These equations govern the changed analytical calculations. They do not qualify
native statics, dynamics, bending or fatigue stress extraction.

E1. Uniform inextensible catenary with a horizontal tangent at the vertex:
`a=H/w`, `z=a(cosh(x/a)-1)`, `s²=z²+2az`,
`x=a acosh(1+z/a)`. Here w is force per unit length (N/m).
The available length bound is `a <= (L²-z²)/(2z)`.
Basis: [Catenary Curve (2019), equations (4), (9), (10), (34)](https://www.mygeodesy.id.au/documents/Catenary%20Curve.pdf).
The vertical coordinate in that reference is `y=a+z`.

E2. `T²=H²+(ws)²` and E1 give `T=H+wz`; hence `H=T-wz > 0`.
The legacy force route gives `s=sqrt(z(2T/w-z))` and `b=w/H` (1/m).
Basis: same reference, equations (3), (4), (14).

E3. The top angle from vertical is `atan2(H,ws)`; angle distributions remain
from horizontal. Basis: same reference, force balance (1).

E4. Archimedes balance per unit length: additional force is
`g [rho_foam A_foam + rho_coat A_coat - rho_water A_displaced]`, with
`A_foam=pi/4 (D_module²-D_riser_coated²)`,
`A_displaced=pi/4 (D_module_coated²-D_riser_coated²)` and
`A_coat=pi/4 (D_module_coated²-D_module²)`.
The bare riser already accounts for its own displaced area.
This is an annular volume balance, not an empirical coefficient.
For `D_module=1 m`, `D_riser_coated=0.3 m`, foam 500 kg/m³,
water 1025 kg/m³ and g=9.81 m/s², the added force is -3680.94717 N/m.

E5. [DNVGL-RP-C203, April 2016, §2.2, equation (2.2.1), pp. 15–16](https://www.dnv.com/energy/standards-guidelines/dnv-rp-c203-fatigue-design-of-offshore-steel-structures/)
uses the historical edition for the criterion below; the publisher page lists the
current edition and access route, rather than a third-party copy. It
requires accumulated damage `D <= eta`, with `eta=1/DFF`.
For annual damage d and design life Y the report checks `d*Y*DFF <= 1`.
DFF and its selection basis must be supplied; no safety class is inferred.
The sample's DFF=2 is illustrative, not a prescribed factor. Invalid or absent
positive finite damage, utilisation, design life or cited DFF gives N/A. A report
containing N/A cannot roll up to PASS. A true zero utilisation is treated as
unqualified and displayed as N/A under this input convention; it does not
establish a completed zero-load check. A strength KPI with any unqualified row
is labelled N/A (incomplete). The existing strength marginal band is a
reporting convention, not a code acceptance factor; fatigue has no marginal band.

Hand worksheet for the unit riser (OD .508 m, ID .458 m):
steel area .037934731293 m²; contents area .164748260347 m²;
displaced area .202682991639 m². At densities 7850/850/1025 kg/m³ and
g=9.81 m/s², weights are 2921.296755, 1373.753369 and 2038.028152 N/m;
effective weight 2257.021972 N/m (no coating).
At H=500000 N and z=1000 m, E1–E3 give s=1201.275032 m,
x=529.928323 m, T=2757021.972 N and angle from vertical=10.448701°.
