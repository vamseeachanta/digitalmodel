# Generic riser tension audit basis

The hand screen uses the specification's member contents densities, section and
attachment geometry, and line-type drag coefficients. It rejects unequal member
contents densities, varying member bores and unequal member lengths for the
uniform sag approximation. Attachment frames must be unrotated for
`z_global = z_reference + z_local`; rotated frames fail loudly. It assumes
straight reference chords and mean sea
level at z=0 m, positive upward. These are explicit limits of the hand screen.

The governing criterion is [ABS Guidance Notes on Drilling Riser Analysis (2017)](https://ww2.eagle.org/content/dam/eagle/rules-and-guides/current/offshore/280_Guidance_Notes_on_Drilling_Riser_Analysis_2017/Drilling_Riser_Analysis_GN_e-July17.pdf),
Section 3, Subsection 3, item iii), Eq (3.2):

`T_SRmin = fwt Ws - fbt Bn + sum(Ai_i g (rho_m Hm_i - rho_w Hw_i))`.

For each member, Hm is upper attachment elevation minus lower attachment
elevation; Hw is sea level minus lower attachment elevation. Local connection
heights are included. The aggregate Hm and Hw in tension_basis are internal-area
weighted equivalents; per-member heads are also retained. The common mud
density comes from ContentsDensity. Thus changing all members by delta rho
changes T_SRmin by `Ai_total g Hm_equivalent delta rho`. The current specification
gives Hm=1020.001058887 m, Hw=1000 m and T_SRmin=8760.265745 kN. The small change
from 8760.255203 kN includes the auxiliary attachment heights. API RP 16Q clause
text still requires confirmation against a held second edition (2017).

Archimedes' equation for flooded steel is
`Ws_i = g As_i (rho_steel L_i - rho_water Lwet_i)`. Each section's wet arclength
is its straight reference chord's intersection with z<=0, measured from End A.
The reference chord interpolates the attachment heights over the specified
unstretched section lengths. This avoids assigning the submerged length from
the upper end regardless of elevation. The auxiliary upper 20 m section is dry;
the lower section has 999.984659 m wet arclength. The main member has 1000 m wet.

The steady normal Morison drag equation is `q_i = 0.5 rho_water Cd_i D_i U^2`.
The equivalent uniform hand load is `q = sum(q_i Lwet_i) / L`, using the
specification's Cdn, outer diameter and section lengths. The existing small-sag
constant-tension approximation `delta L = q^2 L^3 / (24 Tmean^2)` and geometric
shortening `offset^2/(2L)` remain approximate. The ring reference elevation now
comes from the specification. The spring scale and binding acceptance checks
are retained. No native operating window follows from hand results.

## Force-sign guard

Newton's vertical free-body equation on the ring is
`Rz = sum(Flink,z) - m_ring g - Fslip,EndB,z - sum(Fmember,EndA,z)`.
The line end channels are forces applied by the parent to the line, so their
reactions on the ring have opposite sign, per the official
[Line results: End loads](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Lineresults,Endloads.htm)
and [Links: Results](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Links,Results.htm).
This guard assumes negligible ring inertia, buoyancy and additional vertical
forces. It fails closed if those assumptions are inadequate for a sample.

Every static and dynamic sample must have positive upward link support and
`abs(Rz) <= 0.001 abs(sum(Flink,z))`. The 0.1% value is an explicit numerical
screening tolerance selected for this guard, not an engineering code allowance.
Missing, nonfinite or mismatched channels stop extraction. Both maximum absolute
and relative residuals are retained in summaries; static/dynamic aggregation
keeps the larger residual. The extraction version changes, and summaries that
lack equilibrium evidence cannot qualify. Native channel qualification is open;
no commercial solver was imported or run for this change.

## Portable text identities

SHA-256 identities use UTF-8 text with universal newline normalization to LF for
the prepared inputs, screen CSV, numerical sources and runner inputs. The hash
still binds all other characters and labels. Simulation artifact hashes retain
their existing binary behavior. The regression converts copied prepared inputs
and implementation sources to CRLF, checks identity equality, and builds a
synthetic report from CRLF inputs. Modified content still invalidates identity.
