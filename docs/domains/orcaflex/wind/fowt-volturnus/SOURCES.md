# Public reference data and reuse basis

The numerical basis is drawn from public tabulated reference data and input values.
No figure is digitised. No PDF, report image or controller file is redistributed.
The current component export is separated from the original definition-report
comparators; agreement between them is evaluated, not assumed.

## Sources

1. Allen, C., et al. (2020). *Definition of the UMaine VolturnUS-S Reference
   Platform Developed for the IEA Wind 15-Megawatt Offshore Reference Wind
   Turbine*. NREL/TP-5000-76773.
   <https://docs.nlr.gov/docs/fy20osti/76773.pdf>.
   Tables 1–3 provide mass, hydrostatics, draft, freeboard and displacement;
   Table 6 provides mooring properties; Table 11 provides natural frequencies;
   Table 12 provides the two selected environmental conditions. The printed
   page is retained with every value in the basis.
2. Gaertner, E., et al. (2020). *IEA Wind TCP Task 37: Definition of the
   IEA 15-Megawatt Offshore Reference Wind Turbine*. NREL/TP-5000-75698.
   <https://docs.nlr.gov/docs/fy20osti/75698.pdf>.
   This document establishes the reference turbine and original component
   definitions. Its older component masses are not silently substituted into
   the current component aggregation.
3. *IEA-15-240-RWT*, public reference design inputs.
   <https://github.com/IEAWindTask37/IEA-15-240-RWT>.
   Pinned revision: `e4993d63de10f165389534461dd544006750fe60`.
   Apache License, Version 2.0, is present as `LICENSE` at that revision.
   The primary simulation input provides platform mass, centre of gravity,
   inertia, gravity and water density. The tabular workbook provides rotor
   thrust at rated power, the RNA tensor and distributed tower properties.
   The ontology provides column dimensions and explicitly commented original
   rectangular pontoon dimensions. The mooring input provides Cartesian
   endpoint positions and volume-equivalent chain diameter.
4. Publisher disclaimer and data terms.
   <https://www.nlr.gov/disclaimer>.

The two PDF sources were fetched successfully with HTTP 200 on 2026-10-14 UTC.
The source documents are read as text using `pdftotext`; the source PDFs remain
outside the committed deliverables.

## Open-source solver versions

The numerical environment uses Capytaine 3.0.0 and MoorPy 1.3.0 without changing
the project's dependency definition or lock. Both are pinned to the latest
published releases returned by their public release endpoints on 2026-10-14
at 04:20:04 UTC (HTTP 200):

- <https://github.com/capytaine/capytaine/releases/tag/v3.0.0>
- <https://github.com/NatLabRockies/MoorPy/releases/tag/v1.3.0>

The tagged line and catenary formulations were opened with HTTP 200 on
2026-10-14 at 04:25:11 UTC:

- <https://github.com/NatLabRockies/MoorPy/blob/v1.3.0/moorpy/line.py>,
  lines 18 and 34: the default seabed-friction coefficient is zero.
- <https://github.com/NatLabRockies/MoorPy/blob/v1.3.0/moorpy/Catenary.py>,
  elastic grounded and suspended catenary equations.

The response convention is identified from the tagged impedance formulation
(HTTP 200, 2026-10-14 UTC):
<https://github.com/capytaine/capytaine/blob/v3.0.0/src/capytaine/post_pro/impedance.py>,
function `rao_transfer_function`: K minus omega-squared times M+A minus
the imaginary unit times omega times damping. This fixes the excitation/response phasor
convention; the full complex response is tested against that implementation.

Zero seabed friction is retained as an assumption. Anchor holding capacity
and allowable excursion are not established by the tension-strength criterion.

Bibliographic metadata for Longuet-Higgins (1952) was opened from the public
wave-analysis bibliography at revision
`cc06f14d54f21692ae87a1a4858979841cf531c7` (HTTP 200, 2026-10-14 UTC):
<https://github.com/dionhaefner/dionsthesis/blob/cc06f14d54f21692ae87a1a4858979841cf531c7/phd-thesis/src/references.bib>,
entry `longuet1952statisticaldistribution`, lines 98–104. It lists
*On the statistical distribution of the height of sea waves*, JMR 11,
245–266. This confirms bibliographic metadata only; the original paper has
not been opened. The Rayleigh maximum is an explicitly stated narrow-band
approximation, not a digitised or tabulated result from that paper.

## Reuse findings

The platform definition report describes a publicly available benchmark that
can be openly used to explore technologies and designs. The publisher's
copyright notice states that documents can be subject to copyright; it does
not grant a general licence to redistribute every report or illustration.
The separate Data and Software terms grant use and copying of data subject to
retaining the entire notice and crediting DOE/NLR/ALLIANCE. The data notice is
retained below. The reference-design inputs separately carry Apache-2.0; its
complete licence text is retained below.

The stated use is limited to numerical facts, attributed input values,
bibliographic citations and brief quotation. This basis does not claim a
blanket report redistribution right or an endorsement. Figures and PDFs are
not copied. The U.S. Department of Energy, National Laboratory of the Rockies,
and Alliance are credited for the published reference data. Reference-design
contributors are credited for the openly licensed inputs.

## Basis qualifications

- The original report gives platform inertia and centre of gravity, not a full
  assembled-system tensor. The assembled tensor is computed explicitly from
  current component inputs. Its mass is not forced to equal the original
  report's total-system mass.
- The workbook and primary simulation input contain different component
  exports at the pinned revision. Their aggregation is approximate, with the
  component values and formula retained in the basis. A published original
  full-system tensor is not established.
- The workbook names both RNA centre-of-mass and tower-top tensor columns;
  these supplied tensors do not consistently reproduce one another under
  a parallel-axis transform. Both source-label readings are retained
  for the aggregation, with the swapped reading adopted as assumed. This is a source-data limitation, not a numerical
  validation result.
- Tower mass density and transverse section inertia are integrated with
  piecewise-linear interpolation and three-point Gauss quadrature on each
  tabulated interval. The polar inertia is twice the transverse section
  inertia for the circular axisymmetric section. The rigid-body tensor is
  transferred by the parallel-axis identity given in the basis.
- The original rectangular pontoon dimensions remain explicit comments in
  the ontology; the active ontology uses circular equivalent pontoons. The
  rectangular dimensions are used without interpreting a drawing.
- Static offsets are not tabulated in Section 3.1. Figure 12 is not digitised,
  and no numerical offset comparison against that figure is made.
- Added damping and tension factors are labelled "assumed, based on previous
  experience". The quasi-static factors are selected from existing analysis
  inputs, with revision provenance; no factor is attributed to a licensed
  standard.

## Inertia-source finding

The opened workbook reproduces diagonal parallel-axis discrepancies of 27.2 %,
34.2 % and 35.9 %. The identity is I_TT = I_CG + m[(r dot r)I - r outer r].
Mass and centre coordinates are B24:E24; the named CG tensor is F24:K24 and
named tower-top tensor L24:Q24 on Nacelle Mass Properties. The RNA mass is
independently confirmed by summing the nacelle, blades and hub-system groups.

Two interpretations are retained. The named interpretation takes F24:K24
at the RNA CG. The swapped interpretation takes F24:K24 at tower top and
the CG tensor derived as F minus the parallel-axis shift. The latter is
adopted as **assumed** because the swapped-label interpretation has a smaller
full-tensor relative identity residual of 23.1 %, compared with 31.7 % for
the named interpretation. Neither resolves the discrepancy. Retaining F as the endpoint, rather than trusting L at CG, is an additional
assumption; L remains the published comparator. The supplied
mass and CG remain identical in both aggregations; natural periods and
response operators will carry both tensors. No alternate component export
is represented as a qualified replacement.

The documents and publisher terms were reopened on 2026-10-14 UTC, all with
HTTP 200. The source PDFs are identified by SHA-256:

- Platform definition: 0d13ae4d857115f42db629cb58f7d3a620e8abe0c6a145dfd28f7d727a8d5823.
- Turbine definition: 93762e252922b1e75df59a453d2c08ef3f556367adf57229bc1800a4b1b11dbd.

## Publisher data notice

Data and Software

Access to or use of any data or software made available on this server ("Data") shall
    impose the following obligations on the user, and use of the Data constitutes user's
    agreement to these terms. The user is granted the right, without any fee or cost,
    to use or copy the Data, provided that this entire notice appears in all copies of
    the Data.  Further, the user agrees to credit the U.S. Department of Energy (DOE)/NLR/ALLIANCE
    in any publication that results from the use of the Data.  The names DOE/NLR/ALLIANCE,
    however, may not be used in any advertising or publicity to endorse or promote any
    products or commercial entities unless specific written permission is obtained from
    DOE/NLR/ ALLIANCE.  The user also understands that DOE/NLR/ALLIANCE are not obligated
    to provide the user with any support, consulting, training or assistance of any kind
    with regard to the use of the Data or to provide the user with any updates, revisions
    or new versions thereof. DOE, NLR, and ALLIANCE do not guarantee or endorse any results
    generated by use of the Data, and user is entirely responsible for the results and
    any reliance on the results or the Data in general.
USER AGREES TO INDEMNIFY DOE/NLR/ALLIANCE AND ITS SUBSIDIARIES, AFFILIATES, OFFICERS,
    AGENTS, AND EMPLOYEES AGAINST ANY CLAIM OR DEMAND, INCLUDING REASONABLE ATTORNEYS'
    FEES, RELATED TO USER’S USE OF THE DATA.  THE DATA ARE PROVIDED BY DOE/NLR/ALLIANCE
    "AS IS," AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING BUT NOT LIMITED TO THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
    IN NO EVENT SHALL DOE/NLR/ALLIANCE BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
    DAMAGES OR ANY DAMAGES WHATSOEVER, INCLUDING BUT NOT LIMITED TO CLAIMS ASSOCIATED
    WITH THE LOSS OF DATA OR PROFITS, THAT MAY RESULT FROM AN ACTION IN CONTRACT, NEGLIGENCE
    OR OTHER TORTIOUS CLAIM THAT ARISES OUT OF OR IN CONNECTION WITH THE ACCESS, USE OR
    PERFORMANCE OF THE DATA.


## Method bibliography qualification

API RP 2SK, *Design and Analysis of Stationkeeping Systems*, 2005,
is identified by the existing `code_check_engine.py`
module, lines 18 and 278, at commit
`55a91491cad7730d4469e8a379f0f9b533b21a03`. The platform
definition report's bibliography identifies DNVGL-OS-E301, *Position Mooring*,
2018. These bibliographic identifications do not establish clause titles or
licensed clause contents. Clause titles remain unconfirmed; no clause content,
tabulated coefficient or standard-derived factor is reproduced. The intact and
damaged factors remain explicitly assumed, based on previous experience.

## Reference input licence

The following licence accompanies the pinned reference inputs.

                                 Apache License
                           Version 2.0, January 2004
                        http://www.apache.org/licenses/

   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION

   1. Definitions.

      "License" shall mean the terms and conditions for use, reproduction,
      and distribution as defined by Sections 1 through 9 of this document.

      "Licensor" shall mean the copyright owner or entity authorized by
      the copyright owner that is granting the License.

      "Legal Entity" shall mean the union of the acting entity and all
      other entities that control, are controlled by, or are under common
      control with that entity. For the purposes of this definition,
      "control" means (i) the power, direct or indirect, to cause the
      direction or management of such entity, whether by contract or
      otherwise, or (ii) ownership of fifty percent (50%) or more of the
      outstanding shares, or (iii) beneficial ownership of such entity.

      "You" (or "Your") shall mean an individual or Legal Entity
      exercising permissions granted by this License.

      "Source" form shall mean the preferred form for making modifications,
      including but not limited to software source code, documentation
      source, and configuration files.

      "Object" form shall mean any form resulting from mechanical
      transformation or translation of a Source form, including but
      not limited to compiled object code, generated documentation,
      and conversions to other media types.

      "Work" shall mean the work of authorship, whether in Source or
      Object form, made available under the License, as indicated by a
      copyright notice that is included in or attached to the work
      (an example is provided in the Appendix below).

      "Derivative Works" shall mean any work, whether in Source or Object
      form, that is based on (or derived from) the Work and for which the
      editorial revisions, annotations, elaborations, or other modifications
      represent, as a whole, an original work of authorship. For the purposes
      of this License, Derivative Works shall not include works that remain
      separable from, or merely link (or bind by name) to the interfaces of,
      the Work and Derivative Works thereof.

      "Contribution" shall mean any work of authorship, including
      the original version of the Work and any modifications or additions
      to that Work or Derivative Works thereof, that is intentionally
      submitted to Licensor for inclusion in the Work by the copyright owner
      or by an individual or Legal Entity authorized to submit on behalf of
      the copyright owner. For the purposes of this definition, "submitted"
      means any form of electronic, verbal, or written communication sent
      to the Licensor or its representatives, including but not limited to
      communication on electronic mailing lists, source code control systems,
      and issue tracking systems that are managed by, or on behalf of, the
      Licensor for the purpose of discussing and improving the Work, but
      excluding communication that is conspicuously marked or otherwise
      designated in writing by the copyright owner as "Not a Contribution."

      "Contributor" shall mean Licensor and any individual or Legal Entity
      on behalf of whom a Contribution has been received by Licensor and
      subsequently incorporated within the Work.

   2. Grant of Copyright License. Subject to the terms and conditions of
      this License, each Contributor hereby grants to You a perpetual,
      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
      copyright license to reproduce, prepare Derivative Works of,
      publicly display, publicly perform, sublicense, and distribute the
      Work and such Derivative Works in Source or Object form.

   3. Grant of Patent License. Subject to the terms and conditions of
      this License, each Contributor hereby grants to You a perpetual,
      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
      (except as stated in this section) patent license to make, have made,
      use, offer to sell, sell, import, and otherwise transfer the Work,
      where such license applies only to those patent claims licensable
      by such Contributor that are necessarily infringed by their
      Contribution(s) alone or by combination of their Contribution(s)
      with the Work to which such Contribution(s) was submitted. If You
      institute patent litigation against any entity (including a
      cross-claim or counterclaim in a lawsuit) alleging that the Work
      or a Contribution incorporated within the Work constitutes direct
      or contributory patent infringement, then any patent licenses
      granted to You under this License for that Work shall terminate
      as of the date such litigation is filed.

   4. Redistribution. You may reproduce and distribute copies of the
      Work or Derivative Works thereof in any medium, with or without
      modifications, and in Source or Object form, provided that You
      meet the following conditions:

      (a) You must give any other recipients of the Work or
          Derivative Works a copy of this License; and

      (b) You must cause any modified files to carry prominent notices
          stating that You changed the files; and

      (c) You must retain, in the Source form of any Derivative Works
          that You distribute, all copyright, patent, trademark, and
          attribution notices from the Source form of the Work,
          excluding those notices that do not pertain to any part of
          the Derivative Works; and

      (d) If the Work includes a "NOTICE" text file as part of its
          distribution, then any Derivative Works that You distribute must
          include a readable copy of the attribution notices contained
          within such NOTICE file, excluding those notices that do not
          pertain to any part of the Derivative Works, in at least one
          of the following places: within a NOTICE text file distributed
          as part of the Derivative Works; within the Source form or
          documentation, if provided along with the Derivative Works; or,
          within a display generated by the Derivative Works, if and
          wherever such third-party notices normally appear. The contents
          of the NOTICE file are for informational purposes only and
          do not modify the License. You may add Your own attribution
          notices within Derivative Works that You distribute, alongside
          or as an addendum to the NOTICE text from the Work, provided
          that such additional attribution notices cannot be construed
          as modifying the License.

      You may add Your own copyright statement to Your modifications and
      may provide additional or different license terms and conditions
      for use, reproduction, or distribution of Your modifications, or
      for any such Derivative Works as a whole, provided Your use,
      reproduction, and distribution of the Work otherwise complies with
      the conditions stated in this License.

   5. Submission of Contributions. Unless You explicitly state otherwise,
      any Contribution intentionally submitted for inclusion in the Work
      by You to the Licensor shall be under the terms and conditions of
      this License, without any additional terms or conditions.
      Notwithstanding the above, nothing herein shall supersede or modify
      the terms of any separate license agreement you may have executed
      with Licensor regarding such Contributions.

   6. Trademarks. This License does not grant permission to use the trade
      names, trademarks, service marks, or product names of the Licensor,
      except as required for reasonable and customary use in describing the
      origin of the Work and reproducing the content of the NOTICE file.

   7. Disclaimer of Warranty. Unless required by applicable law or
      agreed to in writing, Licensor provides the Work (and each
      Contributor provides its Contributions) on an "AS IS" BASIS,
      WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
      implied, including, without limitation, any warranties or conditions
      of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A
      PARTICULAR PURPOSE. You are solely responsible for determining the
      appropriateness of using or redistributing the Work and assume any
      risks associated with Your exercise of permissions under this License.

   8. Limitation of Liability. In no event and under no legal theory,
      whether in tort (including negligence), contract, or otherwise,
      unless required by applicable law (such as deliberate and grossly
      negligent acts) or agreed to in writing, shall any Contributor be
      liable to You for damages, including any direct, indirect, special,
      incidental, or consequential damages of any character arising as a
      result of this License or out of the use or inability to use the
      Work (including but not limited to damages for loss of goodwill,
      work stoppage, computer failure or malfunction, or any and all
      other commercial damages or losses), even if such Contributor
      has been advised of the possibility of such damages.

   9. Accepting Warranty or Additional Liability. While redistributing
      the Work or Derivative Works thereof, You may choose to offer,
      and charge a fee for, acceptance of support, warranty, indemnity,
      or other liability obligations and/or rights consistent with this
      License. However, in accepting such obligations, You may act only
      on Your own behalf and on Your sole responsibility, not on behalf
      of any other Contributor, and only if You agree to indemnify,
      defend, and hold each Contributor harmless for any liability
      incurred by, or claims asserted against, such Contributor by reason
      of your accepting any such warranty or additional liability.

   END OF TERMS AND CONDITIONS

   APPENDIX: How to apply the Apache License to your work.

      To apply the Apache License to your work, attach the following
      boilerplate notice, with the fields enclosed by brackets "[]"
      replaced with your own identifying information. (Don't include
      the brackets!)  The text should be enclosed in the appropriate
      comment syntax for the file format. We also recommend that a
      file or class name and description of purpose be included on the
      same "printed page" as the copyright notice for easier
      identification within third-party archives.

   Copyright [yyyy] [name of copyright owner]

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
