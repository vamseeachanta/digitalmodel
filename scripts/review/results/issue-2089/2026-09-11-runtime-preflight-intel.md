# GHS runtime and command preflight intelligence

Date: 2026-09-11
Scope: read-only source-neutral discovery supporting issue2089 Milestone1. No solver/Part Maker process, macro, native capture, installation change or key-file access occurred. This record is not authorization for future capture.

## Observed metadata versus unverified runtime

An explicitly selected external Windows program-package directory contained GHS.EXE, 4,612,712 bytes, Windows file-version20.00. ProductVersion was blank. Executable SHA-256:81faa7506d34323951098076e7b680b93d05cd9d6ef1a7ab31ab31f6af0f0e7e. This identifies a readable packaged binary, not a configured runnable installation, successful load, license entitlement or approved execution host.

A first-level exact-name query in that package directory found the executable but no GHS.LF, GHS.SAV or OPEN-RF.RF. This is not proof that active library/search directories, a future working directory or another installation lack startup hooks. Those locations remain execution-packet prerequisites.

Existing private operation records describe two Windows execution bindings for other solver families, last verified in July2026. Neither recorded intended-workflow set includes GHS. No current GHS heartbeat, usable entitlement, process-version attestation or successful GHS run was established. Physical hosts, aliases, holder identities and private locators are intentionally omitted from this public note. Existing records will need fresh operator verification before host selection.

## Manual identity

The selected GHS User's Reference Manual contains936 pages; its version-control checklist is dated12/25. SHA-256:6b5b4b023ae53efa83c6593255996b389267e5a29fdb0d2f0cc89f33ee623634. A package-directory version name does not prove this manual matches executable20.00. Page numbers below are one-based PDF pages, not printed section page numbers. These are paraphrased command semantics, not a redistributed manual.

## Concrete command findings

| PDF pages | Finding and consequence |
|---|---|
| 59–61, Main Program 10-4 Rev H | The OS interface documents explicit runfile, geometry, work/temp paths and suppression of default library/save loading. The candidate argument vector remains `/R:canary.rf /L /S /G:canary.gf /D:<work> /T:<temp>` after the configured executable. No shell wrapper is required by this syntax. Compatibility and path quoting remain untested. |
| 59 | An extension-specific OPEN handler can replace the supplied runfile. Startup-hook qualification must cover the actual configured search locations; /L is not documented as a universal handler bypass. |
| 471, UNITS Rev E | `UNITS MT` selects metric tonnes and metres. Geometry loading can select units, so the analysis command must set the intended units after geometry loading. `UNITS M` alone does not unambiguously select the desired mass unit. |
| 407–408, SPGR Rev C; 491–492, WATER Rev D | `SPGR STANDARD` followed by `WATER 1.000` documents the intended standard-specific-gravity interpretation. In the manual's metric convention this corresponds to one cubic metre per metric tonne. WATER accepts specific gravity, not a kg/m3 value. Changing SPGR alone changes interpretation, not an already established physical density; set scale before assigning WATER. Reassert environmental water after geometry load. No tank density will be inferred from this ambient setting. |
| 211, HEEL Rev D; 457, TRIM Rev B | `HEEL 0` and `TRIM 0` are documented absolute angle settings. Axis state and wave/damage state must also be controlled; zero numeric values alone do not attest a fully neutral condition. |
| 229–230, HS Rev E | `HS 1, 2, 3` represents three origin depths in the current length units. Do not reinterpret these as arbitrary drafts or substitute /G or /KM. Native header/column precision remains unqualified. |
| 345, READ Rev L | READ loads geometry; /CHECK addresses negative upright waterplane areas. It does not certify all geometry correctness. /QUIET suppresses normal screen evidence and is unnecessary for the proposed diagnostic capture. |
| 357–358, REPORT Rev J | A fresh named PF output will be opened; `REPORT OFF` will close without printing. REPORT CLOSE and implicit end-of-session printing must be excluded. Existing filenames can otherwise be overwritten without warning. |
| 157, ECHO; 161, END Rev D | ECHO ON enables screen command display, not a verified log. Explicit END exits an OS-started run; reaching EOF alone need not exit. |

A reviewed candidate analysis sequence can therefore set units, SPGR scale, WATER, heel and trim after loading the approved geometry, open a fresh report, request the three HS depths, close with REPORT OFF and terminate explicitly. This is a sequence proposal, not a fully qualified runnable packet: axis/wave/damage resets, complete diagnostics and actual runtime behavior remain pending.

## Geometry semantics: important frame boundary

PDF p520, Part Maker coordinate system (section2 Rev P), defines native longitudinal positive aft, transverse positive starboard, vertical positive upward. If the neutral contract uses positive-forward coordinates with origin at the aft end, native longitudinal coordinates must be sign-transformed. The20m box will occupy native longitudinal -20..0m; native LCB/LCF will be -10m, while canonical positive-forward values will be +10m. These must not be compared without an explicit transform and source-frame record. A physically different fore-origin convention would require a different declared transform.

Part Maker documentation identifies a geometry-construction route without establishing a tested geometry:

- p532: UNITS=METERS, LOA/WOA and distinct Part Maker WATER syntax.
- pp523,538: CREATE statements and a terminating slash.
- pp540–542: ENDS, INBOARD and LOCUS section definitions; LOCUS uses transverse/vertical point pairs with documented winding.
- pp536,546–547: BOTTOM, TOP, side/spacing and shell settings; optional defaults can change geometry and require explicit review.
- p549: ENTER PM, geometry WRITE and QUIT PM integration; the manual calls for visual model checking after changes.

No complete neutral geometry source was generated or validated here. The next capture packet must either bind an independently checked existing neutral GF or include a separately reviewed original Part Maker build and resulting geometry inspection. Native GF text syntax, station winding, closure, side reflection, effectiveness, shell allowance and section-spacing effects must not be guessed from a generic box description. Part Maker generation is solver-family execution and will require explicit capture authorization too.

## Missing evidence before live capture

1. Operator-selected host/session, configured executable hash and actual runtime version, usable entitlement and manual/version compatibility; metadata alone is insufficient.
2. Exact startup search directories/handler disposition and isolation from default libraries/save state without changing the installation.
3. Complete fixed native geometry/runfile, canonical-to-native frame transform and independently checked dimensions/closure. A no-wave/no-damage/axis-default assumption must be evidenced or replaced by qualified explicit commands.
4. Reliable bounded recording of screen diagnostics and native outputs, including scale/environment evidence; SPGR/WATER query output can be screen-only. Redirected stdout has not been shown complete.
5. Version-qualified completion/error/warning and license/demo indicators. No parser grammar or presumed universal failure strings are established by this discovery.
6. Reviewed fresh working/output/temp directory, timeout and owned-process-tree termination, report print suppression and retention/cleanup policy. No existing output will be overwritten.
7. Explicit authorization for the final capture packet, followed by native-format qualification before parser implementation. This resource note will not activate the capture gate.

## Verification limits and cleanup

The source checks used filesystem metadata/hash reads, bounded directory-name matching and PDF text extraction. An initial text print encountered console encoding limitations; a UTF-8 rerun succeeded. No source files, installation settings, private records or license material were modified. Only this requested evidence artifact was written. Existing implementation/plan worktree contents belong to the orchestrator and were preserved.

## Bounded package/example inventory follow-up

Coverage: first-level metadata only for the selected program root (261 files), its support folder (496 files) and support/tutor (1 file). Program-root/support geometry and runfile suffixes plus barge/box/rectangular/training name matches were inspected. No company project directories, recursive share crawl or additional installation roots were searched. Content reads were limited to selected example headers/commands and four pages of the83-page Standard Course Rev8/21 training PDF after a keyword-page locator pass.

| Rank / candidate | Verified content and qualification boundary |
|---|---|
| 1 — support/MBARGE.HTM | The vendor example contains a short Part Maker barge-generation sequence using CREATE HULL, ENDS, TOP, BOTTOM and OUTBOARD, then geometry WRITE. It demonstrates the dimensional-plane route needed for an original box model. It subsequently launches two solver sessions through SHELL with /spawn; those operations are outside the canary and must not be copied or executed. Dimensions are not20x10x4, and no units are established in the inspected short sequence. This is syntax prior art, not an approved geometry or an authorized runnable script. |
| 2 — support/BARGE.GF | File exists,2436 bytes; SHA-2568d4d1731b7943df41b2e27aa9bbb9c2c49d6f9f82fb0b262ee7cce6feb9fc809. Header declares L:300 and W:54, with early station offsets spanning transverse0..27 and vertical0..12. Tail records include additional named parts, non-unit permeability and density data. The file is not evidenced as the required20x10x4 pure closed box. Native units, every shape and physical interpretation were not fully qualified. support/BARGE.zip also exists (497 bytes) but was not opened. |
| 3 — Standard Course TrainingBook.pdf, pp15,79–80 | p15 identifies rectangular forms as suitable for Part Maker. The exercise on pp79–80 imports a DXF hull, then adds tunnel/skeg/tanks/voids and uses interactive display/wait steps. It is an instructional composite model, not a ready neutral hydrostatics golden. p6 describes Hull Maker as an optional module; documentation presence does not establish its entitlement. |
| 4 — PMDEMO.RF with PMDEMO.GF1 | Runfile header starts from an existing hull and adds appendages/tanks. Inspected commands include interactive waits/display and geometric modifications. It cannot be treated as a simple box source or unattended neutral run. |
| Excluded — SAMPLE.RF | Inspected beginning loads a tanker model, produces GHS rather than plain-HS output and introduces loading/equilibrium operations. It is not the proposed canary. |

No already approved original20x10x4 neutral GF, matching independent oracle or redistribution approval was established in this bounded sample. The best next preparation is an original minimal Part Maker box definition derived from the documented primitive commands, with explicit units/native frame/side/closure settings and independent geometry review. The vendor example will remain external evidence; no raw example geometry/runfile or training PDF will be copied into the public repository. Existence of a vendor-distributed example does not by itself establish redistribution permission or engineering qualification.

Only this inventory summary was appended. No geometry creation, extraction-to-file, macro, solver, archive execution or data promotion occurred.
