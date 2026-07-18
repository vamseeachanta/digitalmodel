# Issue 1618 plan review — Agy — round 4

Verdict: **MAJOR**

- The envelope resolver accepted raw `ReviewedHostResolutionEvidence` rather than the verifier's `VerifiedHostResolution` output.
- `PanelCatalog` was an unsupported dependency because the selection contract bound no panel artifact and #1618 does not own diffraction meshes.

Disposition: the revision makes verified evidence the only resolver input and limits catalog verification to the immutable hull artifact; loading remains bound to producer evidence.
