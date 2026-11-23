# First sentinel-attempt recovery decision

Status: proposed one-time operator disposition; no recovery authorization will be inferred.
Issue: https://github.com/vamseeachanta/digitalmodel/issues/2089

## Observed basis

The first actual Windows qualification attempt returned recovery_required during normal-parent identity verification. The configured virtual-environment executable acted as a redirector: the registered launcher identity differed from the parent readiness identity. Child readiness supplied another identity, but an intermediate child redirector may not have been recorded. The attempted pre-close full census was therefore incomplete.

Original private evidence is retained with SHA256 0bea0961d6ac901509ae587744679271dee68d504c9523598805406712381b3e. The stable active marker remains present. Main-session read-only checks found all three recorded PIDs absent. A later Python-process inventory found no Python process created after the attempt start; four inaccessible command lines belonged to processes created hours/days earlier and two readable processes predated the attempt by ten seconds. These are current-state observations, not reconstruction of the missing historical census.

The approved addendum states: "If completeness cannot be established, the attempt will stay unresolved for operator investigation." No marker was cleared or native retry performed.

## Proposed bounded disposition

If the user explicitly approves this one-time exception, the operator will:
1. Recheck current process creation times and test command lines immediately before disposition; any suspected surviving test process will block the action.
2. Preserve original private observations, readiness records, active-marker bytes and their hashes.
3. Write a private operator disposition referencing the user's authorization, the incomplete historical census, the current-state observations and the corrected implementation identity.
4. Move only this attempt's active marker without overwrite to an archive labeled unresolved-operator-waived, never completed or qualified. Original failure classification will remain unchanged.
5. Run one fresh bounded sentinel qualification only after the interpreter-image fix, deadline/cleanup fixes and regression tests pass. No GHS, Part Maker or other vendor execution will occur.

If approval is absent, the marker will remain active; code and pure-test work will continue but native acceptance and merge will remain pending. This proposal will not add an automatic reset/clear command or weaken the runtime interlock.

## Corrected implementation binding

The proposed retry will use the source hashes in scripts/review/results/issue-2089/containment-main-r3-hashes.json (manifest SHA256 146b9975e0c219c1c889d6789a882d940c5c6013afab0cf93e92aeab91e2bfad). The Linux regression suite will be treated as 145 passed with native acceptance still pending. The original failure will remain a failure.
