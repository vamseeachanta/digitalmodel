# CFD Dispatch Connectivity And HTML Reporting Notes

Date: 2026-07-09

## Active Objective

Keep CFD dispatch operational from `ace-linux-1` while defining a durable,
standardized HTML engineering report for CFD and FEA runs. Tracking context:
[#1495](https://github.com/vamseeachanta/digitalmodel/issues/1495).

## Current Decisions

- Continue reaching `gpu-claw` through the existing VPN path.
- Continue reaching `ace-linux-2` directly over the local LAN.
- Defer the long-term SSH transport decision. No public SSH, Cloudflare SSH
  ingress, router forwarding, or firewall change is authorized in this phase.
- Keep transport behind dispatcher SSH targets so a later connection change does
  not alter CFD selection, execution, or reporting contracts.
- Preserve `ace-linux-2` as fallback; prefer `gpu-claw` for current CFD work and
  cap it at eight MPI ranks until its visible CPU allocation and benchmark change.

## Verified Connectivity Findings

- `gpu-claw` is currently reached through the VPN path from `ace-linux-1`.
- The owner does not have administrative access to the node's edge router, so a
  direct inbound SSH design cannot be implemented or maintained safely.
- An outbound-tunnel alternative could carry SSH without router changes; the
  [Zero Trust free tier](https://www.cloudflare.com/plans/zero-trust-services/)
  and its [service-token limits](https://developers.cloudflare.com/cloudflare-one/account-limits/)
  appear sufficient for this small deployment. This remains a deferred
  alternative, not the selected transport.
- `ace-linux-2` is directly reachable from `ace-linux-1` without VPN.

## Security Observations

- Any future public SSH design must complete key-only authentication, host
  firewall, host-key pinning, restart, and rollback checks before exposure.
- Any future outbound-tunnel design must use least-privilege credentials and
  service permissions, with explicit rollback protection.
- Do not remove the working VPN path until a replacement has passed host-key,
  authentication, restart, and unattended-dispatch tests.
- No credentials, private host address, client identifier, or project location
  identifier is recorded in these notes.

## Existing Dispatch State

- `scripts/setup/dispatch-cfd-run.py` selects between `gpu-claw` and
  `ace-linux-2` using benchmark manifests plus live SSH core/load probes.
- The current dispatcher runs the remote command attached to SSH. Both hosts have
  `systemd-run` and `tmux`, so a future execution enhancement can submit a named,
  detached job that survives control-connection interruption.
- Recommended boundary: SSH submits and inspects jobs; a node-local runner owns
  solver lifetime, logs, exit status, result manifest, report generation, and sync.

## HTML Engineering Report Precedents

The repository already contains the quality-report pattern recalled by the owner:

- `docs/domains/project-docs/HTML_REPORTING_STANDARDS.md`
- `examples/reporting/calculix-beam-validation-wrk-1341.html`
- `examples/reporting/fem-3d-beam-analysis-wrk-1364.html`
- `src/digitalmodel/ansys/report_generator.py`
- workspace-hub `.agents/skills/development/engineering-report-generator/SKILL.md`
- workspace-hub `.claude/skills/development/html-report-verify/SKILL.md`

The generic FEA examples establish this engineering sequence: Scope, Design
Basis, Inputs, Methodology, Calculations, Outputs, Sensitivity, Validation,
Verification, Conclusions, Charts, Data Tables, Change Log, and raw program/test
evidence.

## Proposed Shared Report Contract

This is discovery output, not an approved implementation design.

- One shared HTML report shell with discipline adapters for CFD and FEA.
- Common sections: identity/revision, executive summary, scope, design basis,
  inputs, assumptions, methods, verification, validation, results,
  visualizations, sensitivity/uncertainty, limitations, conclusions,
  recommendations, provenance, change log, and raw execution evidence.
- CFD adapter: geometry/mesh, fluid properties, boundary/initial conditions,
  solver build and numerics, time-step/Courant history, MPI resources,
  convergence, conservation checks, spatial contours, integrated-result plots,
  and mesh or time-step sensitivity where available.
- FEA adapter: geometry/materials, element formulation and mesh quality,
  connections, loads and restraints, load cases, solver controls, convergence,
  stresses/displacements/reactions/utilization, and mesh sensitivity.
- Output bundle: report HTML, machine-readable run manifest, reduced CSV/JSON
  results, provenance record, and logs/checksums referenced by the manifest.
- Verification must cover manifest integrity, source-data cardinality and extrema,
  HTML structure, browser console, interactive controls, rendered charts,
  responsive layout, units, physical-scope labels, and visible caveats.
- Report strings and metadata must be escaped or typed; existing free-form raw HTML
  insertion patterns are not suitable for untrusted run data.

## Open Decisions And Blockers

- Decide whether every successful dispatched run emits the baseline HTML report
  automatically or only explicitly selected runs do so.
- Decide whether the first implementation is CFD-only with a shared schema ready
  for FEA, or delivers both discipline adapters together.
- Cloud data sync still needs an owner-selected `rclone` remote/folder and secret.
- Direct GitHub operations on the compute node still need separate headless
  authentication if they are required at all.
- Public/direct SSH remains blocked by lack of edge-router administration.

## Activity And Residue

- All network and repository archaeology was read-only.
- No router, firewall, SSH daemon, VPN, Cloudflare route, dispatcher, or compute
  configuration was changed.
- Two bounded report-discovery subagents stalled during read-only search and were
  shut down; neither changed files or external state. Later report implementation
  should again use an isolated subagent after the design and issue plan are
  approved.

## Exact Next Checkpoint

1. Confirm the report-generation policy: automatic baseline report for every
   completed run versus opt-in generation.
2. Present the transport-neutral dispatch/report architecture for user approval.
3. Write and self-review the design specification.
4. Create or split GitHub issue plans for detached execution and standardized HTML
   reporting; run adversarial plan review.
5. Stop for explicit user approval before TDD implementation.

## Suggested Skills

- `superpowers:brainstorming` to finish and approve the design.
- `coordination/issue-planning-mode` for issue plans and approval gates.
- `superpowers:test-driven-development` for implementation after approval.
- `engineering-report-generator` for report construction conventions.
- `html-report-verify` for structural, data-contract, and browser verification.
- `superpowers:dispatching-parallel-agents` for isolated report and execution
  implementation lanes.
- `coordination/pre-completion-cleanup-audit` before closeout.
