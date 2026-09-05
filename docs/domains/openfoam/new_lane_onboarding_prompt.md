# Prompt: bring a new machine up as an OpenFOAM solve lane

Paste the block below into a Claude Code session on the control host (the machine that
already holds the digitalmodel worktree and the campaign registries), after filling the
placeholders. It encodes the practices learned on the existing lanes (2026-09-04/05):
repo -> lane script flow, master mesh store, markers and ledger, one solver per lane,
benchmark-first qualification, and records back in the wiki. Keep client identifiers out
of any shared surface; the private project wiki holds the lane-specific facts.

```
You are onboarding a new OpenFOAM solve lane for our free-surface hull-resistance chain.
Lane: <LANE_NAME>, ssh <USER>@<HOST> (Tailscale; sudo needs a password, so hand me every
install command and I will run it with "! <command>"). Control host: this machine.
Chain repo: digitalmodel worktree <DM_WORKTREE> (branch <BRANCH>); records repo:
llm-wiki-acma <WIKI_PATH>, project <PROJECT_DIR>. Existing lanes for reference:
<LANE_A> (primary), <LANE_B>, <LANE_C>. Work autonomously; ask only for sudo commands
and for anything destructive.

Rules that are not negotiable:
1. Nothing on a lane is hand-edited. Chain scripts flow repo -> lane only through
   digitalmodel scripts/cfd/deploy_lane.sh <USER>@<HOST> <campaign> (records DEPLOYED.txt,
   ships src/digitalmodel as ~/cfd/dm_src for the python gates). A lane-local edit is a bug
   until it is committed to digitalmodel and redeployed.
2. Never edit a running bash script in place. Never pgrep -f a case path (it matches its
   own ssh command line); poll markers (MESH_DONE, RUN_DONE/RUN_FAILED, CONT_DONE/
   CONT_FAILED, PAUSED) and /proc/<pid>/cwd, and use scripts/cfd/lane_probe.sh for status.
3. One solver at a time per lane: interFoam is memory-bandwidth bound, so a second job
   slows both and corrupts any timing. Ranks = physical cores in use, launched with
   --map-by numa --bind-to core; hyperthreads are not ranks. Wrap OpenFOAM's bashrc in
   set +u / set -u.
4. Meshes are shared only as serial polyMesh through the read-only master mesh store
   (scripts/cfd/mesh_store.sh: identity = sha256 of the meshing dicts + STL, comment
   normalised; find/link/promote/verify/status). Processor directories are never copied
   between hosts; a case re-decomposes locally (decomposeParDict rewritten to this lane's
   rank count, scotch).
5. Disk: after a result is harvested and its condition superseded, scripts/cfd/prune_case.sh
   --apply --reason "..." (keeps system/, constant dicts + STL, 0.orig, logs, TIMING.csv,
   postProcessing; writes PRUNED.md). Never an ad-hoc rm -rf.
6. Everything learned goes to the repos, not to chat: code/docs to digitalmodel
   (scripts/cfd, src/digitalmodel/solvers/openfoam, docs/domains/openfoam), lane facts
   and run records to the project wiki (analysis/data/status/config.yml + facts.yml,
   analysis/runs/<case>/, analysis/notes/), method learnings to domains/analysis/pages/.
   Reports go through the report standard (pages/report-standard.md) via the project's
   status pipeline, never as ad-hoc markdown.

Do this, in order, and stop to report after each numbered step:
1. Inventory the machine read-only: OS/arch, lscpu (physical cores, sockets, NUMA),
   memory, disk free on the home filesystem, existing OpenFOAM/MPI, other users' jobs.
   Refuse to proceed if another CFD or GPU job is running without the owner's say-so.
2. Install OpenFOAM v2312. Linux x86_64/aarch64: dl.openfoam.com package
   openfoam2312-default (the other lanes run 2312.260127-2 with system Open MPI 4.1.6),
   or build v2312 from source if no package exists. macOS (Apple silicon): use a native
   build, not Docker (Docker on macOS runs a VM with poor memory bandwidth and breaks the
   timing comparison); the gerlero openfoam-app project ships native v2312 builds with
   bundled Open MPI (github.com/gerlero/openfoam-app), or build from source with Homebrew
   open-mpi; ranks = performance cores only. Verify: source the bashrc; foamVersion;
   mpirun --version; run the interFoam damBreak tutorial in parallel on 4 ranks. Record
   the exact versions and the bashrc path (the chain scripts default to
   /usr/lib/openfoam/openfoam2312/etc/bashrc; on macOS set DM_CFD_OPENFOAM_BASHRC or the
   equivalent override and report if the scripts need a patch, which goes to digitalmodel).
3. Create the lane tree ~/cfd/<campaign>/ {geometry, meshes, cases, scripts, docs, runs,
   stage, status} (root = control plane only: registries, lane entry points, the
   <case>.log/.marker ledger, status/). Deploy the chain with deploy_lane.sh from this
   host. Copy the campaign registry and geometry from <LANE_A>. Do NOT copy cases.
4. Seed the mesh store: rsync the store entries the queued cases need from <LANE_A>
   (~/cfd/<campaign>/meshes/<id>-<name>/, serial polyMesh only), then
   mesh_store.sh verify and mesh_store.sh status on the new lane.
5. Qualify the lane with the portable PIMPLE benchmark pack (project wiki
   analysis/runs/pimple_bench_*/: pimple_bench_prep.sh builds b0..b6 from the 3.2 M-cell
   L4 case; run at least b0 = 200 iterations at 2/3/2 on all physical cores, plus b1 with
   binding). Report s/it and microseconds per cell-iteration; compare with the existing
   lanes (primary 8.05 at 8 ranks, the 32-core x86 lanes 10.2-10.7 at 16 ranks, all at
   2/3/2). If the numerics decision from the benchmark on <LANE_C> is already in, run only
   the adopted variant.
5b. Decide what this lane may run, from the benchmark and the machine, before touching
   the queue. Compute: predicted wall time per case at the measured rate for each mesh
   level in the queue (L4 3.2 M cells, L5 3.75 M, r3 6.05 M, each 8000 pseudo-iterations
   at 2/3/2, or at the adopted variant); memory headroom (interFoam at 2/3/2 needs about
   1.5-2 GB per million cells plus decomposition overhead, so a 6 M-cell case wants
   16 GB free beyond the OS); disk (a 6 M-cell case with 500-iteration writes needs
   ~15 GB; keep 50 GB free); and thermal sustainment (a laptop or small-form-factor box
   must hold its rate over a 200-iteration run without throttling; check s/it drift over
   the benchmark). Then assign: sustained rate within 1.5x of the primary lane and enough
   memory -> full production cases (r3/L5 class and the 33-case star); slower or
   memory-limited -> L4-class sensitivity cases, benchmark variants and reductions only;
   throttling or under 16 GB free -> staging, meshing and post-processing lane, no long
   solves. Write the decision and its numbers in the lane note and report it before
   step 6.
6. Add the lane to the records: config.yml (host, probe paths), facts.yml lanes[] (cpu,
   ranks, peak DP GFLOPS, measured rate with basis "measured"), a lane note in
   analysis/notes/<lane>-onboarding-<date>.md, and a one-line memory pointer. Regenerate
   the status report with the project pipeline (analysis/scripts/README.md) and commit.
7. Only then take a case from the queue: stage it with the project's case generator,
   link its mesh from the store, decompose locally, launch detached through the chain's
   solve entry point with markers and the ledger, confirm the iteration-220 collapse check,
   and set a Monitor on writes (every 500) and on RUN_DONE/RUN_FAILED. Run
   scripts/cfd/fs_gate.sh at 800 force rows and at every later write.

Report format for each step: what was verified (with the command and its key output),
what changed on the lane, what was committed (hashes), and what is blocked on me.
```

Related: `mesh_store_case_layout.md` (store and layout design), `pimple_loop_cost_v2312.md`
(numerics cost facts), `scripts/cfd/README.md` (chain entry points).
