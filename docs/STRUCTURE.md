# Repository structure

This guide covers tracked folders through three levels below the repository root. The table is generated from [structure.yaml](structure.yaml). Paths link to their folders.

Consult [CODEOWNERS](../CODEOWNERS) for review ownership. A dash marks an optional field omitted from the inventory.

| Path | Purpose | Owner | Rules |
| --- | --- | --- | --- |
| [`.claude/`](../.claude/) | Collects repository guidance, skills, hooks and configuration for Claude-assisted development. | — | — |
| [`.claude/cli/`](../.claude/cli/) | Implements interactive commands and progress reporting for the agent CLI. | — | — |
| [`.claude/context/`](../.claude/context/) | Defines project patterns and team conventions used as agent context. | — | — |
| [`.claude/helpers/`](../.claude/helpers/) | Provides setup and checkpoint scripts for local agent workflows. | — | — |
| [`.claude/hooks/`](../.claude/hooks/) | Runs pre-task preparation and captures corrections during agent sessions. | — | — |
| [`.claude/instructions/`](../.claude/instructions/) | Describes how to analyze the product, plan work and execute tasks. | — | — |
| [`.claude/integration/`](../.claude/integration/) | Provides Python helpers for assembling context, updating prompts and refreshing workflows. | — | — |
| [`.claude/knowledge/`](../.claude/knowledge/) | Collects reusable observations drawn from repository changes. | — | — |
| [`.claude/knowledge/patterns/`](../.claude/knowledge/patterns/) | Records the development patterns identified in a reviewed commit. | — | — |
| [`.claude/learning/`](../.claude/learning/) | Configures how agent learnings are captured and reported. | — | — |
| [`.claude/memory/`](../.claude/memory/) | Retains repository-specific context for use in later sessions. | — | — |
| [`.claude/product/`](../.claude/product/) | Describes the product mission, architecture, technology stack and roadmap. | — | — |
| [`.claude/progress/`](../.claude/progress/) | Provides the template for recording work progress. | — | — |
| [`.claude/requirements/`](../.claude/requirements/) | Documents the requirements process and user-story format. | — | — |
| [`.claude/resources/`](../.claude/resources/) | Catalogs agent definitions and templates available for reuse. | — | — |
| [`.claude/rules/`](../.claude/rules/) | Defines repository rules for engineering prose, formatting, geometry validation and package releases. | — | — |
| [`.claude/skills/`](../.claude/skills/) | Indexes repository skills and provides workflow and engineering instructions. | — | — |
| [`.claude/skills/converted-agents/`](../.claude/skills/converted-agents/) | Stores skill definitions converted from existing agent descriptions. | — | — |
| [`.claude/skills/engineering/`](../.claude/skills/engineering/) | Provides engineering skills for solver modeling, CAD, meshing and cathodic protection. | — | — |
| [`.claude/specs/`](../.claude/specs/) | Directs readers to the central location for repository specifications. | — | — |
| [`.claude/standards/`](../.claude/standards/) | Defines conventions for code, data files, tests and repository maintenance. | — | — |
| [`.claude/sub-agents/`](../.claude/sub-agents/) | Defines the development subagent role. | — | — |
| [`.claude/work-queue/`](../.claude/work-queue/) | Retains review evidence for queued work. | — | — |
| [`.claude/work-queue/assets/`](../.claude/work-queue/assets/) | Stores a work item's review iteration record. | — | — |
| [`.claude/workflows/`](../.claude/workflows/) | Describes development workflows and how to document their execution. | — | — |
| [`.claude/workflows/implementation/`](../.claude/workflows/implementation/) | Provides planning, change documentation and implementation tracking templates. | — | — |
| [`.codex/`](../.codex/) | Records the repository's pointer to shared skills for Codex. | — | — |
| [`.gemini/`](../.gemini/) | Records the repository's pointer to shared skills for Gemini. | — | — |
| [`.githooks/`](../.githooks/) | Stores the repository's pre-commit hook. | — | — |
| [`.github/`](../.github/) | Configures dependency updates, code scanning and GitHub Actions workflows. | — | — |
| [`.github/ci/`](../.github/ci/) | Tracks flaky checks for continuous integration. | — | — |
| [`.github/codeql/`](../.github/codeql/) | Configures the scope of CodeQL analysis. | — | — |
| [`.github/workflows/`](../.github/workflows/) | Defines automation for tests, documentation, releases and repository checks. | — | — |
| [`.planning/`](../.planning/) | Collects issue implementation plans and supporting review evidence. | — | — |
| [`.planning/plan-approved/`](../.planning/plan-approved/) | Retains issue plans filed under the plan-approved workflow stage. | — | — |
| [`.planning/quick/`](../.planning/quick/) | Preserves focused review notes and test output for individual changes. | — | — |
| [`assets/`](../assets/) | Supplies vessel specification files, project logos and static resources for web pages. | — | — |
| [`assets/data/`](../assets/data/) | Stores heavy-lift and pipelay vessel specifications with schemas and provenance. | — | — |
| [`assets/logo/`](../assets/logo/) | Supplies the project logo in vector and raster formats. | — | — |
| [`assets/static/`](../assets/static/) | Provides shared stylesheets and scripts for web pages. | — | — |
| [`assets/static/css/`](../assets/static/css/) | Styles navigation, charts and page layouts. | — | — |
| [`assets/static/js/`](../assets/static/js/) | Supplies browser libraries and helpers for charts and page interactions. | — | — |
| [`atlases/`](../atlases/) | The atlas collection stores versioned engineering response grids, interpolation models and validation records. | — | — |
| [`atlases/anchor_capacity/`](../atlases/anchor_capacity/) | This family groups lookup tables for anchor holding capacity. | — | — |
| [`atlases/anchor_capacity/597341b1f32b/`](../atlases/anchor_capacity/597341b1f32b/) | The grid and surrogate tabulate anchor holding capacity for code revision 2a9f5e4a9 alongside the validation record. | — | — |
| [`atlases/anchor_capacity/c8f67ad7b69c/`](../atlases/anchor_capacity/c8f67ad7b69c/) | The grid and surrogate tabulate anchor holding capacity for code revision a3a67b4b alongside the validation record. | — | — |
| [`atlases/code_check/`](../atlases/code_check/) | This family groups lookup tables for riser code-check utilisation. | — | — |
| [`atlases/code_check/23c33b5a2782/`](../atlases/code_check/23c33b5a2782/) | The grid and surrogate tabulate riser code-check utilisation for code revision a3a67b4b alongside the validation record. | — | — |
| [`atlases/code_check/4b48e8c976cd/`](../atlases/code_check/4b48e8c976cd/) | The grid and surrogate tabulate riser code-check utilisation for code revision 415b13d0 alongside the validation record. | — | — |
| [`atlases/diffraction_library/`](../atlases/diffraction_library/) | This library holds a synthetic placeholder for heave response amplitude operators. | — | — |
| [`atlases/diffraction_library/e70b23544de6/`](../atlases/diffraction_library/e70b23544de6/) | The synthetic grid illustrates lookup of heave response amplitude operators pending a licensed solver run. | — | — |
| [`atlases/drilling_riser_drift_off/`](../atlases/drilling_riser_drift_off/) | This library holds a synthetic placeholder for drilling riser drift-off amplification. | — | — |
| [`atlases/drilling_riser_drift_off/d53bddd904d1/`](../atlases/drilling_riser_drift_off/d53bddd904d1/) | The synthetic grid illustrates lookup of drilling riser drift-off amplification pending a licensed solver run. | — | — |
| [`atlases/drilling_riser_envelope/`](../atlases/drilling_riser_envelope/) | This library holds a synthetic placeholder for dynamic amplification of drilling riser equivalent stress. | — | — |
| [`atlases/drilling_riser_envelope/7ca25705d065/`](../atlases/drilling_riser_envelope/7ca25705d065/) | The synthetic grid illustrates lookup of dynamic amplification of drilling riser equivalent stress pending a licensed solver run. | — | — |
| [`atlases/drilling_riser_operability/`](../atlases/drilling_riser_operability/) | This family groups lookup tables for governing drilling riser utilisation. | — | — |
| [`atlases/drilling_riser_operability/229487678a1a/`](../atlases/drilling_riser_operability/229487678a1a/) | The grid screens static drilling riser utilisation using the 2025 edition of API Standard 2RD. | — | — |
| [`atlases/drilling_riser_operability/bcc370f46b9c/`](../atlases/drilling_riser_operability/bcc370f46b9c/) | The grid screens static drilling riser utilisation using the 2013 edition of API Standard 2RD. | — | — |
| [`atlases/esp_pump_hydraulics/`](../atlases/esp_pump_hydraulics/) | This family groups lookup tables for governing hydraulic utilisation of an electric submersible pump. | — | — |
| [`atlases/esp_pump_hydraulics/c00e2cfda4f9/`](../atlases/esp_pump_hydraulics/c00e2cfda4f9/) | The grid and surrogate tabulate governing hydraulic utilisation of an electric submersible pump for code revision 385e674f alongside the validation record. | — | — |
| [`atlases/fowt_mooring/`](../atlases/fowt_mooring/) | This family groups lookup tables for minimum bend radius utilisation for floating wind turbine moorings. | — | — |
| [`atlases/fowt_mooring/0af02c3aaf14/`](../atlases/fowt_mooring/0af02c3aaf14/) | The grid and surrogate tabulate minimum bend radius utilisation for floating wind turbine moorings for code revision 79f28b96 alongside the validation record. | — | — |
| [`atlases/fowt_mooring/15372d97a52f/`](../atlases/fowt_mooring/15372d97a52f/) | The grid and surrogate tabulate minimum bend radius utilisation for floating wind turbine moorings for code revision a9f09147 alongside the validation record. | — | — |
| [`atlases/fowt_mooring/1f1871cd817d/`](../atlases/fowt_mooring/1f1871cd817d/) | The grid and surrogate tabulate minimum bend radius utilisation for floating wind turbine moorings for code revision a1fed2cf0 alongside the validation record. | — | — |
| [`atlases/fowt_mooring/7ac02c8bc7f0/`](../atlases/fowt_mooring/7ac02c8bc7f0/) | The grid and surrogate tabulate minimum bend radius utilisation for floating wind turbine moorings for code revision 0fb9dfcc alongside the validation record. | — | — |
| [`atlases/fowt_mooring/b4046a89d219/`](../atlases/fowt_mooring/b4046a89d219/) | The grid and surrogate tabulate minimum bend radius utilisation for floating wind turbine moorings for code revision 493e73176 alongside the validation record. | — | — |
| [`atlases/fpso_mooring_full/`](../atlases/fpso_mooring_full/) | This family groups lookup tables for maximum mooring line tension on a floating production vessel. | — | — |
| [`atlases/fpso_mooring_full/879f4f041243/`](../atlases/fpso_mooring_full/879f4f041243/) | The grid and surrogate tabulate maximum mooring line tension on a floating production vessel for code revision a3a67b4b alongside the validation record. | — | — |
| [`atlases/fpso_mooring_full/a4f8b8069228/`](../atlases/fpso_mooring_full/a4f8b8069228/) | The grid and surrogate tabulate maximum mooring line tension on a floating production vessel for code revision 8d09f5c73 alongside the validation record. | — | — |
| [`atlases/free_span/`](../atlases/free_span/) | This family groups lookup tables for pipeline free-span utilisation. | — | — |
| [`atlases/free_span/2ca6361f3b7a/`](../atlases/free_span/2ca6361f3b7a/) | The grid and surrogate tabulate pipeline free-span utilisation for code revision 52eeecd1 alongside the validation record. | — | — |
| [`atlases/free_span/749c598d4e23/`](../atlases/free_span/749c598d4e23/) | The grid and surrogate tabulate pipeline free-span utilisation for code revision 09148c1c1 alongside the validation record. | — | — |
| [`atlases/free_span/914ec0881a84/`](../atlases/free_span/914ec0881a84/) | The grid and surrogate tabulate pipeline free-span utilisation for code revision 571ecdcb alongside the validation record. | — | — |
| [`atlases/free_span/eac25055a83d/`](../atlases/free_span/eac25055a83d/) | The grid and surrogate tabulate pipeline free-span utilisation for code revision a3a67b4b alongside the validation record. | — | — |
| [`atlases/inspection_planning/`](../atlases/inspection_planning/) | This family groups lookup tables for remaining life for inspection planning. | — | — |
| [`atlases/inspection_planning/ad2fc3ee7a73/`](../atlases/inspection_planning/ad2fc3ee7a73/) | The grid and surrogate tabulate remaining life for inspection planning for code revision 0cefe0c8 alongside the validation record. | — | — |
| [`atlases/legacy-evidence/`](../atlases/legacy-evidence/) | The manifest maps historical evidence references to their archived contents. | — | — |
| [`atlases/legacy-evidence/blobs/`](../atlases/legacy-evidence/blobs/) | The files preserve historical evidence under content digests used by the archive manifest. | — | — |
| [`atlases/lifting_lug/`](../atlases/lifting_lug/) | This family groups lookup tables for governing lifting lug utilisation. | — | — |
| [`atlases/lifting_lug/060345160406/`](../atlases/lifting_lug/060345160406/) | The grid and surrogate tabulate governing lifting lug utilisation for code revision 2441037b alongside the validation record. | — | — |
| [`atlases/lifting_lug/90e306065a25/`](../atlases/lifting_lug/90e306065a25/) | The grid and surrogate tabulate governing lifting lug utilisation for code revision 3554b14fd alongside the validation record. | — | — |
| [`atlases/lifting_lug/d807d703f351/`](../atlases/lifting_lug/d807d703f351/) | The grid and surrogate tabulate governing lifting lug utilisation for code revision 92fe68d7 alongside the validation record. | — | — |
| [`atlases/mooring_fatigue/`](../atlases/mooring_fatigue/) | This family groups lookup tables for mooring line fatigue damage. | — | — |
| [`atlases/mooring_fatigue/0692c1977b15/`](../atlases/mooring_fatigue/0692c1977b15/) | The grid and surrogate tabulate mooring line fatigue damage for code revision 27a3cdb16 alongside the validation record. | — | — |
| [`atlases/mooring_fatigue/3ef6e08f35be/`](../atlases/mooring_fatigue/3ef6e08f35be/) | The grid and surrogate tabulate mooring line fatigue damage for code revision a3a67b4b alongside the validation record. | — | — |
| [`atlases/mooring_fatigue/6663b59124c0/`](../atlases/mooring_fatigue/6663b59124c0/) | The grid and surrogate tabulate mooring line fatigue damage for code revision 35ad550cf alongside the validation record. | — | — |
| [`atlases/mooring_fatigue/74927e2a997f/`](../atlases/mooring_fatigue/74927e2a997f/) | The grid and surrogate tabulate mooring line fatigue damage for code revision 8650964b alongside the validation record. | — | — |
| [`atlases/mooring_fatigue/b79abc111c1a/`](../atlases/mooring_fatigue/b79abc111c1a/) | The grid and surrogate tabulate mooring line fatigue damage for code revision 9230b1e24 alongside the validation record. | — | — |
| [`atlases/mooring_fatigue/f8b3c0dea8de/`](../atlases/mooring_fatigue/f8b3c0dea8de/) | The grid and surrogate tabulate mooring line fatigue damage for code revision 415b13d0 alongside the validation record. | — | — |
| [`atlases/mudmat_bearing_capacity/`](../atlases/mudmat_bearing_capacity/) | This family groups lookup tables for mudmat bearing capacity utilisation. | — | — |
| [`atlases/mudmat_bearing_capacity/3e49b0580be0/`](../atlases/mudmat_bearing_capacity/3e49b0580be0/) | The grid and surrogate tabulate mudmat bearing capacity utilisation for code revision 36e21ae6e alongside the validation record. | — | — |
| [`atlases/mudmat_bearing_capacity/4ab9337f5f3b/`](../atlases/mudmat_bearing_capacity/4ab9337f5f3b/) | The grid and surrogate tabulate mudmat bearing capacity utilisation for code revision 0cefe0c8 alongside the validation record. | — | — |
| [`atlases/mudmat_bearing_capacity/857c35d04107/`](../atlases/mudmat_bearing_capacity/857c35d04107/) | The grid and surrogate tabulate mudmat bearing capacity utilisation for code revision 7207ec61 alongside the validation record. | — | — |
| [`atlases/orcaflex_library/`](../atlases/orcaflex_library/) | This library holds a synthetic placeholder for maximum effective line tension. | — | — |
| [`atlases/orcaflex_library/c198e029c428/`](../atlases/orcaflex_library/c198e029c428/) | The synthetic grid illustrates lookup of maximum effective line tension pending a licensed solver run. | — | — |
| [`atlases/pile_capacity/`](../atlases/pile_capacity/) | This family groups lookup tables for pile capacity. | — | — |
| [`atlases/pile_capacity/ff5a09087842/`](../atlases/pile_capacity/ff5a09087842/) | The grid and surrogate tabulate pile capacity for code revision a3a67b4b alongside the validation record. | — | — |
| [`atlases/rao_tabulation/`](../atlases/rao_tabulation/) | This family groups lookup tables for heave response. | — | — |
| [`atlases/rao_tabulation/6b27320669f6/`](../atlases/rao_tabulation/6b27320669f6/) | The grid and surrogate tabulate heave response for code revision a3a67b4b alongside the validation record. | — | — |
| [`atlases/rao_tabulation/f3acf821a481/`](../atlases/rao_tabulation/f3acf821a481/) | The grid and surrogate tabulate heave response for code revision e402864ea alongside the validation record. | — | — |
| [`atlases/riser_fatigue/`](../atlases/riser_fatigue/) | This family groups lookup tables for annual riser fatigue damage. | — | — |
| [`atlases/riser_fatigue/2629bee51367/`](../atlases/riser_fatigue/2629bee51367/) | The grid and surrogate tabulate annual riser fatigue damage for code revision 27a3cdb16 alongside the validation record. | — | — |
| [`atlases/riser_fatigue/51b7f03704cf/`](../atlases/riser_fatigue/51b7f03704cf/) | The grid and surrogate tabulate annual riser fatigue damage for code revision 21c33ba8 alongside the validation record. | — | — |
| [`atlases/riser_fatigue/9f7d389eed07/`](../atlases/riser_fatigue/9f7d389eed07/) | The grid and surrogate tabulate annual riser fatigue damage for code revision f8552087 alongside the validation record. | — | — |
| [`atlases/riser_fatigue/b53753cd275a/`](../atlases/riser_fatigue/b53753cd275a/) | The grid and surrogate tabulate annual riser fatigue damage for code revision 9230b1e24 alongside the validation record. | — | — |
| [`atlases/riser_fatigue/c77df5a05685/`](../atlases/riser_fatigue/c77df5a05685/) | The grid and surrogate tabulate annual riser fatigue damage for code revision 35ad550cf alongside the validation record. | — | — |
| [`atlases/riser_fatigue/f272ab682259/`](../atlases/riser_fatigue/f272ab682259/) | The grid and surrogate tabulate annual riser fatigue damage for code revision 8650964b alongside the validation record. | — | — |
| [`atlases/riser_fatigue/f2ca1d87007d/`](../atlases/riser_fatigue/f2ca1d87007d/) | The grid and surrogate tabulate annual riser fatigue damage for code revision 3abf0236 alongside the validation record. | — | — |
| [`atlases/span_rectification/`](../atlases/span_rectification/) | This family groups lookup tables for governing utilisation following free-span rectification. | — | — |
| [`atlases/span_rectification/50a333f44a01/`](../atlases/span_rectification/50a333f44a01/) | The grid and surrogate tabulate governing utilisation following free-span rectification for code revision 0cefe0c8 alongside the validation record. | — | — |
| [`atlases/span_rectification/5727560821e4/`](../atlases/span_rectification/5727560821e4/) | The grid and surrogate tabulate governing utilisation following free-span rectification for code revision 09148c1c1 alongside the validation record. | — | — |
| [`atlases/spectral_fatigue/`](../atlases/spectral_fatigue/) | This family groups lookup tables for annual damage from spectral fatigue calculations. | — | — |
| [`atlases/spectral_fatigue/6840fbb96cb7/`](../atlases/spectral_fatigue/6840fbb96cb7/) | The grid and surrogate tabulate annual damage from spectral fatigue calculations for code revision 578268eb0 alongside the validation record. | — | — |
| [`atlases/spectral_fatigue/767cc51c006a/`](../atlases/spectral_fatigue/767cc51c006a/) | The grid and surrogate tabulate annual damage from spectral fatigue calculations for code revision e402864ea alongside the validation record. | — | — |
| [`atlases/spectral_fatigue/8e9bbf404b2e/`](../atlases/spectral_fatigue/8e9bbf404b2e/) | The grid and surrogate tabulate annual damage from spectral fatigue calculations for code revision 34c4a25c3 alongside the validation record. | — | — |
| [`atlases/spectral_fatigue/9006c0eaccb3/`](../atlases/spectral_fatigue/9006c0eaccb3/) | The grid and surrogate tabulate annual damage from spectral fatigue calculations for code revision 701ab147b alongside the validation record. | — | — |
| [`atlases/spectral_fatigue/9e446976dffc/`](../atlases/spectral_fatigue/9e446976dffc/) | The grid and surrogate tabulate annual damage from spectral fatigue calculations for code revision e7c94f0c alongside the validation record. | — | — |
| [`atlases/spectral_fatigue/c48149ab7f9e/`](../atlases/spectral_fatigue/c48149ab7f9e/) | The grid and surrogate tabulate annual damage from spectral fatigue calculations for code revision a3a67b4b alongside the validation record. | — | — |
| [`atlases/synthetic_rope_mooring_fatigue/`](../atlases/synthetic_rope_mooring_fatigue/) | This family groups lookup tables for synthetic mooring rope fatigue damage. | — | — |
| [`atlases/synthetic_rope_mooring_fatigue/cfc09a9f7593/`](../atlases/synthetic_rope_mooring_fatigue/cfc09a9f7593/) | The grid and surrogate tabulate synthetic mooring rope fatigue damage for code revision 2f9ac35d9 alongside the validation record. | — | — |
| [`atlases/synthetic_rope_mooring_fatigue/f2348d61dea8/`](../atlases/synthetic_rope_mooring_fatigue/f2348d61dea8/) | The grid and surrogate tabulate synthetic mooring rope fatigue damage for code revision a3a67b4b alongside the validation record. | — | — |
| [`atlases/viv_analysis/`](../atlases/viv_analysis/) | This family groups lookup tables for the inline vortex-induced vibration safety factor. | — | — |
| [`atlases/viv_analysis/862a28080fc9/`](../atlases/viv_analysis/862a28080fc9/) | The grid and surrogate tabulate the inline vortex-induced vibration safety factor for code revision a3a67b4b alongside the validation record. | — | — |
| [`atlases/viv_analysis/909b1cf34ed0/`](../atlases/viv_analysis/909b1cf34ed0/) | The grid and surrogate tabulate the inline vortex-induced vibration safety factor for code revision a8fe5369a alongside the validation record. | — | — |
| [`atlases/weather_window/`](../atlases/weather_window/) | This family groups lookup tables for weather window operability. | — | — |
| [`atlases/weather_window/382bac9f91be/`](../atlases/weather_window/382bac9f91be/) | The grid and surrogate tabulate weather window operability for code revision 66a5c8da4 alongside the validation record. | — | — |
| [`atlases/weather_window/d5afba812fb2/`](../atlases/weather_window/d5afba812fb2/) | The grid and surrogate tabulate weather window operability for code revision 0cefe0c8 alongside the validation record. | — | — |
| [`benchmarks/`](../benchmarks/) | Holds reproducible solver comparisons with inputs, reference values and recorded outputs. | — | — |
| [`benchmarks/fe10_nafems_le1/`](../benchmarks/fe10_nafems_le1/) | Compares finite-element stresses for the NAFEMS LE1 elliptic membrane and a constant-stress patch case. | — | — |
| [`benchmarks/fe10_nafems_le1/console/`](../benchmarks/fe10_nafems_le1/console/) | Records the commands and console output for deck preparation, solver execution and result comparison. | — | — |
| [`benchmarks/fe10_nafems_le1/inputs/`](../benchmarks/fe10_nafems_le1/inputs/) | Supplies finite-element input decks for three membrane meshes and a patch case. | — | — |
| [`benchmarks/fe10_nafems_le1/native/`](../benchmarks/fe10_nafems_le1/native/) | Retains native solver listings and error logs for the benchmark meshes. | — | — |
| [`benchmarks/fe10_nafems_le1/preflight/`](../benchmarks/fe10_nafems_le1/preflight/) | Records readiness checks and their results before benchmark execution. | — | — |
| [`benchmarks/fe10_nafems_le1/run-requests/`](../benchmarks/fe10_nafems_le1/run-requests/) | Stores the machine-readable request to run the membrane benchmark. | — | — |
| [`benchmarks/fe10_nafems_le1/sources/`](../benchmarks/fe10_nafems_le1/sources/) | Retains the reference code excerpt used to construct the benchmark. | — | — |
| [`benchmarks/fe10_nafems_le1/tools/`](../benchmarks/fe10_nafems_le1/tools/) | Provides scripts to build and validate decks, extract stresses and check expected results. | — | — |
| [`config/`](../config/) | Configures engineering analyses, data workflows, reporting and repository automation. | — | — |
| [`config/ai-tools/`](../config/ai-tools/) | Records the latest tracked agent quota snapshot. | — | — |
| [`config/api579/`](../config/api579/) | Supplies API 579 fitness-for-service input cases for pipe and riser components. | — | — |
| [`config/cfd/`](../config/cfd/) | Defines calm-water CFD study inputs, workflow stages and mesh checks. | — | — |
| [`config/compare_tool/`](../config/compare_tool/) | Configures comparisons of result files, tables and engineering response series. | — | — |
| [`config/data_models/`](../config/data_models/) | Maps public offshore data tables and defines how to refresh them. | — | — |
| [`config/etl/`](../config/etl/) | Specifies transformations for current profiles, fatigue models and wave histograms. | — | — |
| [`config/fatigue_analysis/`](../config/fatigue_analysis/) | Configures wave-fatigue and combined-damage calculations. | — | — |
| [`config/fracture_mechanics/`](../config/fracture_mechanics/) | Defines fracture mechanics cases for riser components and crack-growth assessments. | — | — |
| [`config/gis/`](../config/gis/) | Supplies ArcGIS configuration files. | — | — |
| [`config/input/`](../config/input/) | Provides sample coating conditions for cathodic protection and inputs for pipeline pressure containment. | — | — |
| [`config/pipe/`](../config/pipe/) | Defines pipe design cases for pressure, geometry, water depth and code comparisons. | — | — |
| [`config/publication/`](../config/publication/) | Defines the data, metrics and reports selected for publication from the vibration screening study. | — | — |
| [`config/templates/`](../config/templates/) | Collects reusable engineering project configuration templates. | — | — |
| [`config/templates/mooring/`](../config/templates/mooring/) | Provides a project template for a catenary anchor leg mooring buoy. | — | — |
| [`config/validation/`](../config/validation/) | Provides a template for checking required fields, data types and uniqueness. | — | — |
| [`config/visualization/`](../config/visualization/) | Defines plots for environmental conditions, vessel response and riser loads. | — | — |
| [`config/visualization/results/`](../config/visualization/results/) | Retains footprint plots produced from visualization configurations. | — | — |
| [`config/viv_analysis/`](../config/viv_analysis/) | Defines vortex-induced vibration cases for currents, shielding, modal response and fatigue. | — | — |
| [`data/`](../data/) | Collects engineering reference tables, model inputs and dataset inventories. | — | — |
| [`data/bha_database/`](../data/bha_database/) | Indexes bottom-hole assembly examples and records their dataset manifest. | — | — |
| [`data/bha_standards/`](../data/bha_standards/) | Supplies standard drill collar dimensions for bottom-hole assembly modeling. | — | — |
| [`data/cfd/`](../data/cfd/) | Collects CFD study measurements and run records. | — | — |
| [`data/cfd/calm_water_study/`](../data/cfd/calm_water_study/) | Retains resistance coefficients, mesh diagnostics and provenance for a calm-water CFD study. | — | — |
| [`data/cross_sections/`](../data/cross_sections/) | Catalogs power cable cross-section source data and defines its schema. | — | — |
| [`data/doc-intelligence/`](../data/doc-intelligence/) | Collects engineering curves extracted from documents for reuse in validation. | — | — |
| [`data/doc-intelligence/digitized-curves/`](../data/doc-intelligence/digitized-curves/) | Stores heel angles and corresponding righting arms for digitized vessel stability curves. | — | — |
| [`data/drilling_rigs/`](../data/drilling_rigs/) | Catalogs externally sourced ultra-deepwater drilling rig records and their coverage. | — | — |
| [`data/fatigue/`](../data/fatigue/) | Stores stress-life curves for fatigue calculations. | — | — |
| [`data/fender_design/`](../data/fender_design/) | Indexes fender catalog sizes, design methods, governing bodies and illustrative response curves. | — | — |
| [`data/flexible_pipe/`](../data/flexible_pipe/) | Catalogs flexible pipe properties with a schema and source records. | — | — |
| [`data/geotechnical/`](../data/geotechnical/) | Stores the geotechnical reference database. | — | — |
| [`data/hull_library/`](../data/hull_library/) | Registers hull geometries and pairs panel meshes with profile definitions. | — | — |
| [`data/hull_library/panels/`](../data/hull_library/panels/) | Supplies panel meshes for generic hulls and hydrodynamic test bodies. | — | — |
| [`data/hull_library/profiles/`](../data/hull_library/profiles/) | Describes the hull profiles corresponding to the panel meshes. | — | — |
| [`data/line_pipe/`](../data/line_pipe/) | Catalogs line pipe records with their schema and sources. | — | — |
| [`data/riser_buoyancy/`](../data/riser_buoyancy/) | Catalogs riser buoyancy records with their schema and sources. | — | — |
| [`data/riser_capability/`](../data/riser_capability/) | Defines riser types used for capability screening. | — | — |
| [`data/riser_database/`](../data/riser_database/) | Catalogs riser configurations, stackups, interfaces and fatigue design inputs. | — | — |
| [`data/riser_database/structure_properties/`](../data/riser_database/structure_properties/) | Stores structural property records for riser components with their schema and sources. | — | — |
| [`data/riser_tensioners/`](../data/riser_tensioners/) | Catalogs riser tensioner records with their schema and sources. | — | — |
| [`data/roll_damping/`](../data/roll_damping/) | Collects roll damping benchmarks, calculation methods, sources and acceptance criteria. | — | — |
| [`data/ship_structure_reference/`](../data/ship_structure_reference/) | Stores elastic reference inputs, research schemas and source metadata for ship structures. | — | — |
| [`data/soil_stiffness/`](../data/soil_stiffness/) | Catalogs soil stiffness records with their units, schema and sources. | — | — |
| [`data/vessels/`](../data/vessels/) | Supplies response data, engineering inputs and source records that supplement the vessel fleet catalog. | — | Cite measured values, label estimates with their basis and mark missing values as gaps. |
| [`data/vessels/processed/`](../data/vessels/processed/) | Supplies normalized vessel particulars, response dataset indexes, crane summaries and metocean criteria. | — | — |
| [`data/vessels/raw/`](../data/vessels/raw/) | Retains vessel-class records of particulars, response data and operating criteria before consolidation. | — | — |
| [`data/vessels/sources/`](../data/vessels/sources/) | Indexes source records by vessel class. | — | — |
| [`docs/`](../docs/) | The documentation index routes readers to engineering methods, examples, reports and module registries. | — | — |
| [`docs/adapters/`](../docs/adapters/) | The adapter catalog describes how to parse and reuse existing CalculiX ASCII results. | — | — |
| [`docs/api/`](../docs/api/) | The reference pages present engineering calculations, interactive examples and guidance for contributors. | — | — |
| [`docs/api/_assets/`](../docs/api/_assets/) | The stylesheet and layout guide define the presentation of the API pages. | — | — |
| [`docs/api/artificial-lift/`](../docs/api/artificial-lift/) | The troubleshooting page and its data help readers interpret dynamometer cards for rod pumps. | — | — |
| [`docs/api/buckling/`](../docs/api/buckling/) | The worked pages describe buckling checks for ship plates and stiffened panels. | — | — |
| [`docs/api/capabilities/`](../docs/api/capabilities/) | The index presents the software's engineering capabilities. | — | — |
| [`docs/api/cfd/`](../docs/api/cfd/) | Case files and analysis scripts document computational fluid dynamics benchmarks. | — | — |
| [`docs/api/contributing/`](../docs/api/contributing/) | The contributor notes describe import safety and release procedures. | — | — |
| [`docs/api/corrosion/`](../docs/api/corrosion/) | An interactive explorer and its data illustrate galvanic compatibility. | — | — |
| [`docs/api/drilling/`](../docs/api/drilling/) | Interactive reports show how drilling riser utilisation affects operability. | — | — |
| [`docs/api/ffs/`](../docs/api/ffs/) | Assessment examples and readiness reports document fitness-for-service calculations. | — | — |
| [`docs/api/hydro/`](../docs/api/hydro/) | Interactive pages compare hydrodynamic coefficients, vessel responses and manoeuvring benchmarks. | — | — |
| [`docs/api/installation/`](../docs/api/installation/) | The explorers show installation forecasts and heading effects with manifests for the underlying data. | — | — |
| [`docs/api/orcaflex/`](../docs/api/orcaflex/) | Reports present riser validation and mesh sensitivity results from OrcaFlex. | — | — |
| [`docs/api/production/`](../docs/api/production/) | The explorer shows scale saturation indices for production fluids. | — | — |
| [`docs/api/structural/`](../docs/api/structural/) | Interactive reports illustrate engineering checks including anchor capacity and cathodic protection. | — | — |
| [`docs/api/subsea/`](../docs/api/subsea/) | The report describes offshore cross sections. | — | — |
| [`docs/api/well/`](../docs/api/well/) | The explorer and its data illustrate well casing design checks. | — | — |
| [`docs/apps/`](../docs/apps/) | The application guides describe how to use the repository's browser tools. | — | — |
| [`docs/apps/dynacard/`](../docs/apps/dynacard/) | The application guide explains how a surface dynamometer card is converted to a pump card in the browser. | — | — |
| [`docs/architecture/`](../docs/architecture/) | Design records explain field scene data contracts, catenary workflows and runtime evidence requirements. | — | — |
| [`docs/archive/`](../docs/archive/) | Manifests and review records document the selection and verification of archived material. | — | — |
| [`docs/articles/`](../docs/articles/) | Companion materials provide data, methods and prompts for technical articles. | — | — |
| [`docs/articles/fitness-for-service-corroded-pipelines/`](../docs/articles/fitness-for-service-corroded-pipelines/) | The article companion supplies the measurement window, calculated results and method for assessing corroded pipelines. | — | — |
| [`docs/articles/training-an-ai-assistant/`](../docs/articles/training-an-ai-assistant/) | The companion notes document the methods and reusable prompts behind the engineering assistant article. | — | — |
| [`docs/audits/`](../docs/audits/) | Audit reports record the findings of capability inventories, knowledge checks and calculation regressions. | — | — |
| [`docs/benchmarks/`](../docs/benchmarks/) | Reference cases, qualification criteria and comparisons document selected solver benchmarks. | — | — |
| [`docs/benchmarks/catenary_closed_form/`](../docs/benchmarks/catenary_closed_form/) | Plain and elastic catenary cases supply hand calculations and solver inputs for static equilibrium comparisons. | — | — |
| [`docs/benchmarks/mooring_buoy/`](../docs/benchmarks/mooring_buoy/) | The qualification file defines expected solver settings and mooring properties for the buoy case. | — | — |
| [`docs/benchmarks/near-wall-acceptance/`](../docs/benchmarks/near-wall-acceptance/) | The report explains near-wall mesh acceptance criteria and illustrates the coverage calculation. | — | — |
| [`docs/benchmarks/unit_box/`](../docs/benchmarks/unit_box/) | The benchmark compares solver predictions of response amplitude and phase for a unit box. | — | — |
| [`docs/cad-inventory/`](../docs/cad-inventory/) | The inventory documents CAD and CAM files and the tools used to inspect them. | — | — |
| [`docs/cad-inventory/adapters/`](../docs/cad-inventory/adapters/) | Scripts and evaluation notes describe how to extract, convert and deduplicate CAD inventory records. | — | — |
| [`docs/cad-reader/`](../docs/cad-reader/) | The reader guide and revision record explain the CAD reading workflow. | — | — |
| [`docs/capability-map/`](../docs/capability-map/) | Capability inventories, cards and grouping rules supply the engineering capability map. | — | — |
| [`docs/ci/`](../docs/ci/) | The CI guides define coverage checks, test selection and rerun procedures and record known test-suite issues. | — | — |
| [`docs/collide_pe/`](../docs/collide_pe/) | The petroleum engineering problem collection pairs challenge statements with worked solutions and brief-generation tools. | — | — |
| [`docs/collide_pe/briefs/`](../docs/collide_pe/briefs/) | The rendered briefs present worked solutions to individual petroleum engineering challenges. | — | — |
| [`docs/collide_pe/dynacard-undulations/`](../docs/collide_pe/dynacard-undulations/) | Analysis scripts, reports and a data request investigate undulations in rod-pump dynamometer cards. | — | — |
| [`docs/collide_pe/problems/`](../docs/collide_pe/problems/) | The structured problem files record each challenge's statement, given quantities and solution. | — | — |
| [`docs/collide_pe/templates/`](../docs/collide_pe/templates/) | The letterhead template defines the layout of the generated petroleum engineering briefs. | — | — |
| [`docs/data/`](../docs/data/) | The guides describe dataset coverage, catalog freshness and the mooring coefficient corpus. | — | — |
| [`docs/demos/`](../docs/demos/) | Readiness assessments identify engineering examples available for demonstrations. | — | — |
| [`docs/demos/2026-06-05-client-c29b/`](../docs/demos/2026-06-05-client-c29b/) | The assessment records offshore and subsea example readiness for the June 2026 demonstration. | — | — |
| [`docs/development/`](../docs/development/) | The development guides cover example testing, dependency contracts and report qualification. | — | — |
| [`docs/domains/`](../docs/domains/) | The domain index organises engineering references, software guides and analysis examples by subject. | — | — |
| [`docs/domains/BEMRosetta/`](../docs/domains/BEMRosetta/) | The reference points readers to the BEMRosetta hydrodynamic data tool. | — | — |
| [`docs/domains/agent-os/`](../docs/domains/agent-os/) | The guides cover setup, use, migration and troubleshooting for the specification-writing workflow. | — | — |
| [`docs/domains/agents/`](../docs/domains/agents/) | The design notes document agent registration, orchestration and integration. | — | — |
| [`docs/domains/ai/`](../docs/domains/ai/) | The notes discuss AI strategy and the use of physics in machine learning. | — | — |
| [`docs/domains/ansys/`](../docs/domains/ansys/) | The reference materials describe engineering analysis workflows using Ansys tools. | — | — |
| [`docs/domains/aqwa/`](../docs/domains/aqwa/) | The AQWA guides cover hydrodynamic analysis, output parsing and testing. | — | — |
| [`docs/domains/articles/`](../docs/domains/articles/) | The article collection covers engineering topics and software development practices. | — | — |
| [`docs/domains/artificial_lift/`](../docs/domains/artificial_lift/) | The diagnostic comparisons, worked examples and numerical methods explain rod-pump card analysis. | — | — |
| [`docs/domains/asset-integrity/`](../docs/domains/asset-integrity/) | Method and validation reports document integrity assessments for corrosion, cracks, dents and other defects. | — | — |
| [`docs/domains/assets/`](../docs/domains/assets/) | The illustration depicts a catenary curve. | — | — |
| [`docs/domains/autocad/`](../docs/domains/autocad/) | Macros, AutoLISP routines and notes provide examples of drawing automation in AutoCAD. | — | — |
| [`docs/domains/automation/`](../docs/domains/automation/) | Implementation notes document automated quality checks, agent selection and subprocess review. | — | — |
| [`docs/domains/biomedical/`](../docs/domains/biomedical/) | UNKNOWN - The only tracked item is an AlphaFold2 image with no accompanying explanation. | — | — |
| [`docs/domains/blender/`](../docs/domains/blender/) | The Blender guides cover automation, extensions and handling of drawing files. | — | — |
| [`docs/domains/catenary/`](../docs/domains/catenary/) | The records trace the consolidation, integration and migration of catenary analysis tools. | — | — |
| [`docs/domains/cathodic_protection/`](../docs/domains/cathodic_protection/) | Design notes and benchmark reports cover coatings, anode materials and cathodic protection calculations. | — | — |
| [`docs/domains/charts/`](../docs/domains/charts/) | Animations and plots visualise hydrodynamic added mass and coupling across frequencies. | — | — |
| [`docs/domains/coiled-tubing/`](../docs/domains/coiled-tubing/) | Hydraulics theory, equipment data and reference materials support coiled tubing analysis. | — | — |
| [`docs/domains/connections/`](../docs/domains/connections/) | The notes and diagram explain flange classes. | — | — |
| [`docs/domains/core/`](../docs/domains/core/) | The guides describe the provenance system and its implementation. | — | — |
| [`docs/domains/cranes/`](../docs/domains/cranes/) | The notes and illustration provide references on crane equipment. | — | — |
| [`docs/domains/data/`](../docs/domains/data/) | The guides describe engineering data architecture, procurement and catalog use. | — | — |
| [`docs/domains/data_systems/`](../docs/domains/data_systems/) | Architecture diagrams and component guides explain data collection and management. | — | — |
| [`docs/domains/dave/`](../docs/domains/dave/) | The reference points readers to the Open Ocean site. | — | — |
| [`docs/domains/davelab/`](../docs/domains/davelab/) | The introductory page links to the DAVE Lab website and company profile. | — | — |
| [`docs/domains/design/`](../docs/domains/design/) | Drawing conventions, editing notes and sample geometry document the mechanical design workflow. | — | — |
| [`docs/domains/design-tools/`](../docs/domains/design-tools/) | Conversion guides explain how to produce CAD drawings from PDFs and panel geometry from STL meshes. | — | — |
| [`docs/domains/development/`](../docs/domains/development/) | Checklists and handover records describe testing, navigation and web resource management. | — | — |
| [`docs/domains/diagrams/`](../docs/domains/diagrams/) | Source diagrams and rendered figures illustrate software architecture and engineering workflows. | — | — |
| [`docs/domains/diffraction/`](../docs/domains/diffraction/) | The guides document conversion, validation and comparison of wave diffraction solver outputs. | — | — |
| [`docs/domains/digitalmarketing/`](../docs/domains/digitalmarketing/) | Guides and diagrams document search engine optimisation and web analytics. | — | — |
| [`docs/domains/digitaltwin/`](../docs/domains/digitaltwin/) | The reference notes introduce digital twin concepts. | — | — |
| [`docs/domains/dom_poc_1/`](../docs/domains/dom_poc_1/) | The example pairs a pipeline lateral buckling configuration with its test and saved result. | — | — |
| [`docs/domains/drilling/`](../docs/domains/drilling/) | The notes and illustrations cover casing design, directional drilling and rig equipment. | — | — |
| [`docs/domains/drilling_riser/`](../docs/domains/drilling_riser/) | The methods page explains hand checks for drilling risers. | — | — |
| [`docs/domains/ecosystem/`](../docs/domains/ecosystem/) | The guides document project commands and conventions for invoking them. | — | — |
| [`docs/domains/electrical/`](../docs/domains/electrical/) | The lighting index and illustration cover illumination calculations. | — | — |
| [`docs/domains/engineering/`](../docs/domains/engineering/) | A diagram and its editable source illustrate workplace etiquette. | — | — |
| [`docs/domains/examples/`](../docs/domains/examples/) | Runnable examples and configuration samples demonstrate signal analysis, parallel processing and OrcaFlex automation. | — | — |
| [`docs/domains/fatigue/`](../docs/domains/fatigue/) | The reference materials cover cycle counting, spectral fatigue and allowable stress ranges. | — | — |
| [`docs/domains/fem/`](../docs/domains/fem/) | The reference points readers to Nastran finite element analysis material. | — | — |
| [`docs/domains/fire_safety/`](../docs/domains/fire_safety/) | The guides describe foam system sizing and technical fire safety screening. | — | — |
| [`docs/domains/freecad/`](../docs/domains/freecad/) | The notes and examples cover design, assemblies and drawing automation in FreeCAD. | — | — |
| [`docs/domains/geotechnical/`](../docs/domains/geotechnical/) | Methods and evidence notes document soil interpretation, slope stability and jack-up foundation behaviour. | — | — |
| [`docs/domains/ghs/`](../docs/domains/ghs/) | The guides and examples document GHS hydrostatics workflows and API setup. | — | — |
| [`docs/domains/gmsh/`](../docs/domains/gmsh/) | The Gmsh examples demonstrate meshing operations through geometry files and API programs. | — | — |
| [`docs/domains/guides/`](../docs/domains/guides/) | The index organises legacy engineering guides and their supporting worked files. | — | — |
| [`docs/domains/hull_library/`](../docs/domains/hull_library/) | Reports and catalogs record hull geometry diagnostics, curvature signatures and generated-form checks. | — | — |
| [`docs/domains/hydrodynamics/`](../docs/domains/hydrodynamics/) | The references cover hydrodynamic loads, vessel resistance and solver comparisons. | — | — |
| [`docs/domains/infrastructure/`](../docs/domains/infrastructure/) | Implementation and usage guides describe the configuration registry, quality checks and parallel processing. | — | — |
| [`docs/domains/installation/`](../docs/domains/installation/) | The notes and illustrations describe offshore installation equipment and operational roles. | — | — |
| [`docs/domains/integration/`](../docs/domains/integration/) | The integration guide explains how the project's core components work together. | — | — |
| [`docs/domains/interventions/`](../docs/domains/interventions/) | The illustrated notes cover well intervention equipment and riserless operations. | — | — |
| [`docs/domains/legacy/`](../docs/domains/legacy/) | The collection preserves earlier calculation configurations, figures and worked engineering examples. | — | — |
| [`docs/domains/lifting/`](../docs/domains/lifting/) | The worked report documents the design of a welded padeye. | — | — |
| [`docs/domains/lighting/`](../docs/domains/lighting/) | The illustration records an illumination calculation. | — | — |
| [`docs/domains/lng/`](../docs/domains/lng/) | The note and illustration cover stratification in liquefied natural gas. | — | — |
| [`docs/domains/marine-engineering/`](../docs/domains/marine-engineering/) | Module guides and verification reports document marine analysis and extraction of vessel response operators. | — | — |
| [`docs/domains/marine_ops/`](../docs/domains/marine_ops/) | The collection links operational references for marine analysis and coiled tubing hydraulics. | — | — |
| [`docs/domains/materials/`](../docs/domains/materials/) | Validation notes identify material database gaps and document line-pipe grade checks. | — | — |
| [`docs/domains/mathcad/`](../docs/domains/mathcad/) | The design note outlines conversion of Mathcad calculations to testable Python code. | — | — |
| [`docs/domains/metocean/`](../docs/domains/metocean/) | The references document marine weather data sources, measurement and hurricane conditions. | — | — |
| [`docs/domains/migration/`](../docs/domains/migration/) | The migration records cover OrcaFlex workflows and signal analysis. | — | — |
| [`docs/domains/mooring/`](../docs/domains/mooring/) | Guides and examples document mooring line selection, buoy systems and analysis workflows. | — | — |
| [`docs/domains/naval-architecture/`](../docs/domains/naval-architecture/) | The audit and worked example document generated vessel geometry and tank gauging. | — | — |
| [`docs/domains/naval_architecture/`](../docs/domains/naval_architecture/) | Qualification records document the evidence for vessel resistance and naval architecture acceptance checks. | — | — |
| [`docs/domains/naviworks/`](../docs/domains/naviworks/) | The note links to a script-based method for creating animations from saved images. | — | — |
| [`docs/domains/offshore-engineering/`](../docs/domains/offshore-engineering/) | The method notes explain riser fatigue and the use of stress concentration factors in fatigue calculations. | — | — |
| [`docs/domains/offshore_installation/`](../docs/domains/offshore_installation/) | The illustrated references cover installation vessels, cranes, tow heads and lifting factors. | — | — |
| [`docs/domains/openfoam/`](../docs/domains/openfoam/) | Case studies, benchmark metrics and setup notes document OpenFOAM marine simulations. | — | — |
| [`docs/domains/openscad/`](../docs/domains/openscad/) | The sample OpenSCAD models define simple solids and assemblies for geometry examples. | — | — |
| [`docs/domains/orcaflex/`](../docs/domains/orcaflex/) | The documentation covers OrcaFlex model building, data handling, libraries and dynamic analysis. | — | — |
| [`docs/domains/orcawave/`](../docs/domains/orcawave/) | The guides, model files and validation studies document panel-method wave diffraction analysis in OrcaWave. | — | — |
| [`docs/domains/orcfxapi/`](../docs/domains/orcfxapi/) | The reference explains how to create model objects and set their data through the OrcaFlex Python API. | — | — |
| [`docs/domains/osi-pi/`](../docs/domains/osi-pi/) | The notes explain how to access and verify PI System data through the Asset Framework SDK. | — | — |
| [`docs/domains/passing_ship/`](../docs/domains/passing_ship/) | Campaign records define benchmark candidates, criteria and force samples for passing-ship analysis. | — | — |
| [`docs/domains/pipe/`](../docs/domains/pipe/) | Illustrated references cover pipe manufacturing, induction bending and stress. | — | — |
| [`docs/domains/pipecapacity/`](../docs/domains/pipecapacity/) | Equations, figures and calculation notes describe pipe wall thickness and capacity checks. | — | — |
| [`docs/domains/pipeline-installation/`](../docs/domains/pipeline-installation/) | The coverage report and case records document pipeline installation and shore pull-in analysis. | — | — |
| [`docs/domains/pipelines/`](../docs/domains/pipelines/) | The reference collection covers subsea pipeline design, pressure definitions, buckling and repair. | — | — |
| [`docs/domains/plate-buckling/`](../docs/domains/plate-buckling/) | Studies and coefficient tables document plate and stiffener buckling methods and validation. | — | — |
| [`docs/domains/platecapacity/`](../docs/domains/platecapacity/) | The archived calculation environment and supporting files document stiffened plate capacity calculations. | — | — |
| [`docs/domains/probabilisticrisk/`](../docs/domains/probabilisticrisk/) | The reference introduces probabilistic risk assessment and defines risk significance and importance. | — | — |
| [`docs/domains/process-engineering/`](../docs/domains/process-engineering/) | The introductory note points readers to a resource on reading piping and instrumentation diagrams. | — | — |
| [`docs/domains/process-piping/`](../docs/domains/process-piping/) | The guides explain how pressure affects process piping size selection. | — | — |
| [`docs/domains/production/`](../docs/domains/production/) | The notes and illustrations document production analysis, decline curves and method selection. | — | — |
| [`docs/domains/project-docs/`](../docs/domains/project-docs/) | The project records describe repository organisation, dependencies and earlier restructuring. | — | — |
| [`docs/domains/project-management/`](../docs/domains/project-management/) | The introductory note identifies tools and protocols for managing projects as the subject. | — | — |
| [`docs/domains/pyintegrity/`](../docs/domains/pyintegrity/) | Method diagrams and task notes document fitness-for-service assessments of metal loss. | — | — |
| [`docs/domains/qgis/`](../docs/domains/qgis/) | The illustrated guides describe importing drawings and working with data layers in QGIS. | — | — |
| [`docs/domains/rao_analysis/`](../docs/domains/rao_analysis/) | The note and figure introduce vessel response amplitude operators. | — | — |
| [`docs/domains/references/`](../docs/domains/references/) | The index collects external technical resources on engineering data, drawings and analysis tools. | — | — |
| [`docs/domains/reporting/`](../docs/domains/reporting/) | The guides describe calculation report layouts, evidence checks and report assembly. | — | — |
| [`docs/domains/reservoir/`](../docs/domains/reservoir/) | The notes and figures cover well-test derivatives, stratigraphy and petroleum economics. | — | — |
| [`docs/domains/rigging/`](../docs/domains/rigging/) | Illustrations and checklists document sling selection, rigging arrangements and lift points. | — | — |
| [`docs/domains/riser_analysis/`](../docs/domains/riser_analysis/) | The index and schematic illustrate a riser stack-up. | — | — |
| [`docs/domains/risers/`](../docs/domains/risers/) | The references document riser installation, damping, component costing and global model evidence. | — | — |
| [`docs/domains/risk/`](../docs/domains/risk/) | The reference and illustration present an aviation safety timeline. | — | — |
| [`docs/domains/ship-design/`](../docs/domains/ship-design/) | The design references cover ship structures, tank arrangements, fuel sizing and vessel incidents. | — | — |
| [`docs/domains/signal_processing/`](../docs/domains/signal_processing/) | The guides cover time-series analysis, signal processing methods and checks using synthetic values. | — | — |
| [`docs/domains/solidworks/`](../docs/domains/solidworks/) | The illustrated notes document mesh and assembly export from SolidWorks. | — | — |
| [`docs/domains/solvers/`](../docs/domains/solvers/) | The component guides cover modelling, meshing and post-processing with engineering solvers. | — | — |
| [`docs/domains/specialized/`](../docs/domains/specialized/) | The component guides describe specialised tools for analysis, GIS, finance and project management. | — | — |
| [`docs/domains/spm/`](../docs/domains/spm/) | The illustration provides a schematic of a single-point mooring. | — | — |
| [`docs/domains/standards/`](../docs/domains/standards/) | The index and reference material document engineering standards, data conventions and their use. | — | — |
| [`docs/domains/storage-tanks/`](../docs/domains/storage-tanks/) | The worked examples document tank shell sizing, seismic checks and pressure vessel calculations. | — | — |
| [`docs/domains/structural/`](../docs/domains/structural/) | Implementation notes and worked materials explain structural solvers, beam checks and code-based analysis. | — | — |
| [`docs/domains/submarine-cables/`](../docs/domains/submarine-cables/) | The technical note discusses cable compression at the touchdown point. | — | — |
| [`docs/domains/subsea/`](../docs/domains/subsea/) | The component notes and figures cover subsea connectors, seals, debris caps and risers. | — | — |
| [`docs/domains/test_automation/`](../docs/domains/test_automation/) | The guides explain test-suite automation and mocking patterns for engineering analysis modules. | — | — |
| [`docs/domains/testing/`](../docs/domains/testing/) | The documentation describes test infrastructure, command-line testing and cross-repository test practices. | — | — |
| [`docs/domains/umbilical/`](../docs/domains/umbilical/) | The image collection illustrates umbilical installation equipment and buoyancy modules. | — | — |
| [`docs/domains/unit-conversion/`](../docs/domains/unit-conversion/) | The survey lists candidate libraries for engineering unit conversion. | — | — |
| [`docs/domains/vibrations/`](../docs/domains/vibrations/) | The reference list points to course material on forced vibration and dynamics. | — | — |
| [`docs/domains/visualization/`](../docs/domains/visualization/) | The guides describe engineering visualisation with CAD tools and the agent dashboard. | — | — |
| [`docs/domains/viv/`](../docs/domains/viv/) | The technical notes and mode-finding report cover vortex-induced vibration of tubular members. | — | — |
| [`docs/domains/wamit/`](../docs/domains/wamit/) | The reference list points to WAMIT documentation and guidance on using its hydrodynamic outputs. | — | — |
| [`docs/domains/web/`](../docs/domains/web/) | The guide explains how web examples compare credentials and test authentication outcomes. | — | — |
| [`docs/domains/wec/`](../docs/domains/wec/) | The introductory reference directs readers to example wave energy converter simulations. | — | — |
| [`docs/domains/welding/`](../docs/domains/welding/) | The photographic and reference material covers thick-plate welding and pipe joining. | — | — |
| [`docs/domains/well/`](../docs/domains/well/) | The worked cases and screening report cover well intervention and pump-down transients. | — | — |
| [`docs/domains/wind/`](../docs/domains/wind/) | The reference materials cover offshore wind installation vessels, floating turbines and turbine mapping. | — | — |
| [`docs/domains/workflows/`](../docs/domains/workflows/) | The guides describe automated workflows, engineering parameter sweeps and supporting tools. | — | — |
| [`docs/domains/zoo/`](../docs/domains/zoo/) | The note introduces text-driven generation of CAD geometry. | — | — |
| [`docs/examples/`](../docs/examples/) | Worked calculations illustrate inputs, assumptions and results across engineering modules. | — | — |
| [`docs/field-development/`](../docs/field-development/) | The coverage map relates subsea production systems and installation operations to their software implementations. | — | — |
| [`docs/hydrodynamics/`](../docs/hydrodynamics/) | The documentation defines the evidence required to diagnose wave diffraction results. | — | — |
| [`docs/hydrodynamics/diffraction/`](../docs/hydrodynamics/diffraction/) | The contract specifies diagnostic evidence for diffraction calculations. | — | — |
| [`docs/integrations/`](../docs/integrations/) | Evaluation notes and examples document how external engineering libraries fit the analysis workflows. | — | — |
| [`docs/maintenance/`](../docs/maintenance/) | The maintenance notes describe demonstration pack retirement and test collection for reporting. | — | — |
| [`docs/maps/`](../docs/maps/) | The operator map routes work to the appropriate repository modules and documentation. | — | — |
| [`docs/migration/`](../docs/migration/) | The guides map legacy engineering calculations and batch workflows to their newer implementations. | — | — |
| [`docs/naval_architecture/`](../docs/naval_architecture/) | The method note explains the idealised geometry used to calculate tension-leg platform set-down. | — | — |
| [`docs/notebooks/`](../docs/notebooks/) | Executable notebooks demonstrate fatigue curve lookup and free-span vibration analysis. | — | — |
| [`docs/operations/`](../docs/operations/) | The deployment guide explains how to record and assess parity between deployments. | — | — |
| [`docs/plans/`](../docs/plans/) | The issue index and individual plans describe proposed repository changes. | — | — |
| [`docs/plans/assets/`](../docs/plans/assets/) | The decision pages record the scope and approval questions for the fitness-for-service program. | — | — |
| [`docs/plans/evidence/`](../docs/plans/evidence/) | Test failure records, triage notes and review findings support individual implementation plans. | — | — |
| [`docs/plans/parametric-refresh/`](../docs/plans/parametric-refresh/) | The authoring guides specify parametric atlases, historical provenance and handling of licensed solver libraries. | — | — |
| [`docs/plans/reviews/`](../docs/plans/reviews/) | Issue-specific review and verification records document findings on plans and implementations. | — | — |
| [`docs/postprocess/`](../docs/postprocess/) | The guide explains how to build a multi-tab workbook from workflow results. | — | — |
| [`docs/registry/`](../docs/registry/) | Routing registries map engineering capabilities to implementation families and supporting evidence. | — | — |
| [`docs/registry/agent-pack-audit/`](../docs/registry/agent-pack-audit/) | The audit files record function and citation checks for production engineering and nondestructive examination. | — | — |
| [`docs/reports/`](../docs/reports/) | Engineering reports present analysis results with their supporting figures, manifests and solver records. | — | — |
| [`docs/reports/2026-10-09-compute-program/`](../docs/reports/2026-10-09-compute-program/) | Case reports and scripts record the initial compute program results for marine simulations. | — | — |
| [`docs/reports/abstracted-hulls/`](../docs/reports/abstracted-hulls/) | Schema, provenance and verification records document the preparation of abstracted hull geometries. | — | — |
| [`docs/reports/assets/`](../docs/reports/assets/) | The mesh figure illustrates the padeye pressure analysis. | — | — |
| [`docs/reports/dwg-conversion/`](../docs/reports/dwg-conversion/) | The conversion plan, source manifest and probe results document the investigation of DWG drawings. | — | — |
| [`docs/reports/issue-1173-kcs-paused-runs/`](../docs/reports/issue-1173-kcs-paused-runs/) | The checkpoint records mesh logs and resumption details for two paused hull-resistance simulations. | — | — |
| [`docs/reports/issue-1528-outlet-vent/`](../docs/reports/issue-1528-outlet-vent/) | The measured runs compare outlet and vent placement against tank liquid-volume balance. | — | — |
| [`docs/reports/issue-1959-solver-start/`](../docs/reports/issue-1959-solver-start/) | Mesh and solver logs with file manifests document the investigation of OpenFOAM startup. | — | — |
| [`docs/reports/modules/`](../docs/reports/modules/) | Module-specific reports pair worked engineering cases with figures, data and report builders. | — | — |
| [`docs/reports/viv-parametric-screening/`](../docs/reports/viv-parametric-screening/) | The report describes parametric screening for vortex-induced vibration. | — | — |
| [`docs/research/`](../docs/research/) | Research notes and source registers document the basis and gaps for fitness-for-service screening. | — | — |
| [`docs/reviews/`](../docs/reviews/) | Review reports and dispositions record findings and verification evidence for repository changes. | — | — |
| [`docs/roadmaps/`](../docs/roadmaps/) | The capability note records the scope and test coverage of the parachute drag example. | — | — |
| [`docs/runbooks/`](../docs/runbooks/) | Operational procedures explain automated command input, process discovery and detached remote execution. | — | — |
| [`docs/session-handoffs/`](../docs/session-handoffs/) | Handover notes record results, outstanding issues and resumption points for engineering work. | — | — |
| [`docs/sessions/`](../docs/sessions/) | The session record preserves subprocess review findings collected for an implementation issue. | — | — |
| [`docs/skills/`](../docs/skills/) | Setup instructions and engineering skill definitions describe how to use the repository's analysis workflows. | — | — |
| [`docs/skills/fatigue-assessment/`](../docs/skills/fatigue-assessment/) | The skill and input schema define a workflow for estimating fatigue damage and life from load blocks. | — | — |
| [`docs/small_operator/`](../docs/small_operator/) | The field notes and worked problems address production questions raised by small oilfield operators. | — | — |
| [`docs/small_operator/examples/`](../docs/small_operator/examples/) | The Python examples calculate rod loading, neutral point and pump-off indicators for rod-pumped wells. | — | — |
| [`docs/small_operator/pdf/`](../docs/small_operator/pdf/) | The printable field note presents production engineering guidance for small operators. | — | — |
| [`docs/solvers/`](../docs/solvers/) | The contract describes how workflows request and track hosted solver execution. | — | — |
| [`docs/spikes/`](../docs/spikes/) | Exploratory prototypes and results investigate specific plotting and hull analysis techniques. | — | — |
| [`docs/spikes/2026-05-20-plotly-polar-arrow-technique/`](../docs/spikes/2026-05-20-plotly-polar-arrow-technique/) | The prototype and rendered example demonstrate arrows on Plotly polar charts. | — | — |
| [`docs/spikes/2026-09-25-hullprod-curvature-screening/`](../docs/spikes/2026-09-25-hullprod-curvature-screening/) | The experiment compares hull mesh curvature signatures and checks their convergence. | — | — |
| [`docs/standards/`](../docs/standards/) | Engineering workflow specifications define evidence, acceptance criteria and reference-data verification procedures. | — | — |
| [`docs/subsea/`](../docs/subsea/) | Method notes and reports document tubular checks and flexible pipe cross-section mechanics. | — | — |
| [`docs/subsea/cross_sections/`](../docs/subsea/cross_sections/) | The method note and worked report describe offshore cross sections and the axial stiffness of bonded layers. | — | — |
| [`docs/superpowers/`](../docs/superpowers/) | The specification collection documents engineering workflow designs. | — | — |
| [`docs/superpowers/specs/`](../docs/superpowers/specs/) | The design specification links riser host motion, diffraction, operating envelopes and monitoring. | — | — |
| [`docs/validation/`](../docs/validation/) | Validation reports compare engineering calculations and model transfers with their stated criteria. | — | — |
| [`docs/validation/gmsh-tank/`](../docs/validation/gmsh-tank/) | Manifests, force output and mesh checks support the synthetic tank model transfer acceptance report. | — | — |
| [`docs/verification/`](../docs/verification/) | The verification records cover cathodic protection calculations and quadratic transfer function variance. | — | — |
| [`docs/verification/cp-api651/`](../docs/verification/cp-api651/) | The verification record documents the API 651 cathodic protection implementation. | — | — |
| [`docs/vision/`](../docs/vision/) | The strategy document sets out the intended role of engineering calculations in the software ecosystem. | — | — |
| [`docs/workflows/`](../docs/workflows/) | The campaign contract explains how analysis inputs, execution records and results are linked. | — | — |
| [`examples/`](../examples/) | These examples pair engineering inputs with scripts, notebooks, or reports that illustrate use of the package. | — | — |
| [`examples/ansys/`](../examples/ansys/) | These cases illustrate preparation, execution diagnostics, and result checks for ANSYS structural models. | — | — |
| [`examples/ansys/analysis-evidence/`](../examples/ansys/analysis-evidence/) | These manifests and response tables preserve diagnostic evidence from pressure-vessel, padeye, and mudmat cases. | — | — |
| [`examples/ansys/cylinder-benchmark/`](../examples/ansys/cylinder-benchmark/) | These cylinder decks, analytical references, and checker records document a pressure benchmark. | — | — |
| [`examples/ansys/mudmat/`](../examples/ansys/mudmat/) | This example builds a mudmat screening deck and retains reference responses for comparison. | — | — |
| [`examples/ansys/padeye/`](../examples/ansys/padeye/) | This example builds a padeye screening deck and supports a mesh study. | — | — |
| [`examples/ansys/pressure-transfer/`](../examples/ansys/pressure-transfer/) | A synthetic pressure field illustrates checking transferred forces and moments on planar patches. | — | — |
| [`examples/ansys/pressure-vessel/`](../examples/ansys/pressure-vessel/) | This example builds a pressure-vessel screening deck and retains reference responses for comparison. | — | — |
| [`examples/aqg/`](../examples/aqg/) | A synthetic diffraction example exercises displacement and mesh-quality criteria in the analysis qualification gate. | — | — |
| [`examples/asset_integrity/`](../examples/asset_integrity/) | These examples illustrate fitness-for-service assessments for metal loss, inspection data, and pipe capacity. | — | — |
| [`examples/blender/`](../examples/blender/) | This collection documents a headless Blender rendering check. | — | — |
| [`examples/blender/cube-render/`](../examples/blender/cube-render/) | The instructions describe rendering a cube to check a headless Blender installation. | — | — |
| [`examples/calculix/`](../examples/calculix/) | These scripts illustrate shell-patch calculations and the interpretation of synthetic CalculiX text results. | — | — |
| [`examples/cathodic_protection/`](../examples/cathodic_protection/) | These examples cover pipeline and tank cathodic protection design, edition comparisons, and external corrosion assessment inputs. | — | — |
| [`examples/cfd/`](../examples/cfd/) | This collection supplies an illustrative tank configuration for computational fluid dynamics. | — | — |
| [`examples/cfd/synthetic_l_tank/`](../examples/cfd/synthetic_l_tank/) | This synthetic tank input defines geometry and coordinate axes for a CFD methodology check. | — | — |
| [`examples/code_checks/`](../examples/code_checks/) | These examples exercise floating-roof tank calculations and shell-thickness reporting. | — | — |
| [`examples/complete_workflow/`](../examples/complete_workflow/) | A sample request, hull dataset, and configuration illustrate the development workflow for marine structural analysis. | — | — |
| [`examples/data/`](../examples/data/) | These datasets supply example inputs and reference outputs for jack-up screening and soil studies. | — | — |
| [`examples/data/jackup_failure_sequence/`](../examples/data/jackup_failure_sequence/) | Assumed jack-up inputs, a summary, and a utilization plot support the failure-sequence example. | — | — |
| [`examples/data/jackup_layered_penetration/`](../examples/data/jackup_layered_penetration/) | These illustrative soil profiles supply contrasting layered-ground cases for spudcan penetration calculations. | — | — |
| [`examples/data/jackup_screening/`](../examples/data/jackup_screening/) | The example bundles environmental assumptions, inputs, and source records for a jack-up screening case. | — | — |
| [`examples/data/jackup_soil_sensitivity/`](../examples/data/jackup_soil_sensitivity/) | These cases and reference results capture how variations in soil properties affect jack-up screening. | — | — |
| [`examples/data_systems/`](../examples/data_systems/) | These examples and placeholders cover data handling and pipeline integrity assessment. | — | — |
| [`examples/data_systems/data/`](../examples/data_systems/data/) | This placeholder reserves a usage example for the digitalmodel.data_systems.data module. | — | — |
| [`examples/data_systems/data_manager/`](../examples/data_systems/data_manager/) | This placeholder reserves a usage example for the digitalmodel.data_systems.data_manager module. | — | — |
| [`examples/data_systems/data_procurement/`](../examples/data_systems/data_procurement/) | This placeholder reserves a usage example for the digitalmodel.data_systems.data_procurement module. | — | — |
| [`examples/data_systems/data_scraping/`](../examples/data_systems/data_scraping/) | This placeholder reserves a usage example for the digitalmodel.data_systems.data_scraping module. | — | — |
| [`examples/data_systems/pyintegrity/`](../examples/data_systems/pyintegrity/) | A worked example demonstrates an API 579 general metal loss assessment for a gas pipeline. | — | — |
| [`examples/demos/`](../examples/demos/) | These demonstrations combine engineering calculations with dashboards, reports, and example outputs. | — | — |
| [`examples/demos/asset_integrity/`](../examples/demos/asset_integrity/) | These scripts and case tables illustrate fitness-for-service screening in a field dashboard. | — | — |
| [`examples/demos/diffraction/`](../examples/demos/diffraction/) | These examples build diffraction decks and mass properties from vessel particulars and hull meshes. | — | — |
| [`examples/demos/gtm/`](../examples/demos/gtm/) | These internal demonstrations turn parametric marine installation studies into screening reports. | — | — |
| [`examples/demos/installation/`](../examples/demos/installation/) | These demonstrations screen lift feasibility against crane capacity curves and produce installation suitability reports. | — | — |
| [`examples/demos/structural/`](../examples/demos/structural/) | These structural examples generate standalone ship-plate buckling reports from tabulated panel cases. | — | — |
| [`examples/domains/`](../examples/domains/) | These discipline-specific examples illustrate engineering analyses and the supporting data and plotting workflows. | — | — |
| [`examples/domains/_shared/`](../examples/domains/_shared/) | These reusable input templates describe individual plates, groups of plates, and batch cases. | — | — |
| [`examples/domains/api_standards/`](../examples/domains/api_standards/) | A worked pipeline example demonstrates stress and pressure checks under API STD 2RD. | — | — |
| [`examples/domains/artificial_lift/`](../examples/domains/artificial_lift/) | These scripts and reports illustrate rod-pump diagnostics from dynamometer cards and field measurements. | — | — |
| [`examples/domains/calm_buoy/`](../examples/domains/calm_buoy/) | These configuration examples describe a catenary anchor leg mooring buoy and its mooring lines. | — | — |
| [`examples/domains/core/`](../examples/domains/core/) | These scripts illustrate agent selection, caching, fault handling, scheduling, and provenance tracking. | — | — |
| [`examples/domains/data_systems/`](../examples/domains/data_systems/) | This example demonstrates a round trip for solver reference data. | — | — |
| [`examples/domains/diffraction/`](../examples/domains/diffraction/) | An example batch configuration illustrates conversion of AQWA and OrcaWave diffraction data for multiple vessels. | — | — |
| [`examples/domains/fatigue/`](../examples/domains/fatigue/) | These examples demonstrate fatigue analysis and plotting of stress-life curves. | — | — |
| [`examples/domains/fea/`](../examples/domains/fea/) | Reference images illustrate catenary and lazy-wave riser profiles used in finite element examples. | — | — |
| [`examples/domains/fpso/`](../examples/domains/fpso/) | A script and notebook illustrate mooring analysis for a floating production vessel. | — | — |
| [`examples/domains/geotechnical/`](../examples/domains/geotechnical/) | These examples screen jack-up foundation behavior, including layered penetration and sensitivity to soil assumptions. | — | — |
| [`examples/domains/hydrodynamics/`](../examples/domains/hydrodynamics/) | Scripts and notebooks illustrate vessel force balance, hydrodynamic coefficients, and wave response. | — | — |
| [`examples/domains/input_files/`](../examples/domains/input_files/) | These sample configurations and datasets supply inputs to the discipline examples. | — | — |
| [`examples/domains/installation/`](../examples/domains/installation/) | A worked shore-approach example illustrates pipeline pull-in calculations. | — | — |
| [`examples/domains/metocean/`](../examples/domains/metocean/) | This script demonstrates streaming meteorological and oceanographic data. | — | — |
| [`examples/domains/mooring/`](../examples/domains/mooring/) | These examples illustrate berth mooring, line geometry, and mooring system configuration. | — | — |
| [`examples/domains/ocimf/`](../examples/domains/ocimf/) | A script and notebook illustrate tanker mooring calculations and equipment selection using OCIMF guidance. | — | — |
| [`examples/domains/orcaflex/`](../examples/domains/orcaflex/) | These scripts demonstrate an OrcaFlex modeling workflow, a riser case, and reference-data lookup. | — | — |
| [`examples/domains/reservoir/`](../examples/domains/reservoir/) | These examples illustrate reservoir analysis and production forecasting. | — | — |
| [`examples/domains/stress/`](../examples/domains/stress/) | These examples demonstrate plate capacity and structural stress calculations. | — | — |
| [`examples/domains/structural/`](../examples/domains/structural/) | A pipeline wall-thickness example produces reports for design phases and parameter sweeps. | — | — |
| [`examples/domains/structural_analysis/`](../examples/domains/structural_analysis/) | A legacy demonstration generates a fitness-for-service dashboard. | — | — |
| [`examples/domains/subsea/`](../examples/domains/subsea/) | These examples calculate layered flexible-pipe geometry and illustrate subsea tool selection. | — | — |
| [`examples/domains/tutorials/`](../examples/domains/tutorials/) | These notebooks introduce marine engineering calculations and work through catenary analysis. | — | — |
| [`examples/domains/utilities/`](../examples/domains/utilities/) | Plotting helpers provide consistent figures for the marine engineering tutorials. | — | — |
| [`examples/domains/validation/`](../examples/domains/validation/) | These examples demonstrate data-validation checks, integration, and reporting. | — | — |
| [`examples/domains/vessel/`](../examples/domains/vessel/) | This script illustrates retrieval and in-memory processing of vessel response data. | — | — |
| [`examples/domains/workflows/`](../examples/domains/workflows/) | These scripts work through marine engineering scenarios including spread mooring, pipeline installation, and jack-up installation. | — | — |
| [`examples/field_development/`](../examples/field_development/) | A synthetic planar manifold example illustrates subsea architecture calculations. | — | — |
| [`examples/freecad/`](../examples/freecad/) | This collection documents a parametric FreeCAD geometry example for a flanged spool. | — | — |
| [`examples/freecad/flanged_spool/`](../examples/freecad/flanged_spool/) | This worked example defines synthetic flanged-spool geometry and its analytical comparator. | — | — |
| [`examples/geotechnical/`](../examples/geotechnical/) | A worked example calculates the response of a pipe pile in soft clay. | — | — |
| [`examples/gis/`](../examples/gis/) | This collection records an example of producing a time series from historical map imagery. | — | — |
| [`examples/gis/imagery-timelapse-2538/`](../examples/gis/imagery-timelapse-2538/) | These inputs and batch records document a worked historical-imagery timelapse. | — | — |
| [`examples/hydrodynamics/`](../examples/hydrodynamics/) | These examples cover diffraction inputs, vessel response, damping, currents, and berthing fenders. | — | — |
| [`examples/hydrodynamics/aqwa/`](../examples/hydrodynamics/aqwa/) | This placeholder reserves a usage example for the digitalmodel.hydrodynamics.aqwa module. | — | — |
| [`examples/hydrodynamics/bemrosetta/`](../examples/hydrodynamics/bemrosetta/) | This placeholder reserves a usage example for the digitalmodel.hydrodynamics.bemrosetta module. | — | — |
| [`examples/hydrodynamics/diffraction/`](../examples/hydrodynamics/diffraction/) | These examples prepare diffraction inputs for a unit box and illustrate external matrices and abscissa conventions. | — | — |
| [`examples/hydrodynamics/fender_design/`](../examples/hydrodynamics/fender_design/) | These tools turn berthing cases and a fender catalogue into comparison charts and a design workbook. | — | — |
| [`examples/hydrodynamics/hydrodynamics/`](../examples/hydrodynamics/hydrodynamics/) | This placeholder reserves a usage example for the digitalmodel.hydrodynamics module. | — | — |
| [`examples/hydrodynamics/oc4-semisub/`](../examples/hydrodynamics/oc4-semisub/) | This example generates and checks a diffraction mesh for the OC4 semisubmersible. | — | — |
| [`examples/hydrodynamics/parametric_rao/`](../examples/hydrodynamics/parametric_rao/) | These inputs and a driver define a pilot parameter study of vessel response amplitude operators. | — | — |
| [`examples/hydrodynamics/rao_analysis/`](../examples/hydrodynamics/rao_analysis/) | An analytical oscillator example illustrates plotting a frequency response against its static reference. | — | — |
| [`examples/hydrodynamics/river_currents/`](../examples/hydrodynamics/river_currents/) | A versioned example records the assumptions behind river-flow and towing screens. | — | — |
| [`examples/hydrodynamics/roll_damping/`](../examples/hydrodynamics/roll_damping/) | This script reproduces benchmark and sensitivity calculations for a generic viscous roll-damping study. | — | — |
| [`examples/infrastructure/`](../examples/infrastructure/) | These placeholders reserve usage examples for the infrastructure packages. | — | — |
| [`examples/infrastructure/base_configs/`](../examples/infrastructure/base_configs/) | This placeholder reserves a usage example for the digitalmodel.infrastructure.base_configs module. | — | — |
| [`examples/infrastructure/base_solvers/`](../examples/infrastructure/base_solvers/) | This placeholder reserves a usage example for the digitalmodel.infrastructure.base_solvers module. | — | — |
| [`examples/infrastructure/calculations/`](../examples/infrastructure/calculations/) | This placeholder reserves a usage example for the digitalmodel.infrastructure.calculations module. | — | — |
| [`examples/infrastructure/common/`](../examples/infrastructure/common/) | This placeholder reserves a usage example for the digitalmodel.infrastructure.common module. | — | — |
| [`examples/infrastructure/config/`](../examples/infrastructure/config/) | This placeholder reserves a usage example for the digitalmodel.infrastructure.config module. | — | — |
| [`examples/infrastructure/core/`](../examples/infrastructure/core/) | This placeholder reserves a usage example for the digitalmodel.infrastructure.core module. | — | — |
| [`examples/infrastructure/domains/`](../examples/infrastructure/domains/) | This placeholder reserves a usage example for the digitalmodel.infrastructure.domains module. | — | — |
| [`examples/infrastructure/services/`](../examples/infrastructure/services/) | This placeholder reserves a usage example for the digitalmodel.infrastructure.services module. | — | — |
| [`examples/infrastructure/templates/`](../examples/infrastructure/templates/) | This placeholder reserves a usage example for the digitalmodel.infrastructure.templates module. | — | — |
| [`examples/infrastructure/transformation/`](../examples/infrastructure/transformation/) | This placeholder reserves a usage example for the digitalmodel.infrastructure.transformation module. | — | — |
| [`examples/infrastructure/validation/`](../examples/infrastructure/validation/) | This placeholder reserves a usage example for the digitalmodel.infrastructure.validation module. | — | — |
| [`examples/infrastructure/validators/`](../examples/infrastructure/validators/) | This placeholder reserves a usage example for the digitalmodel.infrastructure.validation module. | — | — |
| [`examples/lifting_lug/`](../examples/lifting_lug/) | A synthetic welded lifting-lug study demonstrates how to specify geometry and load and generate study outputs. | — | — |
| [`examples/manufacturing/`](../examples/manufacturing/) | An input file and sample results illustrate a weld-fatigue calculation from stress-cycle data. | — | — |
| [`examples/marine_ops/`](../examples/marine_ops/) | These examples cover allowable sea states and spread-mooring resilience alongside marine operations module placeholders. | — | — |
| [`examples/marine_ops/artificial_lift/`](../examples/marine_ops/artificial_lift/) | This placeholder reserves a usage example for the digitalmodel.marine_ops.artificial_lift module. | — | — |
| [`examples/marine_ops/ct_hydraulics/`](../examples/marine_ops/ct_hydraulics/) | This placeholder reserves a usage example for the digitalmodel.marine_ops.ct_hydraulics module. | — | — |
| [`examples/marine_ops/marine_analysis/`](../examples/marine_ops/marine_analysis/) | This placeholder reserves a usage example for the digitalmodel.marine_ops.marine_analysis module. | — | — |
| [`examples/marine_ops/marine_engineering/`](../examples/marine_ops/marine_engineering/) | This placeholder reserves a usage example for the digitalmodel.marine_ops.marine_engineering module. | — | — |
| [`examples/marine_ops/mooring_resilience/`](../examples/marine_ops/mooring_resilience/) | These calculations and plots screen a spread mooring against line tension, anchor capacity, and chain fatigue. | — | — |
| [`examples/marine_ops/reservoir/`](../examples/marine_ops/reservoir/) | This placeholder reserves a usage example for the digitalmodel.marine_ops.reservoir module. | — | — |
| [`examples/mining_lift_riser/`](../examples/mining_lift_riser/) | A synthetic lift-riser input and analysis script illustrate a mining riser calculation. | — | — |
| [`examples/naval_architecture/`](../examples/naval_architecture/) | These examples cover hull resistance, effective power, and tension-leg platform setdown. | — | — |
| [`examples/openfoam/`](../examples/openfoam/) | These examples document an OpenFOAM cavity check and illustrate the preparation of resistance, wake, and wave studies. | — | — |
| [`examples/openfoam/cavity-v2312/`](../examples/openfoam/cavity-v2312/) | The instructions describe how to reproduce the cavity smoke check with OpenFOAM v2312. | — | — |
| [`examples/orcaflex/`](../examples/orcaflex/) | These examples cover installation simulation diagnostics, worker configuration, and conversion of model data back into specifications. | — | — |
| [`examples/orcaflex/reverse_extraction/`](../examples/orcaflex/reverse_extraction/) | This example extracts supported model fields from OrcaFlex YAML and writes a specification and conversion report. | — | — |
| [`examples/parametrics/`](../examples/parametrics/) | These scripts illustrate building and querying engineering response atlases from parameter sweeps. | — | — |
| [`examples/process_diagrams/`](../examples/process_diagrams/) | A script generates a legend and symbol sheet for piping and instrumentation diagrams. | — | — |
| [`examples/production_engineering/`](../examples/production_engineering/) | A worked oil-well example illustrates selecting an inflow performance model from reservoir conditions. | — | — |
| [`examples/prototypes/`](../examples/prototypes/) | This collection holds an experimental template-based OrcaFlex input generator. | — | — |
| [`examples/prototypes/orcaflex-agent/`](../examples/prototypes/orcaflex-agent/) | This prototype generates sections of an OrcaFlex model from templates and retains sample output. | — | — |
| [`examples/report-evals/`](../examples/report-evals/) | These checks reproduce numerical claims in published engineering reports and record comparisons with the reported values. | — | — |
| [`examples/report-evals/issue-001-hull-roll-screening/`](../examples/report-evals/issue-001-hull-roll-screening/) | This check reproduces reported peak roll reduction for a hull fitted with a U-tube anti-roll tank. | — | — |
| [`examples/report-evals/issue-001-sloshing-frequency/`](../examples/report-evals/issue-001-sloshing-frequency/) | This check compares the first tank sloshing frequency from CFD with linear theory. | — | — |
| [`examples/report-evals/issue-002-cp-anode-editions/`](../examples/report-evals/issue-002-cp-anode-editions/) | This check reproduces reported sacrificial-anode results across editions of two DNV practices. | — | — |
| [`examples/reporting/`](../examples/reporting/) | These examples pair calculation-report inputs with rendered reports and supporting figures or data. | — | — |
| [`examples/reporting/assets/`](../examples/reporting/assets/) | These figures illustrate model setup, deflection, and equivalent stress in the beam validation report. | — | — |
| [`examples/reporting/envelope_dashboard/`](../examples/reporting/envelope_dashboard/) | A synthetic utilization table supplies example data for the envelope dashboard. | — | — |
| [`examples/riser_bom/`](../examples/riser_bom/) | A synthetic cost basis and worked example illustrate how to construct a riser bill of materials. | — | — |
| [`examples/riser_bom/wikis/`](../examples/riser_bom/wikis/) | This example wiki supplies a reference for the synthetic riser bill of materials. | — | — |
| [`examples/riser_fatigue/`](../examples/riser_fatigue/) | A synthetic lazy-wave riser example ranks buoyancy configurations by excursion. | — | — |
| [`examples/riser_input/`](../examples/riser_input/) | These YAML examples illustrate the common input schema for drilling, flexible, catenary, lazy-wave, and top-tensioned risers. | — | — |
| [`examples/signal_processing/`](../examples/signal_processing/) | These placeholders reserve examples for signal and time-series analysis. | — | — |
| [`examples/signal_processing/signal_analysis/`](../examples/signal_processing/signal_analysis/) | This placeholder reserves a usage example for the digitalmodel.signal_processing.signal_analysis module. | — | — |
| [`examples/signal_processing/time_series/`](../examples/signal_processing/time_series/) | This placeholder reserves a usage example for the digitalmodel.signal_processing.time_series module. | — | — |
| [`examples/solver_smoke/`](../examples/solver_smoke/) | A synthetic finite-response record illustrates the readback expected from a solver smoke check. | — | — |
| [`examples/solvers/`](../examples/solvers/) | These folders collect solver-interface example placeholders and worked OrcaFlex reporting and operability examples. | — | — |
| [`examples/solvers/blender_automation/`](../examples/solvers/blender_automation/) | This placeholder reserves a usage example for the digitalmodel.solvers.blender_automation module. | — | — |
| [`examples/solvers/fea_model/`](../examples/solvers/fea_model/) | This placeholder reserves a usage example for the digitalmodel.solvers.fea_model module. | — | — |
| [`examples/solvers/gmsh_meshing/`](../examples/solvers/gmsh_meshing/) | This placeholder reserves a usage example for the digitalmodel.solvers.gmsh_meshing module. | — | — |
| [`examples/solvers/orcaflex/`](../examples/solvers/orcaflex/) | These examples illustrate OrcaFlex result reporting and wave-only operability screening. | — | — |
| [`examples/solvers/orcaflex_browser/`](../examples/solvers/orcaflex_browser/) | This placeholder reserves a usage example for the digitalmodel.solvers.orcaflex.browser module. | — | — |
| [`examples/solvers/orcaflex_post_process/`](../examples/solvers/orcaflex_post_process/) | This placeholder reserves a usage example for the digitalmodel.solvers.orcaflex.post_process module. | — | — |
| [`examples/solvers/orcawave/`](../examples/solvers/orcawave/) | This placeholder reserves a usage example for the digitalmodel.solvers.orcawave module. | — | — |
| [`examples/specialized/`](../examples/specialized/) | These placeholders reserve examples for specialized analysis, command-line, geographic, and rigging modules. | — | — |
| [`examples/specialized/api_analysis/`](../examples/specialized/api_analysis/) | This placeholder reserves a usage example for the digitalmodel.specialized.api_analysis module. | — | — |
| [`examples/specialized/cli/`](../examples/specialized/cli/) | This placeholder reserves a usage example for the digitalmodel.specialized.cli module. | — | — |
| [`examples/specialized/custom/`](../examples/specialized/custom/) | This placeholder reserves a usage example for the digitalmodel.specialized.custom module. | — | — |
| [`examples/specialized/digitalmarketing/`](../examples/specialized/digitalmarketing/) | This placeholder reserves a usage example for the digitalmodel.specialized.digitalmarketing module. | — | — |
| [`examples/specialized/gis/`](../examples/specialized/gis/) | This placeholder reserves a usage example for the digitalmodel.specialized.gis module. | — | — |
| [`examples/specialized/rigging/`](../examples/specialized/rigging/) | This placeholder reserves a usage example for the digitalmodel.specialized.rigging module. | — | — |
| [`examples/structural/`](../examples/structural/) | These examples cover structural result comparison, wall thickness checks, and offshore container utilization. | — | — |
| [`examples/structural/analysis/`](../examples/structural/analysis/) | This placeholder reserves a usage example for the digitalmodel.structural.structural_analysis module. | — | — |
| [`examples/structural/fatigue/`](../examples/structural/fatigue/) | This placeholder reserves a usage example for the digitalmodel.structural.fatigue module. | — | — |
| [`examples/structural/fatigue_analysis/`](../examples/structural/fatigue_analysis/) | This placeholder reserves a usage example for the digitalmodel.structural.fatigue_apps module. | — | — |
| [`examples/structural/offshore_container_utilization/`](../examples/structural/offshore_container_utilization/) | These examples plot offshore container utilization against geometry and rating for a lift load case. | — | — |
| [`examples/structural/pipe_capacity/`](../examples/structural/pipe_capacity/) | This placeholder reserves a usage example for the digitalmodel.structural.pipe_capacity module. | — | — |
| [`examples/structural/pipe_cross_section/`](../examples/structural/pipe_cross_section/) | This placeholder reserves a usage example for the digitalmodel.structural.pipe_cross_section module. | — | — |
| [`examples/structural/stress/`](../examples/structural/stress/) | This placeholder reserves a usage example for the digitalmodel.structural.stress module. | — | — |
| [`examples/structural/structural_analysis/`](../examples/structural/structural_analysis/) | This placeholder reserves a usage example for the digitalmodel.structural.structural_analysis module. | — | — |
| [`examples/structural/wall_thickness_quickcheck/`](../examples/structural/wall_thickness_quickcheck/) | A cached pipeline case illustrates a wall-thickness sweep and report across two design codes. | — | — |
| [`examples/subsea/`](../examples/subsea/) | These examples illustrate subsea pipelines, risers, jumpers, moorings, umbilicals, and control systems. | — | — |
| [`examples/subsea/catenary/`](../examples/subsea/catenary/) | This placeholder reserves a usage example for the digitalmodel.subsea.catenary_riser module. | — | — |
| [`examples/subsea/catenary_riser/`](../examples/subsea/catenary_riser/) | This placeholder reserves a usage example for the digitalmodel.subsea.catenary_riser module. | — | — |
| [`examples/subsea/cross_sections/`](../examples/subsea/cross_sections/) | A synthetic bonded annulus illustrates how layer geometry and moduli determine axial stiffness. | — | — |
| [`examples/subsea/jumper_tools/`](../examples/subsea/jumper_tools/) | A synthetic jumper example checks section properties, catenary geometry, and mesh generation against hand calculations. | — | — |
| [`examples/subsea/mooring_analysis/`](../examples/subsea/mooring_analysis/) | These examples illustrate single-line mooring and pinned catenary calculations. | — | — |
| [`examples/subsea/pipeline/`](../examples/subsea/pipeline/) | This placeholder reserves a usage example for the digitalmodel.subsea.pipeline module. | — | — |
| [`examples/subsea/vertical_riser/`](../examples/subsea/vertical_riser/) | This placeholder reserves a usage example for the digitalmodel.subsea.vertical_riser module. | — | — |
| [`examples/subsea/viv_analysis/`](../examples/subsea/viv_analysis/) | This placeholder reserves a usage example for the digitalmodel.subsea.viv_analysis module. | — | — |
| [`examples/visualization/`](../examples/visualization/) | These placeholders reserve usage examples for visualization, reporting, and design tools. | — | — |
| [`examples/visualization/design_tools/`](../examples/visualization/design_tools/) | This placeholder reserves a usage example for the digitalmodel.visualization.design_tools module. | — | — |
| [`examples/visualization/reporting/`](../examples/visualization/reporting/) | This placeholder reserves a usage example for the digitalmodel.visualization.reporting module. | — | — |
| [`examples/visualization/visualization/`](../examples/visualization/visualization/) | This placeholder reserves a usage example for the digitalmodel.visualization module. | — | — |
| [`examples/viv/`](../examples/viv/) | These example cases supply inputs for vortex-induced vibration mode identification. | — | — |
| [`examples/viv/shear7_mode_finder/`](../examples/viv/shear7_mode_finder/) | Two synthetic text fixtures demonstrate extraction of modal data from SHEAR7-style output. | — | — |
| [`examples/well/`](../examples/well/) | A synthetic well-barrier example illustrates screening permanent barriers and comparing anomaly risk. | — | — |
| [`examples/workflows/`](../examples/workflows/) | These inputs and scripts illustrate how to invoke individual engineering workflows and inspect their outputs. | — | — |
| [`examples/workflows/agents/`](../examples/workflows/agents/) | This placeholder reserves a usage example for the digitalmodel.workflows.agents module. | — | — |
| [`examples/workflows/ai_workflows/`](../examples/workflows/ai_workflows/) | This placeholder reserves a usage example for the digitalmodel.workflows.ai_workflows module. | — | — |
| [`examples/workflows/alt-fuel-report-pack/`](../examples/workflows/alt-fuel-report-pack/) | This example turns alternative-fuel ship sizing results into a trade-study report. | — | — |
| [`examples/workflows/anchor-capacity/`](../examples/workflows/anchor-capacity/) | This example screens drag-anchor and suction-caisson capacity for specified soil and loads. | — | — |
| [`examples/workflows/anchor-capacity-atlas-query/`](../examples/workflows/anchor-capacity-atlas-query/) | This example queries tabulated anchor capacity for specified dimensions, soil strength, and demand. | — | — |
| [`examples/workflows/api-2sk-mooring/`](../examples/workflows/api-2sk-mooring/) | This example checks extreme mooring line tension against breaking load under API RP 2SK. | — | — |
| [`examples/workflows/api-rp-2rd-riser/`](../examples/workflows/api-rp-2rd-riser/) | This example supplies pipe dimensions and material strengths for an API RP 2RD riser check. | — | — |
| [`examples/workflows/api579-coverage-screen/`](../examples/workflows/api579-coverage-screen/) | Synthetic evidence records illustrate how to check the scope of API 579 assessment requests. | — | — |
| [`examples/workflows/api579-dent-screen/`](../examples/workflows/api579-dent-screen/) | A synthetic pipe dent illustrates how to specify dent geometry and weld location for screening. | — | — |
| [`examples/workflows/api579-part9-crack-fad/`](../examples/workflows/api579-part9-crack-fad/) | Synthetic inspection data illustrate an API 579 Part 9 crack assessment using a failure assessment diagram. | — | — |
| [`examples/workflows/api579-pipe-ffs-b314/`](../examples/workflows/api579-pipe-ffs-b314/) | This example uses wall measurements to assess general and local metal loss in a liquid pipeline. | — | — |
| [`examples/workflows/api579-pipe-ffs-b318/`](../examples/workflows/api579-pipe-ffs-b318/) | This example uses wall measurements to assess general and local metal loss in a gas pipeline. | — | — |
| [`examples/workflows/api579-pitting-screen/`](../examples/workflows/api579-pitting-screen/) | A synthetic pit grid illustrates how to specify geometry for an API 579 pitting screen. | — | — |
| [`examples/workflows/api653-tank-settlement-screen/`](../examples/workflows/api653-tank-settlement-screen/) | This example reduces a tank shell-elevation survey and reports the status of settlement checks. | — | — |
| [`examples/workflows/aqwa-diffraction-deck-prep/`](../examples/workflows/aqwa-diffraction-deck-prep/) | This example converts a diffraction specification into an AQWA input deck. | — | — |
| [`examples/workflows/artificial-lift-field-health/`](../examples/workflows/artificial-lift-field-health/) | This example screens a small set of synthetic dynamometer cards for rod-pump operating problems. | — | — |
| [`examples/workflows/automation/`](../examples/workflows/automation/) | This placeholder reserves a usage example for the digitalmodel.workflows.automation module. | — | — |
| [`examples/workflows/buckling-parametric/`](../examples/workflows/buckling-parametric/) | This example varies plate grade and thickness to compare buckling under specified loads. | — | — |
| [`examples/workflows/capex-estimate/`](../examples/workflows/capex-estimate/) | This example uses offshore benchmarks to estimate capital cost for a tension-leg platform. | — | — |
| [`examples/workflows/casing-design/`](../examples/workflows/casing-design/) | This example illustrates casing design checks using well depths, pressures, and fluid weights. | — | — |
| [`examples/workflows/catenary/`](../examples/workflows/catenary/) | This example calculates a flexible line profile from vertical separation and hang-off angle. | — | — |
| [`examples/workflows/code-check-atlas-query/`](../examples/workflows/code-check-atlas-query/) | This example queries precomputed code checks for a pipe section under tension and bending. | — | — |
| [`examples/workflows/compare-tool/`](../examples/workflows/compare-tool/) | This example aligns two tabulated runs by a common key and compares their values. | — | — |
| [`examples/workflows/concept-screening/`](../examples/workflows/concept-screening/) | This example supplies a deepwater prospect for field development concept screening. | — | — |
| [`examples/workflows/concept-selection/`](../examples/workflows/concept-selection/) | This example ranks host concepts for an offshore reservoir using reservoir size, depth, and infrastructure distance. | — | — |
| [`examples/workflows/cpt-interpretation/`](../examples/workflows/cpt-interpretation/) | This script interprets a synthetic cone penetration test profile through clay and sand. | — | — |
| [`examples/workflows/ct-hydraulics/`](../examples/workflows/ct-hydraulics/) | This example screens steady-state coiled-tubing hydraulics for a well cleanout. | — | — |
| [`examples/workflows/damage-stability/`](../examples/workflows/damage-stability/) | This example specifies intact and damaged vessel conditions for a stability screen. | — | — |
| [`examples/workflows/diffraction-library-query/`](../examples/workflows/diffraction-library-query/) | This example retrieves a diffraction response by vessel case, wave frequency, and heading. | — | — |
| [`examples/workflows/dnv-os-f201-riser/`](../examples/workflows/dnv-os-f201-riser/) | This example supplies tubular properties and safety factors for a DNV-OS-F201 riser check. | — | — |
| [`examples/workflows/drag-anchor-holding/`](../examples/workflows/drag-anchor-holding/) | This example estimates the holding capacity of a drag anchor in soft soil. | — | — |
| [`examples/workflows/drilling-riser-envelope-library-query/`](../examples/workflows/drilling-riser-envelope-library-query/) | This example queries the drilling-riser envelope for a specified offset, current, and sea state. | — | — |
| [`examples/workflows/drilling-riser-operability-query/`](../examples/workflows/drilling-riser-operability-query/) | This example looks up drilling-riser operability by vessel offset and current speed. | — | — |
| [`examples/workflows/dual-gradient/`](../examples/workflows/dual-gradient/) | This example screens a dual-gradient drilling configuration for a deepwater well with a narrow pressure window. | — | — |
| [`examples/workflows/dynacard-diagnostics/`](../examples/workflows/dynacard-diagnostics/) | This example builds synthetic pump cards and demonstrates surface-to-downhole diagnosis. | — | — |
| [`examples/workflows/elastic-buckling/`](../examples/workflows/elastic-buckling/) | This example defines a finite element model for an elastic buckling calculation. | — | — |
| [`examples/workflows/esp-pump-atlas-query/`](../examples/workflows/esp-pump-atlas-query/) | This example looks up electric submersible pump utilization by flow rate and dynamic fluid level. | — | — |
| [`examples/workflows/esp-pump-hydraulics/`](../examples/workflows/esp-pump-hydraulics/) | This example sizes an electric submersible pump from head requirements and stage performance. | — | — |
| [`examples/workflows/extreme-value-post/`](../examples/workflows/extreme-value-post/) | This example fits extreme-value distributions to block maxima and estimates return-period extremes. | — | — |
| [`examples/workflows/fatigue-analysis/`](../examples/workflows/fatigue-analysis/) | This example accumulates fatigue damage from stress-cycle bins and a seawater stress-life curve. | — | — |
| [`examples/workflows/ffs-metal-loss/`](../examples/workflows/ffs-metal-loss/) | This example screens a pipeline with metal loss using design pressure and measured wall dimensions. | — | — |
| [`examples/workflows/flange-analysis/`](../examples/workflows/flange-analysis/) | This example screens an integral flange for bolt demand, stresses, and rigidity under ASME Appendix 2. | — | — |
| [`examples/workflows/floating-wind-chain/`](../examples/workflows/floating-wind-chain/) | This example transfers the mass and center of gravity of a synthetic floating wind platform into diffraction and coupled-model specifications. | — | — |
| [`examples/workflows/floating-wind-economics/`](../examples/workflows/floating-wind-economics/) | This example estimates floating wind energy and lifetime project costs and varies selected cost drivers. | — | — |
| [`examples/workflows/floating-wind-sizing/`](../examples/workflows/floating-wind-sizing/) | This example compares floating wind platform variants across site load cases. | — | — |
| [`examples/workflows/foam-system-report-pack/`](../examples/workflows/foam-system-report-pack/) | This example builds a screening report from tanker deck foam-system sizing results. | — | — |
| [`examples/workflows/four-point-skew/`](../examples/workflows/four-point-skew/) | This example screens unequal load distribution in a four-leg lift with a single hook. | — | — |
| [`examples/workflows/fowt-mooring/`](../examples/workflows/fowt-mooring/) | This example checks floating wind turbine excursion against the minimum bend radius of a dynamic export cable. | — | — |
| [`examples/workflows/fowt-mooring-atlas-query/`](../examples/workflows/fowt-mooring-atlas-query/) | This example looks up floating wind mooring results by excursion radius and minimum cable bend radius. | — | — |
| [`examples/workflows/fowt-volturnus-mooring/`](../examples/workflows/fowt-volturnus-mooring/) | This example defines environmental headings for a semisubmersible floating wind turbine mooring study. | — | — |
| [`examples/workflows/fpso-mooring-atlas-query/`](../examples/workflows/fpso-mooring-atlas-query/) | This example queries spread-mooring response by wave height, wave period, and water depth. | — | — |
| [`examples/workflows/fpso-mooring-full/`](../examples/workflows/fpso-mooring-full/) | This example combines environmental loading and mooring response for a spread-moored floating production vessel. | — | — |
| [`examples/workflows/fpso-spread-mooring/`](../examples/workflows/fpso-spread-mooring/) | This example supplies vessel particulars and mooring geometry for a floating production vessel. | — | — |
| [`examples/workflows/free-span-atlas-query/`](../examples/workflows/free-span-atlas-query/) | This example queries pipeline free-span screening results by span, section dimensions, and current speed. | — | — |
| [`examples/workflows/free-span-f105/`](../examples/workflows/free-span-f105/) | This example screens a pipeline free span for vortex-induced vibration under DNV-RP-F105. | — | — |
| [`examples/workflows/hull-girder-report-pack/`](../examples/workflows/hull-girder-report-pack/) | This report example separates calculated still-water hull-girder demand from unresolved strength inputs. | — | — |
| [`examples/workflows/hull-resistance/`](../examples/workflows/hull-resistance/) | This example estimates vessel resistance from hull geometry. | — | — |
| [`examples/workflows/hull-seakeeping/`](../examples/workflows/hull-seakeeping/) | This example screens natural periods and wave response from vessel particulars. | — | — |
| [`examples/workflows/hull-wastage-screen/`](../examples/workflows/hull-wastage-screen/) | This example supplies representative hull thickness measurements for a wastage screen. | — | — |
| [`examples/workflows/hydro-coefficients/`](../examples/workflows/hydro-coefficients/) | This example queries hydrodynamic coefficients by wave frequency and degrees of freedom. | — | — |
| [`examples/workflows/inspection-planning/`](../examples/workflows/inspection-planning/) | This example estimates remaining life and inspection intervals from thickness and corrosion rate. | — | — |
| [`examples/workflows/inspection-planning-atlas-query/`](../examples/workflows/inspection-planning-atlas-query/) | This example looks up remaining service life from wall thickness and corrosion rate. | — | — |
| [`examples/workflows/jacket-damaged-member-screen/`](../examples/workflows/jacket-damaged-member-screen/) | Synthetic damage cases illustrate dent, bow, stress, and severed-member diagnostics for an offshore jacket. | — | — |
| [`examples/workflows/jacket-member-joint-checks/`](../examples/workflows/jacket-member-joint-checks/) | This example defines tubular jacket members and joints for structural checks. | — | — |
| [`examples/workflows/jlay-pipelay/`](../examples/workflows/jlay-pipelay/) | This example screens sagbend response for J-lay installation of a deepwater rigid pipeline. | — | — |
| [`examples/workflows/jumper-installation/`](../examples/workflows/jumper-installation/) | This example screens a deepwater rigid-jumper installation using vessel and jumper catalogue data. | — | — |
| [`examples/workflows/lifting-lug-atlas-query/`](../examples/workflows/lifting-lug-atlas-query/) | This example looks up lifting-lug utilization by static load and plate thickness. | — | — |
| [`examples/workflows/lifting-lug-design/`](../examples/workflows/lifting-lug-design/) | This example checks a padeye under sling load for competing strength limits. | — | — |
| [`examples/workflows/liquefaction/`](../examples/workflows/liquefaction/) | This example screens layers of saturated soil for earthquake-induced liquefaction. | — | — |
| [`examples/workflows/loading-computer/`](../examples/workflows/loading-computer/) | This example evaluates a loaded box barge for equilibrium, stability, and hull-girder loads. | — | — |
| [`examples/workflows/mcp_server/`](../examples/workflows/mcp_server/) | This placeholder reserves a usage example for the digitalmodel.workflows.mcp_server module. | — | — |
| [`examples/workflows/mooring-chain-degradation-screen/`](../examples/workflows/mooring-chain-degradation-screen/) | This synthetic example screens dimensional degradation in a mooring chain and records the resulting escalation. | — | — |
| [`examples/workflows/mooring-design-mbl/`](../examples/workflows/mooring-design-mbl/) | This example determines the minimum breaking load for a mooring design case. | — | — |
| [`examples/workflows/mooring-fatigue/`](../examples/workflows/mooring-fatigue/) | This example estimates metallic mooring line fatigue from tension-range bins. | — | — |
| [`examples/workflows/mooring-fatigue-atlas-query/`](../examples/workflows/mooring-fatigue-atlas-query/) | This example queries precomputed mooring fatigue results for a line section and tension-cycle history. | — | — |
| [`examples/workflows/mooring-fatigue-v2/`](../examples/workflows/mooring-fatigue-v2/) | This example repeats a mooring line fatigue assessment with a free-corrosion stress-life curve. | — | — |
| [`examples/workflows/mudmat-bearing-atlas-query/`](../examples/workflows/mudmat-bearing-atlas-query/) | This example queries mudmat bearing utilization by vertical load and undrained shear strength. | — | — |
| [`examples/workflows/mudmat-bearing-capacity/`](../examples/workflows/mudmat-bearing-capacity/) | This example checks a subsea mudmat for vertical bearing and sliding. | — | — |
| [`examples/workflows/naval-arch-maneuvering-envelope/`](../examples/workflows/naval-arch-maneuvering-envelope/) | This example screens a vessel's low-speed maneuvering and station-keeping envelope. | — | — |
| [`examples/workflows/naval-arch-yaw-moment/`](../examples/workflows/naval-arch-yaw-moment/) | This example estimates vessel yaw moment from a defined hull and loading case. | — | — |
| [`examples/workflows/nodal-analysis/`](../examples/workflows/nodal-analysis/) | This example finds a well operating point at the intersection of inflow and vertical-lift performance curves. | — | — |
| [`examples/workflows/nodal-analysis-ipr-vlp/`](../examples/workflows/nodal-analysis-ipr-vlp/) | This example invokes the production workflow to match well inflow and vertical-lift performance. | — | — |
| [`examples/workflows/ocimf-tanker-loads/`](../examples/workflows/ocimf-tanker-loads/) | This example calculates tanker environmental loads from vessel geometry and tabulated OCIMF coefficients. | — | — |
| [`examples/workflows/on-bottom-stability-e305/`](../examples/workflows/on-bottom-stability-e305/) | This example exercises a simplified pipeline on-bottom stability calculation with an incomplete comparison basis. | — | — |
| [`examples/workflows/on-bottom-stability-f109/`](../examples/workflows/on-bottom-stability-f109/) | This example evaluates lateral on-bottom stability for a concrete-coated pipeline using DNV-RP-F109. | — | — |
| [`examples/workflows/openfoam-run-batch/`](../examples/workflows/openfoam-run-batch/) | This example specifies a matrix of OpenFOAM cases for parallel execution. | — | — |
| [`examples/workflows/opex-estimate/`](../examples/workflows/opex-estimate/) | This example estimates annual operating cost for a semisubmersible production facility. | — | — |
| [`examples/workflows/orcaflex-6dbuoy-dnvrph103/`](../examples/workflows/orcaflex-6dbuoy-dnvrph103/) | This example specifies buoy mass and shape for a simplified DNV-RP-H103 calculation. | — | — |
| [`examples/workflows/orcaflex-library-query/`](../examples/workflows/orcaflex-library-query/) | This example queries precomputed OrcaFlex results by case and heading. | — | — |
| [`examples/workflows/orcaflex-run-batch/`](../examples/workflows/orcaflex-run-batch/) | This example sets up concurrent OrcaFlex simulations from a base model and a factorial matrix of variants. | — | — |
| [`examples/workflows/orcaflex-spec-authoring-llm/`](../examples/workflows/orcaflex-spec-authoring-llm/) | This example illustrates turning a project description and natural-language request into an OrcaFlex analysis specification. | — | — |
| [`examples/workflows/orcaflex-strength-post/`](../examples/workflows/orcaflex-strength-post/) | This example extracts tension, bending, and curvature statistics from OrcaFlex simulation results. | — | — |
| [`examples/workflows/orcawave-control-surface/`](../examples/workflows/orcawave-control-surface/) | This example prepares an OrcaWave diffraction case with a control surface. | — | — |
| [`examples/workflows/orcawave-diffraction-solve/`](../examples/workflows/orcawave-diffraction-solve/) | This example configures a licensed OrcaWave diffraction solve from a model specification. | — | — |
| [`examples/workflows/orcawave-field-points/`](../examples/workflows/orcawave-field-points/) | This example prepares synthetic OrcaWave field-point inputs for an air-gap and splash request. | — | — |
| [`examples/workflows/orcawave-input-prep/`](../examples/workflows/orcawave-input-prep/) | This example converts a diffraction specification into an OrcaWave input model. | — | — |
| [`examples/workflows/orcawave-mean-drift/`](../examples/workflows/orcawave-mean-drift/) | This example prepares an OrcaWave diffraction case for mean drift calculations. | — | — |
| [`examples/workflows/passing-ship/`](../examples/workflows/passing-ship/) | This example defines moored and passing vessels for a ship-interaction load calculation. | — | — |
| [`examples/workflows/perry-robertson/`](../examples/workflows/perry-robertson/) | This example tabulates column buckling strength as a function of slenderness. | — | — |
| [`examples/workflows/pile-axial-capacity/`](../examples/workflows/pile-axial-capacity/) | This example screens axial capacity for an open-ended pile using the API RP 2GEO alpha method. | — | — |
| [`examples/workflows/pile-capacity/`](../examples/workflows/pile-capacity/) | This example supplies embedded pile dimensions and clay properties for an axial capacity calculation. | — | — |
| [`examples/workflows/pile-capacity-atlas-query/`](../examples/workflows/pile-capacity-atlas-query/) | This example queries pile capacity for specified pile dimensions, soil properties, and demand. | — | — |
| [`examples/workflows/pipe-capacity/`](../examples/workflows/pipe-capacity/) | This example checks the internal pressure capacity of an export pipeline under API STD 2RD. | — | — |
| [`examples/workflows/pipe-ovality/`](../examples/workflows/pipe-ovality/) | This example compares measured diameter variation with an allowable pipe ovality. | — | — |
| [`examples/workflows/pipelay-screening/`](../examples/workflows/pipelay-screening/) | This synthetic case illustrates installation screening for subsea pipelay. | — | — |
| [`examples/workflows/pipeline-corroded-defect-screen/`](../examples/workflows/pipeline-corroded-defect-screen/) | This example screens a corroded pipeline defect using supplied geometry and operating conditions. | — | — |
| [`examples/workflows/pipeline-ffs-assessment/`](../examples/workflows/pipeline-ffs-assessment/) | This synthetic example shows how pipeline integrity inputs produce a fitness-for-service assessment report. | — | — |
| [`examples/workflows/pipeline-lateral-buckling/`](../examples/workflows/pipeline-lateral-buckling/) | This example screens lateral buckling of a high-pressure, high-temperature subsea pipeline. | — | — |
| [`examples/workflows/pipeline-upheaval-buckling/`](../examples/workflows/pipeline-upheaval-buckling/) | This example screens buried-pipeline upheaval and the cover required to restrain it. | — | — |
| [`examples/workflows/plate-buckling/`](../examples/workflows/plate-buckling/) | This example checks a stiffened deck plate under combined longitudinal, transverse, and shear stresses. | — | — |
| [`examples/workflows/platform-stability/`](../examples/workflows/platform-stability/) | This example screens intact stability and wind heel for a floating production platform. | — | — |
| [`examples/workflows/pore-pressure/`](../examples/workflows/pore-pressure/) | This example estimates pore pressure and drilling-window uncertainty using offset-well calibration. | — | — |
| [`examples/workflows/pressure-vessel-metal-loss/`](../examples/workflows/pressure-vessel-metal-loss/) | This example calculates the effect of progressive uniform wall loss on a cylindrical pressure shell. | — | — |
| [`examples/workflows/process-piping/`](../examples/workflows/process-piping/) | This example supplies a synthetic flanged joint and water line for process piping calculations. | — | — |
| [`examples/workflows/propeller-rudder-interaction/`](../examples/workflows/propeller-rudder-interaction/) | This example estimates propeller-rudder interaction from propulsion and vessel inputs. | — | — |
| [`examples/workflows/rao-atlas-query/`](../examples/workflows/rao-atlas-query/) | This example retrieves tabulated vessel response by wave period and heading. | — | — |
| [`examples/workflows/rao-spectral-fatigue/`](../examples/workflows/rao-spectral-fatigue/) | This example estimates fatigue from a stress response amplitude operator and a wave spectrum. | — | — |
| [`examples/workflows/rao-tabulation/`](../examples/workflows/rao-tabulation/) | This example tabulates vessel response amplitudes at specified wave periods and headings. | — | — |
| [`examples/workflows/reel-lay-pipelay/`](../examples/workflows/reel-lay-pipelay/) | This example screens installation strain and residual curvature from reeling a rigid pipeline. | — | — |
| [`examples/workflows/report-pack/`](../examples/workflows/report-pack/) | This example renders a marine engineering report from a synthetic spectral fatigue case. | — | — |
| [`examples/workflows/response-spectrum/`](../examples/workflows/response-spectrum/) | This example generates a response spectrum from a ground acceleration record. | — | — |
| [`examples/workflows/rig-capability-assessment/`](../examples/workflows/rig-capability-assessment/) | This example screens and ranks onshore drilling rigs against well program requirements. | — | — |
| [`examples/workflows/rigging/`](../examples/workflows/rigging/) | This example resolves rigging components against catalogue entries and summarizes their ratings and geometry. | — | — |
| [`examples/workflows/riser-configuration-screen/`](../examples/workflows/riser-configuration-screen/) | These synthetic inputs illustrate numerical screening of riser configurations. | — | — |
| [`examples/workflows/riser-fatigue/`](../examples/workflows/riser-fatigue/) | This example combines wave and vortex-induced fatigue damage for segments of a steel catenary riser. | — | — |
| [`examples/workflows/riser-fatigue-atlas-query/`](../examples/workflows/riser-fatigue-atlas-query/) | This example queries riser fatigue results by stress concentration, vibration stress scale, and design life. | — | — |
| [`examples/workflows/riser-joint-ffs/`](../examples/workflows/riser-joint-ffs/) | This example supplies a riser joint for a fitness-for-service assessment. | — | — |
| [`examples/workflows/riser-screening/`](../examples/workflows/riser-screening/) | This example screens a deepwater production riser for pressure and combined loading under API STD 2RD. | — | — |
| [`examples/workflows/riser-screening-f201/`](../examples/workflows/riser-screening-f201/) | This example screens a deepwater production riser for pressure and combined loading under DNV-ST-F201. | — | — |
| [`examples/workflows/riser-stackup/`](../examples/workflows/riser-stackup/) | This example supplies submerged weight, buoyancy, and design pressure for riser stack-up calculations. | — | — |
| [`examples/workflows/riser-stackup-drawing/`](../examples/workflows/riser-stackup-drawing/) | This example renders a synthetic riser stack-up drawing and reconciles it with its source data. | — | — |
| [`examples/workflows/riser-stackup-parametric/`](../examples/workflows/riser-stackup-parametric/) | This example runs a matrix of riser stack-up configurations from tabulated inputs. | — | — |
| [`examples/workflows/riser-stackup-registry-batch/`](../examples/workflows/riser-stackup-registry-batch/) | This example selects planned riser stack-ups from the registry for a batch assessment. | — | — |
| [`examples/workflows/riser-viv-screening/`](../examples/workflows/riser-viv-screening/) | This example checks a synthetic pinned riser span for cross-flow and in-line vortex-induced vibration. | — | — |
| [`examples/workflows/riser-worked-configurations/`](../examples/workflows/riser-worked-configurations/) | These worked configurations illustrate different riser arrangements using generic pipe properties. | — | — |
| [`examples/workflows/rop-analysis/`](../examples/workflows/rop-analysis/) | This example compares drilling rate-of-penetration models on the same inputs. | — | — |
| [`examples/workflows/rudder-stock-torque/`](../examples/workflows/rudder-stock-torque/) | This example estimates preliminary rudder-stock torque from a vessel and rudder case. | — | — |
| [`examples/workflows/scour-assessment/`](../examples/workflows/scour-assessment/) | This example checks sediment motion and an armour criterion around a pipeline. | — | — |
| [`examples/workflows/skills/`](../examples/workflows/skills/) | This placeholder reserves a usage example for the digitalmodel.workflows.skills module. | — | — |
| [`examples/workflows/slope-stability/`](../examples/workflows/slope-stability/) | This example defines soil layers, ground geometry, and water properties for a slope stability calculation. | — | — |
| [`examples/workflows/sn-curve/`](../examples/workflows/sn-curve/) | This example evaluates a selected fatigue stress-life curve at specified stress ranges. | — | — |
| [`examples/workflows/sn-curve-comparison/`](../examples/workflows/sn-curve-comparison/) | This example compares several fatigue stress-life curves across the same stress range. | — | — |
| [`examples/workflows/span-rectification/`](../examples/workflows/span-rectification/) | This example designs intermediate support spacing for a pipeline free span that exceeds its allowable length. | — | — |
| [`examples/workflows/span-rectification-atlas-query/`](../examples/workflows/span-rectification-atlas-query/) | This example queries free-span utilization by surveyed span length and current speed. | — | — |
| [`examples/workflows/spec-authoring-llm/`](../examples/workflows/spec-authoring-llm/) | This example illustrates producing an analysis specification from project data and a natural-language request. | — | — |
| [`examples/workflows/spectral-fatigue/`](../examples/workflows/spectral-fatigue/) | This example estimates fatigue damage and life from a stress response power spectrum. | — | — |
| [`examples/workflows/spectral-fatigue-atlas-query/`](../examples/workflows/spectral-fatigue-atlas-query/) | This example queries precomputed spectral fatigue results by sea state and design life. | — | — |
| [`examples/workflows/stress-strain/`](../examples/workflows/stress-strain/) | This example generates a nonlinear stress-strain curve for a pipeline steel. | — | — |
| [`examples/workflows/stress-strain-parametric/`](../examples/workflows/stress-strain-parametric/) | This example sweeps pipeline steel yield strengths to compare nonlinear stress-strain responses. | — | — |
| [`examples/workflows/subsea-lowering/`](../examples/workflows/subsea-lowering/) | This example prepares sea-state cases and screens imported phase extrema for a subsea lowering operation. | — | — |
| [`examples/workflows/swab-surge/`](../examples/workflows/swab-surge/) | This example estimates pressure changes as pipe moves into or out of a well. | — | — |
| [`examples/workflows/synthetic-rope-atlas-query/`](../examples/workflows/synthetic-rope-atlas-query/) | This example queries synthetic-rope mooring fatigue results by breaking load and cyclic tension history. | — | — |
| [`examples/workflows/synthetic-rope-mooring-fatigue/`](../examples/workflows/synthetic-rope-mooring-fatigue/) | This example screens a synthetic mooring rope for tension fatigue, creep rupture, and minimum-tension limits. | — | — |
| [`examples/workflows/time-series/`](../examples/workflows/time-series/) | This example calculates a frequency spectrum from a synthetic periodic signal. | — | — |
| [`examples/workflows/tsj-sizing/`](../examples/workflows/tsj-sizing/) | This example supplies taper geometry, tension, and pressure for sizing a tapered stress joint. | — | — |
| [`examples/workflows/tubular-design-envelope/`](../examples/workflows/tubular-design-envelope/) | This example evaluates the load envelope for a casing section from its dimensions and material strength. | — | — |
| [`examples/workflows/vessel-seakeeping/`](../examples/workflows/vessel-seakeeping/) | This example screens seakeeping operability from vessel motion responses and sea-state frequencies. | — | — |
| [`examples/workflows/viv-analysis/`](../examples/workflows/viv-analysis/) | This example screens a free-spanning tubular member using natural frequency and vortex-shedding frequency. | — | — |
| [`examples/workflows/viv-atlas-query/`](../examples/workflows/viv-atlas-query/) | This example looks up vortex-induced vibration results by span length and current speed. | — | — |
| [`examples/workflows/viv-parametric/`](../examples/workflows/viv-parametric/) | This example varies tubular span and loading inputs for a vortex-induced vibration study. | — | — |
| [`examples/workflows/viv-parametric-screening/`](../examples/workflows/viv-parametric-screening/) | This example evaluates reduced velocity and lock-in for a synthetic tubular member across vibration modes. | — | — |
| [`examples/workflows/vlp-correlations/`](../examples/workflows/vlp-correlations/) | This example calculates a vertical-lift performance curve for an oil well. | — | — |
| [`examples/workflows/von-mises/`](../examples/workflows/von-mises/) | This example supplies a mesh and loads for calculating equivalent stress. | — | — |
| [`examples/workflows/wall-thickness-compare/`](../examples/workflows/wall-thickness-compare/) | This example compares pipeline wall thickness requirements across several design codes. | — | — |
| [`examples/workflows/wall-thickness-quickcheck/`](../examples/workflows/wall-thickness-quickcheck/) | This example runs a cached pipeline thickness check and produces an HTML report and tabulated results. | — | — |
| [`examples/workflows/wave-spectra/`](../examples/workflows/wave-spectra/) | This example calculates a fully developed sea spectrum on a specified frequency grid. | — | — |
| [`examples/workflows/wave-spectrum/`](../examples/workflows/wave-spectrum/) | This example calculates a peaked JONSWAP sea spectrum from wave height and peak period. | — | — |
| [`examples/workflows/weather-window/`](../examples/workflows/weather-window/) | This example screens a wave-height record for uninterrupted operating windows of a required duration. | — | — |
| [`examples/workflows/weather-window-atlas-query/`](../examples/workflows/weather-window-atlas-query/) | This example queries operating-window estimates by limiting wave height and required duration. | — | — |
| [`examples/workflows/well-access/`](../examples/workflows/well-access/) | This example compares dry-tree and subsea access over the life of a deepwater well program. | — | — |
| [`examples/workflows/well-bore-design/`](../examples/workflows/well-bore-design/) | This example compares offshore wellbore designs against a specified well objective. | — | — |
| [`examples/workflows/well-hydraulics/`](../examples/workflows/well-hydraulics/) | This example screens well hydraulics for a defined pipe and hole section. | — | — |
| [`examples/workflows/well-intervention-screening/`](../examples/workflows/well-intervention-screening/) | This script illustrates well-intervention screening using public production records. | — | — |
| [`examples/workflows/wellbore-stability/`](../examples/workflows/wellbore-stability/) | This example specifies an offline wellbore stability screen and records sources for its inputs. | — | — |
| [`examples/workflows/wellpath/`](../examples/workflows/wellpath/) | This example reconstructs a well trajectory from measured depths, inclinations, and azimuths. | — | — |
| [`examples/workflows/workflow_automation/`](../examples/workflows/workflow_automation/) | This placeholder reserves a usage example for the digitalmodel.workflows.workflow_automation module. | — | — |
| [`knowledge/`](../knowledge/) | Collects engineering reference notes organized as wikis. | — | — |
| [`knowledge/wikis/`](../knowledge/wikis/) | Houses the repository's engineering standards wiki. | — | — |
| [`knowledge/wikis/engineering-standards/`](../knowledge/wikis/engineering-standards/) | Summarizes individual engineering standards for reference. | — | — |
| [`notebooks/`](../notebooks/) | Collects interactive engineering examples, including geospatial coordinate conversion. | — | — |
| [`notebooks/gis/`](../notebooks/gis/) | Demonstrates conversion of geospatial coordinates in a notebook. | — | — |
| [`references/`](../references/) | Collects reference material for vessel operability research. | — | — |
| [`references/vessel-suitability/`](../references/vessel-suitability/) | Preserves research references on offshore vessel access and the limits on their use. | — | Limit use to internal research and methodology development. |
| [`references/vessel-suitability/data/`](../references/vessel-suitability/data/) | Retains the digitized reference values and metadata for a crew-transfer vessel operability study. | — | Use digitized values for internal reference only. |
| [`scripts/`](../scripts/) | Provides command-line tools for preparing inputs, running studies, checking results and maintaining the repository. | — | — |
| [`scripts/ansys/`](../scripts/ansys/) | Runs pressure and zero-load control diagnostics in ANSYS. | — | — |
| [`scripts/artificial_lift/`](../scripts/artificial_lift/) | Extracts artificial-lift reference catalogs and builds evidence for solver checks. | — | — |
| [`scripts/automation/`](../scripts/automation/) | Coordinates agent tasks, subprocess reviews and workflow checks. | — | — |
| [`scripts/bash/`](../scripts/bash/) | Collects Bash utilities for local development and testing. | — | — |
| [`scripts/bash/digitalmodel/`](../scripts/bash/digitalmodel/) | Provides shell setup, repository maintenance and test runner scripts. | — | — |
| [`scripts/benchmark/`](../scripts/benchmark/) | Builds benchmark inputs, extracts reference data and compares hydrodynamic solver results. | — | — |
| [`scripts/bha_database/`](../scripts/bha_database/) | Builds the manifest for the bottom-hole assembly database. | — | — |
| [`scripts/capabilities/`](../scripts/capabilities/) | Generates the engineering capability inventory, interactive explorers and summary pages. | — | — |
| [`scripts/capabilities/installation_heading/`](../scripts/capabilities/installation_heading/) | Supplies limits, revision records and a page template for the installation heading explorer. | — | — |
| [`scripts/cathodic_protection/`](../scripts/cathodic_protection/) | Generates cathodic protection potential as a function of distance. | — | — |
| [`scripts/cfd/`](../scripts/cfd/) | Prepares, runs and monitors ship hydrodynamics CFD cases and checks the output. | — | — |
| [`scripts/cfd/lib/`](../scripts/cfd/lib/) | Provides configuration, worker routines and benchmark metrics shared by the CFD scripts. | — | — |
| [`scripts/cfd/status/`](../scripts/cfd/status/) | Collects and displays CFD execution status. | — | — |
| [`scripts/ci/`](../scripts/ci/) | Checks CI inputs, selects test groups and measures coverage and execution time. | — | — |
| [`scripts/context/`](../scripts/context/) | Automates checks and updates for development session context. | — | — |
| [`scripts/conversion/`](../scripts/conversion/) | Converts YAML model definitions into include files. | — | — |
| [`scripts/corrosion/`](../scripts/corrosion/) | Builds the galvanic corrosion explorer. | — | — |
| [`scripts/data/`](../scripts/data/) | Fetches engineering data, checks registry freshness and generates mooring component records. | — | — |
| [`scripts/dev/`](../scripts/dev/) | Checks development dependencies in linked worktrees. | — | — |
| [`scripts/docs/`](../scripts/docs/) | Checks agent documentation packs and resolves source references. | — | — |
| [`scripts/drilling_riser/`](../scripts/drilling_riser/) | Builds the drilling riser operability explorer and monitoring view. | — | — |
| [`scripts/enforcement/`](../scripts/enforcement/) | Checks repository text, report provenance and file contents against project conventions. | — | — |
| [`scripts/examples/`](../scripts/examples/) | Provides a worked evaluation of three-dimensional visualization with PyVista. | — | — |
| [`scripts/fender_design/`](../scripts/fender_design/) | Imports fender catalog tables and compares workbook cell values. | — | — |
| [`scripts/ffs/`](../scripts/ffs/) | Builds fitness-for-service readiness and riser joint views and assesses a measured thickness grid. | — | — |
| [`scripts/field_development/`](../scripts/field_development/) | Produces example field layouts, flythroughs and a field development API demonstration. | — | — |
| [`scripts/fowt/`](../scripts/fowt/) | Prepares reference floating wind turbine cases, extracts model data and runs checks. | — | — |
| [`scripts/geotechnical/`](../scripts/geotechnical/) | Checks tabulated scour data against its recorded values. | — | — |
| [`scripts/git/`](../scripts/git/) | Provides Python utilities for Git operations and related command shortcuts. | — | — |
| [`scripts/hf/`](../scripts/hf/) | Builds engineering capability datasets and prepares atlas explorers for publication. | — | — |
| [`scripts/hooks/`](../scripts/hooks/) | Provides a Git hook check for machine-specific paths. | — | — |
| [`scripts/hull_library/`](../scripts/hull_library/) | Screens hull geometry inventories and investigates profile conversion and CAD geometry quality. | — | — |
| [`scripts/integrations/`](../scripts/integrations/) | Evaluates third-party engineering libraries through examples, comparisons and benchmarks. | — | — |
| [`scripts/integrations/output/`](../scripts/integrations/output/) | Retains mesh images and numerical results from composite section property evaluations. | — | — |
| [`scripts/inventory/`](../scripts/inventory/) | Builds an inventory of legacy OrcaFlex models. | — | — |
| [`scripts/legal/`](../scripts/legal/) | Contains checks and rules for reviewing material intended for public release. | — | — |
| [`scripts/maintenance/`](../scripts/maintenance/) | Audits repository structure, archive candidates, generated files and issue records. | — | — |
| [`scripts/metocean/`](../scripts/metocean/) | Extracts, ranks and standardizes metocean reference tables. | — | — |
| [`scripts/monitoring/`](../scripts/monitoring/) | Checks agent usage and suggests a model for the available capacity. | — | — |
| [`scripts/orcaflex/`](../scripts/orcaflex/) | Prepares OrcaFlex model libraries and campaigns and checks generic riser cases. | — | — |
| [`scripts/parametric/`](../scripts/parametric/) | Builds parametric result atlases and queryable model libraries. | — | — |
| [`scripts/pilots/`](../scripts/pilots/) | Runs the vortex-induced vibration pilot and prepares its results for publication. | — | — |
| [`scripts/pipelay/`](../scripts/pipelay/) | Generates the evidence note for pipelay analysis. | — | — |
| [`scripts/process/`](../scripts/process/) | Finds running script processes. | — | — |
| [`scripts/production_chemistry/`](../scripts/production_chemistry/) | Builds an explorer for production chemistry scale saturation indices. | — | — |
| [`scripts/python/`](../scripts/python/) | Collects Python utilities for engineering analysis and data management. | — | — |
| [`scripts/python/digitalmodel/`](../scripts/python/digitalmodel/) | Provides engineering analysis scripts alongside asset and standards index builders. | — | — |
| [`scripts/reporting/`](../scripts/reporting/) | Builds engineering assessment reports and checks their provenance and coverage. | — | — |
| [`scripts/reports/`](../scripts/reports/) | Builds the vessel seakeeping module report. | — | — |
| [`scripts/review/`](../scripts/review/) | Collects issue review records and supporting evidence. | — | — |
| [`scripts/review/results/`](../scripts/review/results/) | Retains reviewer findings, syntheses and baseline test output for individual issues. | — | — |
| [`scripts/riser_capability/`](../scripts/riser_capability/) | Builds riser inventory and capability registers and reports worked configurations. | — | — |
| [`scripts/riser_database/`](../scripts/riser_database/) | Builds riser database tables from the listed sources. | — | — |
| [`scripts/riser_study/`](../scripts/riser_study/) | Records and derives wall thickness inputs for the riser study. | — | — |
| [`scripts/risers/`](../scripts/risers/) | Replays analytical riser cases and compares them with closed-form results. | — | — |
| [`scripts/roll_damping/`](../scripts/roll_damping/) | Builds the roll damping database manifest. | — | — |
| [`scripts/setup/`](../scripts/setup/) | Provisions and verifies CFD execution environments and transfers study data. | — | — |
| [`scripts/studies/`](../scripts/studies/) | Generates structural stability studies and prepares, processes and reports surface-crack analyses. | — | — |
| [`scripts/subsea/`](../scripts/subsea/) | Converts the on-bottom stability reference workbook. | — | — |
| [`scripts/testing/`](../scripts/testing/) | Exercises model generators, converters and their end-to-end workflow. | — | — |
| [`scripts/tools/`](../scripts/tools/) | Builds the LibreDWG conversion utility. | — | — |
| [`scripts/validation/`](../scripts/validation/) | Checks solver input sections, hull meshes and engineering model assumptions. | — | — |
| [`scripts/verification/`](../scripts/verification/) | Verifies OrcaFlex campaign outputs. | — | — |
| [`scripts/web/`](../scripts/web/) | Demonstrates authentication behavior with Unicode inputs. | — | — |
| [`src/`](../src/) | Houses the installable Python source tree. | — | Keep tests in the repository test tree. |
| [`src/digitalmodel/`](../src/digitalmodel/) | Provides the engineering package, its workflow entry points and domain calculations. | — | — |
| [`src/digitalmodel/alt_fuel_ship_sizing/`](../src/digitalmodel/alt_fuel_ship_sizing/) | Compares ship sizing and fuel-chain requirements for alternative fuels. | — | — |
| [`src/digitalmodel/ansys/`](../src/digitalmodel/ansys/) | Builds, records and reads back ANSYS analyses and their supporting evidence. | — | — |
| [`src/digitalmodel/aqg/`](../src/digitalmodel/aqg/) | Evaluates diffraction analysis quality from offline evidence. | — | — |
| [`src/digitalmodel/asset_integrity/`](../src/digitalmodel/asset_integrity/) | Implements fitness-for-service assessments and integrity screening for equipment and structures. | — | — |
| [`src/digitalmodel/base_configs/`](../src/digitalmodel/base_configs/) | Supplies packaged YAML defaults for engineering workflows and exposes configuration helpers. | — | — |
| [`src/digitalmodel/benchmarks/`](../src/digitalmodel/benchmarks/) | Inventories example models and computes benchmark metrics. | — | — |
| [`src/digitalmodel/bha_database/`](../src/digitalmodel/bha_database/) | Loads drilling bottom-hole assembly records and checks their dimensions and properties. | — | — |
| [`src/digitalmodel/cad/`](../src/digitalmodel/cad/) | Reads and converts CAD geometry with explicit coordinates and units. | — | — |
| [`src/digitalmodel/cathodic_protection/`](../src/digitalmodel/cathodic_protection/) | Calculates cathodic protection demand, anode sizing and related design outputs. | — | — |
| [`src/digitalmodel/citations/`](../src/digitalmodel/citations/) | Resolves and validates citations for standards-derived calculation inputs. | — | — |
| [`src/digitalmodel/code_checks/`](../src/digitalmodel/code_checks/) | Applies engineering code checks to pipes and storage tanks. | — | — |
| [`src/digitalmodel/common/`](../src/digitalmodel/common/) | Supplies shared unit, hydraulic, assumption-tracking and specification helpers. | — | — |
| [`src/digitalmodel/compare_tool/`](../src/digitalmodel/compare_tool/) | Compares tabular inputs in an offline workflow. | — | — |
| [`src/digitalmodel/contract_validation/`](../src/digitalmodel/contract_validation/) | Checks riser configuration, length and manifest data against their declared contracts. | — | — |
| [`src/digitalmodel/corrosion/`](../src/digitalmodel/corrosion/) | Screens galvanic corrosion for material combinations. | — | — |
| [`src/digitalmodel/data/`](../src/digitalmodel/data/) | Packages equipment specifications and engineering reference tables for use by the library. | — | — |
| [`src/digitalmodel/data_models/`](../src/digitalmodel/data_models/) | Defines data structures for digitized curves and interpolation. | — | — |
| [`src/digitalmodel/data_systems/`](../src/digitalmodel/data_systems/) | Acquires, catalogs, validates and migrates engineering data. | — | — |
| [`src/digitalmodel/drilling_riser/`](../src/digitalmodel/drilling_riser/) | Models drilling-riser assemblies, static loads, dynamic response and operability. | — | — |
| [`src/digitalmodel/dynmoor_mooring/`](../src/digitalmodel/dynmoor_mooring/) | Routes mooring analysis inputs through the DYNMOOR workflow. | — | — |
| [`src/digitalmodel/economics/`](../src/digitalmodel/economics/) | Estimates pipeline costs from supplied prices. | — | — |
| [`src/digitalmodel/fatg_spectral_fatigue/`](../src/digitalmodel/fatg_spectral_fatigue/) | Runs the legacy closed-form FATG spectral fatigue calculation. | — | — |
| [`src/digitalmodel/fatigue/`](../src/digitalmodel/fatigue/) | Calculates fatigue damage from stress ranges, design curves and spectral inputs. | — | — |
| [`src/digitalmodel/field_development/`](../src/digitalmodel/field_development/) | Compares field development concepts using layouts, cost estimates and economic models. | — | — |
| [`src/digitalmodel/floating_wind/`](../src/digitalmodel/floating_wind/) | Models floating wind concepts and screens their technical and economic characteristics. | — | — |
| [`src/digitalmodel/foam_system/`](../src/digitalmodel/foam_system/) | Sizes firefighting foam systems against a library of design criteria. | — | — |
| [`src/digitalmodel/geotechnical/`](../src/digitalmodel/geotechnical/) | Evaluates soil data, foundation response, anchor capacity and slope stability. | — | — |
| [`src/digitalmodel/gis/`](../src/digitalmodel/gis/) | Transforms coordinates and supports spatial queries and geographic data exchange. | — | — |
| [`src/digitalmodel/hull_girder_screening/`](../src/digitalmodel/hull_girder_screening/) | Screens the longitudinal strength of a vessel hull girder. | — | — |
| [`src/digitalmodel/hull_wastage_screen/`](../src/digitalmodel/hull_wastage_screen/) | Screens hull structural wastage and buckling using thickness measurements. | — | — |
| [`src/digitalmodel/hydrodynamics/`](../src/digitalmodel/hydrodynamics/) | Models waves, vessel response, resistance and interactions with nearby vessels and structures. | — | — |
| [`src/digitalmodel/hydrostatics/`](../src/digitalmodel/hydrostatics/) | Calculates hydrostatic pressure in fluid columns. | — | — |
| [`src/digitalmodel/infrastructure/`](../src/digitalmodel/infrastructure/) | Provides shared configuration, solver interfaces and runtime services for engineering modules. | — | — |
| [`src/digitalmodel/installation/`](../src/digitalmodel/installation/) | Generates installation pamphlets with calculations and risk references. | — | — |
| [`src/digitalmodel/lifting_lug/`](../src/digitalmodel/lifting_lug/) | Evaluates lifting-lug mechanics and welded connections and reports the assessment. | — | — |
| [`src/digitalmodel/marine_ops/`](../src/digitalmodel/marine_ops/) | Groups marine operations, installation, vessel and production analysis workflows. | — | — |
| [`src/digitalmodel/materials/`](../src/digitalmodel/materials/) | Provides material grades, properties and lookup functions for engineering assessments. | — | — |
| [`src/digitalmodel/mooring_fatigue/`](../src/digitalmodel/mooring_fatigue/) | Calculates fatigue damage in chain and steel wire mooring lines from load histories. | — | — |
| [`src/digitalmodel/mooring_resilience/`](../src/digitalmodel/mooring_resilience/) | Screens mooring-system resilience using line tension and foundation capacity. | — | — |
| [`src/digitalmodel/motion_forecast/`](../src/digitalmodel/motion_forecast/) | Forecasts short-horizon vessel or structure motions from incident waves and motion transfer functions. | — | — |
| [`src/digitalmodel/naval_architecture/`](../src/digitalmodel/naval_architecture/) | Calculates hull geometry, hydrostatics, stability and vessel performance. | — | — |
| [`src/digitalmodel/nde/`](../src/digitalmodel/nde/) | Calculates nondestructive examination pulse timing and loads well acoustic scattering data. | — | — |
| [`src/digitalmodel/orcaflex/`](../src/digitalmodel/orcaflex/) | Prepares and processes OrcaFlex analyses, including environmental inputs and engineering checks. | — | — |
| [`src/digitalmodel/orcawave/`](../src/digitalmodel/orcawave/) | Processes OrcaWave diffraction models, drift forces and hydrodynamic coefficients. | — | — |
| [`src/digitalmodel/parametric/`](../src/digitalmodel/parametric/) | Generates and catalogs parametric models and their atlases. | — | — |
| [`src/digitalmodel/parametrics/`](../src/digitalmodel/parametrics/) | Runs parameter sweeps and connects their results to engineering atlases. | — | — |
| [`src/digitalmodel/passing_ship_forces/`](../src/digitalmodel/passing_ship_forces/) | Calculates the forces induced on a vessel by a passing ship. | — | — |
| [`src/digitalmodel/pipelay/`](../src/digitalmodel/pipelay/) | Prepares analytical screening calculations for J-lay and reel-lay installation. | — | — |
| [`src/digitalmodel/power/`](../src/digitalmodel/power/) | Models electrical power networks, data centers, protection and microgrid controls. | — | — |
| [`src/digitalmodel/process_diagrams/`](../src/digitalmodel/process_diagrams/) | Defines geometry, symbols, tags and sheets for process diagrams. | — | — |
| [`src/digitalmodel/process_piping/`](../src/digitalmodel/process_piping/) | Calculates process line sizes, pressure requirements and bolted-joint checks. | — | — |
| [`src/digitalmodel/production_chemistry/`](../src/digitalmodel/production_chemistry/) | Predicts mineral-scale tendency in produced water from saturation indices. | — | — |
| [`src/digitalmodel/production_engineering/`](../src/digitalmodel/production_engineering/) | Models well inflow, pump hydraulics and production economics. | — | — |
| [`src/digitalmodel/provider_data/`](../src/digitalmodel/provider_data/) | Defines equipment specification records and their source-document provenance. | — | — |
| [`src/digitalmodel/rao_spectral_fatigue/`](../src/digitalmodel/rao_spectral_fatigue/) | Calculates fatigue damage from a stress response transfer function and a wave spectrum. | — | — |
| [`src/digitalmodel/report_pack/`](../src/digitalmodel/report_pack/) | Assembles marine engineering workflow results into a standard report pack. | — | — |
| [`src/digitalmodel/reporting/`](../src/digitalmodel/reporting/) | Renders engineering results as self-contained HTML reports using reusable sections. | — | — |
| [`src/digitalmodel/reservoir/`](../src/digitalmodel/reservoir/) | Models stratigraphy and estimates reservoir volumes. | — | — |
| [`src/digitalmodel/residual/`](../src/digitalmodel/residual/) | Fits a bounded correction to numerical predictions and measures residual error. | — | — |
| [`src/digitalmodel/riser_bom/`](../src/digitalmodel/riser_bom/) | Builds and costs a riser bill of materials from configuration data and supplied rates. | — | — |
| [`src/digitalmodel/riser_configuration_screen/`](../src/digitalmodel/riser_configuration_screen/) | Screens static riser configurations using simplified calculations. | — | — |
| [`src/digitalmodel/riser_database/`](../src/digitalmodel/riser_database/) | Loads riser data and retrieves component properties and code-check factors. | — | — |
| [`src/digitalmodel/riser_fatigue/`](../src/digitalmodel/riser_fatigue/) | Screens fatigue of catenary and lazy-wave risers from stress ranges and touchdown conditions. | — | — |
| [`src/digitalmodel/riser_reporting/`](../src/digitalmodel/riser_reporting/) | Renders fatigue reports from supplied riser workflow observations. | — | — |
| [`src/digitalmodel/riser_results/`](../src/digitalmodel/riser_results/) | Exchanges and validates riser results independently of the originating solver. | — | — |
| [`src/digitalmodel/riser_study/`](../src/digitalmodel/riser_study/) | Processes exported riser stress ranges and cycle counts into benchmark fatigue damage. | — | — |
| [`src/digitalmodel/seismic/`](../src/digitalmodel/seismic/) | Reads strong-motion records and calculates baseline corrections and response spectra. | — | — |
| [`src/digitalmodel/signal_processing/`](../src/digitalmodel/signal_processing/) | Processes time series for spectral analysis and cycle counting. | — | — |
| [`src/digitalmodel/solvers/`](../src/digitalmodel/solvers/) | Connects engineering solvers to model preparation, execution, result comparison and benchmarking. | — | — |
| [`src/digitalmodel/specialized/`](../src/digitalmodel/specialized/) | Groups specialist tools for design checks, rigging, ship analysis and other workflows. | — | — |
| [`src/digitalmodel/specs/`](../src/digitalmodel/specs/) | Defines schemas for engineering results and module manifests. | — | — |
| [`src/digitalmodel/spectral_fatigue/`](../src/digitalmodel/spectral_fatigue/) | Calculates frequency-domain fatigue damage from a stress power spectrum. | — | — |
| [`src/digitalmodel/structural/`](../src/digitalmodel/structural/) | Calculates structural response and capacity for pipes, plates, frames and offshore structures. | — | — |
| [`src/digitalmodel/subsea/`](../src/digitalmodel/subsea/) | Models subsea equipment, risers, moorings, pipelines and their cross sections. | — | — |
| [`src/digitalmodel/synthetic_rope_mooring_fatigue/`](../src/digitalmodel/synthetic_rope_mooring_fatigue/) | Evaluates fatigue for synthetic rope in mooring systems. | — | — |
| [`src/digitalmodel/technical_safety/`](../src/digitalmodel/technical_safety/) | Screens accidental impact loads and fire heat flux against selected criteria. | — | — |
| [`src/digitalmodel/tug/`](../src/digitalmodel/tug/) | Evaluates tug performance, towing loads, fendering and emissions. | — | — |
| [`src/digitalmodel/uncertainty/`](../src/digitalmodel/uncertainty/) | Samples uncertain inputs and evaluates and summarizes the resulting model outputs. | — | — |
| [`src/digitalmodel/usecase_registry/`](../src/digitalmodel/usecase_registry/) | Lists solver analysis use cases and their preparation and dispatch requirements. | — | — |
| [`src/digitalmodel/vessel_seakeeping/`](../src/digitalmodel/vessel_seakeeping/) | Screens vessel operability from motion transfer functions and a specified sea state. | — | — |
| [`src/digitalmodel/vessel_stability_screening/`](../src/digitalmodel/vessel_stability_screening/) | Screens intact vessel stability. | — | — |
| [`src/digitalmodel/visualization/`](../src/digitalmodel/visualization/) | Provides dashboards, CAD design tools and engineering visualizations. | — | — |
| [`src/digitalmodel/viv_screening/`](../src/digitalmodel/viv_screening/) | Screens risers for vortex-induced vibration. | — | — |
| [`src/digitalmodel/weather_window/`](../src/digitalmodel/weather_window/) | Identifies weather windows for marine operations from wave heights and operational limits. | — | — |
| [`src/digitalmodel/web/`](../src/digitalmodel/web/) | Implements web application authentication, views and engineering result interfaces. | — | — |
| [`src/digitalmodel/well/`](../src/digitalmodel/well/) | Provides workflows for well barriers, construction, intervention and tubular design. | — | — |
| [`src/digitalmodel/well_access/`](../src/digitalmodel/well_access/) | Compares well-access strategies using intervention rates, resource availability and lifecycle logistics. | — | — |
| [`src/digitalmodel/workflow_api/`](../src/digitalmodel/workflow_api/) | Exposes an in-process interface for running engineering workflows and recording result provenance. | — | — |
| [`src/digitalmodel/workflows/`](../src/digitalmodel/workflows/) | Automates engineering workflows and coordinates model generation and validation. | — | — |
| [`tests/`](../tests/) | Collects automated checks, input fixtures and reference outputs for the package and its supporting workflows. | — | — |
| [`tests/ansys/`](../tests/ansys/) | Tests ANSYS model preparation, run protocols and interpretation of analysis evidence. | — | — |
| [`tests/ansys/fixtures/`](../tests/ansys/fixtures/) | Provides sample design-point logs, solver journals and license-status records for ANSYS tests. | — | — |
| [`tests/asset_integrity/`](../tests/asset_integrity/) | Checks asset fitness-for-service and integrity assessments against worked cases. | — | — |
| [`tests/asset_integrity/hull_wastage_screen/`](../tests/asset_integrity/hull_wastage_screen/) | Checks hull wastage screening from thickness data through workflow output. | — | — |
| [`tests/asset_integrity/test_data/`](../tests/asset_integrity/test_data/) | Supplies example inputs and reference cases for API 579 fitness-for-service tests. | — | — |
| [`tests/benchmarks/`](../tests/benchmarks/) | Checks engineering benchmark inputs, classifications and numerical results. | — | — |
| [`tests/bha_database/`](../tests/bha_database/) | Tests drilling bottom-hole assembly records and tabulated reference dimensions. | — | — |
| [`tests/cad/`](../tests/cad/) | Tests CAD geometry readers, unit conversion, mesh handling and export. | — | — |
| [`tests/capabilities/`](../tests/capabilities/) | Checks inventories of supported analyses and the documentation that advertises them. | — | — |
| [`tests/cathodic_protection/`](../tests/cathodic_protection/) | Tests anode sizing, design demand, depletion and standards-based cathodic protection calculations. | — | — |
| [`tests/cfd/`](../tests/cfd/) | Checks an analytical sloshing fixture for computational fluid dynamics cases. | — | — |
| [`tests/citations/`](../tests/citations/) | Tests whether engineering citations resolve to the intended standards and source tables. | — | — |
| [`tests/citations/fixtures/`](../tests/citations/fixtures/) | Provides a miniature standards library for exercising citation resolution. | — | — |
| [`tests/code_checks/`](../tests/code_checks/) | Tests standards-based pipe and storage tank design checks. | — | — |
| [`tests/common/`](../tests/common/) | Tests common units, specification authoring and assumption ledger serialization. | — | — |
| [`tests/compare_tool/`](../tests/compare_tool/) | Checks the workflow for comparing tabular results. | — | — |
| [`tests/compat/`](../tests/compat/) | Checks backward-compatible import paths after engineering modules were regrouped. | — | — |
| [`tests/contracts/`](../tests/contracts/) | Tests interfaces with package dependencies and project configuration contracts. | — | — |
| [`tests/corrosion/`](../tests/corrosion/) | Checks galvanic corrosion screening. | — | — |
| [`tests/cross_repo/`](../tests/cross_repo/) | Checks repository layout and agent configuration against shared workspace conventions. | — | — |
| [`tests/data/`](../tests/data/) | Tests data retrieval, manifests, registry freshness and stored dataset references. | — | — |
| [`tests/data_systems/`](../tests/data_systems/) | Tests data acquisition, catalogs, migration and quality checks. | — | — |
| [`tests/data_systems/data/`](../tests/data_systems/data/) | Tests engineering data catalog and migration behavior. | — | — |
| [`tests/data_systems/data_procurement/`](../tests/data_systems/data_procurement/) | Checks metocean procurement adapters with recorded response structures and synthetic data. | — | — |
| [`tests/data_systems/data_quality/`](../tests/data_systems/data_quality/) | Tests how data completeness requirements are enforced. | — | — |
| [`tests/data_systems/data_scraping/`](../tests/data_systems/data_scraping/) | Checks retrieval and extraction of equipment data from manufacturer pages and PDF documents. | — | — |
| [`tests/data_systems/fixtures/`](../tests/data_systems/fixtures/) | Supplies a bathymetric grid subset and its provenance record. | — | — |
| [`tests/docs/`](../tests/docs/) | Checks that documented examples, package references and runbooks match their implementation. | — | — |
| [`tests/domains/`](../tests/domains/) | Supplies shared pytest configuration and fixtures for domain module tests. | — | — |
| [`tests/drilling_riser/`](../tests/drilling_riser/) | Tests drilling-riser assembly, global response and postprocessing. | — | — |
| [`tests/drilling_riser/fixtures/`](../tests/drilling_riser/fixtures/) | Provides drilling-riser component tables, drift-off and drive-off cases and reference records. | — | — |
| [`tests/drilling_riser/global_model/`](../tests/drilling_riser/global_model/) | Tests global drilling-riser model setup, campaign parameters and response calculations. | — | — |
| [`tests/drilling_riser/postprocess/`](../tests/drilling_riser/postprocess/) | Tests extraction, aggregation and evaluation of drilling-riser response channels. | — | — |
| [`tests/drilling_riser/stackup_drawing/`](../tests/drilling_riser/stackup_drawing/) | Tests reconciliation of riser stackup drawings with their component specifications. | — | — |
| [`tests/drilling_riser/stackup_static_weights/`](../tests/drilling_riser/stackup_static_weights/) | Tests component weights, tension and capacity in static drilling-riser stackups. | — | — |
| [`tests/engineering_validation/`](../tests/engineering_validation/) | Groups engineering validation cases and their calculation reports. | — | — |
| [`tests/engineering_validation/pipe_sizing/`](../tests/engineering_validation/pipe_sizing/) | Checks pipe sizing formulas and design-code cases and records the validation results. | — | — |
| [`tests/fatigue/`](../tests/fatigue/) | Tests fatigue damage calculations, design curves and references for fatigue methods. | — | — |
| [`tests/field_development/`](../tests/field_development/) | Checks field concept selection, architecture, cost models and related interfaces. | — | — |
| [`tests/fire_safety/`](../tests/fire_safety/) | Tests foam-system sizing and its firefighting criteria library. | — | — |
| [`tests/fixtures/`](../tests/fixtures/) | Collects reusable engineering test inputs, recorded outputs and reference values. | — | — |
| [`tests/fixtures/cathodic_protection/`](../tests/fixtures/cathodic_protection/) | Supplies YAML inputs for cathodic protection workflows across several offshore asset types. | — | — |
| [`tests/fixtures/geotechnical/`](../tests/fixtures/geotechnical/) | Provides worked slope-stability spreadsheets with source and license records. | — | — |
| [`tests/fixtures/orcaflex/`](../tests/fixtures/orcaflex/) | Supplies OrcaFlex overbend and sagbend model files for pipelay tests. | — | — |
| [`tests/fixtures/reporting/`](../tests/fixtures/reporting/) | Supplies result data, report metadata and HTML snapshots for rendering tests. | — | — |
| [`tests/fixtures/solver/`](../tests/fixtures/solver/) | Pairs native hydrodynamic solver results with spreadsheet references for comparison. | — | — |
| [`tests/fixtures/test_vectors/`](../tests/fixtures/test_vectors/) | Provides expected numerical values and inputs for engineering calculation tests. | — | — |
| [`tests/floating_wind/`](../tests/floating_wind/) | Tests floating wind turbine concepts, floater models and economic workflows. | — | — |
| [`tests/geotechnical/`](../tests/geotechnical/) | Checks soil, slope and foundation calculations against fixtures and solver benchmarks. | — | — |
| [`tests/geotechnical/slope_input_labels/`](../tests/geotechnical/slope_input_labels/) | Checks slope input labels and spreadsheet formula evidence against reference fixtures. | — | — |
| [`tests/geotechnical/tabulated_py_curves/`](../tests/geotechnical/tabulated_py_curves/) | Tests tabulated soil load–displacement curves and their input validation. | — | — |
| [`tests/gis/`](../tests/gis/) | Tests coordinate transformations, spatial features and geographic data export. | — | — |
| [`tests/governance/`](../tests/governance/) | Checks the retirement of repository identifier gates. | — | — |
| [`tests/hydrodynamics/`](../tests/hydrodynamics/) | Tests hydrodynamic model preparation, vessel response, berthing and environmental loads. | — | — |
| [`tests/hydrodynamics/aqwa/`](../tests/hydrodynamics/aqwa/) | Tests AQWA model formats, result parsing and command-line integration. | — | — |
| [`tests/hydrodynamics/bemrosetta/`](../tests/hydrodynamics/bemrosetta/) | Checks BEMRosetta format handling with sample meshes and hydrodynamic results. | — | — |
| [`tests/hydrodynamics/berth_mooring/`](../tests/hydrodynamics/berth_mooring/) | Tests the berth-mooring workflow and its reports. | — | — |
| [`tests/hydrodynamics/berthing_energy/`](../tests/hydrodynamics/berthing_energy/) | Tests berthing energy calculations against example fender data. | — | — |
| [`tests/hydrodynamics/capytaine/`](../tests/hydrodynamics/capytaine/) | Tests Capytaine benchmark cases, diffraction model setup and export. | — | — |
| [`tests/hydrodynamics/diffraction/`](../tests/hydrodynamics/diffraction/) | Checks diffraction benchmarks against stored reference cases and expected results. | — | — |
| [`tests/hydrodynamics/fender_design/`](../tests/hydrodynamics/fender_design/) | Tests fender selection methods, coefficient bases, catalog import and parameter sweeps. | — | — |
| [`tests/hydrodynamics/hull_library/`](../tests/hydrodynamics/hull_library/) | Tests hull geometry generation and exchange between line drawings, surfaces and export files. | — | — |
| [`tests/hydrodynamics/parametric_hull_analysis/`](../tests/hydrodynamics/parametric_hull_analysis/) | Tests hull parameter sweeps, depth effects, forward speed and chart production. | — | — |
| [`tests/hydrodynamics/passing_ship/`](../tests/hydrodynamics/passing_ship/) | Tests passing-ship forces, model configuration and campaign preparation. | — | — |
| [`tests/hydrodynamics/rao_analysis/`](../tests/hydrodynamics/rao_analysis/) | Tests analysis and plotting of vessel motion response amplitude operators. | — | — |
| [`tests/hydrodynamics/rao_tabulation/`](../tests/hydrodynamics/rao_tabulation/) | Tests frequency-based reliability labels for tabulated vessel response data. | — | — |
| [`tests/hydrodynamics/river_currents/`](../tests/hydrodynamics/river_currents/) | Checks river current profiles and the modeled effects of nearby banks. | — | — |
| [`tests/hydrostatics/`](../tests/hydrostatics/) | Tests hydrostatic pressure calculations for fluid columns. | — | — |
| [`tests/hygiene/`](../tests/hygiene/) | Checks the format of tracked TODO annotations. | — | — |
| [`tests/infrastructure/`](../tests/infrastructure/) | Tests shared application services, numerical utilities and runtime behavior. | — | — |
| [`tests/infrastructure/common/`](../tests/infrastructure/common/) | Tests shared data and visualization helpers. | — | — |
| [`tests/infrastructure/contracts/`](../tests/infrastructure/contracts/) | Checks the API contracts of infrastructure components. | — | — |
| [`tests/infrastructure/core/`](../tests/infrastructure/core/) | Tests caching, configuration, database management and provenance tracking. | — | — |
| [`tests/infrastructure/factories/`](../tests/infrastructure/factories/) | Supplies shared factories for constructing simulation test data. | — | — |
| [`tests/infrastructure/installation/`](../tests/infrastructure/installation/) | Checks the structure of installation workflow results using a mocked engine. | — | — |
| [`tests/infrastructure/performance/`](../tests/infrastructure/performance/) | Runs benchmarks and load tests against recorded performance baselines. | — | — |
| [`tests/infrastructure/property/`](../tests/infrastructure/property/) | Checks mathematical and state-machine properties over generated input cases. | — | — |
| [`tests/infrastructure/security/`](../tests/infrastructure/security/) | Tests input validation and defenses against injection in shared infrastructure. | — | — |
| [`tests/infrastructure/unit/`](../tests/infrastructure/unit/) | Checks solver base classes and utilities for engineering calculations and data access. | — | — |
| [`tests/infrastructure/utils/`](../tests/infrastructure/utils/) | Tests shared helper functions and parallel benchmark utilities. | — | — |
| [`tests/infrastructure/validation/`](../tests/infrastructure/validation/) | Checks validation pipeline composition, severity filters and reports. | — | — |
| [`tests/installation/`](../tests/installation/) | Tests installation pamphlets and pipe installation screening calculations. | — | — |
| [`tests/integration/`](../tests/integration/) | Checks CAD geometry round trips through FreeCAD. | — | — |
| [`tests/legal/`](../tests/legal/) | Tests handling of private identifiers and paths in repository and publication tooling. | — | — |
| [`tests/lifting_lug/`](../tests/lifting_lug/) | Tests lifting-lug mechanics, assessment boundaries and report generation. | — | — |
| [`tests/maintenance/`](../tests/maintenance/) | Checks archive candidate selection, archive handling and issue update utilities. | — | — |
| [`tests/marine_ops/`](../tests/marine_ops/) | Tests marine installation, production, vessel and operational analysis workflows. | — | — |
| [`tests/marine_ops/artificial_lift/`](../tests/marine_ops/artificial_lift/) | Tests artificial lift diagnostics, rod-pump behavior and dynamometer card benchmarks. | — | — |
| [`tests/marine_ops/fowt_quasi_static/`](../tests/marine_ops/fowt_quasi_static/) | Tests geometry, mooring, dependencies and reporting in the quasi-static floating wind workflow. | — | — |
| [`tests/marine_ops/installation/`](../tests/marine_ops/installation/) | Tests marine lifting and installation decisions, vessel motions and load distribution. | — | — |
| [`tests/marine_ops/marine_analysis/`](../tests/marine_ops/marine_analysis/) | Checks marine analysis charts, vessel response quality and visualization imports. | — | — |
| [`tests/marine_ops/marine_engineering/`](../tests/marine_ops/marine_engineering/) | Tests marine engineering calculations including static catenary solutions. | — | — |
| [`tests/marine_ops/reservoir/`](../tests/marine_ops/reservoir/) | Tests the reservoir modeling interface used by marine operations. | — | — |
| [`tests/marine_ops/vessel_db/`](../tests/marine_ops/vessel_db/) | Checks vessel capability records, confidence and operational screening decisions. | — | — |
| [`tests/materials/`](../tests/materials/) | Tests material grades, pipe properties and fitness-for-service material lookups. | — | — |
| [`tests/metocean/`](../tests/metocean/) | Tests handling of network failures in live meteorological and oceanographic data checks. | — | — |
| [`tests/motion_forecast/`](../tests/motion_forecast/) | Tests vessel motion forecast conventions, consistency and decision criteria. | — | — |
| [`tests/motion_forecast/fixtures/`](../tests/motion_forecast/fixtures/) | Supplies a minimal OrcaFlex response table for motion forecast tests. | — | — |
| [`tests/naming_convention_validation/`](../tests/naming_convention_validation/) | Checks sea-state naming conventions used by the integrated fatigue processor. | — | — |
| [`tests/naval_architecture/`](../tests/naval_architecture/) | Tests hull geometry, vessel sizing, hydrostatics, stability and propulsion calculations. | — | — |
| [`tests/naval_architecture/fixtures/`](../tests/naval_architecture/fixtures/) | Supplies an alternative-fuel bulk carrier sizing case. | — | — |
| [`tests/nde/`](../tests/nde/) | Tests nondestructive examination calculations and pulse timing. | — | — |
| [`tests/orcaflex/`](../tests/orcaflex/) | Checks OrcaFlex domain workflows, model conventions and report generation. | — | — |
| [`tests/orcaflex/fixtures/`](../tests/orcaflex/fixtures/) | Supplies reference report sections and synthetic rope load data for OrcaFlex tests. | — | — |
| [`tests/orcaflex/reporting/`](../tests/orcaflex/reporting/) | Tests OrcaFlex report sections, shared section logic and rendered outputs. | — | — |
| [`tests/orcawave/`](../tests/orcawave/) | Checks OrcaWave model inputs, audit evidence and calculated hydrodynamic quantities. | — | — |
| [`tests/output/`](../tests/output/) | Holds tracked CLI output decks used by tests. | — | — |
| [`tests/output/test_cli_base/`](../tests/output/test_cli_base/) | Holds modular vessel, line and buoy model outputs for CLI generation cases. | — | — |
| [`tests/parametric/`](../tests/parametric/) | Tests the parametric model catalog, atlases and default model references. | — | — |
| [`tests/parametrics/`](../tests/parametrics/) | Tests parameter sweeps, pilot workflows and their connections to published atlases. | — | — |
| [`tests/performance/`](../tests/performance/) | Stores baseline measurements for performance comparisons. | — | — |
| [`tests/pipelay/`](../tests/pipelay/) | Tests pipelay screening inputs, evidence manifests and numerical reference cases. | — | — |
| [`tests/postprocess/`](../tests/postprocess/) | Checks the conversion of result files into multi-sheet workbooks. | — | — |
| [`tests/power/`](../tests/power/) | Tests electrical network analysis, protection, controls and commissioning records. | — | — |
| [`tests/power/analysis/`](../tests/power/analysis/) | Tests electrical load-flow calculations against reference cases. | — | — |
| [`tests/power/commissioning/`](../tests/power/commissioning/) | Checks commissioning test sequence generation, result validation and export. | — | — |
| [`tests/power/datacenter/`](../tests/power/datacenter/) | Tests data center power topology, model inputs and examples. | — | — |
| [`tests/power/microgrid/`](../tests/power/microgrid/) | Tests battery control, island detection and energy management for microgrids. | — | — |
| [`tests/power/protection/`](../tests/power/protection/) | Tests protective relay coordination. | — | — |
| [`tests/power/protocols/`](../tests/power/protocols/) | Tests IEC 61850 GOOSE messaging. | — | — |
| [`tests/process_diagrams/`](../tests/process_diagrams/) | Tests geometry, sheet layout and tagging in process diagrams. | — | — |
| [`tests/process_piping/`](../tests/process_piping/) | Tests pressure design, bolting and line sizing calculations. | — | — |
| [`tests/production_chemistry/`](../tests/production_chemistry/) | Tests mineral-scale saturation index calculations. | — | — |
| [`tests/production_engineering/`](../tests/production_engineering/) | Tests well inflow models, production inputs and reported reference values. | — | — |
| [`tests/production_engineering/fixtures/`](../tests/production_engineering/fixtures/) | Supplies published inflow performance reference values for well model checks. | — | — |
| [`tests/provider_data/`](../tests/provider_data/) | Tests equipment specification loading, companion file discovery and source provenance. | — | — |
| [`tests/rainflow_comparison_results/`](../tests/rainflow_comparison_results/) | Stores a plot and table comparing rainflow cycle-counting results. | — | — |
| [`tests/repo_structure/`](../tests/repo_structure/) | Checks repository layout, naming constraints and retirement of generated artifacts. | — | — |
| [`tests/report_evals/`](../tests/report_evals/) | Tests report evaluation contracts, comparison rules and run records. | — | — |
| [`tests/report_pack/`](../tests/report_pack/) | Tests the assembly of engineering workflow results into report packs. | — | — |
| [`tests/report_pack/golden/`](../tests/report_pack/golden/) | Stores the expected HTML results section for report pack comparisons. | — | — |
| [`tests/reporting/`](../tests/reporting/) | Tests report rendering, adapters and generated sections against reference documents. | — | — |
| [`tests/reporting/fixtures/`](../tests/reporting/fixtures/) | Supplies an independent vessel seakeeping calculation for report checks. | — | — |
| [`tests/reporting/golden/`](../tests/reporting/golden/) | Stores reference HTML reports for comparison with generated output. | — | — |
| [`tests/reservoir/`](../tests/reservoir/) | Tests stratigraphy and reservoir volume calculations. | — | — |
| [`tests/residual/`](../tests/residual/) | Tests numerical residual correction and error measurement. | — | — |
| [`tests/riser_bom/`](../tests/riser_bom/) | Tests riser bill-of-materials inputs, configuration adapters and cost calculations. | — | — |
| [`tests/riser_capability/`](../tests/riser_capability/) | Checks the riser database register, inventory and worked riser configurations. | — | — |
| [`tests/riser_configuration_screen/`](../tests/riser_configuration_screen/) | Tests the static riser configuration screening workflow. | — | — |
| [`tests/riser_database/`](../tests/riser_database/) | Checks riser records, engineering factors, code editions and citations. | — | — |
| [`tests/riser_fatigue/`](../tests/riser_fatigue/) | Tests riser fatigue screening, input handling, design factors and reporting. | — | — |
| [`tests/riser_results/`](../tests/riser_results/) | Tests solver-independent riser result exchange and native result handling. | — | — |
| [`tests/riser_study/`](../tests/riser_study/) | Compares riser wall sizing calculations with a fixed reference matrix. | — | — |
| [`tests/scripts/`](../tests/scripts/) | Tests repository utilities for data processing, maintenance and engineering automation. | — | — |
| [`tests/scripts/artificial_lift/`](../tests/scripts/artificial_lift/) | Checks extraction of artificial lift reference catalogs. | — | — |
| [`tests/scripts/fixtures/`](../tests/scripts/fixtures/) | Supplies coverage and collection records for testing repository scripts. | — | — |
| [`tests/scripts/hull_library/`](../tests/scripts/hull_library/) | Checks hull abstractions, CAD conversion probes and source manifests. | — | — |
| [`tests/scripts/tools/`](../tests/scripts/tools/) | Tests the script that builds LibreDWG. | — | — |
| [`tests/security/`](../tests/security/) | Checks regressions addressed by repository security remediation. | — | — |
| [`tests/seismic/`](../tests/seismic/) | Tests earthquake record readers, baseline corrections and seismic response calculations. | — | — |
| [`tests/seismic/fixtures/`](../tests/seismic/fixtures/) | Provides sample one- and two-column acceleration records for reader tests. | — | — |
| [`tests/signal_processing/`](../tests/signal_processing/) | Tests signal processing, spectral calculations and time-series workflows. | — | — |
| [`tests/signal_processing/signal_analysis/`](../tests/signal_processing/signal_analysis/) | Checks spectral calculations, inverse transforms and fatigue analysis against test signals. | — | — |
| [`tests/signal_processing/time_series/`](../tests/signal_processing/time_series/) | Checks windowing, frequency components and discrete transforms of sampled time series. | — | — |
| [`tests/solver/`](../tests/solver/) | Exercises end-to-end solver workflows, including licensed diffraction runs. | — | — |
| [`tests/solvers/`](../tests/solvers/) | Tests individual solver adapters and their shared benchmarking and execution tools. | — | — |
| [`tests/solvers/benchmark/`](../tests/solvers/benchmark/) | Tests solver benchmark case selection, execution, receipts and reporting. | — | — |
| [`tests/solvers/blender_automation/`](../tests/solvers/blender_automation/) | Tests Blender batch processing, CAD imports and geometry calculations. | — | — |
| [`tests/solvers/calculix/`](../tests/solvers/calculix/) | Tests CalculiX model generation, beam connectivity and native solution results. | — | — |
| [`tests/solvers/fea/`](../tests/solvers/fea/) | Supplies YAML test cases for structural integrity, catenaries, drilling, mooring and pipelay. | — | — |
| [`tests/solvers/ghs/`](../tests/solvers/ghs/) | Tests GHS result comparison, qualification boundaries and evidence requirements. | — | — |
| [`tests/solvers/gmsh_meshing/`](../tests/solvers/gmsh_meshing/) | Tests mesh generation and conversion for component and tank geometries with Gmsh. | — | — |
| [`tests/solvers/mystran/`](../tests/solvers/mystran/) | Tests MYSTRAN model inputs and native results against stored solver fixtures. | — | — |
| [`tests/solvers/openfoam/`](../tests/solvers/openfoam/) | Tests OpenFOAM case setup, wave damping and tracking of run artifacts. | — | — |
| [`tests/solvers/orcaflex/`](../tests/solvers/orcaflex/) | Tests OrcaFlex solver integration, mooring case preparation and analysis scripts. | — | — |
| [`tests/solvers/orcawave/`](../tests/solvers/orcawave/) | Checks OrcaWave diffraction setup, pack validation and solver startup. | — | — |
| [`tests/solvers/smoke/`](../tests/solvers/smoke/) | Exercises native solver launch and result readback with small test models. | — | — |
| [`tests/specialized/`](../tests/specialized/) | Tests specialist engineering tools, command-line workflows and supporting applications. | — | — |
| [`tests/specialized/cathodic_protection/`](../tests/specialized/cathodic_protection/) | Tests specialist cathodic protection calculations for ships and offshore structures. | — | — |
| [`tests/specialized/cli/`](../tests/specialized/cli/) | Checks command-line entry points for riser, diffraction, hydrodynamics and pipe design workflows. | — | — |
| [`tests/specialized/code_dnvrph103/`](../tests/specialized/code_dnvrph103/) | Checks DNV-RP-H103 workflows for circular and rectangular sections. | — | — |
| [`tests/specialized/custom/`](../tests/specialized/custom/) | Tests the custom pipe sizing implementation. | — | — |
| [`tests/specialized/digitalmarketing/`](../tests/specialized/digitalmarketing/) | Tests digital marketing and search engine analysis using sample configurations. | — | — |
| [`tests/specialized/gis/`](../tests/specialized/gis/) | Checks GIS coordinate transformations, map data formats and Blender integration. | — | — |
| [`tests/specialized/rigging/`](../tests/specialized/rigging/) | Tests rigging component lookup, fractional-inch dimensions and load statics. | — | — |
| [`tests/specialized/ship_design/`](../tests/specialized/ship_design/) | Checks ship design interfaces for hydrodynamic analysis and combined fatigue processing. | — | — |
| [`tests/specialized/umbilical_analysis/`](../tests/specialized/umbilical_analysis/) | Tests umbilical installation analysis and line property calculations. | — | — |
| [`tests/specs/`](../tests/specs/) | Checks engineering result and module manifest schemas. | — | — |
| [`tests/structural/`](../tests/structural/) | Tests structural stress, fatigue, buckling and component capacity calculations. | — | — |
| [`tests/structural/analysis/`](../tests/structural/analysis/) | Tests structural design calculations, including code-specific wall thickness and combined loading. | — | — |
| [`tests/structural/fatigue/`](../tests/structural/fatigue/) | Checks design curves, damage accumulation, migration and fatigue reporting. | — | — |
| [`tests/structural/fatigue_analysis/`](../tests/structural/fatigue_analysis/) | Tests fatigue analyses driven by stress-life curves, time histories and counted cycles. | — | — |
| [`tests/structural/fatigue_apps/`](../tests/structural/fatigue_apps/) | Checks load scaling and conversion of tension into stress for fatigue applications. | — | — |
| [`tests/structural/fe/`](../tests/structural/fe/) | Tests finite element workflows for elastic buckling and equivalent stress. | — | — |
| [`tests/structural/jacket_topside/`](../tests/structural/jacket_topside/) | Tests jacket and topside member screening, tubular geometry and load scaling. | — | — |
| [`tests/structural/offshore_resilience/`](../tests/structural/offshore_resilience/) | Tests offshore structural health, installation readiness and lifecycle planning. | — | — |
| [`tests/structural/ovality/`](../tests/structural/ovality/) | Checks the pipe ovality workflow. | — | — |
| [`tests/structural/parachute/`](../tests/structural/parachute/) | Tests parachute canopy loads, frame geometry and structural response. | — | — |
| [`tests/structural/pipe_capacity/`](../tests/structural/pipe_capacity/) | Checks pipe capacity calculations across design-code and custom methods. | — | — |
| [`tests/structural/pipe_cross_section/`](../tests/structural/pipe_cross_section/) | Tests pipe cross-section properties and buoyancy for layered pipes. | — | — |
| [`tests/structural/plate_capacity/`](../tests/structural/plate_capacity/) | Tests plate buckling and compressive capacity calculations. | — | — |
| [`tests/structural/stress/`](../tests/structural/stress/) | Checks equivalent stress, nonlinear stress–strain behavior and related workflows. | — | — |
| [`tests/structural/structural_analysis/`](../tests/structural/structural_analysis/) | Tests buckling, connections and strength of structural members, panels and grillages. | — | — |
| [`tests/subsea/`](../tests/subsea/) | Tests subsea equipment design, riser and pipeline response, mooring and intervention workflows. | — | — |
| [`tests/subsea/capping_stack/`](../tests/subsea/capping_stack/) | Tests subsea capping stack performance and containment flow calculations. | — | — |
| [`tests/subsea/catenary_riser/`](../tests/subsea/catenary_riser/) | Tests catenary riser geometry, equilibrium and workflow outputs. | — | — |
| [`tests/subsea/connectors/`](../tests/subsea/connectors/) | Tests subsea connector design, fatigue integration and OrcaFlex export. | — | — |
| [`tests/subsea/control_systems/`](../tests/subsea/control_systems/) | Tests subsea control system architecture. | — | — |
| [`tests/subsea/cross_sections/`](../tests/subsea/cross_sections/) | Checks layered cross-section geometry, stiffness, schemas and reports. | — | — |
| [`tests/subsea/field/`](../tests/subsea/field/) | Tests steady-flow handoffs between subsea production trees and field components. | — | — |
| [`tests/subsea/flexcom_tools/`](../tests/subsea/flexcom_tools/) | Tests creation of steel catenary riser models for Flexcom. | — | — |
| [`tests/subsea/flexible_pipes/`](../tests/subsea/flexible_pipes/) | Tests flexible pipe geometry, configuration comparisons and mechanical calculations. | — | — |
| [`tests/subsea/hipps/`](../tests/subsea/hipps/) | Tests high-integrity pressure protection hydraulics, overpressure, valve sizing and integrity calculations. | — | — |
| [`tests/subsea/jumper_tools/`](../tests/subsea/jumper_tools/) | Tests subsea jumper analysis tools and their workbook formulas. | — | — |
| [`tests/subsea/manifolds/`](../tests/subsea/manifolds/) | Tests subsea manifold design checks, flow aggregation and pressure balance. | — | — |
| [`tests/subsea/mooring_analysis/`](../tests/subsea/mooring_analysis/) | Tests mooring geometry, DYNMOOR calculations and command-line workflows. | — | — |
| [`tests/subsea/on_bottom_stability/`](../tests/subsea/on_bottom_stability/) | Checks seabed pipeline stability calculations and their reference table inputs. | — | — |
| [`tests/subsea/pipeline/`](../tests/subsea/pipeline/) | Tests subsea pipeline design and installation checks against applicable codes and examples. | — | — |
| [`tests/subsea/riser/`](../tests/subsea/riser/) | Checks clearance along riser trajectories. | — | — |
| [`tests/subsea/riser_study/`](../tests/subsea/riser_study/) | Tests riser study execution and fatigue result exchange. | — | — |
| [`tests/subsea/rov/`](../tests/subsea/rov/) | Checks remotely operated vehicle intervention sequencing, interfaces, tools and sea-state limits. | — | — |
| [`tests/subsea/structural_checks/`](../tests/subsea/structural_checks/) | Checks subsea structural calculations using API RP 2A. | — | — |
| [`tests/subsea/survey_qa/`](../tests/subsea/survey_qa/) | Tests survey patch-test corrections and timing estimates against synthetic swath geometry. | — | — |
| [`tests/subsea/trees/`](../tests/subsea/trees/) | Tests subsea production tree flow capacity, pressure control and design checks. | — | — |
| [`tests/subsea/umbilicals/`](../tests/subsea/umbilicals/) | Tests subsea umbilical sizing, lay analysis and chemical delivery flow. | — | — |
| [`tests/subsea/vertical_riser/`](../tests/subsea/vertical_riser/) | Checks component units and suspended tension in vertical risers. | — | — |
| [`tests/subsea/viv_analysis/`](../tests/subsea/viv_analysis/) | Tests vortex-induced vibration calculations, frequency limits and evidence checks. | — | — |
| [`tests/subsea/viv_tools/`](../tests/subsea/viv_tools/) | Tests mode identification for SHEAR7 vibration analysis. | — | — |
| [`tests/technical_safety/`](../tests/technical_safety/) | Tests accidental load calculations, fire heat flux and associated screening criteria. | — | — |
| [`tests/test_automation/`](../tests/test_automation/) | Implements utilities for discovering, analyzing, repairing and reporting on tests. | — | — |
| [`tests/test_automation/cli/`](../tests/test_automation/cli/) | Provides command-line access to the test automation tools. | — | — |
| [`tests/test_automation/core/`](../tests/test_automation/core/) | Implements test discovery, analysis, comparison and automated repairs. | — | — |
| [`tests/test_automation/coverage/`](../tests/test_automation/coverage/) | Tracks coverage for the test automation framework. | — | — |
| [`tests/test_automation/fixtures/`](../tests/test_automation/fixtures/) | Supplies engineering fixtures for automated test generation and execution. | — | — |
| [`tests/test_automation/integration/`](../tests/test_automation/integration/) | Connects test automation to continuous integration jobs. | — | — |
| [`tests/test_automation/reporting/`](../tests/test_automation/reporting/) | Generates reports from test automation results. | — | — |
| [`tests/test_automation/scripts/`](../tests/test_automation/scripts/) | Runs batch test repairs and coordinates repair campaigns. | — | — |
| [`tests/test_configs/`](../tests/test_configs/) | Supplies a base YAML configuration for tests. | — | — |
| [`tests/test_data/`](../tests/test_data/) | Supplies a sample time series in CSV format. | — | — |
| [`tests/test_failure_recovery_temp/`](../tests/test_failure_recovery_temp/) | Preserves malformed input and recorded outputs from failed-run recovery exercises. | — | — |
| [`tests/test_failure_recovery_temp/output/`](../tests/test_failure_recovery_temp/output/) | Stores failed-run lists and rerun configurations from recovery tests. | — | — |
| [`tests/test_wall_thickness_codes/`](../tests/test_wall_thickness_codes/) | Checks pipe wall thickness and collapse calculations across design codes. | — | — |
| [`tests/tug/`](../tests/tug/) | Tests tug performance calculations, fender geometry and towline inputs. | — | — |
| [`tests/uncertainty/`](../tests/uncertainty/) | Tests uncertainty sampling, experimental design and propagation through model evaluations. | — | — |
| [`tests/unit/`](../tests/unit/) | Collects focused unit checks for hull geometry, hydrodynamics, marine operations and structural calculations. | — | — |
| [`tests/unit/hull_library/`](../tests/unit/hull_library/) | Checks hull catalog coverage and parametric geometry. | — | — |
| [`tests/unit/hydrodynamics/`](../tests/unit/hydrodynamics/) | Tests wave spectra, added resistance and related hydrodynamic calculations in isolation. | — | — |
| [`tests/unit/marine_ops/`](../tests/unit/marine_ops/) | Tests mooring resilience screening independently of other marine workflows. | — | — |
| [`tests/unit/structural/`](../tests/unit/structural/) | Checks offshore container, sling geometry and structural calculations. | — | — |
| [`tests/validation/`](../tests/validation/) | Checks riser configuration semantics, length and monitoring bindings. | — | — |
| [`tests/verification/`](../tests/verification/) | Checks the verification script for OrcaFlex campaigns. | — | — |
| [`tests/visualization/`](../tests/visualization/) | Tests CAD design tables, vessel geometry and visualization tools. | — | — |
| [`tests/visualization/design_tools/`](../tests/visualization/design_tools/) | Tests design tables, CAD geometry, finite element integration and hull hydrostatics. | — | — |
| [`tests/web/`](../tests/web/) | Tests authentication examples, web application routes and engineering result handling. | — | — |
| [`tests/well/`](../tests/well/) | Tests well barriers, drilling, intervention, completion and tubular design. | — | — |
| [`tests/well/barriers/`](../tests/well/barriers/) | Checks well barrier leak paths, permanent barriers and worked probability examples. | — | — |
| [`tests/well/completion/`](../tests/well/completion/) | Tests pump-down calculations and transient responses for well completion. | — | — |
| [`tests/well/drilling/`](../tests/well/drilling/) | Tests drilling hydraulics, cementing, dysfunction detection and penetration rate models. | — | — |
| [`tests/well/intervention/`](../tests/well/intervention/) | Tests well intervention screening. | — | — |
| [`tests/well/tubulars/`](../tests/well/tubulars/) | Tests casing design, tubular stress, capacity and design envelopes. | — | — |
| [`tests/well_access/`](../tests/well_access/) | Tests well-access screening and calibration data handling. | — | — |
| [`tests/workflow_api/`](../tests/workflow_api/) | Checks the deterministic workflow API against expected results and provenance contracts. | — | — |
| [`tests/workflow_api/goldens/`](../tests/workflow_api/goldens/) | Supplies expected JSON results for deterministic engineering workflow comparisons. | — | — |
| [`tests/workflows/`](../tests/workflows/) | Tests workflow automation, model generation and solver integration. | — | — |
| [`tests/workflows/agents/`](../tests/workflows/agents/) | Checks the OrcaFlex agent template library. | — | — |
| [`tests/workflows/automation/`](../tests/workflows/automation/) | Tests scanning, analysis and minimization of reference folders used for workflow automation. | — | — |
| [`tests/workflows/integration/`](../tests/workflows/integration/) | Checks interactions between workflow components and end-to-end execution. | — | — |
| [`tests/workflows/orcawave/`](../tests/workflows/orcawave/) | Tests OrcaWave automation connections and end-to-end workflow integration. | — | — |
| [`tests/workflows/standalone/`](../tests/workflows/standalone/) | Tests the standalone document-to-Markdown converter. | — | — |
| [`tests/workflows/workflow_automation/`](../tests/workflows/workflow_automation/) | Checks workflow callbacks, caching, raw data access and continuous integration behavior. | — | — |
