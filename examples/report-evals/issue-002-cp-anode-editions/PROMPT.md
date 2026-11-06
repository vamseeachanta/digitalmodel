# Run it yourself: anodes for six offshore structures on each DNV edition

Article: https://www.aceengineer.com/newsletter/issue-002-energy-street/

For decision makers: this prompt lets your team, or your AI assistant, repeat the study with public code and public data. If you would rather have it done for you, use the contact link on the article page.

For practitioners: paste the block below into an AI assistant that can run Python and use the internet. It needs about 1 GB of disk and 10 minutes. The key results it must reach are in `results.json` next to this file. `eval.py` grades them.

## Prompt (copy all of it)

```text
Task: reproduce the anode counts of the article "Sacrificial anodes needed for six
offshore structures on each edition of DNV-RP-B401 and DNV-RP-F103"
(https://www.aceengineer.com/newsletter/issue-002-energy-street/), then count the
platform structures still standing in today's BSEE file.

Inputs (all public, no account needed):
- Code: https://github.com/vamseeachanta/digitalmodel at commit
  09089a121a16069c4b657d1a320a630a23ab1dd5, MIT licence.
- The six structure input files in tests/fixtures/cathodic_protection/workflow_inputs:
  pipeline, multi_component_riser, published_jacket_84m,
  reference_jacket_60m_members, monopile, manifold.
- Figure 4 data, as published:
  https://www.aceengineer.com/assets/data/newsletter/issue-002/energy-fig4-platforms-standing.csv
- Today's BSEE Platform Structures raw data (US federal government data, updated daily):
  https://www.data.bsee.gov/Platform/Files/PlatStrucRawData.zip

Steps:
1. Use Python 3.11. Python 3.13 and later cannot build some pinned packages.
   git clone --filter=blob:none https://github.com/vamseeachanta/digitalmodel
   cd digitalmodel && git checkout 09089a121a16069c4b657d1a320a630a23ab1dd5
   uv venv --python 3.11 && uv pip install --no-sources -e .
   (--no-sources makes uv take assetutilities from PyPI, not from a sibling folder.)
2. For each structure file, set inputs.design_data.edition to each edition the code
   accepts: 2010 and 2019 for the pipeline (DNV-RP-F103); 2005, 2010, 2017 and 2021
   for the other five (DNV-RP-B401). Keep every other input as in the file. Run
   digitalmodel.cathodic_protection.engine_adapter.run_cathodic_protection(config).
3. Read the anodes required:
   - pipeline: results.anode_requirements.anode_count and total_anode_mass_kg;
   - the others: results.current_output_verification.recommended_anode_count, or for
     the riser the sum of results.anode_families.<family>.recommended_anode_count.
   Ignore results.status.result: it judges the installed count in the input file,
   not the recommended count.
4. Sweep the pipeline on both F103 editions: design life 10, 15, 20, 25, 30, 35 and
   40 years at 60 C (inputs.design_data.design_life), and fluid temperature 5, 20, 30,
   40, 50, 60, 70 and 80 C at 30 years
   (inputs.pipeline.internal_fluid_temperature_C). Record the count, the mass and
   results.current_densities_A_m2.mean_current_density_A_m2.
5. Count the published Figure 4 CSV: rows (structures still standing), rows of type
   FIXED, rows installed before 1993 and before 2005, and the median year.
6. Count today's BSEE file the same way, from
   PlatStrucRawData/mv_platstruc_structures.txt: a structure with no REMOVAL_DATE is
   still standing; the year is the year of INSTALL_DATE; a fixed jacket has
   STRUC_TYPE_CODE FIXED.

Expected key results (from the article):
- Pipeline: 9 bracelets on F103 2010, 34 on 2019 (+277.8 %).
- Hybrid riser: 26 on B401 2005, 2010 and 2017; 25 on 2021.
- 84.1 m jacket 575, 60 m wind jacket 424, monopile 48, manifold 64, on all four.
- Figure 4 CSV: 1,301 standing, 980 fixed jackets, 849 before 1993, 1,124 before
  2005, median year 1983.
- Today's BSEE file gives a few fewer standing structures, because the file changes
  daily (6 November 2026: 1,294). Report the difference; it is not an error.

Ask me before you go on if:
- you cannot install Python 3.11: say so, and do not substitute another version;
- you want to adapt the study to my own structure: ask me for its geometry, coating,
  design life, fluid temperature, anode type and alloy, and say which of these
  changes the count most.

You cannot:
- check the library's edition values against the DNV documents, which are paid;
- rebuild the exact counted BSEE copy of 29 October 2026, which is not archived;
- give a CP design for a real structure: each structure is one modelled case.

Output: a table of your counts against the expected values, and a file
my-results.json in this form, with the result ids of
https://github.com/vamseeachanta/digitalmodel/blob/main/examples/report-evals/issue-002-cp-anode-editions/results.json
{"results": [{"id": "pipeline_f103_2010_anodes", "value": 9}, ...]}
Then grade it. The eval files are on the main branch, so bring them into the clone:
git checkout origin/main -- examples/report-evals
python examples/report-evals/issue-002-cp-anode-editions/eval.py --compare my-results.json
```

## Check your answer

From the clone, after `git checkout origin/main -- examples/report-evals`:

```text
python examples/report-evals/issue-002-cp-anode-editions/eval.py --compare my-results.json
python examples/report-evals/issue-002-cp-anode-editions/eval.py
```

The first line grades your own results. The second rebuilds every key result itself.
`PASS` means every key result is within its tolerance. `PARTIAL` means a result is missing or the live BSEE count has moved outside its 1 % band. `FAIL` means a result differs.

## Adapt it to your project

Copy `pipeline.yml`, change the outer diameter, length, coating, design life and fluid temperature, and run the same edition loop. The checks of step 3 still apply. Where your count differs between editions, the coating breakdown factor and the mean current density are the first two values to compare (article, Figure 2).
