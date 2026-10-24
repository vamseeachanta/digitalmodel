# Asset integrity report validation

The [generated report](report.html) is reproduced from the retained public
example and [report template](report.template.html). The validation counts below
record the test selections separately so the report tests are not counted twice.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-24 | Record current validation counts and reproduction requirements to replace outdated figures. |

## Validation counts

| Selection | Test files, relative to the repository root | Result |
| --- | --- | --- |
| Report | `tests/asset_integrity/test_module_report.py` | 40 passed |
| Focused | Report selection plus `tests/asset_integrity/test_ffs_published_examples.py` and `tests/asset_integrity/test_corroded_pipe.py` | 73 passed, including the 40 report tests |
| Generated pages | `tests/capabilities/test_generated_html_freshness.py` | 52 passed separately |

These counts were reproduced on 2026-10-24. The generated-page selection rebuilds
all registered pages in an isolated temporary tree and compares the outputs with
the committed pages.

## Reproduction requirements

Use the complete isolated test command in [report section 9.2](report.html#section-9-2)
from the repository root. It sets `PYTHONPATH=src` and uses `uv run --no-project`
with the following dependencies; a neighboring package checkout is not required:

- `numpy>=1.24,<2`, `jinja2>=3.1,<4`, `pytest>=7.4.3,<9` and `pytest-asyncio==0.21.1`.
- `beautifulsoup4>=4.13,<5`, `pandas>=2,<3`, `pyyaml>=6,<7` and `scipy>=1.10,<2`.
- `pint>=0.25.3,<1`, `plotly==5.17.0` and `pydantic>=2.7,<3`.

Node.js 18 or later must be available as `node`; the recorded runs used
Node.js 18.20.8. The JavaScript tests execute the delivered review controls with
controlled DOM and file APIs. Browser rendering and folder permission prompts
are outside this evidence.

For the focused selection, supply all three test-file paths in the table to the
same isolated command. For the generated-page selection, replace the test-file
path and also add `--with matplotlib --with seaborn --with openpyxl --with loguru
--with 'pyarrow==14.0.1'` before `python`. The PyArrow pin retains the compatible
version used for this validation; compatibility with newer versions is not
established here.

The recorded pytest runs added `-q -rA -p no:cacheprovider` and a unique
`--basetemp` directory outside the checkout. Set `PYTHONDONTWRITEBYTECODE=1` and
keep temporary output and caches outside the checkout when reproducing them.

The replacement validation sentence is:

> 40 report tests and 73 focused tests pass using the documented isolated dependencies and Node.js 18; browser rendering and folder permissions are outside this evidence. The complete generated-page selection separately passes 52 tests.
