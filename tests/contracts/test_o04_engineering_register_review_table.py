"""Review-table contracts for source masking and retained prose findings."""

import importlib.util
import re
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "review_checker", ROOT / "scripts/enforcement/check-engineering-register.py"
)
CHECKER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CHECKER)

CORRECTED = {
    "D10",
    "D19",
    "D23",
    "G08",
    "G09",
    "G10",
    "G11",
    "G12",
    "G23",
    "G24",
    "G26",
    "G27",
    "B23",
}
VISIBLE_LINES = {"M01": {2, 4, 6}, "M02": {1, 4}, "M03": {5}, "M04": {3}}
# The raw review table retains input pipes, leading spaces, and the literal tab.
RAW_TABLE = r"""| id | exp | input | main | prev | new | delta |
|---|---|---|---|---|---|---|
| E01 | P | |https://example.org/a|conservative| | 1:R5(conservative) | - | 1:R5(conservative) | same |
| E02 | P | www.example.org/a|safe| | 1:R5(safe) | - | 1:R5(safe) | same |
| E03 | P | **https://example.org/a**conservative | 1:R5(conservative) | - | 1:R5(conservative) | same |
| E04 | P | (https://example.org/a)conservative | 1:R5(conservative) | - | 1:R5(conservative) | same |
| E05 | P | https://example.org/a,conservative | 1:R5(conservative) | - | 1:R5(conservative) | same |
| E06 | P | [source](design.md "our conservative note") | 1:R1(our), 1:R5(conservative) | - | 1:R1(our), 1:R5(conservative) | same |
| E07 | P | __https://example.org/a__conservative | - | - | - | same |
| E08 | P | *https://example.org/a*conservative | 1:R5(conservative) | - | 1:R5(conservative) | same |
| E09 | P | _https://example.org/a_conservative | - | - | - | same |
| E10 | P | **www.example.org/a**safe | 1:R5(safe) | - | 1:R5(safe) | same |
| E11 | P | (www.example.org/a)safe | 1:R5(safe) | - | 1:R5(safe) | same |
| E12 | P | www.example.org/a,safe | 1:R5(safe) | - | 1:R5(safe) | same |
| P01 | P | See https://example.org/a. Conservative result. | 1:R5(Conservative) | 1:R5(Conservative) | 1:R5(Conservative) | same |
| P02 | P | See https://example.org/a, conservative result. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| P03 | P | See https://example.org/a; conservative result. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| P04 | P | See https://example.org/a: conservative result. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| P05 | P | See https://example.org/a! Conservative result. | 1:R5(Conservative) | 1:R5(Conservative) | 1:R5(Conservative) | same |
| P06 | P | See https://example.org/a? Conservative result. | 1:R5(Conservative) | 1:R5(Conservative) | 1:R5(Conservative) | same |
| P07 | P | (see https://example.org/a) conservative result. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| P08 | P | See https://example.org/a conservative result. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| P09 | P | See https://example.org/a. Our note follows. | 1:R1(Our) | 1:R1(Our) | 1:R1(Our) | same |
| G01 | A | https://example.org/a.conservative | 1:R5(conservative) | - | - | DECREASE |
| G02 | P | https://example.org/a,conservative text | 1:R5(conservative) | - | 1:R5(conservative) | same |
| G03 | P | https://example.org/a;conservative | 1:R5(conservative) | - | 1:R5(conservative) | same |
| G04 | A | https://example.org/a:conservative | 1:R5(conservative) | - | - | DECREASE |
| G05 | A | https://example.org/a!conservative | 1:R5(conservative) | - | - | DECREASE |
| G06 | A | https://example.org/a?conservative | 1:R5(conservative) | - | - | DECREASE |
| G07 | P | https://example.org/a)conservative | 1:R5(conservative) | - | 1:R5(conservative) | same |
| G08 | P | https://example.org/a—conservative | 1:R5(conservative) | - | - | DECREASE |
| G09 | P | https://example.org/a–conservative | 1:R5(conservative) | - | - | DECREASE |
| G10 | P | https://example.org/a…conservative | 1:R5(conservative) | - | - | DECREASE |
| G11 | P | ~~https://example.org/a~~conservative | 1:R5(conservative) | - | - | DECREASE |
| G12 | P | https://example.org/a}conservative | 1:R5(conservative) | - | - | DECREASE |
| G13 | P | "https://example.org/a"conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| G14 | P | 'https://example.org/a'conservative | 1:R5(conservative) | - | 1:R5(conservative) | same |
| G15 | P | https://example.org/a]conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| G16 | P | https://example.org/a&nbsp;conservative | 1:R5(conservative) | - | 1:R5(conservative) | same |
| G17 | P | https://example.org/a<br>our note | 1:R1(our) | 1:R1(our) | 1:R1(our) | same |
| G18 | A | https://example.org/a*conservative* | 1:R5(conservative) | - | - | DECREASE |
| G19 | A | https://example.org/a_conservative_ | - | - | - | same |
| G20 | P | https://example.org/a**conservative** | 1:R5(conservative) | - | 1:R5(conservative) | same |
| G21 | P | https://example.org/a.**conservative** | 1:R5(conservative) | - | 1:R5(conservative) | same |
| G22 | P | https://example.org/a conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| G23 | P | https://example.org/a、conservative | 1:R5(conservative) | - | - | DECREASE |
| G24 | P | https://example.org/a”conservative | 1:R5(conservative) | - | - | DECREASE |
| G25 | P | “https://example.org/a” conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| G26 | P | https://example.org/a\conservative | 1:R5(conservative) | - | - | DECREASE |
| G27 | P | https://example.org/a{conservative} | 1:R5(conservative) | - | - | DECREASE |
| G28 | P | https://example.org/a=conservative | 1:R5(conservative) | - | - | DECREASE |
| G29 | P | https://example.org/a/conservative | 1:R5(conservative) | - | - | DECREASE |
| A01 | P | <https://example.org/a> our note | 1:R1(our) | 1:R1(our) | 1:R1(our) | same |
| A02 | P | <https://example.org/a>our note | 1:R1(our) | 1:R1(our) | 1:R1(our) | same |
| A03 | P | <https://example.org/a> conservative | - | - | - | same |
| A04 | P | our note <https://example.org/a> | 1:R1(our) | 1:R1(our) | 1:R1(our) | same |
| A05 | P | <www.example.org/a>our note | 1:R1(our) | 1:R1(our) | 1:R1(our) | same |
| L01 | P | [conservative source](design.md) | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L02 | P | [source](design.md "conservative note") | 1:R5(conservative) | - | 1:R5(conservative) | same |
| L03 | P | [source](design.md 'conservative note') | 1:R5(conservative) | - | 1:R5(conservative) | same |
| L04 | P | [source](design.md (conservative note)) | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L05 | P | [source](design.md) conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L06 | P | [source](design.md)conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L07 | S | [source](conservative-design.md) | 1:R5(conservative) | - | - | DECREASE |
| L08 | S | [source](https://example.org/conservative-design) | 1:R5(conservative) | - | - | DECREASE |
| L09 | P | [source](https://example.org/a "conservative note") | 1:R5(conservative) | - | 1:R5(conservative) | same |
| L10 | P | [source](https://example.org/a (conservative note)) | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L11 | S | [source](<our note.md>) | 1:R1(our) | - | - | DECREASE |
| L12 | P | [source](<design.md> "our note") | 1:R1(our) | - | 1:R1(our) | same |
| L13 | P | [our source](design.md "note") | 1:R1(our) | 1:R1(our) | 1:R1(our) | same |
| L14 | S | [source](conservative) | 1:R5(conservative) | - | - | DECREASE |
| L15 | P | [source]( design.md "conservative note" ) | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L16 | P | [a](x.md) conservative [b](y.md) | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L17 | P | [a](x.md "t") conservative [b](y.md "t") | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L18 | P | [source](design.md "note") conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L19 | P | [source](design.md "a) conservative") | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L20 | P | [source](design.md "note" conservative) | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L21 | P | [source](design.md conservative) | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L22 | S | [source](design.md"conservative") | 1:R5(conservative) | - | - | DECREASE |
| L23 | P | [source](a(b)c) conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L24 | P | [source](a\) conservative) | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L25 | P | [x] (conservative) | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L26 | P | \[source](conservative) | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L27 | P | [source](design.md)**conservative** | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L28 | P | [source](design.md "conservative") | 1:R5(conservative) | - | 1:R5(conservative) | same |
| L29 | P | [source](design.md 'our note') | 1:R1(our) | - | 1:R1(our) | same |
| L30 | P | [source](design.md (our note)) | 1:R1(our) | 1:R1(our) | 1:R1(our) | same |
| L31 | P | [source](https://example.org/a "our note") text | 1:R1(our) | - | 1:R1(our) | same |
| L32 | S | [source](https://example.org/conservative-design_(draft)) | 1:R5(conservative) | - | - | DECREASE |
| L33 | P | [source](https://example.org/a_(draft))conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L34 | P | [source](https://example.org/a),conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L35 | P | [source][id] conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L36 | P | [source](conservative-design.md conservative) | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L37 | P | [a [b] c](design.md) conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L38 | P | f[i](conservative estimate) | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| L39 | P | [source](design.md	"conservative note") | 1:R5(conservative) | - | 1:R5(conservative) | same |
| L40 | P | [source](design.md “conservative note”) | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| I01 | P | ![conservative diagram](fig.png) | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| I02 | P | ![diagram](fig.png "conservative view") | 1:R5(conservative) | - | 1:R5(conservative) | same |
| I03 | P | ![diagram](fig.png 'conservative view') | 1:R5(conservative) | - | 1:R5(conservative) | same |
| I04 | P | ![diagram](fig.png (conservative view)) | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| I05 | S | ![diagram](conservative-fig.png) | 1:R5(conservative) | - | - | DECREASE |
| I06 | P | ![diagram](fig.png) conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| I07 | P | ![our diagram](fig.png "view") | 1:R1(our) | 1:R1(our) | 1:R1(our) | same |
| I08 | P | ![diagram](fig.png "our view") | 1:R1(our) | - | 1:R1(our) | same |
| I09 | S | ![diagram](https://example.org/safe-fig.png) | 1:R5(safe) | - | - | DECREASE |
| I10 | P | ![diagram](fig.png)conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| D01 | S | [id]: https://example.org/conservative-design | 1:R5(conservative) | - | - | DECREASE |
| D02 | S | [id]: docs/conservative-design.md | 1:R5(conservative) | 1:R5(conservative) | - | DECREASE |
| D03 | P | [id]: https://example.org/a "conservative note" | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| D04 | P | [id]: docs/a.md 'conservative note' | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| D05 | P | [id]: docs/a.md (conservative note) | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| D06 | P | [id]: docs/a.md "our note" | 1:R1(our) | 1:R1(our) | 1:R1(our) | same |
| D07 | S | [id]: <docs/our note.md> | 1:R1(our) | 1:R1(our) | - | DECREASE |
| D08 | P | [id]: docs/a.md conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| D09 | S | [id]: conservative | 1:R5(conservative) | 1:R5(conservative) | - | DECREASE |
| D10 | P | [^1]: Conservative. | 1:R5(Conservative) | 1:R5(Conservative) | - | DECREASE |
| D11 | P | [^1]: Conservative result. | 1:R5(Conservative) | 1:R5(Conservative) | 1:R5(Conservative) | same |
| D12 | P | [id]: docs/a.md "note" conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| D13 | S | [id]:conservative-design.md | 1:R5(conservative) | 1:R5(conservative) | - | DECREASE |
| D14 | P | Text [id]: conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| D15 | P | [id]: https://example.org/a "our note" | 1:R1(our) | 1:R1(our) | 1:R1(our) | same |
| D16 | P | [id]: https://example.org/a (our note) | 1:R1(our) | 1:R1(our) | 1:R1(our) | same |
| D17 | P | [id]: https://example.org/a 'our note' | 1:R1(our) | 1:R1(our) | 1:R1(our) | same |
| D18 | S |    [id]: docs/safe-design.md | 1:R5(safe) | 1:R5(safe) | - | DECREASE |
| D19 | P | [^note]: Safe. | 1:R5(Safe) | 1:R5(Safe) | - | DECREASE |
| D20 | P | [id]: https://example.org/a conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| D21 | P | [id]: docs/conservative-a.md "conservative note" | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| D22 | P | [Note]: Conservative. | 1:R5(Conservative) | 1:R5(Conservative) | - | DECREASE |
| D23 | P | [^1]: conservative | 1:R5(conservative) | 1:R5(conservative) | - | DECREASE |
| D24 | S | [id]: docs/a_(conservative).md | 1:R5(conservative) | 1:R5(conservative) | - | DECREASE |
| D25 | P | [id]: docs/a_(b conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| B01 | S | Text `conservative` end. | - | - | - | same |
| B02 | P | Text `a`conservative end. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| B03 | P | Text `a` conservative end. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| B04 | S | Text ``conservative`` end. | 1:R5(conservative) | - | - | DECREASE |
| B05 | P | Text ``a``conservative end. | - | 1:R5(conservative) | 1:R5(conservative) | increase |
| B06 | P | Text ``a`` conservative end. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| B07 | S | Text ```conservative``` end. | - | - | - | same |
| B08 | P | Text ```a```conservative end. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| B09 | P | Text ```a``` conservative end. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| B10 | S | Text ````conservative```` end. | 1:R5(conservative) | - | - | DECREASE |
| B11 | P | Text ````a````conservative end. | - | 1:R5(conservative) | 1:R5(conservative) | increase |
| B12 | P | Text ` conservative prose. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| B13 | P | Text `` conservative prose. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| B14 | P | Text `` conservative ` end. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| B15 | P | Text ``` conservative `` end. | - | - | - | same |
| B16 | P | Text `a` conservative ` end. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| B17 | P | Text ``a`` conservative `` end. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| B18 | S | Text `` a ` conservative ` b `` end. | - | - | - | same |
| B19 | P | Text ``a`` conservative ``b`` end. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| B20 | P | Text `a` conservative `b` end. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| B21 | P | Text ```a``` conservative ```b``` end. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| B22 | P | Text \`\` conservative \`\` end. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| B23 | P | Text \`` conservative \`` end. | 1:R5(conservative) | - | - | DECREASE |
| B24 | S | Text ` `` conservative `` ` end. | - | - | - | same |
| B25 | P | Text `` conservative ``` end. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| B26 | P | Text ``a`` our note ``b`` end. | 1:R1(our) | 1:R1(our) | 1:R1(our) | same |
| B27 | S | Text ``our note`` end. | 1:R1(our) | - | - | DECREASE |
| B28 | P | Text ``a``our note. | - | 1:R1(our) | 1:R1(our) | increase |
| B29 | P | Text `a`our note. | 1:R1(our) | 1:R1(our) | 1:R1(our) | same |
| B30 | P | Text ```a```our note. | 1:R1(our) | 1:R1(our) | 1:R1(our) | same |
| B31 | S | Text `our note` end. | - | - | - | same |
| B32 | S | Text ```our note``` end. | - | - | - | same |
| B33 | P | Text ` conservative `` a `` end. | - | 1:R5(conservative) | 1:R5(conservative) | increase |
| B34 | P | Text ``a`` conservative `b` conservative ``c``. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| B35 | P | conservative ``a`` | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| B36 | S | Text `` conservative `` end. | 1:R5(conservative) | - | - | DECREASE |
| B37 | P | Text `` `` conservative `` `` end. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| B38 | P | Text ``a` conservative `b`` end. | - | - | - | same |
| B39 | P | Text `a`` conservative ``b` end. | - | - | - | same |
| B40 | P | Text `` conservative end ```` more. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| B41 | P | Text ``a`` conservative ` b. | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| B42 | P | Text \`conservative\` end. | - | - | - | same |
| T01 | P | https://example.org/a conservative https://example.org/b | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| T02 | P | https://example.org/a|conservative|https://example.org/b | 1:R5(conservative) | - | 1:R5(conservative) | same |
| T03 | P | https://example.org/a,conservative,https://example.org/b | 1:R5(conservative) | - | 1:R5(conservative) | same |
| T04 | P | <https://example.org/a>our note<https://example.org/b> | 1:R1(our) | 1:R1(our) | 1:R1(our) | same |
| T05 | P | https://example.org/a our note www.example.org/b | 1:R1(our) | 1:R1(our) | 1:R1(our) | same |
| T06 | S | https://example.org/safe-a text www.example.org/conservative-b | 1:R5(safe) | - | - | DECREASE |
| T07 | P | (https://example.org/a)conservative(https://example.org/b) | 1:R5(conservative) | - | 1:R5(conservative) | same |
| R01 | P | https://example.org/a_(b) conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| R02 | A | https://example.org/a_(b)conservative | 1:R5(conservative) | - | - | DECREASE |
| R03 | P | (https://example.org/a_(b))conservative | 1:R5(conservative) | - | 1:R5(conservative) | same |
| R04 | P | https://example.org/a_(b conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| R05 | A | https://example.org/a_(bconservative | - | - | - | same |
| R06 | P | (see https://example.org/a_(b)) conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| R07 | S | https://example.org/a_(conservative) | 1:R5(conservative) | - | - | DECREASE |
| R08 | P | https://example.org/a_(b)) conservative | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| R09 | P | https://example.org/a_(b)),conservative | 1:R5(conservative) | - | 1:R5(conservative) | same |
| R10 | P | (https://example.org/a_(b)).conservative | 1:R5(conservative) | - | 1:R5(conservative) | same |
| W01 | P | | a | conservative | | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| W02 | P | |a|conservative| | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| W03 | P | | https://example.org/a | conservative | | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| W04 | P | |[s](d.md)|conservative| | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| W05 | P | | [s](d.md "conservative") | b | | 1:R5(conservative) | - | 1:R5(conservative) | same |
| W06 | P | |``a``|conservative| | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| W07 | S | |www.example.org/conservative|a| | 1:R5(conservative) | - | - | DECREASE |
| W08 | S | | https://example.org/conservative-x | b | | 1:R5(conservative) | - | - | DECREASE |
| W09 | P | |`a`|conservative| | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| W10 | P | | www.example.org/a | safe | | 1:R5(safe) | 1:R5(safe) | 1:R5(safe) | same |
| W11 | P | |our note|https://example.org/a| | 1:R1(our) | 1:R1(our) | 1:R1(our) | same |
| W12 | P | |https://example.org/a|our note| | 1:R1(our) | - | 1:R1(our) | same |
| W13 | S | | [s](conservative.md) | b | | 1:R5(conservative) | - | - | DECREASE |
| W14 | S | |``conservative``|b| | 1:R5(conservative) | - | - | DECREASE |
| O01 | P | The coating shall be ``recommended`` here. | 1:R4 | 1:R4 | 1:R4 | same |
| O02 | P | The thickness is ``12.5 mm`` here. | 1:R7(12.5 mm) | 1:R7(12.5 mm) | 1:R7(12.5 mm) | same |
| O03 | P | The thickness is 12.5 mm at https://example.org/a. | 1:R7(12.5 mm) | 1:R7(12.5 mm) | 1:R7(12.5 mm) | same |
| O04 | P | The coating shall follow https://example.org/recommended-practice. | - | - | - | same |
| O05 | P | [The coating shall be](x.md "recommended") | - | - | - | same |
| O06 | P | The thickness [is](https://example.org/12.5mm) fine. | 1:R7(12.5mm) | 1:R7(12.5mm) | 1:R7(12.5mm) | same |
| O07 | P | Results are conservative https://example.org/a | 1:R5(conservative) | 1:R5(conservative) | 1:R5(conservative) | same |
| O08 | S | https://example.org/conservative-a Results are Conservative. | 1:R5(conservative) | 1:R5(Conservative) | 1:R5(Conservative) | changed |
| M01 | P | https://example.org/conservative-a ⏎ Results are conservative. ⏎ [s](safe.md) ⏎ Our note follows. ⏎ ``our note`` ⏎ The thickness is 12.5 mm. | 1:R5(conservative), 2:R5(conservative), 3:R5(safe), 4:R1(Our), 5:R1(our), 6:R7(12.5 mm) | 2:R5(conservative), 4:R1(Our), 6:R7(12.5 mm) | 2:R5(conservative), 4:R1(Our), 6:R7(12.5 mm) | DECREASE |
| M02 | P | Table 1: data ⏎ | a | b | c | ⏎ Text ``safe`` end. ⏎ The design is safe. | 3:R5(safe), 4:R5(safe), 1:R6 | 4:R5(safe), 1:R6 | 4:R5(safe), 1:R6 | DECREASE |
| M03 | P | ``` ⏎ conservative ⏎ ``` ⏎ [id]: docs/conservative.md ⏎ conservative text | 4:R5(conservative), 5:R5(conservative) | 4:R5(conservative), 5:R5(conservative) | 5:R5(conservative) | DECREASE |
| M04 | P | [id]: docs/conservative.md ⏎     "conservative note" ⏎ conservative text | 1:R5(conservative), 3:R5(conservative) | 1:R5(conservative), 3:R5(conservative) | 3:R5(conservative) | DECREASE |
| M05 | P | [id]: ⏎     docs/a.md ⏎ conservative text | 3:R5(conservative) | 3:R5(conservative) | 3:R5(conservative) | same |
| M06 | P | Text ``conservative ⏎ span`` conservative end. | 1:R5(conservative), 2:R5(conservative) | 1:R5(conservative), 2:R5(conservative) | 1:R5(conservative), 2:R5(conservative) | same |
"""


def review_rows():
    """Transcribe the left two and right four fields without stripping inputs."""
    rows = []
    for line in RAW_TABLE.splitlines()[2:]:
        left = line[1:-1].split("|", 2)
        middle = left[2].rsplit("|", 4)
        assert middle[0].startswith(" ") and middle[0].endswith(" ")
        identifier, classification = left[0].strip(), left[1].strip()
        if identifier in {"G28", "G29"}:
            classification = "A"  # PM ruling R-D: legal URL join characters.
        elif identifier == "D22":
            classification = "S"  # Reference definition; pending PM confirmation.
        text = middle[0][1:-1].replace(" ⏎ ", "\n")
        main, _, previous, _ = (field.strip() for field in middle[1:])
        rows.append((identifier, classification, text, main, previous))
    return rows


def finding_tokens(text):
    """Retain line numbers, rule IDs and the exact matched word."""
    tokens = []
    for line, rule, message in CHECKER.check_file_text(text):
        word = re.search("'([^']*)'", message)
        token = f"{line}:{rule.split('-')[0]}"
        tokens.append(token + (f"({word[1]})" if word else ""))
    return tokens


def expected_tokens(column):
    """Decode the review's no-finding sentinel and ordered finding list."""
    return [] if column == "-" else column.split(", ")


ROWS = review_rows()


@pytest.mark.parametrize(
    "identifier, classification, text, main, previous",
    ROWS,
    ids=[row[0] for row in ROWS],
)
def test_review_table(identifier, classification, text, main, previous):
    """Correct thirteen defects; retain the other 204 review results exactly."""
    result = finding_tokens(text)
    reference = main if identifier in CORRECTED else previous
    assert result == expected_tokens(reference), identifier
    if identifier in VISIBLE_LINES:
        visible = [
            token
            for token in expected_tokens(main)
            if int(token.split(":", 1)[0]) in VISIBLE_LINES[identifier]
        ]
        assert set(visible) <= set(result), identifier
    elif classification == "P":
        assert set(expected_tokens(main)) <= set(result), identifier
    elif classification == "A":
        assert result == expected_tokens(previous), "PM ruling R-D: URL token limit"
    else:
        assert classification == "S"
        assert result == expected_tokens(previous), "Source-token regression pin"


def test_review_table_inventory():
    assert len(ROWS) == len({row[0] for row in ROWS}) == 217
    assert len(CORRECTED) == 13
    assert len(ROWS) - len(CORRECTED) == 204


@pytest.mark.parametrize(
    "text",
    [
        "www.example.org/a—conservative",
        "www.example.org/a–conservative",
        "www.example.org/a…conservative",
        "https://example.org/a^conservative",
        "https://example.org/a`conservative",
    ],
    ids=["www-em-dash", "www-en-dash", "www-ellipsis", "caret", "backtick-pin"],
)
def test_url_boundary_supplements(text):
    """Retain the measured main finding at each additional token boundary."""
    assert finding_tokens(text) == ["1:R5(conservative)"]
