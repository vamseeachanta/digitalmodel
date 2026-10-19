"""Knowledge-pack acceptance contracts for audit section 11."""
import re
from pathlib import Path


def test_followup_register_has_five_concrete_issue_links():
    path = Path(__file__).parents[2] / 'src/digitalmodel/marine_ops/docs/agent-pack.md'
    content = path.read_text(encoding='utf-8')
    assert 'issue pending' not in content
    for number in range(2768, 2773):
        assert f'https://github.com/vamseeachanta/digitalmodel/issues/{number}' in content
    checks = re.findall(r'^\| C\d.*$', content, re.MULTILINE)
    assert checks
    assert all('`' in row and 'test_' in row for row in checks)
