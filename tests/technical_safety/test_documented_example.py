"""Execute the user guide's worked calculation as published."""

from html.parser import HTMLParser
from pathlib import Path


def test_documented_worked_example():
    class Examples(HTMLParser):
        in_example = False
        code = ""

        def handle_starttag(self, tag, attrs):
            if tag == "pre" and ("data-python", None) in attrs:
                self.in_example = True

        def handle_endtag(self, tag):
            if tag == "pre":
                self.in_example = False

        def handle_data(self, data):
            if self.in_example:
                self.code += data

    guide = (
        Path(__file__).resolve().parents[2]
        / "docs/domains/fire_safety/technical-safety-screens.html"
    )
    parser = Examples()
    parser.feed(guide.read_text(encoding="utf-8"))
    assert parser.code.strip()
    exec(compile(parser.code, str(guide), "exec"), {})
