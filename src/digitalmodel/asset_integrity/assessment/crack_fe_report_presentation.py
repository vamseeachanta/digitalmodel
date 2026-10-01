"""Bounded report presentation changes with source markup preserved.

Only exact supplied identifiers and owner-card locators are translated. This is
not a general engineering-prose sanitizer. The generator owns technical wording.
"""

from __future__ import annotations

import html
import re
from html.parser import HTMLParser
from typing import Mapping

MARKER = '<!-- REPORT_PRESENTATION_INTERNAL_REFERENCES -->'
_VOID = {'area', 'base', 'br', 'col', 'embed', 'hr', 'img', 'input', 'link',
         'meta', 'param', 'source', 'track', 'wbr'}
_PROTECTED = {'svg', 'script', 'style', 'code', 'pre', 'a', 'textarea'}
_HEADINGS = {'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'th'}
_WEIGHT_CSS = ('<style id="report-regular-body">'
               '.doc .v,.doc .assumed-label,.doc .labeltag,.doc .caption,'
               '.doc .status-within,.doc .status-exceeds,.doc .status-ne,'
               '.doc .bold-line,.doc .plabel{font-weight:400}'
               '</style>')


class _Inventory(HTMLParser):
    def __init__(self):
        super().__init__(convert_charrefs=False)
        self.stack = []
        self.ids = set()
        self.targets = {}

    def handle_starttag(self, tag, attrs):
        values = dict(attrs)
        target = values.get('id')
        if target:
            self.ids.add(target)
            if re.fullmatch(r's\d+', target):
                self.targets['Section ' + target[1:]] = target
        if tag not in _VOID:
            self.stack.append((tag, values))

    def handle_startendtag(self, tag, attrs):
        self.handle_starttag(tag, attrs)
        if tag not in _VOID:
            self.handle_endtag(tag)

    def handle_endtag(self, tag):
        for index in range(len(self.stack) - 1, -1, -1):
            if self.stack[index][0] == tag:
                del self.stack[index:]
                break

    def handle_data(self, data):
        if not any('caption' in a.get('class', '').split() for _, a in self.stack):
            return
        label = re.match(r'(Figure|Table) ([A-Z]|\d+)\.\d+', data)
        if label:
            target = next((a['id'] for _, a in reversed(self.stack) if a.get('id')), None)
            if target:
                self.targets[label.group()] = target


class _Polish(HTMLParser):
    def __init__(self, targets, aliases):
        super().__init__(convert_charrefs=False)
        self.targets = targets
        self.aliases = aliases
        self.stack = []
        self.output = []
        self.ledger = {}
        alternatives = sorted(set(targets) | set(aliases), key=len, reverse=True)
        parts = [re.escape(item) for item in alternatives]
        parts.append(r'owner (?:(?:cards?|decision) )?[A-Z]\d{2}(?:, [A-Z]\d{2})*')
        self.pattern = re.compile(r'(?<![\w])(?:' + '|'.join(parts) + r')(?!\w|\.\d)')

    def protected(self):
        return any(tag in _PROTECTED or 'data-src' in attrs
                   or attrs.get('id') in {'s9', 's9-2', 'internal-references'}
                   or 'caption' in attrs.get('class', '').split()
                   for tag, attrs, _ in self.stack)

    def handle_starttag(self, tag, attrs):
        structural = any(t in _HEADINGS for t, _, _ in self.stack)
        changed = tag in {'b', 'strong'} and not structural and not self.protected()
        raw = self.get_starttag_text()
        if changed:
            raw = re.sub(r'^<\s*' + tag + r'\b', '<span', raw, count=1)
        self.output.append(raw)
        if tag not in _VOID:
            values = dict(attrs)
            values['_output_index'] = len(self.output) - 1
            self.stack.append((tag, values, changed))

    def handle_startendtag(self, tag, attrs):
        self.output.append(self.get_starttag_text())

    def handle_endtag(self, tag):
        changed = False
        for index in range(len(self.stack) - 1, -1, -1):
            if self.stack[index][0] == tag:
                changed = self.stack[index][2]
                del self.stack[index:]
                break
        self.output.append('</' + ('span' if changed else tag) + '>')

    def citation(self, original, description):
        if original not in self.ledger:
            self.ledger[original] = (len(self.ledger) + 1, description)
        number = self.ledger[original][0]
        return f'<a href="#presentation-i{number}">[I{3 + number}]</a>'

    def replace(self, match):
        text = match.group()
        if text in self.targets:
            return f'<a href="#{html.escape(self.targets[text], quote=True)}">{text}</a>'
        if text in self.aliases:
            description = self.aliases[text]
            citation = self.citation(text, f'{text}: {description}.')
            return html.escape(description) + ' ' + citation
        return self.citation(text, f'Owner decision record: {text}.')

    def handle_data(self, data):
        if self.replace_source_alias(data):
            return
        self.output.append(data if self.protected() else self.pattern.sub(self.replace, data))

    def replace_source_alias(self, data):
        numeric = re.fullmatch(r'\s*[+-]?[\d,.]+(?:[eE][+-]?\d+)?\s*', data)
        if not self.stack or numeric or not re.search(r'[A-Za-z_]', data):
            return False
        _, attrs, _ = self.stack[-1]
        if 'data-src' not in attrs or any(
                t in _PROTECTED or a.get('id') in {'s9', 's9-2', 'internal-references'}
                for t, a, _ in self.stack):
            return False
        index = attrs['_output_index']
        if index != len(self.output) - 1 or 'data-raw' in attrs:
            return False
        citations = []

        def replace_alias(match):
            original = match.group()
            if original not in self.aliases:
                return html.escape(original)
            description = self.aliases[original]
            citations.append(self.citation(original, f'{original}: {description}.'))
            return html.escape(description)

        description = self.pattern.sub(replace_alias, data)
        if not citations:
            return False
        self.output[index] = self.output[index][:-1] + f' data-raw="{html.escape(data, quote=True)}">'
        self.output.append(description + ' ' + ' '.join(dict.fromkeys(citations)))
        return True

    def handle_entityref(self, name):
        self.output.append('&' + name + ';')

    def handle_charref(self, name):
        self.output.append('&#' + name + ';')

    def handle_comment(self, data):
        self.output.append('<!--' + data + '-->')

    def handle_decl(self, decl):
        self.output.append('<!' + decl + '>')

    def ledger_html(self):
        rows = [f'<li id="presentation-i{n}">[I{3 + n}] {html.escape(description)}</li>'
                for n, description in self.ledger.values()]
        return '<ol class="internal-presentation-references">' + ''.join(rows) + '</ol>'


def polish(page: str, *, reference_targets: Mapping[str, str] | None = None,
           narrative_aliases: Mapping[str, str] | None = None) -> str:
    """Link exact labels and relocate exact operational locators to a 9.2 marker.

    ``reference_targets`` maps visible labels (for example ``Section 3.2``) to
    element IDs. ``narrative_aliases`` maps opaque identifiers to engineering
    descriptions. String aliases in plain source-valued elements retain their
    whole original in ``data-raw`` and append linked ledger citations. Numerical
    values, SVG, scripts and existing links
    remain untouched. Presentation references use I4 onward; the generator owns
    I1 to I3. Section 9 is excluded from narrative transformations.
    """
    inventory = _Inventory()
    inventory.feed(page)
    targets = {**inventory.targets, **dict(reference_targets or {})}
    missing = set(targets.values()) - inventory.ids
    if missing:
        raise ValueError(f'missing reference target: {sorted(missing)}')
    parser = _Polish(targets, dict(narrative_aliases or {}))
    parser.feed(page)
    rendered = ''.join(parser.output)
    if parser.ledger and MARKER not in rendered:
        raise ValueError('internal-reference marker required for presentation ledger')
    rendered = rendered.replace(MARKER, parser.ledger_html() if parser.ledger else '')
    return rendered.replace('</head>', _WEIGHT_CSS + '</head>', 1)
