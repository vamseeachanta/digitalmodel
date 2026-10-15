# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rank private help sections through read-only GitHub API access.

Only section pointers, citation URLs and attribution leave the retrieval layer.
No licensed text is copied to its JSON output. Supply the private repository
and the maintainer-confirmed corpus prefix explicitly; no public corpus fallback is
used. GitHub authentication is supplied by the existing gh configuration.
"""

from __future__ import annotations

import argparse
import base64
import json
import re
import subprocess
from urllib.parse import quote

PRODUCTS = ("orcaflex", "orcawave", "orcfxapi")


def github_api(endpoint):
    """Read a JSON endpoint without logging response bodies on failure."""
    result = subprocess.run(["gh", "api", endpoint], capture_output=True, text=True)
    if result.returncode:
        # Responses can include private locations; expose only the status here.
        raise ValueError("GitHub read failed; inspect access to the configured repository")
    return json.loads(result.stdout)


def load_github_topics(repository, revision, prefix, api=github_api):
    """Read product topics at one resolved revision, rejecting incomplete trees."""
    if (not prefix or prefix.startswith("/") or ".." in prefix.split("/")
            or "\\" in prefix or "?" in prefix or "#" in prefix):
        raise ValueError("corpus prefix must be a repository-relative directory")
    if not re.fullmatch(r"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+", repository):
        raise ValueError("repository must have an organisation/repository shape")
    pinned = api(f"repos/{repository}/commits/{quote(revision, safe='')}")["sha"]
    subtree = pinned
    for directory in prefix.rstrip("/").split("/"):
        entries = api(f"repos/{repository}/git/trees/{subtree}")
        if entries.get("truncated"):
            raise ValueError("private tree is truncated; corpus discovery is incomplete")
        node = next((row for row in entries["tree"]
                     if row["type"] == "tree" and row["path"] == directory), None)
        if node is None:
            raise ValueError("configured private corpus prefix is absent")
        subtree = node["sha"]
    tree = api(f"repos/{repository}/git/trees/{subtree}?recursive=1")
    if tree.get("truncated"):
        raise ValueError("private tree is truncated; a narrower corpus tree is required")
    topics = []
    for item in sorted(tree["tree"], key=lambda row: row["path"]):
        relative = item["path"]
        path = prefix.rstrip("/") + "/" + relative
        if item["type"] != "blob":
            continue
        parts = relative.split("/")
        if (len(parts) < 2 or parts[0] not in PRODUCTS or not path.endswith(".md")
                or parts[-1].upper() in {"INDEX.MD", "PROVENANCE.MD", "README.MD"}):
            continue
        blob = api(f"repos/{repository}/git/blobs/{item['sha']}")
        if blob["encoding"] != "base64":
            raise ValueError("unsupported help blob encoding")
        topics.append({"path": path, "product": parts[0],
                       "text": base64.b64decode(blob["content"]).decode("utf-8")})
    if not topics:
        raise ValueError("no help topics at the configured private corpus prefix")
    return topics, pinned


def chunk_sections(topic):
    """Split Markdown headings while keeping fenced code inside its section."""
    text = topic["text"]
    source = re.search(r"https://www\.orcina\.com/webhelp/[^\s)<>]*", text)
    if not source or not re.search(r"Orcina", text):
        raise ValueError("help topic lacks Orcina source attribution")
    lines = text.splitlines()
    title = next((line[2:].strip() for line in lines if line.startswith("# ")),
                 topic["path"].rsplit("/", 1)[-1])
    sections = []
    heading, start, body, fence = title, 1, [], None

    def append():
        content = "\n".join(body).strip()
        if content:
            sections.append({"path": topic["path"], "product": topic["product"],
                             "topic": title, "section": heading, "line": start,
                             "text": content, "source_url": source.group(0),
                             "attribution": "Orcina WebHelp"})

    for number, line in enumerate(lines, 1):
        marker = re.match(r"^\s{0,3}(`{3,}|~{3,})", line)
        if marker:
            token = marker.group(1)
            if fence is None:
                fence = token
            elif token[0] == fence[0] and len(token) >= len(fence):
                fence = None
            body.append(line)
            continue
        match = re.match(r"^#{2,6}\s+(.+?)\s*#*\s*$", line) if fence is None else None
        if match:
            # The introduction may consist only of the title and attribution.
            meaningful = [s for s in body if s.strip() and not s.startswith(("# ", ">"))]
            if meaningful:
                append()
            heading, start, body = match.group(1), number, []
        else:
            body.append(line)
    append()
    return sections


def search(topics, query, limit=6):
    """Rank sections deterministically and emit metadata without section text."""
    tokens = set(re.findall(r"[a-z0-9]+", query.casefold()))
    if not tokens or limit < 1:
        return []
    scored = []
    for topic in topics:
        for chunk in chunk_sections(topic):
            heading = set(re.findall(r"[a-z0-9]+", chunk["section"].casefold()))
            body = set(re.findall(r"[a-z0-9]+", chunk["text"].casefold()))
            score = 3 * len(tokens & heading) + len(tokens & body)
            if score:
                result = {key: value for key, value in chunk.items() if key != "text"}
                result["score"] = score
                scored.append(result)
    return sorted(scored, key=lambda row: (-row["score"], row["path"], row["line"]))[:limit]


def main():
    """Print citation pointers for a query of the configured private corpus."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("query")
    parser.add_argument("--wiki-repo", required=True)
    parser.add_argument("--prefix", required=True)
    parser.add_argument("--revision", default="main")
    parser.add_argument("--limit", type=int, default=6)
    args = parser.parse_args()
    try:
        topics, revision = load_github_topics(args.wiki_repo, args.revision, args.prefix)
        result = {"revision": revision, "matches": search(topics, args.query, args.limit)}
    except (ValueError, KeyError, OSError) as error:
        parser.exit(1, f"retrieval: {error}\n")
    print(json.dumps(result, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
