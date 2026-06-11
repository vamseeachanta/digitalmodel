# Skill Search

Search for skills without loading all skill definitions into context.

## Usage

```
/skill-search <query>
```

## How It Works

1. Searches `.claude/skill-registry.yaml` for matching skills
2. Shows matches with brief descriptions
3. Use `/skill-name` to load the full skill

## Examples

```bash
# Find swarm-related skills
/skill-search swarm

# Find testing skills
/skill-search test

# Find GitHub integration skills
/skill-search github pr
```

## Registry Location

- Active: `.claude/commands/<category>/<skill>.md`
- Archived: `.claude/commands/_archive/<category>/<skill>.md`
- Index: `.claude/skill-registry.yaml`

## Search Implementation

When invoked, search the registry:

```yaml
# From .claude/skill-registry.yaml
skills:
  - name: "category/skill-name"
    description: "Brief description"
    archived: false
    path: ".claude/commands/category/skill-name.md"
```

Match against `name` and `description` fields. Return top 10 matches.

## Context Savings

- Without search: ~2,500 tokens (all 130 skill descriptions loaded)
- With search: ~200 tokens (only registry header + search results)
- Savings: ~2,300 tokens (92%)
