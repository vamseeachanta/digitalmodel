# Technical Specification

This is the technical specification for the spec detailed in @.agent-os/specs/2025-07-25-docs-organization-ai-friendly/spec.md

> Created: 2025-07-25
> Version: 1.0.0

## Technical Requirements

### Hierarchical Organization Structure

- **Root-level categories**: Organize by primary function rather than arbitrary prefixes
  - `software/` - Tools and software-specific documentation (replaces `pkg_*`)
  - `domains/` - Engineering subject matter expertise (replaces `sub_*`)
  - `modules/` - Active development modules (enhanced existing)
  - `legacy/` - Historical and superseded content (replaces `leg_*`)
  - `references/` - Standards, papers, and external documentation
  - `guides/` - Cross-cutting procedural documentation

- **Consistent naming**: Use kebab-case throughout, no prefixes in folder names
- **Maximum depth**: Limit to 4 levels deep for predictable AI navigation
- **Clear boundaries**: Each category has distinct scope and purpose

### Metadata Integration Requirements

- **Frontmatter format**: YAML frontmatter in all markdown files with required fields:
  ```yaml
  ---
  title: "Document Title"
  category: "domains/ship-design" 
  tags: ["stability", "naval-architecture", "regulatory"]
  last_updated: "2025-07-25"
  status: "active" | "legacy" | "deprecated"
  complexity: "beginner" | "intermediate" | "advanced"
  related: ["path/to/related/doc1.md", "path/to/related/doc2.md"]
  industry_standards: ["API", "DNV", "ABS"]
  ---
  ```

- **Semantic relationships**: Cross-reference system using relative paths
- **Content lifecycle tracking**: Clear status indicators for maintenance

### Navigation System Architecture

- **Master index**: `docs/README.md` with complete taxonomy and quick-start guides
- **Category indexes**: `_index.md` files in each major category describing scope and contents  
- **Topic maps**: Cross-reference matrices showing relationships between domains
- **Search aids**: Tag clouds and category listings for content discovery

### File Organization Patterns

- **Predictable paths**: `docs/{category}/{domain}/{topic}/content.md`
- **Content type separation**: Separate folders for `calculations/`, `examples/`, `references/` within domains
- **Version management**: Use `archive/` subfolders for superseded content rather than `Rev1/Rev2/` patterns

## Approach Options

**Option A: Complete Restructure**
- Pros: Clean, optimal AI-friendly structure, eliminates all organizational debt
- Cons: Breaks all existing paths, requires extensive link updates, high migration risk

**Option B: Gradual Migration with Symlinks** (Selected)
- Pros: Maintains backward compatibility, allows incremental migration, low risk
- Cons: Temporary complexity with dual path system, requires cleanup phase

**Option C: Metadata-Only Enhancement**  
- Pros: No structural changes, immediate benefit from metadata
- Cons: Preserves existing organizational problems, doesn't solve navigation issues

**Rationale:** Option B provides the best balance of improvement and safety. By creating the new structure alongside the old and using symbolic links for backward compatibility, we can migrate content systematically while ensuring existing references continue to work. This approach allows for validation and refinement before fully committing to the new structure.

## External Dependencies

- **Python scripts** - For automated frontmatter generation and content migration
  - **Justification:** Large-scale metadata addition requires automation for consistency
- **Git** - For tracking file moves and maintaining history
  - **Justification:** Preserve content history during reorganization
- **Markdown processing tools** - For frontmatter validation and link checking
  - **Justification:** Ensure metadata consistency and prevent broken references

## Implementation Strategy

### Phase 1: Structure Creation
1. Create new hierarchical folder structure alongside existing
2. Develop content categorization mapping
3. Create migration scripts for systematic content movement

### Phase 2: Content Migration  
1. Migrate content to new structure with automated frontmatter addition
2. Create symbolic links for backward compatibility
3. Update critical navigation files and indexes

### Phase 3: Enhancement
1. Add comprehensive cross-reference system
2. Create category guides and topic maps
3. Implement search and discovery aids

### Phase 4: Validation and Cleanup
1. Validate all links and references
2. Remove redundant content and symbolic links
3. Update any remaining hardcoded paths