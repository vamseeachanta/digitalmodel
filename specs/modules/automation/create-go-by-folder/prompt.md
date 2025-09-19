# Create Go-By Folder Tool - Prompt History

## Initial Request
User requested to review the go-by-folder spec (now at `template.md` in this folder) and indicated they would provide additional requirements.

## Requirements Discussion

### Original File Preservation Requirement
**User**: During the creation of go-by folder, the go-by folder should:
a/ Preserve one file of each kind in its original condition without any changes/edits.
b/ All other variation files can be capturing the changes and can be representative files

**Response**: Updated the specification to include:
- Mandatory preservation of one original file per type/pattern
- First instance rule for selecting originals
- Separate `_originals/` folder for preserved files
- Clear distinction between preserved and representative files

### Agent-Friendly Documentation
**User**: Also add a markdown file to the folder describing the contents for agents to easily catch up on the folder contents and folder structure

**Response**: Added AGENT_OVERVIEW.md requirement with:
- Quick start guide for AI agents
- Comprehensive folder structure overview
- File type inventory
- Pattern documentation
- Usage instructions
- Cross-references to other metadata files

## Clarification Questions and Answers

### 1. Conflict Resolution
**Q**: What happens if target_folder already contains a partial go-by from a previous failed run?
**A**: Use --overwrite flag

**Q**: How to handle merge scenarios where user wants to update existing go-by?
**A**: Warn the user and go ahead and update. This will usually be part of git anyway

### 2. Size Boundaries
**Q**: What's the maximum recommended source folder size?
**A**: 10GB limit

**Q**: How to handle binary files that are already compressed?
**A**: Warn the user and ask specific instructions on case-by-case basis

**Q**: Should there be different strategies for files >1GB vs >100MB?
**A**: None

### 3. Selection Logic
**Q**: When preserving "first instance" - is it alphabetical, chronological, or discovery order?
**A**: Chronological

**Q**: How to handle symlinks and hard links?
**A**: Highlight to user and ask for handling guidance. Document in the markdown for future reference

**Q**: What about files with identical names in different directories?
**A**: Check if they are of duplicate content. Ask user for handling. Usually keep them as long as they are <50KB

### 4. Security & Privacy
**Q**: How to handle files with sensitive data?
**A**: Not expected. Bring to user attention

**Q**: Should there be a --redact-sensitive flag?
**A**: Not expected

**Q**: Integration with .gitignore patterns?
**A**: Not expected

## Specification Enhancements Added

Based on the discussion, the following sections were added to the specification:

1. **Conflict Resolution** - Handling existing folders and updates
2. **Size Boundaries** - 10GB limit and compressed file handling
3. **Selection Logic** - Chronological ordering and duplicate handling
4. **Security & Privacy** - Minimal expectations with user alerts
5. **Progress Reporting** - Real-time progress with ETA
6. **Performance Guidelines** - Expected performance by size
7. **Validation Rules** - Pre/runtime/post validation checks
8. **Error Handling & Recovery** - Checkpoint system and recovery
9. **Original File Preservation Strategy** - Detailed selection criteria
10. **Agent Overview Documentation** - Comprehensive guide structure

## Final Specification Request
**User**: Using the updated template document (now `template.md`), create a detailed specification for implementation.

## Curated Reuse Prompt

```
Create a comprehensive implementation specification for a "Create Go-By Folder" tool based on the following requirements:

The tool should create lightweight, representative versions of existing folders with these key features:

1. **Core Functionality**:
   - Mirror complete directory structure
   - Preserve ONE original file per type/pattern completely unchanged
   - Create minimized representatives for all variation files
   - Generate comprehensive metadata

2. **Key Requirements**:
   - Maximum source folder size: 10GB
   - Selection strategy: Chronological (oldest first)
   - Include --overwrite flag for existing targets
   - User prompts for: compressed files, symlinks, large duplicates
   - Progress reporting with ETA
   - Checkpoint saves every 1000 files
   - Error recovery with resume capability

3. **Output Structure**:
   - AGENT_OVERVIEW.md (AI-friendly documentation)
   - GO_BY_METADATA.json (statistics and rules)
   - _originals/ folder (preserved files)
   - _templates/ and _samples/ (patterns and representatives)
   - Full directory tree with minimized files

4. **File Minimization Strategies**:
   - Text files: First/last 10 lines
   - Code files: Structure and signatures only
   - Config files: Keys with placeholder values
   - Binary files: Stub files with metadata

5. **Expected Performance**:
   - <1% of original size (excluding originals)
   - Process 1GB in <2 minutes
   - Memory usage <500MB for 10GB source

Create detailed specification including:
- Technical architecture
- Module design
- Implementation code samples
- Task breakdown with time estimates
- Testing strategy
```