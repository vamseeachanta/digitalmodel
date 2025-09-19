# Create Go-By Folder Specification

## 📁 Folder Structure

This folder contains the complete specification for the Create Go-By Folder tool, combining both the requirements template and implementation specifications in one organized location.

```
create-go-by-folder/
├── README.md           # This file - folder overview
├── template.md         # Original requirements and feature template
├── spec.md            # Detailed technical implementation specification
├── tasks.md           # Task breakdown with time estimates (64 hours)
├── prompt.md          # Complete prompt history and decisions
└── AGENT_OVERVIEW.md  # AI-agent friendly quick reference
```

## 📄 File Descriptions

### template.md (Requirements Document)
The comprehensive requirements template that defines:
- Core functionality and arguments
- File preservation strategies
- Metadata generation requirements
- User interaction points
- Performance expectations
- Validation rules

### spec.md (Technical Specification)
The implementation specification including:
- Python module architecture
- Detailed class designs
- Code implementation samples
- Testing strategy
- Deployment approach

### tasks.md (Implementation Plan)
Complete task breakdown with:
- 8 development phases
- 64 total hours estimate
- Critical path identification
- Risk areas and dependencies

### prompt.md (History & Decisions)
Documentation of:
- Original user requirements
- Clarification Q&A
- Design decisions made
- Curated reuse prompt

### AGENT_OVERVIEW.md (Quick Reference)
AI-optimized summary with:
- Tool overview and purpose
- Technical architecture
- Key decisions
- Implementation guidelines

## 🎯 Purpose

This tool creates lightweight "go-by" versions of existing folders that:
- Reduce size by >99% while preserving structure
- Keep one original file per type unchanged
- Generate comprehensive metadata for agents and humans
- Support folders up to 10GB

## 🔧 Key Features

- **Smart Preservation**: Chronological selection of originals
- **Intelligent Minimization**: Context-aware file reduction
- **Progress Tracking**: Real-time updates with ETA
- **Error Recovery**: Checkpoint/resume capability
- **User Interaction**: Prompts for ambiguous decisions
- **AI-Friendly**: AGENT_OVERVIEW.md for quick understanding

## 📊 Specifications Summary

| Aspect | Specification |
|--------|--------------|
| **Max Source Size** | 10GB |
| **Target Reduction** | >99% (excluding originals) |
| **Performance** | 1GB in <2 minutes |
| **Memory Usage** | <500MB for 10GB source |
| **Checkpoint Interval** | Every 1000 files |
| **Development Time** | 64 hours estimated |

## 🚀 Implementation Status

- ✅ Requirements documented (template.md)
- ✅ Technical specification complete (spec.md)
- ✅ Tasks breakdown ready (tasks.md)
- ✅ Decisions documented (prompt.md)
- ⏳ Awaiting implementation

## 💡 For AI Agents

When working with this specification:
1. Start with `AGENT_OVERVIEW.md` for quick orientation
2. Review `template.md` for detailed requirements
3. Consult `spec.md` for implementation details
4. Follow `tasks.md` for development sequence
5. Reference `prompt.md` for design decisions

## 🔄 Related Specifications

This specification follows the repository's modular pattern and should be kept together as a single unit. All files in this folder are interconnected and should be maintained together.

## 📝 Notes

- All related documentation is now consolidated in this single folder
- The original template has been moved here as `template.md`
- References have been updated to reflect the new structure
- This organization ensures agents keep related work together