# AI Assistant Configuration

This directory contains configuration and guidance for AI assistants working with this repository.

## Directory Structure

- `settings.json` - AI assistant permissions and preferences
- `code-guidance/` - Code-specific guidance documents
- `commands/` - Custom automation commands
- `workflows/` - Development workflow documentation
- `project-context.md` - High-level project context for AI assistants

## Usage

AI assistants should:
1. Read `project-context.md` for high-level understanding
2. Follow guidelines in `code-guidance/` for code standards
3. Use `commands/` for automated tasks
4. Reference `workflows/` for development processes
5. Respect settings in `settings.json`

## Specification-Driven Development

This project follows specification-driven development:
- All features start with specifications in `specs/`
- AI assistants should reference specifications before implementing
- Keep documentation and code in sync with specifications
- Document AI assistant interactions in `specs/` using appropriate templates:
  - New features: Create specification using `specs/templates/module-template.md` or `specs/templates/enhancement-template.md`
  - Bug fixes: Use `specs/templates/bugfix-template.md`
  - Infrastructure changes: Use `specs/templates/infrastructure-template.md`
  - Save in relevant subdirectory: `modules/`, `enhancements/`, `bugfixes/`, or `infrastructure/`