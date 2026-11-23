# Documentation Structure

This directory contains all documentation for the DigitalModel project, organized into clear categories for easy navigation and maintenance.

## 📁 Directory Structure

### Core Documentation
- **`domains/`** - Domain-specific engineering knowledge (cathodic protection, hydrodynamics, etc.)
- **`modules/`** - All technical and software module documentation (orcaflex, ansys, aqwa, autocad, blender, dave, etc.)

### Supporting Documentation
- **`ecosystem/`** - Ecosystem and integration documentation
- **`guides/`** - User guides and tutorials
- **`references/`** - Reference materials and external resources
- **`legacy/`** - Historical documentation (preserved for reference)
- **`migration/`** - Migration guides and upgrade paths
- **`test_automation/`** - Test automation documentation
- **`user_stories/`** - User stories and requirements

### Internal Organization
- **`_tools/`** - Python scripts and tools for documentation management
- **`_config/`** - Configuration files and JSON data
- **`_assets/`** - Images, PDFs, and other binary assets
- **`_development/`** - Development and deployment documentation
- **`_lib/`** - Library files and shared resources

## 📄 Root Files
Only essential documentation remains in the root:
- `DEPENDENCIES.md` - Project dependencies
- `PARALLEL_PROCESSING_STANDARD.md` - Parallel processing guidelines
- `REPOSITORY_ORGANIZATION.md` - Repository organization standards
- `README.md` - This file

## 🏗️ Organization Principles

1. **Module-based Architecture**: All technical and software documentation consolidated in modules/
2. **Domain Separation**: Engineering domains clearly organized in domains/
3. **Clean Root**: Only essential files remain in the root directory
4. **No Duplicates**: All duplicate content has been consolidated
5. **Unified Structure**: Software tools now integrated with technical modules

## 🔍 Finding Documentation

- For engineering topics → Check `domains/`
- For technical modules & software tools → Check `modules/`
- For how-to guides → Check `guides/`
- For migration info → Check `migration/`

## 🛠️ Maintenance

- Tools for documentation management are in `_tools/`
- Configuration files are in `_config/`
- Keep the root directory clean - only add essential documentation
- Follow the established directory structure when adding new content