# SUPERSEDED SPECIFICATION

**Status**: SUPERSEDED
**Date**: 2025-09-02
**Superseded By**: `specs/modules/orcaflex/mooring-analysis/`

## Notice

This specification has been superseded by a new, more comprehensive mooring analysis specification.

### Reason for Supersession
- Original spec was based on specific test cases (go-by files)
- New spec provides a more general and reusable framework
- Test files have been moved to appropriate test directory

### Migration Information
- **Test Files**: Moved to `tests/modules/orcaflex/mooring-tension-iteration/go-by/`
- **New Spec**: See `specs/modules/orcaflex/mooring-analysis/`
- **Analysis Module**: `src/digitalmodel/modules/orcaflex/analysis/`

### Legacy Content
The original specification files remain here for historical reference but should not be used for new development.

## Recommended Actions
1. Use the new mooring-analysis specification for all new work
2. Reference test files in `tests/modules/orcaflex/mooring-tension-iteration/` for examples
3. Use the comparative analysis module at `src/digitalmodel/modules/orcaflex/analysis/`