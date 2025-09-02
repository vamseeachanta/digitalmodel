# Spec Requirements Document

> Spec: Passing Ship Forces Calculation Module
> Created: 2025-01-01
> Status: Planning

## Overview

Implement a Python-based calculation module for determining hydrodynamic forces and moments on a moored vessel caused by a passing ship, based on Wang's paper methodology. This module will convert existing MathCAD calculations into a configuration-driven, reusable Python component integrated into the ship_design module.

## User Stories

### Marine Engineer Force Analysis

As a marine engineer, I want to calculate passing ship interaction forces on moored vessels, so that I can assess mooring line requirements and vessel station-keeping capabilities.

**Workflow:**
1. Define vessel parameters (dimensions, cross-sectional areas) in YAML configuration
2. Specify passing ship characteristics and environmental conditions
3. Execute calculation to obtain surge force, sway force, and yaw moment
4. Generate visual plots showing force distributions vs. stagger distance
5. Export results for integration with mooring analysis tools

### Naval Architect Safety Assessment

As a naval architect, I want to evaluate ship-to-ship interaction effects for various separation distances and water depths, so that I can determine safe passing zones and operational limits.

**Workflow:**
1. Load standard vessel configurations from templates
2. Run parametric studies varying separation distance, speed, and water depth
3. Visualize force magnitudes across operational envelope
4. Generate safety assessment reports with critical thresholds
5. Compare results with industry standards and guidelines

## Spec Scope

1. **Configuration System** - YAML-based input configuration for vessel parameters, environmental conditions, and calculation settings
2. **Core Calculation Engine** - Python implementation of Wang's formulation for surge, sway, and yaw forces with both infinite and finite water depth
3. **Visualization Module** - Interactive plots showing force distributions, parametric studies, and safety envelopes
4. **Batch Processing** - Support for multiple calculation scenarios with parallel execution
5. **Integration Interface** - API endpoints and CLI commands for seamless integration with existing ship_design module

## Out of Scope

- Real-time simulation capabilities
- Wave-induced effects beyond steady-state passing scenarios
- Detailed mooring line dynamics (handled by separate mooring module)
- CFD validation or mesh generation
- Direct OrcaFlex model generation (can be added later)

## Expected Deliverable

1. Fully functional Python module calculating passing ship forces matching MathCAD reference results within 0.1% accuracy
2. Comprehensive test suite validating calculations against known MathCAD outputs and published data
3. Interactive visualization dashboard showing force distributions and parametric sensitivities
4. Integration with existing ship_design module following established patterns

## Spec Documentation

- Tasks: @specs/modules/ship_design/passing-ship-forces/tasks.md
- Technical Specification: @specs/modules/ship_design/passing-ship-forces/sub-specs/technical-spec.md
- Tests Specification: @specs/modules/ship_design/passing-ship-forces/sub-specs/tests.md

## Agent Delegation

- **Primary Agent**: Ship Design Agent (to be created or extended)
- **Supporting Agents**: 
  - Testing Agent for parallel test execution
  - Documentation Agent for API documentation
  - Visualization Agent for chart generation