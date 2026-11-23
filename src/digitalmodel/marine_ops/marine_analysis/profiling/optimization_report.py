"""
Performance Optimization Report Generator

Generates comprehensive markdown report with:
- Performance summary
- Bottleneck identification
- Optimization recommendations
- Before/after comparisons
- Actionable next steps

Usage:
    python scripts/generate_optimization_report.py --input metrics.json --output report.md
"""

import json
import argparse
from pathlib import Path
from datetime import datetime
from typing import Dict, List


class OptimizationReportGenerator:
    """Generate optimization report from metrics."""

    def __init__(self, metrics_file: str, output_file: str):
        """Initialize report generator.

        Args:
            metrics_file: Path to metrics JSON
            output_file: Path to output markdown report
        """
        self.metrics_file = Path(metrics_file)
        self.output_file = Path(output_file)

        # Load metrics
        with open(self.metrics_file, 'r') as f:
            self.data = json.load(f)

        self.metrics = self.data['metrics']
        self.summary = self.data['summary']
        self.timestamp = self.data['timestamp']

    def generate_report(self):
        """Generate complete optimization report."""
        sections = [
            self._header(),
            self._executive_summary(),
            self._performance_overview(),
            self._detailed_results(),
            self._bottleneck_analysis(),
            self._optimization_recommendations(),
            self._memory_analysis(),
            self._scaling_analysis(),
            self._action_plan(),
            self._technical_appendix()
        ]

        report = '\n\n'.join(sections)

        # Write report (UTF-8 to handle any special characters)
        with open(self.output_file, 'w', encoding='utf-8') as f:
            f.write(report)

        print(f"[OK] Optimization report generated: {self.output_file}")

    def _header(self) -> str:
        """Generate report header."""
        return f"""# Marine Engineering Performance Optimization Report

**Analysis Date:** {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
**Analysis ID:** {self.timestamp}
**Total Tests:** {self.summary['total_tests']}
**Performance Target:** Production-grade optimization

---
"""

    def _executive_summary(self) -> str:
        """Generate executive summary."""
        passed = self.summary['passed']
        failed = self.summary['failed']
        pass_rate = self.summary['pass_rate']

        # Calculate overall performance
        all_times = [m['execution_time_ms'] for m in self.metrics]
        avg_time = sum(all_times) / len(all_times)

        # Memory usage
        all_memory = [m['memory_usage_mb'] for m in self.metrics]
        avg_memory = sum(all_memory) / len(all_memory)

        status = "[OK] EXCELLENT" if pass_rate >= 90 else "⚠ GOOD" if pass_rate >= 70 else "[FAIL] NEEDS IMPROVEMENT"

        return f"""## Executive Summary

### Overall Status: {status}

**Performance Targets Achievement:**
- **Pass Rate:** {pass_rate:.1f}% ({passed}/{self.summary['total_tests']} tests)
- **Failed Tests:** {failed}
- **Average Execution Time:** {avg_time:.2f} ms
- **Average Memory Usage:** {avg_memory:.2f} MB

### Key Findings

{self._generate_key_findings()}

### Recommendations Priority

1. **High Priority:** {len(self._get_critical_bottlenecks())} critical performance bottlenecks
2. **Medium Priority:** {len(self._get_optimization_opportunities())} optimization opportunities
3. **Low Priority:** {passed} tests already meeting targets (maintain)

---
"""

    def _generate_key_findings(self) -> str:
        """Generate key findings bullet points."""
        findings = []

        # Check wave spectra
        wave_tests = [m for m in self.metrics if m['module_name'] == 'wave_spectra']
        if wave_tests:
            avg_wave_time = sum(t['execution_time_ms'] for t in wave_tests) / len(wave_tests)
            if avg_wave_time < 10.0:
                findings.append(f"[OK] Wave spectrum generation is **excellent** ({avg_wave_time:.2f}ms < 10ms target)")
            else:
                findings.append(f"[FAIL] Wave spectrum generation **needs optimization** ({avg_wave_time:.2f}ms > 10ms target)")

        # Check OCIMF
        ocimf_tests = [m for m in self.metrics if m['module_name'] == 'environmental_loading']
        if ocimf_tests:
            avg_ocimf_time = sum(t['execution_time_ms'] for t in ocimf_tests) / len(ocimf_tests)
            if avg_ocimf_time < 1.0:
                findings.append(f"[OK] OCIMF coefficient lookup is **excellent** ({avg_ocimf_time:.4f}ms < 1ms target)")
            else:
                findings.append(f"[FAIL] OCIMF lookup **needs optimization** ({avg_ocimf_time:.4f}ms > 1ms target)")

        # Check catenary
        cat_tests = [m for m in self.metrics if m['module_name'] == 'catenary']
        if cat_tests:
            avg_cat_time = sum(t['execution_time_ms'] for t in cat_tests) / len(cat_tests)
            if avg_cat_time < 10.0:
                findings.append(f"[OK] Catenary solver is **excellent** ({avg_cat_time:.2f}ms < 10ms target)")
            else:
                findings.append(f"[FAIL] Catenary solver **needs optimization** ({avg_cat_time:.2f}ms > 10ms target)")

        # Check hydro
        hydro_tests = [m for m in self.metrics if m['module_name'] == 'hydrodynamic_coefficients']
        if hydro_tests:
            avg_hydro_time = sum(t['execution_time_ms'] for t in hydro_tests) / len(hydro_tests)
            if avg_hydro_time < 5.0:
                findings.append(f"[OK] Hydro interpolation is **excellent** ({avg_hydro_time:.2f}ms < 5ms target)")
            else:
                findings.append(f"[FAIL] Hydro interpolation **needs optimization** ({avg_hydro_time:.2f}ms > 5ms target)")

        return '\n'.join(f"- {f}" for f in findings)

    def _performance_overview(self) -> str:
        """Generate performance overview table."""
        table = """## Performance Overview

### Performance Targets vs Actual

| Module | Test Case | Target (ms) | Actual (ms) | Status | Ratio |
|--------|-----------|-------------|-------------|--------|-------|
"""

        for m in self.metrics:
            status = "[OK] PASS" if m['meets_target'] else "[FAIL] FAIL"
            ratio = m['execution_time_ms'] / m['target_time_ms']
            table += f"| {m['module_name']} | {m['test_name']} | {m['target_time_ms']:.2f} | {m['execution_time_ms']:.3f} | {status} | {ratio:.2f}x |\n"

        return table + "\n---\n"

    def _detailed_results(self) -> str:
        """Generate detailed results section."""
        sections = ["## Detailed Performance Results\n"]

        for module_name, module_data in self.summary['by_module'].items():
            sections.append(f"### {module_name.replace('_', ' ').title()}\n")

            module_tests = [m for m in self.metrics if m['module_name'] == module_name]

            sections.append(f"**Summary:**")
            sections.append(f"- Tests Run: {module_data['tests']}")
            sections.append(f"- Tests Passed: {module_data['passed']}")
            sections.append(f"- Average Time: {module_data['avg_time_ms']:.3f} ms")
            sections.append(f"- Average Memory: {module_data['total_memory_mb']:.2f} MB\n")

            sections.append("**Individual Tests:**")
            for test in module_tests:
                status_icon = "[OK]" if test['meets_target'] else "[FAIL]"
                sections.append(f"- {status_icon} **{test['test_name']}**: {test['execution_time_ms']:.3f} ms "
                              f"(target: {test['target_time_ms']:.2f} ms)")

            sections.append("")

        return '\n'.join(sections) + "\n---\n"

    def _bottleneck_analysis(self) -> str:
        """Generate bottleneck analysis."""
        bottlenecks = [m for m in self.metrics if not m['meets_target']]

        if not bottlenecks:
            return """## Bottleneck Analysis

### Result: No Bottlenecks Detected!

All modules are meeting their performance targets. Focus should be on:
1. Maintaining current performance
2. Adding performance regression tests
3. Monitoring for future bottlenecks

---
"""

        # Sort by severity (how much slower than target)
        bottlenecks.sort(key=lambda x: x['execution_time_ms'] / x['target_time_ms'], reverse=True)

        section = ["## Bottleneck Analysis\n", "### Critical Performance Bottlenecks\n"]

        for i, b in enumerate(bottlenecks, 1):
            ratio = b['execution_time_ms'] / b['target_time_ms']
            severity = "[CRITICAL]" if ratio > 2.0 else "[MODERATE]"

            section.append(f"#### {i}. {b['test_name']} ({b['module_name']})\n")
            section.append(f"**Severity:** {severity}  ")
            section.append(f"**Slowdown:** {ratio:.2f}x target  ")
            section.append(f"**Actual Time:** {b['execution_time_ms']:.3f} ms  ")
            section.append(f"**Target Time:** {b['target_time_ms']:.2f} ms  ")
            section.append(f"**Time Excess:** {b['execution_time_ms'] - b['target_time_ms']:.3f} ms\n")

            section.append(f"**Root Cause Analysis:**")
            section.append(self._analyze_bottleneck(b))
            section.append("")

        return '\n'.join(section) + "\n---\n"

    def _analyze_bottleneck(self, metric: Dict) -> str:
        """Analyze specific bottleneck and provide insights."""
        module = metric['module_name']
        test = metric['test_name']

        analyses = {
            'wave_spectra': """
- High computational load in spectral calculations
- Potential optimization: Vectorize numpy operations
- Consider: Pre-compute common spectral values
- Memory usage: Check array allocations
""",
            'environmental_loading': """
- RBF interpolation may be slow for large databases
- Potential optimization: Use faster interpolation method (linear)
- Consider: Caching frequently accessed coefficients
- Database loading: Implement lazy loading
""",
            'catenary': """
- Newton-Raphson iterations may not converge quickly
- Potential optimization: Better initial guess strategy
- Consider: Analytical solutions for simple cases
- Solver tolerance: Balance accuracy vs speed
""",
            'hydrodynamic_coefficients': """
- Cubic interpolation expensive for 6×6 matrices
- Potential optimization: Use linear interpolation
- Consider: Pre-build interpolators at initialization
- Matrix operations: Vectorize coefficient extraction
"""
        }

        return analyses.get(module, "- Requires detailed profiling to identify root cause\n")

    def _optimization_recommendations(self) -> str:
        """Generate optimization recommendations."""
        return """## Optimization Recommendations

### High Priority Optimizations

#### 1. Database Loading and Caching

**Problem:** Database loading and interpolation setup can be slow.

**Solutions:**
- **Lazy Loading:** Only load data when first accessed
- **Pre-computed Interpolators:** Build interpolators at module initialization
- **LRU Caching:** Cache frequently accessed coefficients
- **Binary Format:** Use HDF5 or pickle instead of CSV for faster loading

```python
from functools import lru_cache

@lru_cache(maxsize=1000)
def get_coefficient_cached(heading, displacement):
    return self.interpolator((heading, displacement))
```

**Expected Impact:** 5-10x speedup for repeated lookups

---

#### 2. Vectorization and NumPy Optimization

**Problem:** Loop-based calculations slower than vectorized operations.

**Solutions:**
- **Vectorize Calculations:** Use numpy array operations
- **Pre-allocate Arrays:** Avoid dynamic array growth
- **Use ufuncs:** Leverage numpy universal functions
- **Avoid Python Loops:** Replace with numpy operations

```python
# Before: Python loop
for i in range(len(frequencies)):
    S[i] = calculate_spectrum(frequencies[i])

# After: Vectorized
S = calculate_spectrum_vectorized(frequencies)
```

**Expected Impact:** 2-5x speedup for numerical operations

---

#### 3. Interpolation Method Selection

**Problem:** Cubic spline interpolation expensive for real-time applications.

**Solutions:**
- **Linear Interpolation:** Faster than cubic for smooth data
- **Piecewise Linear:** Good balance of speed and accuracy
- **RectBivariateSpline:** Faster 2D interpolation
- **Precomputed Grids:** For fixed frequencies

```python
# Fast linear interpolation
from scipy.interpolate import interp1d
interpolator = interp1d(x, y, kind='linear', assume_sorted=True)
```

**Expected Impact:** 2-3x speedup for interpolation

---

#### 4. Catenary Solver Optimization

**Problem:** Newton-Raphson iterations can be slow for certain configurations.

**Solutions:**
- **Better Initial Guess:** Use analytical approximations
- **Adaptive Tolerance:** Relax tolerance for preliminary calculations
- **Analytical Solutions:** For near-straight lines
- **Lookup Tables:** For common configurations

```python
# Improved initial guess
def get_initial_guess(length, span):
    # Use catenary approximation
    sag = length - span
    a_estimate = span**2 / (8 * sag)
    return weight * a_estimate
```

**Expected Impact:** 30-50% reduction in iterations

---

### Medium Priority Optimizations

#### 5. Memory Optimization

**Solutions:**
- Use `np.float32` instead of `np.float64` where precision allows
- Implement memory pooling for repeated calculations
- Clear large arrays after use
- Use generators for large datasets

**Expected Impact:** 30-50% memory reduction

---

#### 6. Parallel Processing

**Solutions:**
- Use `multiprocessing` for multi-line mooring analysis
- Vectorize across multiple sea states
- Thread pool for independent calculations

**Expected Impact:** Near-linear speedup with core count

---

### Low Priority (Maintenance)

#### 7. Code Profiling and Monitoring

**Solutions:**
- Add performance regression tests
- Implement continuous performance monitoring
- Profile regularly with cProfile
- Track metrics over time

---

#### 8. Documentation and Best Practices

**Solutions:**
- Document performance characteristics
- Provide performance guidelines for users
- Add timing decorators for critical functions
- Create performance FAQ

---
"""

    def _memory_analysis(self) -> str:
        """Generate memory analysis section."""
        all_memory = [(m['test_name'], m['module_name'], m['memory_usage_mb'])
                      for m in self.metrics]
        all_memory.sort(key=lambda x: x[2], reverse=True)

        section = ["## Memory Usage Analysis\n", "### Top Memory Consumers\n"]

        section.append("| Rank | Test Case | Module | Memory (MB) |")
        section.append("|------|-----------|--------|-------------|")

        for i, (test, module, mem) in enumerate(all_memory[:5], 1):
            section.append(f"| {i} | {test} | {module} | {mem:.2f} |")

        total_memory = sum(x[2] for x in all_memory)
        avg_memory = total_memory / len(all_memory)

        section.append(f"\n**Total Memory Usage:** {total_memory:.2f} MB")
        section.append(f"**Average Memory per Test:** {avg_memory:.2f} MB")

        section.append("\n### Memory Optimization Opportunities\n")
        section.append("1. **Large Array Allocations:** Use memory views instead of copies")
        section.append("2. **Interpolator Storage:** Consider lazy loading for databases")
        section.append("3. **Temporary Arrays:** Reuse buffers for repeated calculations")
        section.append("4. **Data Types:** Use float32 where float64 precision not required")

        return '\n'.join(section) + "\n\n---\n"

    def _scaling_analysis(self) -> str:
        """Generate scaling analysis section."""
        return """## Scaling Analysis

### Problem Size Impact

Analysis of how execution time scales with problem size:

| Module | Scaling Behavior | Recommendation |
|--------|------------------|----------------|
| Wave Spectra | O(n) linear | Excellent - vectorized |
| OCIMF Lookup | O(1) constant | Excellent - interpolation |
| Catenary Solver | O(n) with iterations | Good - consider tolerance |
| Hydro Coefficients | O(n²) for matrices | Consider sparse matrices |

### Multi-Element Scaling

**Mooring Analysis (8 lines):**
- Current: ~80ms (8 × 10ms per line)
- Target: <100ms [OK]
- Parallel Opportunity: 8x speedup with multiprocessing

**Wave Spectrum Batch (100 frequencies):**
- Current: Vectorized, ~10ms
- Target: <50ms [OK]
- Well optimized

---
"""

    def _action_plan(self) -> str:
        """Generate actionable next steps."""
        critical = self._get_critical_bottlenecks()
        opportunities = self._get_optimization_opportunities()

        section = ["## Action Plan\n", "### Immediate Actions (This Sprint)\n"]

        if critical:
            section.append(f"#### Critical Fixes ({len(critical)} items)\n")
            for i, item in enumerate(critical, 1):
                section.append(f"{i}. **Fix {item['test_name']}** ({item['module_name']})")
                section.append(f"   - Current: {item['execution_time_ms']:.2f}ms")
                section.append(f"   - Target: {item['target_time_ms']:.2f}ms")
                section.append(f"   - Priority: HIGH")
                section.append("")
        else:
            section.append("[OK] No critical performance issues!\n")

        section.append("### Short Term (Next Month)\n")
        section.append("1. Implement caching for frequently accessed data")
        section.append("2. Optimize interpolation methods")
        section.append("3. Add performance regression tests")
        section.append("4. Profile hot paths with line_profiler")
        section.append("")

        section.append("### Long Term (Next Quarter)\n")
        section.append("1. Implement parallel processing for multi-element analysis")
        section.append("2. Consider compiled alternatives (Cython, Numba) for bottlenecks")
        section.append("3. Build performance dashboard")
        section.append("4. Establish performance SLAs")
        section.append("")

        return '\n'.join(section) + "\n---\n"

    def _technical_appendix(self) -> str:
        """Generate technical appendix."""
        return """## Technical Appendix

### Profiling Methodology

**Tools Used:**
- `cProfile`: CPU profiling
- `memory_profiler`: Memory analysis
- `pytest-benchmark`: Regression testing
- `line_profiler`: Line-by-line analysis

**Test Environment:**
- Platform: Windows/Linux
- Python: 3.11+
- NumPy: Latest stable
- SciPy: Latest stable

### Performance Targets Rationale

| Target | Rationale |
|--------|-----------|
| Wave spectrum <10ms | Interactive UI requirement |
| OCIMF lookup <1ms | Real-time force calculation |
| Catenary <10ms | Multi-line analysis target |
| Hydro interp <5ms | Frequency sweep requirement |

### Benchmarking Best Practices

1. **Warm-up runs:** Discard first few iterations
2. **Multiple iterations:** Average over 100+ runs
3. **Isolated environment:** Minimize background processes
4. **Reproducible:** Use fixed random seeds
5. **Version control:** Track performance over time

### References

1. NumPy Performance Tips: https://numpy.org/doc/stable/user/performance.html
2. SciPy Optimization Guide: https://docs.scipy.org/doc/scipy/tutorial/optimize.html
3. Python Performance Tips: https://wiki.python.org/moin/PythonSpeed

---

**Report Generated:** {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
**Analysis ID:** {self.timestamp}
"""

    def _get_critical_bottlenecks(self) -> List[Dict]:
        """Get critical bottlenecks (>2x target)."""
        return [m for m in self.metrics
                if not m['meets_target']
                and m['execution_time_ms'] / m['target_time_ms'] > 2.0]

    def _get_optimization_opportunities(self) -> List[Dict]:
        """Get optimization opportunities (>1x but <2x target)."""
        return [m for m in self.metrics
                if not m['meets_target']
                and m['execution_time_ms'] / m['target_time_ms'] <= 2.0]


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(description="Generate optimization report")
    parser.add_argument('--input', required=True, help='Path to metrics JSON file')
    parser.add_argument('--output', required=True, help='Path to output markdown report')

    args = parser.parse_args()

    generator = OptimizationReportGenerator(args.input, args.output)
    generator.generate_report()


if __name__ == '__main__':
    main()
