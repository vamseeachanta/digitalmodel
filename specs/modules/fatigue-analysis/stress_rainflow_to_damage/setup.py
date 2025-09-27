"""
Setup configuration for Fatigue Analysis Module
"""
from setuptools import setup, find_packages

with open("README.md", "r", encoding="utf-8") as fh:
    long_description = fh.read()

setup(
    name="fatigue-analysis-module",
    version="1.0.1",
    author="Engineering Analysis Division",
    author_email="engineering@company.com",
    description="Stress rainflow counting to fatigue damage analysis module",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/company/digitalmodel",
    packages=find_packages(),
    classifiers=[
        "Development Status :: 5 - Production/Stable",
        "Intended Audience :: Engineers",
        "Topic :: Scientific/Engineering :: Structural Analysis",
        "License :: Proprietary",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Operating System :: OS Independent",
    ],
    python_requires=">=3.8",
    install_requires=[
        "pandas>=1.3.0",
        "numpy>=1.21.0",
        "matplotlib>=3.4.0",
        "pyyaml>=5.4.0",
        "tqdm>=4.62.0",
    ],
    extras_require={
        "dev": [
            "pytest>=6.2.0",
            "pytest-cov>=2.12.0",
            "black>=21.6b0",
            "flake8>=3.9.0",
            "mypy>=0.910",
        ],
        "monitoring": [
            "schedule>=1.1.0",
            "watchdog>=2.1.0",
        ],
    },
    entry_points={
        "console_scripts": [
            "fatigue-analyze=run_damage_analysis:main",
            "fatigue-monitor=monitor_fatigue_health:main",
        ],
    },
    package_data={
        "": ["*.yml", "*.yaml", "*.json", "*.md"],
    },
    include_package_data=True,
)