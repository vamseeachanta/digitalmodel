#!/usr/bin/env python
"""
Test script to verify the examples integration infrastructure.

This script performs basic checks to ensure all components are properly set up.
"""

import os
import sys
from pathlib import Path
import logging

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


def check_directories():
    """Check if required directories exist."""
    base_dir = Path("docs/modules/orcaflex/examples")
    required_dirs = [
        base_dir / "raw",
        base_dir / "yaml", 
        base_dir / "metadata",
        base_dir / "reports",
        base_dir / "catalog"
    ]
    
    logger.info("Checking directory structure...")
    all_exist = True
    
    for dir_path in required_dirs:
        if dir_path.exists():
            logger.info(f"✓ {dir_path} exists")
        else:
            logger.error(f"✗ {dir_path} missing")
            all_exist = False
    
    return all_exist


def check_imports():
    """Check if all required modules can be imported."""
    logger.info("\nChecking module imports...")
    imports_ok = True
    
    required_modules = [
        ('requests', 'Web requests'),
        ('bs4', 'BeautifulSoup for HTML parsing'),
        ('yaml', 'YAML processing'),
        ('tqdm', 'Progress bars'),
    ]
    
    for module_name, description in required_modules:
        try:
            __import__(module_name)
            logger.info(f"✓ {module_name} ({description}) - OK")
        except ImportError as e:
            logger.error(f"✗ {module_name} ({description}) - FAILED: {e}")
            imports_ok = False
    
    return imports_ok


def check_downloader():
    """Check if downloader module can be instantiated."""
    logger.info("\nChecking downloader module...")
    
    try:
        from digitalmodel.orcaflex.examples_integration.downloader import OrcaflexExampleDownloader
        
        # Try to create an instance
        downloader = OrcaflexExampleDownloader()
        logger.info("✓ Downloader module initialized successfully")
        
        # Check manifest
        if hasattr(downloader, 'manifest'):
            logger.info("✓ Manifest system working")
        
        # Check session
        if hasattr(downloader, 'session'):
            logger.info("✓ HTTP session configured")
        
        return True
        
    except Exception as e:
        logger.error(f"✗ Failed to initialize downloader: {e}")
        return False


def check_network_connectivity():
    """Check basic network connectivity."""
    logger.info("\nChecking network connectivity...")
    
    try:
        import requests
        response = requests.get("https://www.orcina.com", timeout=5)
        if response.status_code == 200:
            logger.info("✓ Can reach Orcina website")
            return True
        else:
            logger.warning(f"⚠ Orcina website returned status {response.status_code}")
            return True  # Still considered OK if we got a response
    except Exception as e:
        logger.warning(f"⚠ Cannot reach Orcina website: {e}")
        logger.info("  (This is OK for offline development)")
        return True  # Don't fail the test for network issues


def run_infrastructure_test():
    """Run all infrastructure tests."""
    logger.info("=" * 60)
    logger.info("OrcaFlex Examples Integration - Infrastructure Test")
    logger.info("=" * 60)
    
    tests = [
        ("Directory Structure", check_directories),
        ("Python Imports", check_imports),
        ("Downloader Module", check_downloader),
        ("Network Connectivity", check_network_connectivity)
    ]
    
    results = {}
    for test_name, test_func in tests:
        try:
            results[test_name] = test_func()
        except Exception as e:
            logger.error(f"Test '{test_name}' failed with exception: {e}")
            results[test_name] = False
    
    # Summary
    logger.info("\n" + "=" * 60)
    logger.info("Test Summary")
    logger.info("=" * 60)
    
    all_passed = True
    for test_name, passed in results.items():
        status = "PASSED" if passed else "FAILED"
        symbol = "✓" if passed else "✗"
        logger.info(f"{symbol} {test_name}: {status}")
        if not passed:
            all_passed = False
    
    logger.info("=" * 60)
    
    if all_passed:
        logger.info("✓ All infrastructure tests passed!")
        logger.info("The examples integration system is ready to use.")
    else:
        logger.warning("⚠ Some tests failed. Please check the logs above.")
        logger.info("You may need to:")
        logger.info("  1. Create missing directories")
        logger.info("  2. Install missing dependencies: pip install beautifulsoup4 requests pyyaml tqdm")
    
    return all_passed


if __name__ == "__main__":
    # Change to project root if needed
    project_root = Path(__file__).parent.parent.parent.parent.parent
    
    # Verify we have the correct project root
    if (project_root / 'pyproject.toml').exists():
        os.chdir(project_root)
        logger.info(f"Working directory: {os.getcwd()}")
        # Add src to path for imports
        sys.path.insert(0, str(project_root / 'src'))
    else:
        # Try alternative path resolution
        for _ in range(5):
            if (Path.cwd() / 'pyproject.toml').exists():
                break
            os.chdir('..')
        logger.info(f"Working directory: {os.getcwd()}")
        sys.path.insert(0, str(Path.cwd() / 'src'))
    
    success = run_infrastructure_test()
    sys.exit(0 if success else 1)