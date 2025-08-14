"""
Test client for OrcaFlex Browser API
"""

import requests
import json
from datetime import datetime
import time


class APITestClient:
    """Test client for OrcaFlex API"""
    
    def __init__(self, base_url="http://localhost:8000"):
        self.base_url = base_url
        self.session = requests.Session()
    
    def test_root(self):
        """Test root endpoint"""
        print("\n1. Testing root endpoint...")
        response = self.session.get(f"{self.base_url}/")
        
        if response.status_code == 200:
            data = response.json()
            print(f"   [OK] API Online: {data['name']} v{data['version']}")
            print(f"   Base Path: {data['base_path']}")
            return True
        else:
            print(f"   [FAIL] Failed: {response.status_code}")
            return False
    
    def test_health(self):
        """Test health check"""
        print("\n2. Testing health check...")
        response = self.session.get(f"{self.base_url}/health")
        
        if response.status_code == 200:
            data = response.json()
            print(f"   [OK] Status: {data['status']}")
            print(f"   File Processor: {data['file_processor']}")
            return True
        else:
            print(f"   [FAIL] Failed: {response.status_code}")
            return False
    
    def test_get_files(self):
        """Test file listing"""
        print("\n3. Testing file listing...")
        response = self.session.get(
            f"{self.base_url}/api/files",
            params={"pattern": "dm_*_strut_dyn.csv", "limit": 10}
        )
        
        if response.status_code == 200:
            files = response.json()
            print(f"   [OK] Found {len(files)} files")
            if files:
                print(f"   First file: {files[0]['filename']}")
                print(f"   Size: {files[0]['size_kb']:.1f} KB")
            return files
        else:
            print(f"   [FAIL] Failed: {response.status_code}")
            return []
    
    def test_analyze(self):
        """Test analysis endpoint"""
        print("\n4. Testing analysis...")
        
        payload = {
            "file_pattern": "dm_*_strut_dyn.csv",
            "max_files": 10,
            "force_refresh": True
        }
        
        response = self.session.post(
            f"{self.base_url}/api/analyze",
            json=payload
        )
        
        if response.status_code == 200:
            data = response.json()
            print(f"   [OK] Analysis complete")
            print(f"   Files analyzed: {data['files_analyzed']}")
            print(f"   Struts found: {data['struts_found']}")
            print(f"   Max tension: {data['absolute_max']:.2f}")
            print(f"   Min tension: {data['absolute_min']:.2f}")
            
            if data['critical_case']:
                critical = data['critical_case']
                print(f"\n   Critical Case:")
                print(f"   - Value: {critical['value']:.2f}")
                print(f"   - Strut: {critical['strut']}")
                print(f"   - FE File: {critical['fe_filename'].split('/')[-1]}")
            
            return data
        else:
            print(f"   [FAIL] Failed: {response.status_code}")
            if response.text:
                print(f"   Error: {response.text}")
            return None
    
    def test_critical(self):
        """Test critical case endpoint"""
        print("\n5. Testing critical case...")
        response = self.session.get(f"{self.base_url}/api/critical")
        
        if response.status_code == 200:
            critical = response.json()
            if critical:
                print(f"   [OK] Critical case found")
                print(f"   Value: {critical['value']:.2f}")
                print(f"   Strut: {critical['strut']}")
                
                if critical['metadata']:
                    print(f"   Metadata:")
                    for key, value in critical['metadata'].items():
                        if value:
                            print(f"     - {key}: {value}")
            return critical
        else:
            print(f"   [FAIL] Failed: {response.status_code}")
            return None
    
    def test_timeseries(self, fe_file="fsts_l015_hwl_ncl_240deg.sim"):
        """Test time series endpoint"""
        print(f"\n6. Testing time series for {fe_file}...")
        
        params = {
            "strut_id": 7,
            "start_time": 0,
            "end_time": 100,
            "downsample": 50
        }
        
        response = self.session.get(
            f"{self.base_url}/api/timeseries/{fe_file}",
            params=params
        )
        
        if response.status_code == 200:
            data = response.json()
            print(f"   [OK] Time series retrieved")
            print(f"   Time points: {data['time_points']}")
            print(f"   Max value: {data['max_value']:.2f}")
            print(f"   Max at time: {data['max_time']:.1f}s")
            print(f"   Mean: {data['mean_value']:.2f}")
            print(f"   Std Dev: {data['std_dev']:.2f}")
            return data
        else:
            print(f"   [FAIL] Failed: {response.status_code}")
            return None
    
    def test_metadata(self):
        """Test metadata categories"""
        print("\n7. Testing metadata categories...")
        response = self.session.get(f"{self.base_url}/api/metadata/categories")
        
        if response.status_code == 200:
            categories = response.json()
            print(f"   [OK] Found {len(categories)} categories")
            for cat in categories:
                print(f"   - {cat['name']}: {len(cat['options'])} options")
            return categories
        else:
            print(f"   [FAIL] Failed: {response.status_code}")
            return []
    
    def test_stats(self):
        """Test statistics endpoint"""
        print("\n8. Testing statistics...")
        response = self.session.get(f"{self.base_url}/api/stats")
        
        if response.status_code == 200:
            stats = response.json()
            print(f"   [OK] Statistics retrieved")
            print(f"   Cache status: {stats['cache_status']}")
            if 'summary' in stats:
                print(f"   Files analyzed: {stats['summary']['files_analyzed']}")
                print(f"   Absolute max: {stats['summary']['absolute_max']:.2f}")
            return stats
        else:
            print(f"   [FAIL] Failed: {response.status_code}")
            return None
    
    def test_browse(self):
        """Test browse endpoint"""
        print("\n9. Testing browse with filters...")
        
        payload = {
            "lng_loading": ["15%"],
            "tide_levels": ["hwl"],
            "min_tension": 5000
        }
        
        response = self.session.post(
            f"{self.base_url}/api/browse",
            json=payload
        )
        
        if response.status_code == 200:
            data = response.json()
            print(f"   [OK] Browse results")
            print(f"   Total files: {data['total_files']}")
            print(f"   Filtered: {data['filtered_files']}")
            return data
        else:
            print(f"   [FAIL] Failed: {response.status_code}")
            return None
    
    def run_all_tests(self):
        """Run all tests"""
        print("=" * 60)
        print("OrcaFlex Browser API Test Suite")
        print("=" * 60)
        print(f"Testing API at: {self.base_url}")
        print(f"Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        
        # Check if API is running
        try:
            self.test_root()
        except requests.exceptions.ConnectionError:
            print("\n[ERROR] API is not running!")
            print("Please start the API server with: python main.py")
            return False
        
        # Run tests
        self.test_health()
        files = self.test_get_files()
        
        if files:
            analysis = self.test_analyze()
            
            if analysis:
                self.test_critical()
                
                if analysis['critical_case']:
                    fe_file = analysis['critical_case']['fe_filename'].split('/')[-1]
                    self.test_timeseries(fe_file)
        
        self.test_metadata()
        self.test_stats()
        self.test_browse()
        
        print("\n" + "=" * 60)
        print("[SUCCESS] All tests completed!")
        print("=" * 60)
        
        return True


def main():
    """Main test runner"""
    import argparse
    
    parser = argparse.ArgumentParser(description="Test OrcaFlex Browser API")
    parser.add_argument('--url', default='http://localhost:8000', 
                       help='API base URL')
    parser.add_argument('--test', help='Run specific test')
    
    args = parser.parse_args()
    
    client = APITestClient(args.url)
    
    if args.test:
        # Run specific test
        test_method = getattr(client, f"test_{args.test}", None)
        if test_method:
            test_method()
        else:
            print(f"Unknown test: {args.test}")
            print("Available tests: root, health, get_files, analyze, critical, timeseries, metadata, stats, browse")
    else:
        # Run all tests
        client.run_all_tests()


if __name__ == "__main__":
    main()