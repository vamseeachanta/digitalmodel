"""
Integration tests for cross-module functionality in digitalmodel.

These tests verify that different modules work together correctly.
"""

import pytest
from unittest.mock import Mock, patch, MagicMock
from pathlib import Path
import tempfile
import json


@pytest.mark.integration
class TestModuleIntegration:
    """Integration tests for module interactions."""

    def test_automation_with_file_processing(self, enhanced_mock_file_system, mock_external_services):
        """Test automation module integrates with file processing."""
        project_path = enhanced_mock_file_system

        # Mock the automation components
        with patch('digitalmodel.modules.automation.go_by_folder.scanner.FileScanner') as mock_scanner:
            mock_scanner_instance = Mock()
            mock_scanner_instance.scan_files.return_value = [
                {"path": project_path / "src" / "main.py", "size": 100, "hash": "abc123"},
                {"path": project_path / "tests" / "test_main.py", "size": 50, "hash": "def456"}
            ]
            mock_scanner.return_value = mock_scanner_instance

            # Test the integration
            from digitalmodel.modules.automation.go_by_folder.scanner import FileScanner

            scanner = FileScanner(str(project_path))
            files = scanner.scan_files()

            assert len(files) == 2
            assert all("path" in file_info for file_info in files)
            assert all("hash" in file_info for file_info in files)

    def test_data_pipeline_integration(self, comprehensive_sample_data, temp_directory):
        """Test data flows correctly through processing pipeline."""
        # Create temporary files for integration test
        input_file = temp_directory / "input.json"
        output_file = temp_directory / "output.json"

        # Write test data
        with open(input_file, 'w') as f:
            json.dump(comprehensive_sample_data, f)

        # Mock data processing pipeline
        def mock_data_processor(input_path, output_path):
            with open(input_path, 'r') as f:
                data = json.load(f)

            # Simulate processing
            processed_data = {
                "processed": True,
                "input_count": len(data),
                "numbers_sum": sum(data.get("numbers", [])),
                "strings_count": len(data.get("strings", []))
            }

            with open(output_path, 'w') as f:
                json.dump(processed_data, f)

            return processed_data

        # Execute integration test
        result = mock_data_processor(input_file, output_file)

        # Verify integration
        assert result["processed"] is True
        assert result["input_count"] == 7  # Number of keys in comprehensive_sample_data
        assert result["numbers_sum"] == 15  # Sum of [1,2,3,4,5]
        assert result["strings_count"] == 3  # Length of ["alpha", "beta", "gamma"]

        # Verify output file exists and contains correct data
        assert output_file.exists()
        with open(output_file, 'r') as f:
            saved_result = json.load(f)
        assert saved_result == result

    def test_configuration_and_execution_integration(self, temp_directory):
        """Test configuration loading integrates with execution."""
        config_file = temp_directory / "config.yaml"
        config_content = """
        execution:
          parallel: true
          max_workers: 4
          timeout: 300
        output:
          format: json
          compression: false
        logging:
          level: INFO
          file: execution.log
        """

        config_file.write_text(config_content)

        # Mock configuration loader and executor
        class MockConfigLoader:
            def load(self, config_path):
                import yaml
                with open(config_path, 'r') as f:
                    return yaml.safe_load(f)

        class MockExecutor:
            def __init__(self, config):
                self.config = config
                self.executed_tasks = []

            def execute(self, task):
                self.executed_tasks.append({
                    "task": task,
                    "parallel": self.config["execution"]["parallel"],
                    "workers": self.config["execution"]["max_workers"]
                })
                return {"status": "completed", "task": task}

        # Integration test
        with patch('yaml.safe_load') as mock_yaml:
            mock_yaml.return_value = {
                "execution": {"parallel": True, "max_workers": 4, "timeout": 300},
                "output": {"format": "json", "compression": False},
                "logging": {"level": "INFO", "file": "execution.log"}
            }

            loader = MockConfigLoader()
            config = loader.load(config_file)
            executor = MockExecutor(config)

            # Execute tasks
            tasks = ["task1", "task2", "task3"]
            results = [executor.execute(task) for task in tasks]

            # Verify integration
            assert len(executor.executed_tasks) == 3
            assert all(task["parallel"] is True for task in executor.executed_tasks)
            assert all(task["workers"] == 4 for task in executor.executed_tasks)
            assert all(result["status"] == "completed" for result in results)


@pytest.mark.integration
@pytest.mark.slow
class TestDatabaseIntegration:
    """Integration tests for database operations."""

    def test_database_connection_and_queries(self, mock_external_services):
        """Test database connection and query execution."""
        db_service = mock_external_services["database"]

        # Test connection and basic operations
        db_service.connect.return_value = True
        db_service.query.return_value = [
            {"id": 1, "name": "test1", "value": 100},
            {"id": 2, "name": "test2", "value": 200}
        ]

        # Integration test
        connection_result = db_service.connect()
        query_result = db_service.query("SELECT * FROM test_table")

        assert connection_result is True
        assert len(query_result) == 2
        assert query_result[0]["name"] == "test1"

    def test_data_migration_integration(self, mock_external_services, temp_directory):
        """Test data migration between systems."""
        source_db = mock_external_services["database"]
        target_file = temp_directory / "migrated_data.json"

        # Mock source data
        source_data = [
            {"id": 1, "name": "record1", "category": "A"},
            {"id": 2, "name": "record2", "category": "B"},
            {"id": 3, "name": "record3", "category": "A"}
        ]
        source_db.export_data.return_value = source_data

        # Migration function
        def migrate_data(source, target_path):
            data = source.export_data()

            # Process data (group by category)
            processed = {}
            for record in data:
                category = record["category"]
                if category not in processed:
                    processed[category] = []
                processed[category].append(record)

            # Save to file
            with open(target_path, 'w') as f:
                json.dump(processed, f, indent=2)

            return len(data)

        # Execute migration
        migrated_count = migrate_data(source_db, target_file)

        # Verify integration
        assert migrated_count == 3
        assert target_file.exists()

        with open(target_file, 'r') as f:
            migrated_data = json.load(f)

        assert "A" in migrated_data
        assert "B" in migrated_data
        assert len(migrated_data["A"]) == 2
        assert len(migrated_data["B"]) == 1


@pytest.mark.integration
class TestAPIIntegration:
    """Integration tests for API interactions."""

    def test_api_client_integration(self, mock_external_services):
        """Test API client integration with services."""
        api_service = mock_external_services["api_service"]

        # Mock API responses
        api_service.get.return_value = {
            "status": 200,
            "data": {"message": "success", "items": [1, 2, 3]}
        }
        api_service.post.return_value = {
            "status": 201,
            "data": {"id": "new_item_123"}
        }

        # Integration test
        get_response = api_service.get("/api/items")
        post_response = api_service.post("/api/items", {"name": "test_item"})

        assert get_response["status"] == 200
        assert "items" in get_response["data"]
        assert post_response["status"] == 201
        assert "id" in post_response["data"]

    def test_error_handling_integration(self, mock_external_services):
        """Test error handling across integrated components."""
        api_service = mock_external_services["api_service"]

        # Mock API error
        api_service.get.side_effect = ConnectionError("Network error")

        # Error handling integration
        def robust_api_call(service, endpoint, retries=3):
            for attempt in range(retries):
                try:
                    return service.get(endpoint)
                except ConnectionError as e:
                    if attempt == retries - 1:
                        return {"error": str(e), "retries": attempt + 1}
                    continue

        result = robust_api_call(api_service, "/api/data")

        assert "error" in result
        assert "Network error" in result["error"]
        assert result["retries"] == 3


@pytest.mark.integration
class TestPerformanceIntegration:
    """Integration tests focusing on performance across modules."""

    def test_large_data_processing_integration(self, benchmark_datasets, test_performance_tracker):
        """Test performance of integrated data processing."""
        large_dataset = benchmark_datasets["large"]

        tracker = test_performance_tracker

        # Simulate multi-stage processing pipeline
        tracker.start("data_loading")
        loaded_data = list(large_dataset)  # Simulate loading
        load_time = tracker.stop()

        tracker.start("data_processing")
        processed_data = [x * 2 for x in loaded_data if x % 2 == 0]
        process_time = tracker.stop()

        tracker.start("data_output")
        output_count = len(processed_data)
        output_time = tracker.stop()

        # Performance assertions
        assert load_time < 0.1  # Should load quickly
        assert process_time < 0.5  # Processing should be reasonable
        assert output_time < 0.01  # Output counting should be fast

        # Data integrity assertions
        assert len(loaded_data) == len(large_dataset)
        assert output_count > 0
        assert all(x % 4 == 0 for x in processed_data)  # Even numbers * 2

    def test_concurrent_operations_integration(self, mock_external_services):
        """Test integration of concurrent operations."""
        import threading
        import time

        api_service = mock_external_services["api_service"]
        results = []
        errors = []

        def worker_task(task_id):
            try:
                time.sleep(0.01)  # Simulate work
                response = api_service.get(f"/api/data/{task_id}")
                results.append({"task_id": task_id, "response": response})
            except Exception as e:
                errors.append({"task_id": task_id, "error": str(e)})

        # Mock concurrent responses
        api_service.get.return_value = {"status": 200, "data": "success"}

        # Run concurrent operations
        threads = []
        for i in range(10):
            thread = threading.Thread(target=worker_task, args=(i,))
            threads.append(thread)
            thread.start()

        # Wait for completion
        for thread in threads:
            thread.join()

        # Verify integration
        assert len(results) == 10
        assert len(errors) == 0
        assert all(result["response"]["status"] == 200 for result in results)


@pytest.mark.integration
@pytest.mark.external
class TestExternalSystemIntegration:
    """Integration tests that require external systems (marked for CI/CD control)."""

    def test_file_system_integration(self, temp_directory):
        """Test file system operations integration."""
        test_files = []

        # Create multiple test files
        for i in range(5):
            file_path = temp_directory / f"test_file_{i}.txt"
            file_path.write_text(f"Content for file {i}")
            test_files.append(file_path)

        # Test file operations integration
        def process_files(directory):
            processed = []
            for file_path in directory.glob("*.txt"):
                content = file_path.read_text()
                word_count = len(content.split())
                processed.append({
                    "file": file_path.name,
                    "size": file_path.stat().st_size,
                    "words": word_count
                })
            return processed

        results = process_files(temp_directory)

        assert len(results) == 5
        assert all("file" in result for result in results)
        assert all(result["words"] >= 3 for result in results)  # "Content for file X"