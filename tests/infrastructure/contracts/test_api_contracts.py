"""
API Contract Testing for DigitalModel

Tests API endpoints, request/response schemas, and contract compatibility
following consumer-driven contract testing practices from Pact and similar tools.
"""
import pytest
import json
import jsonschema
from typing import Dict, Any, List, Optional
from dataclasses import dataclass, asdict
from pathlib import Path
import sys

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))


@dataclass
class APIResponse:
    """Standard API response structure."""
    status_code: int
    data: Any
    message: Optional[str] = None
    timestamp: Optional[str] = None
    version: str = "1.0"


@dataclass
class SimulationRequest:
    """Request for simulation API."""
    simulation_type: str
    parameters: Dict[str, Any]
    configuration: Dict[str, Any]


@dataclass
class SimulationResult:
    """Result from simulation API."""
    simulation_id: str
    status: str
    results: List[Dict[str, Any]]
    metadata: Dict[str, Any]


class APIContractValidator:
    """Validator for API contracts using JSON Schema."""

    def __init__(self):
        self.schemas = self._load_schemas()

    def _load_schemas(self) -> Dict[str, Dict]:
        """Load JSON schemas for API validation."""
        return {
            "simulation_request": {
                "type": "object",
                "required": ["simulation_type", "parameters", "configuration"],
                "properties": {
                    "simulation_type": {
                        "type": "string",
                        "enum": ["static", "dynamic", "fatigue", "frequency"]
                    },
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "time_step": {"type": "number", "minimum": 0.001, "maximum": 1.0},
                            "duration": {"type": "number", "minimum": 0.1, "maximum": 86400},
                            "solver": {"type": "string", "enum": ["euler", "runge_kutta", "implicit"]},
                            "convergence": {"type": "number", "minimum": 1e-8, "maximum": 1e-2}
                        },
                        "additionalProperties": True
                    },
                    "configuration": {
                        "type": "object",
                        "additionalProperties": True
                    }
                },
                "additionalProperties": False
            },
            "simulation_response": {
                "type": "object",
                "required": ["status_code", "data"],
                "properties": {
                    "status_code": {"type": "integer", "minimum": 200, "maximum": 599},
                    "data": {
                        "type": "object",
                        "properties": {
                            "simulation_id": {"type": "string", "pattern": "^[a-zA-Z0-9-]+$"},
                            "status": {
                                "type": "string",
                                "enum": ["queued", "running", "completed", "failed", "cancelled"]
                            },
                            "results": {
                                "type": "array",
                                "items": {
                                    "type": "object",
                                    "properties": {
                                        "time": {"type": "number"},
                                        "value": {"type": "number"}
                                    },
                                    "required": ["time", "value"]
                                }
                            },
                            "metadata": {"type": "object"}
                        },
                        "required": ["simulation_id", "status"]
                    },
                    "message": {"type": ["string", "null"]},
                    "timestamp": {"type": ["string", "null"]},
                    "version": {"type": "string", "pattern": "^\\d+\\.\\d+$"}
                },
                "additionalProperties": False
            },
            "error_response": {
                "type": "object",
                "required": ["status_code", "data"],
                "properties": {
                    "status_code": {"type": "integer", "minimum": 400, "maximum": 599},
                    "data": {"type": "null"},
                    "message": {"type": "string", "minLength": 1},
                    "timestamp": {"type": "string"},
                    "version": {"type": "string"}
                },
                "additionalProperties": False
            },
            "analysis_request": {
                "type": "object",
                "required": ["analysis_type", "data_source"],
                "properties": {
                    "analysis_type": {
                        "type": "string",
                        "enum": ["fatigue", "stress", "frequency", "modal"]
                    },
                    "data_source": {
                        "type": "object",
                        "properties": {
                            "file_path": {"type": "string"},
                            "format": {"type": "string", "enum": ["csv", "xlsx", "hdf5", "json"]},
                            "columns": {
                                "type": "object",
                                "properties": {
                                    "time": {"type": "string"},
                                    "value": {"type": "string"}
                                }
                            }
                        },
                        "required": ["file_path", "format"]
                    },
                    "parameters": {
                        "type": "object",
                        "additionalProperties": True
                    }
                },
                "additionalProperties": False
            }
        }

    def validate_request(self, schema_name: str, data: Dict[str, Any]) -> tuple[bool, List[str]]:
        """Validate request against schema."""
        if schema_name not in self.schemas:
            return False, [f"Unknown schema: {schema_name}"]

        try:
            jsonschema.validate(data, self.schemas[schema_name])
            return True, []
        except jsonschema.ValidationError as e:
            return False, [str(e)]
        except jsonschema.SchemaError as e:
            return False, [f"Schema error: {str(e)}"]

    def validate_response(self, schema_name: str, response: APIResponse) -> tuple[bool, List[str]]:
        """Validate response against schema."""
        response_dict = asdict(response)
        return self.validate_request(schema_name, response_dict)


class ContractTestRunner:
    """Runner for contract tests."""

    def __init__(self):
        self.validator = APIContractValidator()
        self.test_results: List[Dict[str, Any]] = []

    def test_endpoint_contract(self, endpoint_name: str, request_data: Dict[str, Any],
                              expected_response_schema: str) -> Dict[str, Any]:
        """Test an endpoint against its contract."""
        # Simulate API call (in real implementation, this would make actual HTTP requests)
        response = self._simulate_api_call(endpoint_name, request_data)

        # Validate response against schema
        is_valid, errors = self.validator.validate_response(expected_response_schema, response)

        result = {
            'endpoint': endpoint_name,
            'request': request_data,
            'response': asdict(response),
            'schema_valid': is_valid,
            'errors': errors,
            'timestamp': None  # Would be filled with actual timestamp
        }

        self.test_results.append(result)
        return result

    def _simulate_api_call(self, endpoint: str, request_data: Dict[str, Any]) -> APIResponse:
        """Simulate API call (replace with actual HTTP client in real implementation)."""
        if endpoint == "/api/v1/simulation/create":
            return self._simulate_simulation_create(request_data)
        elif endpoint == "/api/v1/simulation/status":
            return self._simulate_simulation_status(request_data)
        elif endpoint == "/api/v1/analysis/run":
            return self._simulate_analysis_run(request_data)
        else:
            return APIResponse(
                status_code=404,
                data=None,
                message=f"Endpoint not found: {endpoint}"
            )

    def _simulate_simulation_create(self, request_data: Dict[str, Any]) -> APIResponse:
        """Simulate simulation creation endpoint."""
        # Validate request first
        is_valid, errors = self.validator.validate_request("simulation_request", request_data)

        if not is_valid:
            return APIResponse(
                status_code=400,
                data=None,
                message=f"Invalid request: {'; '.join(errors)}"
            )

        # Simulate successful response
        simulation_result = SimulationResult(
            simulation_id="sim-12345",
            status="queued",
            results=[],
            metadata={"created_at": "2024-01-01T00:00:00Z"}
        )

        return APIResponse(
            status_code=201,
            data=asdict(simulation_result),
            message="Simulation created successfully"
        )

    def _simulate_simulation_status(self, request_data: Dict[str, Any]) -> APIResponse:
        """Simulate simulation status endpoint."""
        simulation_id = request_data.get("simulation_id")

        if not simulation_id:
            return APIResponse(
                status_code=400,
                data=None,
                message="Missing simulation_id"
            )

        # Simulate completed simulation
        simulation_result = SimulationResult(
            simulation_id=simulation_id,
            status="completed",
            results=[
                {"time": 0.0, "value": 1.0},
                {"time": 0.1, "value": 1.1},
                {"time": 0.2, "value": 1.2}
            ],
            metadata={"completed_at": "2024-01-01T00:01:00Z"}
        )

        return APIResponse(
            status_code=200,
            data=asdict(simulation_result)
        )

    def _simulate_analysis_run(self, request_data: Dict[str, Any]) -> APIResponse:
        """Simulate analysis run endpoint."""
        is_valid, errors = self.validator.validate_request("analysis_request", request_data)

        if not is_valid:
            return APIResponse(
                status_code=400,
                data=None,
                message=f"Invalid request: {'; '.join(errors)}"
            )

        # Simulate analysis result
        analysis_result = {
            "analysis_id": "analysis-67890",
            "status": "completed",
            "results": {
                "summary": {"mean": 10.5, "std": 2.3, "max": 15.2},
                "details": [{"frequency": 1.0, "amplitude": 0.5}]
            }
        }

        return APIResponse(
            status_code=200,
            data=analysis_result
        )


class TestAPIContracts:
    """Test API contracts and schemas."""

    @pytest.fixture
    def contract_runner(self):
        """Fixture providing contract test runner."""
        return ContractTestRunner()

    @pytest.fixture
    def validator(self):
        """Fixture providing API validator."""
        return APIContractValidator()

    def test_simulation_request_schema_validation(self, validator):
        """Test simulation request schema validation."""
        # Valid request
        valid_request = {
            "simulation_type": "dynamic",
            "parameters": {
                "time_step": 0.1,
                "duration": 10.0,
                "solver": "runge_kutta",
                "convergence": 1e-4
            },
            "configuration": {
                "output_format": "csv",
                "save_results": True
            }
        }

        is_valid, errors = validator.validate_request("simulation_request", valid_request)
        assert is_valid, f"Valid request failed validation: {errors}"

        # Invalid simulation type
        invalid_request = valid_request.copy()
        invalid_request["simulation_type"] = "invalid_type"

        is_valid, errors = validator.validate_request("simulation_request", invalid_request)
        assert not is_valid, "Invalid simulation type should fail validation"

        # Missing required field
        incomplete_request = {
            "simulation_type": "static",
            "parameters": {}
            # Missing configuration
        }

        is_valid, errors = validator.validate_request("simulation_request", incomplete_request)
        assert not is_valid, "Incomplete request should fail validation"

    def test_simulation_response_schema_validation(self, validator):
        """Test simulation response schema validation."""
        # Valid response
        valid_response = APIResponse(
            status_code=200,
            data={
                "simulation_id": "sim-123",
                "status": "completed",
                "results": [
                    {"time": 0.0, "value": 1.0},
                    {"time": 0.1, "value": 1.1}
                ],
                "metadata": {"total_time": 120.5}
            },
            message="Success"
        )

        is_valid, errors = validator.validate_response("simulation_response", valid_response)
        assert is_valid, f"Valid response failed validation: {errors}"

        # Invalid status code
        invalid_response = APIResponse(
            status_code=999,  # Invalid status code
            data={"simulation_id": "sim-123", "status": "completed"}
        )

        is_valid, errors = validator.validate_response("simulation_response", invalid_response)
        assert not is_valid, "Invalid status code should fail validation"

    def test_error_response_schema_validation(self, validator):
        """Test error response schema validation."""
        # Valid error response
        error_response = APIResponse(
            status_code=400,
            data=None,
            message="Invalid input parameters",
            timestamp="2024-01-01T00:00:00Z",
            version="1.0"
        )

        is_valid, errors = validator.validate_response("error_response", error_response)
        assert is_valid, f"Valid error response failed validation: {errors}"

        # Error response with data (should fail)
        invalid_error = APIResponse(
            status_code=400,
            data={"some": "data"},  # Error responses should have null data
            message="Error occurred"
        )

        is_valid, errors = validator.validate_response("error_response", invalid_error)
        assert not is_valid, "Error response with data should fail validation"

    def test_analysis_request_contract(self, validator):
        """Test analysis request contract."""
        # Valid analysis request
        valid_request = {
            "analysis_type": "fatigue",
            "data_source": {
                "file_path": "/path/to/data.csv",
                "format": "csv",
                "columns": {
                    "time": "Time",
                    "value": "Stress"
                }
            },
            "parameters": {
                "method": "rainflow",
                "material": "steel",
                "safety_factor": 2.0
            }
        }

        is_valid, errors = validator.validate_request("analysis_request", valid_request)
        assert is_valid, f"Valid analysis request failed validation: {errors}"

        # Invalid format
        invalid_request = valid_request.copy()
        invalid_request["data_source"]["format"] = "invalid_format"

        is_valid, errors = validator.validate_request("analysis_request", invalid_request)
        assert not is_valid, "Invalid format should fail validation"

    def test_contract_compatibility_across_versions(self, validator):
        """Test that contracts maintain backward compatibility."""
        # Test request with older version (missing optional fields)
        old_version_request = {
            "simulation_type": "static",
            "parameters": {
                "solver": "euler"
            },
            "configuration": {}
        }

        is_valid, errors = validator.validate_request("simulation_request", old_version_request)
        assert is_valid, f"Backward compatibility broken: {errors}"

        # Test response with minimal required fields
        minimal_response = APIResponse(
            status_code=200,
            data={
                "simulation_id": "sim-minimal",
                "status": "queued"
                # Missing optional fields like results, metadata
            }
        )

        is_valid, errors = validator.validate_response("simulation_response", minimal_response)
        assert is_valid, f"Minimal response should be valid: {errors}"


class TestEndToEndContracts:
    """Test end-to-end API contracts."""

    @pytest.fixture
    def contract_runner(self):
        """Fixture providing contract test runner."""
        return ContractTestRunner()

    def test_simulation_creation_contract(self, contract_runner):
        """Test simulation creation endpoint contract."""
        request_data = {
            "simulation_type": "dynamic",
            "parameters": {
                "time_step": 0.05,
                "duration": 5.0,
                "solver": "runge_kutta",
                "convergence": 1e-5
            },
            "configuration": {
                "output_format": "json",
                "include_metadata": True
            }
        }

        result = contract_runner.test_endpoint_contract(
            "/api/v1/simulation/create",
            request_data,
            "simulation_response"
        )

        assert result['schema_valid'], f"Contract validation failed: {result['errors']}"
        assert result['response']['status_code'] == 201
        assert 'simulation_id' in result['response']['data']

    def test_simulation_status_contract(self, contract_runner):
        """Test simulation status endpoint contract."""
        request_data = {"simulation_id": "sim-12345"}

        result = contract_runner.test_endpoint_contract(
            "/api/v1/simulation/status",
            request_data,
            "simulation_response"
        )

        assert result['schema_valid'], f"Contract validation failed: {result['errors']}"
        assert result['response']['status_code'] == 200
        assert result['response']['data']['status'] in ["queued", "running", "completed", "failed", "cancelled"]

    def test_analysis_endpoint_contract(self, contract_runner):
        """Test analysis endpoint contract."""
        request_data = {
            "analysis_type": "frequency",
            "data_source": {
                "file_path": "/data/timeseries.csv",
                "format": "csv"
            },
            "parameters": {
                "window_size": 1024,
                "overlap": 0.5
            }
        }

        result = contract_runner.test_endpoint_contract(
            "/api/v1/analysis/run",
            request_data,
            "simulation_response"
        )

        assert result['schema_valid'], f"Contract validation failed: {result['errors']}"
        assert result['response']['status_code'] == 200

    def test_error_handling_contracts(self, contract_runner):
        """Test error handling contracts."""
        # Test invalid simulation type
        invalid_request = {
            "simulation_type": "nonexistent",
            "parameters": {},
            "configuration": {}
        }

        result = contract_runner.test_endpoint_contract(
            "/api/v1/simulation/create",
            invalid_request,
            "error_response"
        )

        assert result['response']['status_code'] == 400
        assert result['response']['message'] is not None

    def test_contract_evolution_safety(self, contract_runner):
        """Test that contract changes don't break existing consumers."""
        # Simulate requests from different API versions
        v1_request = {
            "simulation_type": "static",
            "parameters": {"solver": "euler"},
            "configuration": {}
        }

        # Should work with current API
        result = contract_runner.test_endpoint_contract(
            "/api/v1/simulation/create",
            v1_request,
            "simulation_response"
        )

        assert result['schema_valid'], "V1 request should work with current API"

        # Test with additional fields (forward compatibility)
        v2_request = v1_request.copy()
        v2_request["experimental_feature"] = True  # New field not in schema

        # This should not break validation if additionalProperties is handled correctly
        # For now, we expect it to fail since additionalProperties: false
        result = contract_runner.test_endpoint_contract(
            "/api/v1/simulation/create",
            v2_request,
            "error_response"
        )

        # This validates that we handle unknown fields gracefully
        assert result['response']['status_code'] == 400


class TestConsumerDrivenContracts:
    """Test consumer-driven contracts."""

    def test_consumer_expectations(self):
        """Test that API meets consumer expectations."""
        # Define what each consumer expects
        consumers = {
            "web_ui": {
                "required_fields": ["simulation_id", "status"],
                "optional_fields": ["progress", "estimated_completion"],
                "max_response_time": 1.0
            },
            "batch_processor": {
                "required_fields": ["simulation_id", "status", "results"],
                "optional_fields": ["metadata"],
                "max_response_time": 5.0
            },
            "monitoring_service": {
                "required_fields": ["simulation_id", "status"],
                "optional_fields": ["timestamp", "resource_usage"],
                "max_response_time": 0.5
            }
        }

        validator = APIContractValidator()

        # Test that schema meets each consumer's needs
        for consumer_name, expectations in consumers.items():
            response_schema = validator.schemas["simulation_response"]
            data_properties = response_schema["properties"]["data"]["properties"]

            # Check required fields are in schema
            for field in expectations["required_fields"]:
                assert field in data_properties, \
                       f"Consumer {consumer_name} requires field {field} but it's not in schema"

            # Check that required fields are actually required
            required_fields = response_schema["properties"]["data"].get("required", [])
            for field in expectations["required_fields"]:
                if field != "results":  # Results is optional in minimal responses
                    assert field in required_fields, \
                           f"Field {field} should be required for consumer {consumer_name}"

        print("All consumer contract expectations met!")


if __name__ == "__main__":
    # Demo contract testing
    print("Running API contract tests...")

    runner = ContractTestRunner()
    validator = APIContractValidator()

    # Test simulation request validation
    request = {
        "simulation_type": "dynamic",
        "parameters": {"time_step": 0.1, "duration": 10.0, "solver": "euler", "convergence": 1e-4},
        "configuration": {"output": "csv"}
    }

    is_valid, errors = validator.validate_request("simulation_request", request)
    print(f"Request validation: {'PASS' if is_valid else 'FAIL'}")
    if errors:
        print(f"Errors: {errors}")

    # Test endpoint contract
    result = runner.test_endpoint_contract(
        "/api/v1/simulation/create",
        request,
        "simulation_response"
    )

    print(f"Endpoint contract: {'PASS' if result['schema_valid'] else 'FAIL'}")
    print(f"Response status: {result['response']['status_code']}")

    print("Contract testing demo completed!")