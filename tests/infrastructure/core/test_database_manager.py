# ABOUTME: Comprehensive TDD tests for DatabaseManager with connection pooling
# Tests connection pooling, retry logic, failover, and multi-database support

import os
import time
from unittest.mock import MagicMock, Mock, patch, PropertyMock

import pytest
from sqlalchemy import create_engine
from sqlalchemy.exc import OperationalError, DatabaseError
from sqlalchemy.pool import QueuePool


class TestDatabaseManagerConfiguration:
    """Test configuration loading from env vars and config files"""

    def test_load_config_from_env_vars(self):
        """Should load database config from environment variables"""
        from digitalmodel.core.database_manager import DatabaseManager

        with patch.dict(os.environ, {
            'DB_HOST': 'localhost',
            'DB_PORT': '5432',
            'DB_NAME': 'testdb',
            'DB_USER': 'testuser',
            'DB_PASSWORD': 'testpass'
        }):
            manager = DatabaseManager(db_type='postgresql')
            config = manager.get_config()

            assert config['host'] == 'localhost'
            assert config['port'] == '5432'
            assert config['database'] == 'testdb'
            assert config['user'] == 'testuser'
            assert config['password'] == 'testpass'

    def test_env_vars_override_config_file(self):
        """Environment variables should take precedence over config file"""
        from digitalmodel.core.database_manager import DatabaseManager

        config_file = {
            'host': 'config-host',
            'port': '1234',
            'database': 'configdb'
        }

        with patch.dict(os.environ, {
            'DB_HOST': 'env-host',
            'DB_PORT': '5678'
        }):
            manager = DatabaseManager(db_type='postgresql', config=config_file)
            config = manager.get_config()

            # Env vars should override
            assert config['host'] == 'env-host'
            assert config['port'] == '5678'
            # Config file value should be used when env var not set
            assert config['database'] == 'configdb'

    def test_different_pool_sizes_per_database(self):
        """Different databases should have appropriate pool sizes"""
        from digitalmodel.core.database_manager import DatabaseManager

        # PostgreSQL: 20 connections
        pg_manager = DatabaseManager(db_type='postgresql')
        assert pg_manager.pool_size == 20

        # MSSQL: 15 connections
        mssql_manager = DatabaseManager(db_type='mssql')
        assert mssql_manager.pool_size == 15

        # MongoDB: 10 connections
        mongo_manager = DatabaseManager(db_type='mongodb')
        assert mongo_manager.pool_size == 10

        # Access: 1 connection (no pooling)
        access_manager = DatabaseManager(db_type='access')
        assert access_manager.pool_size == 1


class TestConnectionPooling:
    """Test SQLAlchemy connection pooling functionality"""

    @patch('sqlalchemy.create_engine')
    def test_creates_engine_with_queue_pool(self, mock_create_engine):
        """Should create engine with QueuePool configuration"""
        from digitalmodel.core.database_manager import DatabaseManager

        manager = DatabaseManager(db_type='postgresql', config={
            'host': 'localhost',
            'database': 'testdb',
            'user': 'test',
            'password': 'pass'
        })

        manager.get_connection()

        # Verify create_engine was called with pooling parameters
        call_kwargs = mock_create_engine.call_args[1]
        assert call_kwargs.get('pool_pre_ping') is True
        assert call_kwargs.get('pool_size') == 20  # PostgreSQL default
        assert 'pool_recycle' in call_kwargs

    @patch('sqlalchemy.create_engine')
    def test_pool_pre_ping_enabled(self, mock_create_engine):
        """Should enable pool_pre_ping for auto-reconnect"""
        from digitalmodel.core.database_manager import DatabaseManager

        manager = DatabaseManager(db_type='postgresql')
        manager.get_connection()

        call_kwargs = mock_create_engine.call_args[1]
        assert call_kwargs['pool_pre_ping'] is True

    @patch('sqlalchemy.create_engine')
    def test_configurable_pool_size_and_overflow(self, mock_create_engine):
        """Should support custom pool size and max overflow"""
        from digitalmodel.core.database_manager import DatabaseManager

        manager = DatabaseManager(
            db_type='postgresql',
            pool_size=30,
            max_overflow=10
        )
        manager.get_connection()

        call_kwargs = mock_create_engine.call_args[1]
        assert call_kwargs['pool_size'] == 30
        assert call_kwargs['max_overflow'] == 10


class TestRetryLogic:
    """Test retry logic with exponential backoff"""

    @patch('time.sleep')
    @patch('sqlalchemy.create_engine')
    def test_retry_on_operational_error(self, mock_create_engine, mock_sleep):
        """Should retry connection on OperationalError"""
        from digitalmodel.core.database_manager import DatabaseManager

        # Simulate failure then success
        mock_engine = MagicMock()
        mock_engine.connect.side_effect = [
            OperationalError("Connection refused", None, None),
            OperationalError("Connection refused", None, None),
            MagicMock()  # Success on 3rd attempt
        ]
        mock_create_engine.return_value = mock_engine

        manager = DatabaseManager(db_type='postgresql')
        connection = manager.get_connection()

        # Should have retried 2 times before success
        assert mock_engine.connect.call_count == 3
        assert connection is not None

    @patch('time.sleep')
    @patch('sqlalchemy.create_engine')
    def test_exponential_backoff(self, mock_create_engine, mock_sleep):
        """Should use exponential backoff: 1s, 2s, 4s"""
        from digitalmodel.core.database_manager import DatabaseManager

        mock_engine = MagicMock()
        mock_engine.connect.side_effect = [
            OperationalError("Connection refused", None, None),
            OperationalError("Connection refused", None, None),
            OperationalError("Connection refused", None, None),
            MagicMock()
        ]
        mock_create_engine.return_value = mock_engine

        manager = DatabaseManager(db_type='postgresql', max_retries=3)
        manager.get_connection()

        # Verify sleep was called with exponential backoff
        sleep_calls = [call[0][0] for call in mock_sleep.call_args_list]
        assert sleep_calls == [1, 2, 4]

    @patch('sqlalchemy.create_engine')
    def test_no_retry_on_authentication_error(self, mock_create_engine):
        """Should NOT retry on authentication failures"""
        from digitalmodel.core.database_manager import DatabaseManager

        mock_engine = MagicMock()
        auth_error = DatabaseError("authentication failed", None, None)
        auth_error.orig = Exception("password authentication failed")
        mock_engine.connect.side_effect = auth_error
        mock_create_engine.return_value = mock_engine

        manager = DatabaseManager(db_type='postgresql')

        with pytest.raises(DatabaseError):
            manager.get_connection()

        # Should NOT retry authentication errors
        assert mock_engine.connect.call_count == 1

    @patch('sqlalchemy.create_engine')
    def test_no_retry_on_permission_error(self, mock_create_engine):
        """Should NOT retry on permission errors"""
        from digitalmodel.core.database_manager import DatabaseManager

        mock_engine = MagicMock()
        perm_error = DatabaseError("permission denied", None, None)
        perm_error.orig = Exception("permission denied for database")
        mock_engine.connect.side_effect = perm_error
        mock_create_engine.return_value = mock_engine

        manager = DatabaseManager(db_type='postgresql')

        with pytest.raises(DatabaseError):
            manager.get_connection()

        assert mock_engine.connect.call_count == 1

    @patch('sqlalchemy.create_engine')
    def test_max_retries_exceeded(self, mock_create_engine):
        """Should raise error after max retries exceeded"""
        from digitalmodel.core.database_manager import DatabaseManager

        mock_engine = MagicMock()
        mock_engine.connect.side_effect = OperationalError("Connection refused", None, None)
        mock_create_engine.return_value = mock_engine

        manager = DatabaseManager(db_type='postgresql', max_retries=3)

        with pytest.raises(OperationalError):
            manager.get_connection()

        # Should attempt initial + 3 retries = 4 total
        assert mock_engine.connect.call_count == 4


class TestHighAvailabilityFailover:
    """Test HA failover with primary and replica connection strings"""

    @patch('sqlalchemy.create_engine')
    def test_supports_multiple_connection_strings(self, mock_create_engine):
        """Should accept list of connection strings for HA"""
        from digitalmodel.core.database_manager import DatabaseManager

        connection_strings = [
            "postgresql://primary:5432/db",
            "postgresql://replica1:5432/db",
            "postgresql://replica2:5432/db"
        ]

        manager = DatabaseManager(
            db_type='postgresql',
            connection_strings=connection_strings
        )

        assert manager.connection_strings == connection_strings
        assert manager.primary_connection == connection_strings[0]
        assert manager.replica_connections == connection_strings[1:]

    @patch('sqlalchemy.create_engine')
    def test_writes_go_to_primary(self, mock_create_engine):
        """Write connections should always use primary"""
        from digitalmodel.core.database_manager import DatabaseManager

        connection_strings = [
            "postgresql://primary:5432/db",
            "postgresql://replica1:5432/db"
        ]

        mock_engine = MagicMock()
        mock_create_engine.return_value = mock_engine

        manager = DatabaseManager(
            db_type='postgresql',
            connection_strings=connection_strings
        )

        manager.get_connection(read_only=False)

        # Should use primary connection string
        call_args = mock_create_engine.call_args[0]
        assert "primary" in call_args[0]

    @patch('sqlalchemy.create_engine')
    def test_reads_round_robin_across_all(self, mock_create_engine):
        """Read connections should round-robin across all servers"""
        from digitalmodel.core.database_manager import DatabaseManager

        connection_strings = [
            "postgresql://primary:5432/db",
            "postgresql://replica1:5432/db",
            "postgresql://replica2:5432/db"
        ]

        mock_engine = MagicMock()
        mock_create_engine.return_value = mock_engine

        manager = DatabaseManager(
            db_type='postgresql',
            connection_strings=connection_strings
        )

        # Get 3 read connections
        used_hosts = []
        for _ in range(3):
            manager.get_connection(read_only=True)
            call_args = mock_create_engine.call_args[0][0]
            used_hosts.append(call_args)

        # Should cycle through all servers
        assert "primary" in used_hosts[0]
        assert "replica1" in used_hosts[1]
        assert "replica2" in used_hosts[2]

    @patch('sqlalchemy.create_engine')
    def test_failover_to_replica_on_primary_failure(self, mock_create_engine):
        """Should failover to replica if primary fails"""
        from digitalmodel.core.database_manager import DatabaseManager

        connection_strings = [
            "postgresql://primary:5432/db",
            "postgresql://replica1:5432/db"
        ]

        # Primary fails, replica succeeds
        def create_engine_side_effect(conn_str, **kwargs):
            if "primary" in conn_str:
                engine = MagicMock()
                engine.connect.side_effect = OperationalError("Primary down", None, None)
                return engine
            else:
                engine = MagicMock()
                engine.connect.return_value = MagicMock()
                return engine

        mock_create_engine.side_effect = create_engine_side_effect

        manager = DatabaseManager(
            db_type='postgresql',
            connection_strings=connection_strings
        )

        # Should failover to replica
        connection = manager.get_connection(read_only=False)
        assert connection is not None


class TestMongoDBIntegration:
    """Test MongoDB-specific connection handling"""

    @patch('motor.motor_asyncio.AsyncIOMotorClient')
    def test_uses_motor_for_async(self, mock_motor_client):
        """Should use motor for async MongoDB connections"""
        from digitalmodel.core.database_manager import DatabaseManager

        manager = DatabaseManager(db_type='mongodb', config={
            'host': 'localhost',
            'port': 27017,
            'database': 'testdb'
        })

        manager.get_connection()

        mock_motor_client.assert_called_once()

    @patch('motor.motor_asyncio.AsyncIOMotorClient')
    def test_mongodb_pool_size_default_10(self, mock_motor_client):
        """MongoDB should default to pool size of 10"""
        from digitalmodel.core.database_manager import DatabaseManager

        manager = DatabaseManager(db_type='mongodb')

        assert manager.pool_size == 10

    @patch('motor.motor_asyncio.AsyncIOMotorClient')
    def test_mongodb_similar_interface_to_sql(self, mock_motor_client):
        """MongoDB manager should have similar interface to SQL managers"""
        from digitalmodel.core.database_manager import DatabaseManager

        mongo_manager = DatabaseManager(db_type='mongodb')
        sql_manager = DatabaseManager(db_type='postgresql')

        # Both should have same core methods
        assert hasattr(mongo_manager, 'get_connection')
        assert hasattr(mongo_manager, 'get_metrics')
        assert hasattr(mongo_manager, 'close')
        assert hasattr(sql_manager, 'get_connection')
        assert hasattr(sql_manager, 'get_metrics')
        assert hasattr(sql_manager, 'close')


class TestMSAccessHandling:
    """Test MS Access-specific handling (no pooling)"""

    @patch('sqlalchemy.create_engine')
    def test_access_single_connection_only(self, mock_create_engine):
        """MS Access should use single connection (no pooling)"""
        from digitalmodel.core.database_manager import DatabaseManager

        manager = DatabaseManager(db_type='access')

        assert manager.pool_size == 1

    @patch('sqlalchemy.create_engine')
    def test_access_warns_in_production(self, mock_create_engine):
        """Should warn when Access is used in production"""
        from digitalmodel.core.database_manager import DatabaseManager

        with patch.dict(os.environ, {'ENVIRONMENT': 'production'}):
            with pytest.warns(UserWarning, match="MS Access.*production"):
                manager = DatabaseManager(db_type='access')

    @patch('sqlalchemy.create_engine')
    def test_access_ok_in_development(self, mock_create_engine):
        """Should not warn in development environment"""
        from digitalmodel.core.database_manager import DatabaseManager

        with patch.dict(os.environ, {'ENVIRONMENT': 'development'}):
            # Should not raise warning
            manager = DatabaseManager(db_type='access')


class TestMetricsTracking:
    """Test connection metrics tracking"""

    @patch('sqlalchemy.create_engine')
    def test_tracks_active_connections(self, mock_create_engine):
        """Should track number of active connections"""
        from digitalmodel.core.database_manager import DatabaseManager

        mock_engine = MagicMock()
        mock_create_engine.return_value = mock_engine

        manager = DatabaseManager(db_type='postgresql')

        # Get connections
        manager.get_connection()
        manager.get_connection()

        metrics = manager.get_metrics()
        assert 'active_connections' in metrics
        assert metrics['active_connections'] >= 0

    @patch('sqlalchemy.create_engine')
    def test_tracks_pool_size(self, mock_create_engine):
        """Should track configured pool size"""
        from digitalmodel.core.database_manager import DatabaseManager

        manager = DatabaseManager(db_type='postgresql', pool_size=25)

        metrics = manager.get_metrics()
        assert metrics['pool_size'] == 25

    @patch('sqlalchemy.create_engine')
    def test_tracks_connection_errors(self, mock_create_engine):
        """Should track connection error count"""
        from digitalmodel.core.database_manager import DatabaseManager

        mock_engine = MagicMock()
        mock_engine.connect.side_effect = OperationalError("Error", None, None)
        mock_create_engine.return_value = mock_engine

        manager = DatabaseManager(db_type='postgresql', max_retries=2)

        try:
            manager.get_connection()
        except OperationalError:
            pass

        metrics = manager.get_metrics()
        assert 'connection_errors' in metrics
        assert metrics['connection_errors'] > 0

    @patch('sqlalchemy.create_engine')
    @patch('time.time')
    def test_tracks_query_times(self, mock_time, mock_create_engine):
        """Should track query execution times"""
        from digitalmodel.core.database_manager import DatabaseManager

        # Simulate time passage
        mock_time.side_effect = [0.0, 0.5, 1.0, 1.3]

        mock_engine = MagicMock()
        mock_connection = MagicMock()
        mock_engine.connect.return_value = mock_connection
        mock_create_engine.return_value = mock_engine

        manager = DatabaseManager(db_type='postgresql')

        with manager.get_connection() as conn:
            # Simulate query
            pass

        metrics = manager.get_metrics()
        assert 'avg_query_time' in metrics or 'total_queries' in metrics

    @patch('sqlalchemy.create_engine')
    def test_tracks_failure_rate(self, mock_create_engine):
        """Should track connection failure rate"""
        from digitalmodel.core.database_manager import DatabaseManager

        mock_engine = MagicMock()
        # 2 failures, 1 success
        mock_engine.connect.side_effect = [
            OperationalError("Error", None, None),
            OperationalError("Error", None, None),
            MagicMock()
        ]
        mock_create_engine.return_value = mock_engine

        manager = DatabaseManager(db_type='postgresql', max_retries=3)
        manager.get_connection()

        metrics = manager.get_metrics()
        assert 'failure_rate' in metrics


class TestContextManager:
    """Test context manager for safe connection handling"""

    @patch('sqlalchemy.create_engine')
    def test_context_manager_returns_connection(self, mock_create_engine):
        """Context manager should return valid connection"""
        from digitalmodel.core.database_manager import DatabaseManager

        mock_engine = MagicMock()
        mock_connection = MagicMock()
        mock_engine.connect.return_value = mock_connection
        mock_create_engine.return_value = mock_engine

        manager = DatabaseManager(db_type='postgresql')

        with manager.get_connection() as conn:
            assert conn == mock_connection

    @patch('sqlalchemy.create_engine')
    def test_context_manager_closes_connection(self, mock_create_engine):
        """Context manager should close connection on exit"""
        from digitalmodel.core.database_manager import DatabaseManager

        mock_engine = MagicMock()
        mock_connection = MagicMock()
        mock_engine.connect.return_value = mock_connection
        mock_create_engine.return_value = mock_engine

        manager = DatabaseManager(db_type='postgresql')

        with manager.get_connection() as conn:
            pass

        mock_connection.close.assert_called_once()

    @patch('sqlalchemy.create_engine')
    def test_context_manager_closes_on_exception(self, mock_create_engine):
        """Context manager should close connection even on exception"""
        from digitalmodel.core.database_manager import DatabaseManager

        mock_engine = MagicMock()
        mock_connection = MagicMock()
        mock_engine.connect.return_value = mock_connection
        mock_create_engine.return_value = mock_engine

        manager = DatabaseManager(db_type='postgresql')

        try:
            with manager.get_connection() as conn:
                raise ValueError("Test error")
        except ValueError:
            pass

        mock_connection.close.assert_called_once()


class TestBackwardCompatibility:
    """Test integration with existing database.py"""

    def test_supports_legacy_db_properties_dict(self):
        """Should support legacy db_properties dict format"""
        from digitalmodel.core.database_manager import DatabaseManager

        # Legacy format from existing database.py
        db_properties = {
            'server_type': 'postgresql',
            'server': 'localhost',
            'database': 'testdb',
            'user': 'testuser',
            'password': 'testpass',
            'port': 5432
        }

        manager = DatabaseManager.from_legacy_properties(db_properties)

        assert manager.db_type == 'postgresql'
        config = manager.get_config()
        assert config['host'] == 'localhost'
        assert config['database'] == 'testdb'

    def test_provides_legacy_get_db_connection_wrapper(self):
        """Should provide wrapper for legacy get_db_connection function"""
        from digitalmodel.core.database_manager import get_db_connection_pooled

        db_properties = {
            'server_type': 'postgresql',
            'server': 'localhost',
            'database': 'testdb'
        }

        # Should work like old function but with pooling
        manager, status = get_db_connection_pooled(db_properties)

        assert manager is not None
        assert isinstance(status, bool)

    @patch('sqlalchemy.create_engine')
    def test_connection_string_compatibility(self, mock_create_engine):
        """Should work with existing connection_string parameter"""
        from digitalmodel.core.database_manager import DatabaseManager

        connection_string = "postgresql://user:pass@localhost:5432/testdb"

        manager = DatabaseManager(
            db_type='postgresql',
            connection_string=connection_string
        )

        manager.get_connection()

        # Should use provided connection string
        call_args = mock_create_engine.call_args[0]
        assert connection_string in call_args[0]


class TestHealthCheck:
    """Test health check functionality"""

    @patch('sqlalchemy.create_engine')
    def test_health_check_returns_status(self, mock_create_engine):
        """Health check should return connection status"""
        from digitalmodel.core.database_manager import DatabaseManager

        mock_engine = MagicMock()
        mock_connection = MagicMock()
        mock_engine.connect.return_value = mock_connection
        mock_create_engine.return_value = mock_engine

        manager = DatabaseManager(db_type='postgresql')

        status = manager.health_check()

        assert 'status' in status
        assert 'response_time' in status
        assert status['status'] in ['healthy', 'unhealthy']

    @patch('sqlalchemy.create_engine')
    def test_health_check_detects_failure(self, mock_create_engine):
        """Health check should detect connection failures"""
        from digitalmodel.core.database_manager import DatabaseManager

        mock_engine = MagicMock()
        mock_engine.connect.side_effect = OperationalError("Error", None, None)
        mock_create_engine.return_value = mock_engine

        manager = DatabaseManager(db_type='postgresql', max_retries=0)

        status = manager.health_check()

        assert status['status'] == 'unhealthy'


@pytest.mark.integration
class TestIntegrationWithContainers:
    """Integration tests with real databases (requires testcontainers)"""

    @pytest.mark.skip(reason="Requires testcontainers and Docker")
    def test_real_postgresql_connection_pooling(self):
        """Test actual PostgreSQL connection pooling"""
        pass

    @pytest.mark.skip(reason="Requires testcontainers and Docker")
    def test_real_mssql_connection_pooling(self):
        """Test actual MSSQL connection pooling"""
        pass

    @pytest.mark.skip(reason="Requires testcontainers and Docker")
    def test_real_mongodb_connection_pooling(self):
        """Test actual MongoDB connection pooling"""
        pass
