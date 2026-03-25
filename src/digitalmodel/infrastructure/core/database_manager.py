# ABOUTME: DatabaseManager with connection pooling for MSSQL, PostgreSQL, MongoDB, MS Access
# Implements retry logic, HA failover, metrics tracking, and backward compatibility

import logging
import os
import time
import warnings
from contextlib import contextmanager
from typing import Any, Dict, List, Optional, Union
from urllib.parse import quote_plus

from sqlalchemy import create_engine, exc
from sqlalchemy.engine import Engine
from sqlalchemy.pool import QueuePool

logger = logging.getLogger(__name__)


class DatabaseManager:
    """
    Manages database connections with pooling, retry logic, and HA failover.

    Supports:
    - MSSQL (pool: 15 connections)
    - PostgreSQL (pool: 20 connections)
    - MongoDB (pool: 10 connections)
    - MS Access (single connection, development only)

    Features:
    - Connection pooling with SQLAlchemy QueuePool
    - Exponential backoff retry logic (1s, 2s, 4s)
    - High-availability failover (primary + replicas)
    - Connection metrics tracking
    - Context manager for safe connection handling
    """

    # Default pool sizes per database type
    DEFAULT_POOL_SIZES = {
        'postgresql': 20,
        'mssql': 15,
        'mongodb': 10,
        'access': 1,
        'accdb': 1,
        'mdb': 1
    }

    # Non-retryable error patterns
    NON_RETRYABLE_ERRORS = [
        'authentication',
        'password',
        'permission denied',
        'access denied',
        'invalid database',
        'does not exist'
    ]

    def __init__(
        self,
        db_type: str,
        config: Optional[Dict[str, Any]] = None,
        connection_string: Optional[str] = None,
        connection_strings: Optional[List[str]] = None,
        pool_size: Optional[int] = None,
        max_overflow: int = 10,
        pool_recycle: int = 3600,
        max_retries: int = 3
    ):
        """
        Initialize DatabaseManager.

        Args:
            db_type: Database type ('postgresql', 'mssql', 'mongodb', 'access')
            config: Configuration dict (env vars override these)
            connection_string: Single connection string
            connection_strings: List of connection strings for HA (primary + replicas)
            pool_size: Pool size (defaults based on db_type)
            max_overflow: Max connections beyond pool_size
            pool_recycle: Recycle connections after N seconds
            max_retries: Max retry attempts on connection failure
        """
        self.db_type = db_type.lower()
        self._config = config or {}
        self.connection_string = connection_string
        self.connection_strings = connection_strings or []
        self.max_overflow = max_overflow
        self.pool_recycle = pool_recycle
        self.max_retries = max_retries

        # Set pool size
        self.pool_size = pool_size if pool_size is not None else self.DEFAULT_POOL_SIZES.get(self.db_type, 10)

        # Setup HA if multiple connection strings provided
        if self.connection_strings:
            self.primary_connection = self.connection_strings[0]
            self.replica_connections = self.connection_strings[1:]
            self._replica_index = 0  # For round-robin
        else:
            self.primary_connection = None
            self.replica_connections = []

        # Metrics tracking
        self._metrics = {
            'active_connections': 0,
            'total_connections': 0,
            'connection_errors': 0,
            'total_queries': 0,
            'total_query_time': 0.0,
            'retry_attempts': 0
        }

        # Engine cache
        self._engines: Dict[str, Engine] = {}

        # Warn if Access in production
        if self.db_type in ['access', 'accdb', 'mdb']:
            env = os.environ.get('ENVIRONMENT', 'development').lower()
            if env == 'production':
                warnings.warn(
                    "MS Access is not recommended for production use. "
                    "Connection pooling is disabled for Access databases.",
                    UserWarning
                )

    def get_config(self) -> Dict[str, Any]:
        """
        Get configuration with environment variable overrides.

        Environment variables take precedence over config dict.

        Returns:
            Configuration dict with env var overrides applied
        """
        config = self._config.copy()

        # Environment variables override config file
        env_mappings = {
            'DB_HOST': 'host',
            'DB_PORT': 'port',
            'DB_NAME': 'database',
            'DB_USER': 'user',
            'DB_PASSWORD': 'password',
            'DB_SCHEMA': 'schema',
            'DB_CONNECTION_STRING': 'connection_string'
        }

        for env_var, config_key in env_mappings.items():
            if env_var in os.environ:
                config[config_key] = os.environ[env_var]

        return config

    def _build_connection_string(self, config: Optional[Dict[str, Any]] = None) -> str:
        """
        Build connection string from configuration.

        Args:
            config: Configuration dict (uses self.get_config() if None)

        Returns:
            SQLAlchemy connection string
        """
        if self.connection_string:
            return self.connection_string

        cfg = config or self.get_config()

        if self.db_type == 'postgresql':
            return self._build_postgresql_string(cfg)
        elif self.db_type == 'mssql':
            return self._build_mssql_string(cfg)
        elif self.db_type == 'mongodb':
            return self._build_mongodb_string(cfg)
        elif self.db_type in ['access', 'accdb', 'mdb']:
            return self._build_access_string(cfg)
        else:
            raise ValueError(f"Unsupported database type: {self.db_type}")

    def _build_postgresql_string(self, config: Dict[str, Any]) -> str:
        """Build PostgreSQL connection string"""
        if 'connection_string' in config:
            return config['connection_string']

        user = config.get('user', 'postgres')
        password = config.get('password', '')
        host = config.get('host', 'localhost')
        port = config.get('port', 5432)
        database = config.get('database', 'postgres')

        return f"postgresql+psycopg2://{user}:{password}@{host}:{port}/{database}"

    def _build_mssql_string(self, config: Dict[str, Any]) -> str:
        """Build MSSQL connection string"""
        if 'connection_string' in config:
            return config['connection_string']

        server = config.get('server', config.get('host', 'localhost'))
        database = config.get('database', 'master')
        user = config.get('user')
        password = config.get('password')

        if user and password:
            # SQL Server authentication
            driver = config.get('driver', 'ODBC Driver 13 for SQL Server')
            conn_str = f"Driver={{{driver}}};Server={server};Database={database};Uid={user};Pwd={password};"

            # Add MultiSubnetFailover for HA
            if config.get('highAvailability', True):
                conn_str += "MultiSubnetFailover=Yes;"

            return f"mssql+pyodbc:///?odbc_connect={quote_plus(conn_str)}"
        else:
            # Windows authentication
            conn_str = f"Driver={{SQL Server}};Server={server};Database={database};Trusted_Connection=yes;"
            return f"mssql+pyodbc:///?odbc_connect={quote_plus(conn_str)}"

    def _build_mongodb_string(self, config: Dict[str, Any]) -> str:
        """Build MongoDB connection string"""
        if 'connection_string' in config:
            return config['connection_string']

        user = config.get('user', '')
        password = config.get('password', '')
        host = config.get('host', 'localhost')
        port = config.get('port', 27017)
        database = config.get('database', 'admin')

        if user and password:
            return f"mongodb://{user}:{password}@{host}:{port}/{database}"
        else:
            return f"mongodb://{host}:{port}/{database}"

    def _build_access_string(self, config: Dict[str, Any]) -> str:
        """Build MS Access connection string"""
        database_path = config.get('database', config.get('database_path'))
        if not database_path:
            raise ValueError("MS Access requires 'database' or 'database_path' in config")

        driver = config.get('driver', 'Microsoft Access Driver (*.mdb, *.accdb)')
        conn_str = f"Driver={{{driver}}};DBQ={database_path};"

        return f"access+pyodbc:///?odbc_connect={quote_plus(conn_str)}"

    def _is_retryable_error(self, error: Exception) -> bool:
        """
        Check if error is retryable.

        Authentication, permission, and invalid database errors are NOT retryable.

        Args:
            error: Exception to check

        Returns:
            True if error should be retried
        """
        error_msg = str(error).lower()

        # Check for non-retryable patterns
        for pattern in self.NON_RETRYABLE_ERRORS:
            if pattern in error_msg:
                return False

        # Check if it's an OperationalError (network/connection issues)
        return isinstance(error, exc.OperationalError)

    def _get_engine(self, connection_string: str) -> Engine:
        """
        Get or create engine for connection string.

        Caches engines to reuse connection pools.

        Args:
            connection_string: SQLAlchemy connection string

        Returns:
            SQLAlchemy Engine with pooling configured
        """
        if connection_string in self._engines:
            return self._engines[connection_string]

        # MongoDB uses motor, not SQLAlchemy
        if self.db_type == 'mongodb':
            # For MongoDB, we'll use motor directly in get_connection
            return None

        # Create engine with pooling parameters
        pooling_kwargs = {
            'pool_pre_ping': True,  # Auto-reconnect on stale connections
            'pool_size': self.pool_size,
            'max_overflow': self.max_overflow,
            'pool_recycle': self.pool_recycle,
            'echo': False
        }

        # Access doesn't support pooling well
        if self.db_type in ['access', 'accdb', 'mdb']:
            pooling_kwargs = {
                'poolclass': None,  # Disable pooling
                'echo': False
            }

        engine = create_engine(connection_string, **pooling_kwargs)
        self._engines[connection_string] = engine

        return engine

    def _get_next_replica(self) -> str:
        """
        Get next replica connection string using round-robin.

        Returns:
            Connection string for next replica (includes primary)
        """
        if not self.connection_strings:
            return None

        # Round-robin across ALL servers (primary + replicas)
        conn_str = self.connection_strings[self._replica_index]
        self._replica_index = (self._replica_index + 1) % len(self.connection_strings)

        return conn_str

    @contextmanager
    def get_connection(self, read_only: bool = False):
        """
        Get database connection with retry logic and HA failover.

        Use as context manager for automatic cleanup:
            with manager.get_connection() as conn:
                result = conn.execute(query)

        Args:
            read_only: If True, use replica for reads (round-robin).
                      If False, always use primary for writes.

        Yields:
            Database connection

        Raises:
            Exception: After max retries exceeded or on non-retryable error
        """
        if self.db_type == 'mongodb':
            # MongoDB uses motor
            conn = self._get_mongodb_connection()
            try:
                yield conn
            finally:
                if hasattr(conn, 'close'):
                    conn.close()
            return

        # Determine which connection string to use
        if self.connection_strings:
            if read_only:
                # Round-robin across all servers for reads
                conn_str = self._get_next_replica()
            else:
                # Always use primary for writes
                conn_str = self.primary_connection
        else:
            conn_str = self._build_connection_string()

        # Retry logic with exponential backoff
        last_error = None
        attempt = 0

        while attempt <= self.max_retries:
            try:
                engine = self._get_engine(conn_str)
                connection = engine.connect()

                # Track metrics
                self._metrics['total_connections'] += 1
                self._metrics['active_connections'] += 1

                start_time = time.time()

                try:
                    yield connection
                finally:
                    # Track query time
                    query_time = time.time() - start_time
                    self._metrics['total_query_time'] += query_time
                    self._metrics['total_queries'] += 1
                    self._metrics['active_connections'] -= 1

                    connection.close()

                return  # Success, exit retry loop

            except Exception as error:
                last_error = error
                self._metrics['connection_errors'] += 1

                # Check if retryable
                if not self._is_retryable_error(error):
                    logger.error(f"Non-retryable error: {error}")
                    raise

                # Check if we should retry
                if attempt >= self.max_retries:
                    logger.error(f"Max retries ({self.max_retries}) exceeded")
                    raise

                # Exponential backoff: 1s, 2s, 4s
                backoff_time = 2 ** attempt
                logger.warning(
                    f"Connection attempt {attempt + 1} failed: {error}. "
                    f"Retrying in {backoff_time}s..."
                )

                self._metrics['retry_attempts'] += 1
                time.sleep(backoff_time)
                attempt += 1

                # Try failover to replica if available
                if self.replica_connections and attempt <= self.max_retries:
                    conn_str = self._get_next_replica()
                    logger.info(f"Failing over to: {conn_str}")

    def _get_mongodb_connection(self):
        """Get MongoDB connection using motor"""
        try:
            from motor.motor_asyncio import AsyncIOMotorClient
        except ImportError:
            raise ImportError("motor package required for MongoDB support: pip install motor")

        conn_str = self._build_connection_string()

        # Extract database name from config
        config = self.get_config()
        database_name = config.get('database', 'admin')

        # Create motor client with pooling
        client = AsyncIOMotorClient(
            conn_str,
            maxPoolSize=self.pool_size,
            serverSelectionTimeoutMS=5000
        )

        return client[database_name]

    def get_metrics(self) -> Dict[str, Any]:
        """
        Get connection metrics.

        Returns:
            Dict containing:
            - active_connections: Current active connections
            - total_connections: Total connections created
            - connection_errors: Number of connection errors
            - total_queries: Total queries executed
            - avg_query_time: Average query time in seconds
            - failure_rate: Connection failure rate (0-1)
            - pool_size: Configured pool size
            - retry_attempts: Total retry attempts
        """
        metrics = self._metrics.copy()
        metrics['pool_size'] = self.pool_size

        # Calculate derived metrics
        if metrics['total_queries'] > 0:
            metrics['avg_query_time'] = metrics['total_query_time'] / metrics['total_queries']
        else:
            metrics['avg_query_time'] = 0.0

        if metrics['total_connections'] > 0:
            metrics['failure_rate'] = metrics['connection_errors'] / metrics['total_connections']
        else:
            metrics['failure_rate'] = 0.0

        return metrics

    def health_check(self) -> Dict[str, Any]:
        """
        Check database connection health.

        Returns:
            Dict with 'status' ('healthy' or 'unhealthy') and 'response_time'
        """
        start_time = time.time()

        try:
            with self.get_connection() as conn:
                # Simple query to test connection
                if self.db_type == 'postgresql':
                    conn.execute("SELECT 1")
                elif self.db_type == 'mssql':
                    conn.execute("SELECT 1")
                # MongoDB doesn't use execute

            response_time = time.time() - start_time

            return {
                'status': 'healthy',
                'response_time': response_time,
                'db_type': self.db_type
            }
        except Exception as error:
            response_time = time.time() - start_time

            return {
                'status': 'unhealthy',
                'response_time': response_time,
                'db_type': self.db_type,
                'error': str(error)
            }

    def close(self):
        """Close all engine connections and cleanup resources"""
        for engine in self._engines.values():
            engine.dispose()
        self._engines.clear()

        logger.info("All database connections closed")

    @classmethod
    def from_legacy_properties(cls, db_properties: Dict[str, Any]) -> 'DatabaseManager':
        """
        Create DatabaseManager from legacy db_properties dict.

        Provides backward compatibility with existing database.py format.

        Args:
            db_properties: Legacy database properties dict with keys:
                - server_type: Database type
                - server: Host server
                - database: Database name
                - user: Username
                - password: Password
                - port: Port number
                - connection_string: Optional connection string

        Returns:
            DatabaseManager instance
        """
        db_type = db_properties.get('server_type', 'postgresql')

        config = {
            'host': db_properties.get('server'),
            'database': db_properties.get('database'),
            'user': db_properties.get('user'),
            'password': db_properties.get('password'),
            'port': db_properties.get('port'),
            'schema': db_properties.get('schema'),
            'highAvailability': db_properties.get('highAvailability', True)
        }

        connection_string = db_properties.get('connection_string')

        return cls(
            db_type=db_type,
            config=config,
            connection_string=connection_string
        )


def get_db_connection_pooled(db_properties: Dict[str, Any]) -> tuple:
    """
    Legacy wrapper for backward compatibility.

    Provides same interface as original get_db_connection() but with pooling.

    Args:
        db_properties: Database properties dict

    Returns:
        Tuple of (DatabaseManager, connection_status)
    """
    try:
        manager = DatabaseManager.from_legacy_properties(db_properties)
        # Test connection
        with manager.get_connection():
            pass
        return manager, True
    except Exception as error:
        logger.error(f"Connection failed: {error}")
        manager = DatabaseManager.from_legacy_properties(db_properties)
        return manager, False
