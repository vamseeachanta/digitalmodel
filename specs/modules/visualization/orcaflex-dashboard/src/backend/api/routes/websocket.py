"""
WebSocket endpoints for real-time updates
"""

import asyncio
import json
from typing import Dict, Set

from fastapi import APIRouter, WebSocket, WebSocketDisconnect
from sqlalchemy.ext.asyncio import AsyncSession

from services.database import get_db
from services.file_monitor import FileMonitor
from utils.logger import setup_logger

logger = setup_logger(__name__)
router = APIRouter()

# Active WebSocket connections
active_connections: Dict[str, Set[WebSocket]] = {
    "data": set(),
    "progress": set(),
    "notifications": set(),
}

file_monitor = FileMonitor()


@router.websocket("/data")
async def websocket_data(websocket: WebSocket) -> None:
    """WebSocket endpoint for real-time data updates"""
    await websocket.accept()
    active_connections["data"].add(websocket)
    
    try:
        # Start file monitoring if not already running
        if not file_monitor.is_running:
            asyncio.create_task(file_monitor.start())
        
        # Listen for file updates
        async for update in file_monitor.get_updates():
            message = json.dumps({
                "type": "data_update",
                "data": update,
            })
            
            # Broadcast to all connected clients
            disconnected = set()
            for connection in active_connections["data"]:
                try:
                    await connection.send_text(message)
                except:
                    disconnected.add(connection)
            
            # Remove disconnected clients
            active_connections["data"] -= disconnected
            
    except WebSocketDisconnect:
        logger.info("WebSocket client disconnected")
    except Exception as e:
        logger.error(f"WebSocket error: {e}")
    finally:
        active_connections["data"].discard(websocket)


@router.websocket("/progress")
async def websocket_progress(websocket: WebSocket) -> None:
    """WebSocket endpoint for task progress updates"""
    await websocket.accept()
    active_connections["progress"].add(websocket)
    
    try:
        while True:
            # Wait for progress updates
            data = await websocket.receive_text()
            request = json.loads(data)
            
            if request.get("type") == "subscribe":
                task_id = request.get("task_id")
                # Subscribe to task progress
                await send_progress_updates(websocket, task_id)
            
    except WebSocketDisconnect:
        logger.info("Progress WebSocket disconnected")
    except Exception as e:
        logger.error(f"Progress WebSocket error: {e}")
    finally:
        active_connections["progress"].discard(websocket)


@router.websocket("/notifications")
async def websocket_notifications(websocket: WebSocket) -> None:
    """WebSocket endpoint for system notifications"""
    await websocket.accept()
    active_connections["notifications"].add(websocket)
    
    try:
        # Send initial connection confirmation
        await websocket.send_json({
            "type": "connected",
            "message": "Connected to notification service",
        })
        
        # Keep connection alive
        while True:
            # Ping every 30 seconds
            await asyncio.sleep(30)
            await websocket.send_json({"type": "ping"})
            
    except WebSocketDisconnect:
        logger.info("Notification WebSocket disconnected")
    except Exception as e:
        logger.error(f"Notification WebSocket error: {e}")
    finally:
        active_connections["notifications"].discard(websocket)


async def broadcast_notification(
    notification_type: str,
    message: str,
    data: Optional[Dict] = None,
) -> None:
    """
    Broadcast notification to all connected clients
    
    Args:
        notification_type: Type of notification
        message: Notification message
        data: Optional additional data
    """
    payload = json.dumps({
        "type": notification_type,
        "message": message,
        "data": data or {},
    })
    
    disconnected = set()
    for connection in active_connections["notifications"]:
        try:
            await connection.send_text(payload)
        except:
            disconnected.add(connection)
    
    # Remove disconnected clients
    active_connections["notifications"] -= disconnected


async def send_progress_updates(
    websocket: WebSocket,
    task_id: str,
) -> None:
    """
    Send progress updates for a specific task
    
    Args:
        websocket: WebSocket connection
        task_id: Task identifier
    """
    # TODO: Implement actual progress tracking
    for progress in range(0, 101, 10):
        await asyncio.sleep(1)
        await websocket.send_json({
            "type": "progress",
            "task_id": task_id,
            "progress": progress,
            "message": f"Processing... {progress}%",
        })