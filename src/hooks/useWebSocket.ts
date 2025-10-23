import { useEffect, useRef, useState } from 'react';

interface WebSocketMessage {
  jsonrpc: string;
  method: string;
  params: any;
}

export function useWebSocket(url: string) {
  const wsRef = useRef<WebSocket | null>(null);
  const heartbeatIntervalRef = useRef<ReturnType<typeof setInterval> | undefined>(undefined);
  const reconnectTimeoutRef = useRef<ReturnType<typeof setTimeout> | undefined>(undefined);
  const hasConnectedRef = useRef(false);
  const [isConnected, setIsConnected] = useState(false);
  const [lastMessage, setLastMessage] = useState<WebSocketMessage | null>(null);

  useEffect(() => {
    const ws = new WebSocket(url);
    wsRef.current = ws;

    ws.addEventListener("open", () => {
      if (!hasConnectedRef.current) {
        console.log("WebSocket connected");
        hasConnectedRef.current = true;
      }
      setIsConnected(true);

      // Start heartbeat every 30 seconds
      if (heartbeatIntervalRef.current) {
        clearInterval(heartbeatIntervalRef.current);
      }

      heartbeatIntervalRef.current = setInterval(() => {
        try {
          if (ws.readyState === WebSocket.OPEN) {
            ws.send(JSON.stringify({
              jsonrpc: "2.0",
              method: "ping",
              params: {}
            }));
          } else {
            console.log("Connection not open, clearing heartbeat");
            if (heartbeatIntervalRef.current) {
              clearInterval(heartbeatIntervalRef.current);
            }
          }
        } catch (e) {
          console.log("Failed to send ping:", e);
          if (heartbeatIntervalRef.current) {
            clearInterval(heartbeatIntervalRef.current);
          }
        }
      }, 30000);
    });

    ws.addEventListener("close", (event) => {
      setIsConnected(false);
      if (heartbeatIntervalRef.current) {
        clearInterval(heartbeatIntervalRef.current);
      }

      // Only log if we've successfully connected before (to avoid React StrictMode noise)
      if (hasConnectedRef.current) {
        console.log("WebSocket disconnected. Code:", event.code);
        if (event.code !== 1006) {
          console.log("Connection closed. Refresh the page to reconnect.");
        }
      }
    });

    ws.addEventListener("error", () => {
      // Suppress error logging - close event will handle it
    });

    ws.addEventListener("message", (event) => {
      try {
        const message = JSON.parse(event.data);
        setLastMessage(message);
      } catch (e) {
        console.log("Failed to parse message:", e);
      }
    });

    // Cleanup on unmount
    return () => {
      if (heartbeatIntervalRef.current) {
        clearInterval(heartbeatIntervalRef.current);
      }
      if (reconnectTimeoutRef.current) {
        clearTimeout(reconnectTimeoutRef.current);
      }
      ws.close();
    };
  }, [url]);

  const sendNotification = (method: string, params: any) => {
    if (wsRef.current && wsRef.current.readyState === WebSocket.OPEN) {
      wsRef.current.send(JSON.stringify({
        jsonrpc: "2.0",
        method: method,
        params: params
      }));
    }
  };

  return {
    isConnected,
    lastMessage,
    sendNotification
  };
}
