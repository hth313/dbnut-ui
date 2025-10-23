import { useEffect, useRef, useState } from 'react';

interface WebSocketMessage {
  jsonrpc: string;
  method: string;
  params: any;
}

export function useWebSocket(url: string) {
  const wsRef = useRef<WebSocket | null>(null);
  const heartbeatIntervalRef = useRef<ReturnType<typeof setInterval> | undefined>(undefined);
  const [isConnected, setIsConnected] = useState(false);
  const [lastMessage, setLastMessage] = useState<WebSocketMessage | null>(null);

  useEffect(() => {
    const ws = new WebSocket(url);
    wsRef.current = ws;

    ws.addEventListener("open", () => {
      console.log("WebSocket connected");
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

    ws.addEventListener("close", () => {
      console.log("WebSocket disconnected");
      setIsConnected(false);
      if (heartbeatIntervalRef.current) {
        clearInterval(heartbeatIntervalRef.current);
      }
      // Attempt to reconnect after 2 seconds
      setTimeout(() => {
        location.reload();
      }, 2000);
    });

    ws.addEventListener("error", (error) => {
      console.log("WebSocket error:", error);
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
