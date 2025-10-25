import { useEffect, useState } from 'react';
import { Calculator } from './components/Calculator';
import { useWebSocket } from './hooks/useWebSocket';
import './App.css';

function App() {
  const { isConnected, lastMessage, sendNotification } = useWebSocket('ws://localhost:8080');
  const [lcdText, setLcdText] = useState('MEMORY LOST');
  const [annunciators, setAnnunciators] = useState('');

  // Handle incoming WebSocket messages
  useEffect(() => {
    if (lastMessage && lastMessage.method === 'lcd-update') {
      const params = lastMessage.params;
      if (params.lcd !== undefined) {
        setLcdText(params.lcd);
      }
      if (params.ann !== undefined) {
        setAnnunciators(params.ann);
      }
    }
  }, [lastMessage]);

  const handleKeyEvent = (method: string, code: number) => {
    sendNotification(method, {
      code: code,
      timestamp: Date.now()
    });
  };

  return (
    <div className="app-container">
      <h1 className="app-title">HP-41 Calculator</h1>

      <div className="calculator-wrapper">
        <Calculator
          onKeyEvent={handleKeyEvent}
          lcdText={lcdText}
          annunciators={annunciators}
        />
      </div>

      <div className="connection-status">
        Status: {isConnected ? '🟢 Connected' : '🔴 Disconnected'}
      </div>
    </div>
  );
}

export default App;
