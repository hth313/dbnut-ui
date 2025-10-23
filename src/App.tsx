import { useEffect, useState } from 'react';
import { Calculator } from './components/Calculator';
import { useWebSocket } from './hooks/useWebSocket';

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
    <div style={{
      display: 'flex',
      flexDirection: 'column',
      alignItems: 'center',
      padding: '20px',
      minHeight: '100vh',
      backgroundColor: '#f0f0f0'
    }}>
      <h1 style={{ marginBottom: '20px' }}>HP-41 Calculator</h1>

      <div style={{
        backgroundColor: 'white',
        padding: '20px',
        borderRadius: '10px',
        boxShadow: '0 4px 6px rgba(0, 0, 0, 0.1)'
      }}>
        <Calculator
          onKeyEvent={handleKeyEvent}
          lcdText={lcdText}
          annunciators={annunciators}
        />
      </div>

      <div style={{ marginTop: '20px', fontSize: '14px', color: '#666' }}>
        Status: {isConnected ? '🟢 Connected' : '🔴 Disconnected'}
      </div>
    </div>
  );
}

export default App;
