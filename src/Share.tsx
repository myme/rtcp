import React, { useCallback, useEffect, useState } from 'react';
import { Link } from 'react-router-dom';

import logo from '../favicon.svg';
import Form from './Form';
import connectSocket, { Socket } from './socket';

interface Message {
  direction: '>' | '<',
  text: string,
}

export default function Share(): JSX.Element {
  const [socket, setSocket] = useState<Socket>();
  const [connected, setConnected] = useState(false);
  const [messages, setMessages] = useState<Array<Message>>([]);

  const submit = useCallback((value) => {
    setMessages(m => m.concat({ direction: '>', text: value }));
    if (!socket) {
      throw new Error('Socket not initialized');
    }
    socket.send(value);
  }, [setMessages, socket]);

  useEffect(() => {
    const socket = connectSocket({
      onConnectionStateChange(state) {
        setConnected(state === 'connected');
      },
      onMessage(message) {
        setMessages(m => m.concat({ direction: '<', text: message }));
      },
    });
    setSocket(socket);
  }, [setSocket]);

  return (
    <>
      <h3>
        <Link to="/">
          <img src={logo} height="30px" className="App-logo" alt="logo" />
        </Link>
        xchg
      </h3>
      Status: <span className="status">{connected ? 'Connected' : 'Disconnected'}</span>
      <Form onSubmit={submit} />
      <pre>
        {messages.map(({ direction, text }) => `${direction} ${text}`).join('\n')}
      </pre>
    </>
  );
}
