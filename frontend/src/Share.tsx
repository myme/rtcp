import React, { useCallback, useEffect, useState } from 'react';
import { Link } from 'react-router-dom';

import logo from '../favicon.svg';
import Form, { Item } from './Form';
import connectSocket, { Socket } from './socket';

interface Share {
  direction: '>' | '<',
  item: Item,
}

function Item(props: { item: Item }) {
  const { item: { type, value } } = props;
  const output = type === 'hidden' ? value.replace(/./g, '*') : value;
  return (
    <span>{output}</span>
  );
}

export default function Share(): JSX.Element {
  const [socket, setSocket] = useState<Socket>();
  const [connected, setConnected] = useState(false);
  const [shares, setShares] = useState<Array<Share>>([]);

  const submit = useCallback<(item: Item) => void>((item) => {
    setShares(m => m.concat({ direction: '>', item }));
    if (!socket) {
      throw new Error('Socket not initialized');
    }
    socket.send(JSON.stringify(item));
  }, [setShares, socket]);

  useEffect(() => {
    const socket = connectSocket({
      onConnectionStateChange(state) {
        setConnected(state === 'connected');
      },
      onItem(item) {
        setShares(m => m.concat({ direction: '<', item }));
      },
    });
    setSocket(socket);
    return () => { socket.close(); };
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
        {shares.map(({ direction, item }, idx) => (
          <p key={idx}>
            {direction}
            {' '}
            <Item key={idx} item={item} />
          </p>
        ))}
      </pre>
    </>
  );
}
