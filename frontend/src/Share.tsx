import React, { useEffect } from 'react';
import { Link, useParams } from 'react-router-dom';

import logo from '../favicon.svg';
import ControlSocket from './ControlSocket';
import Form from './Form';
import { ConnectionState, Item } from './PeerConnection';

export interface Share {
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

interface Props {
  connectionState: ConnectionState,
  socket: ControlSocket,
  shares: Share[],
  onSend(item: Item): void,
}

export default function Share(props: Props): JSX.Element {
  const { connectionState, socket, shares, onSend } = props;
  const { shareId } = useParams();

  const connectionText = (function () {
    switch (connectionState) {
      case 'connected':
        return 'Connected';
      case 'disconnected':
        return 'Disconnected';
    }
  })();

  useEffect(() => {
    if (shareId) {
      socket.joinSession(shareId);
    }
    return () => { socket.leaveSession(); };
  }, [socket]);

  return (
    <>
      <h3>
        <Link to="/">
          <img src={logo} height="30px" className="App-logo" alt="logo" />
        </Link>
        xchg
      </h3>
      <span>
        Share ID: {shareId}
      </span>
      <span>
        Status: <span className="status">{connectionText}</span>
      </span>
      <Form onSubmit={onSend} />
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
