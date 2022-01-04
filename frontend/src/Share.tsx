import React, { useEffect } from 'react';
import { Link, useParams } from 'react-router-dom';

import Header from './Header';
import ControlSocket from './ControlSocket';
import ShareForm from './ShareForm';
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
      case 'pending':
        return 'Pending'
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
      <Header />
      {(function () {
        switch (connectionState) {
          case 'pending':
            return (
              <Link to={`/${shareId}`}>
                <h1>{shareId}</h1>
              </Link>
            );
          case 'disconnected':
            return (
              <>
                <h2>Peer disconnected</h2>
                <Link className="button" to="/">Home</Link>
              </>
            );
          case 'connected':
            return (
              <>
                <span>
                  Share ID: {shareId}
                </span>
                <span>
                  Status: <span className="status">{connectionText}</span>
                </span>
                <ShareForm onSubmit={onSend} />
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
      })()}
    </>
  );
}
