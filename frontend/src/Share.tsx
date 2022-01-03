import React from 'react';
import { Link } from 'react-router-dom';

import logo from '../favicon.svg';
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
  shares: Share[],
  onSend(item: Item): void,
}

export default function Share(props: Props): JSX.Element {
  const { connectionState, shares, onSend } = props;
  // const params = useParams();

  const connectionText = (function () {
    switch (connectionState) {
      case 'connected':
        return 'Connected';
      case 'disconnected':
        return 'Disconnected';
    }
  })();

  return (
    <>
      <h3>
        <Link to="/">
          <img src={logo} height="30px" className="App-logo" alt="logo" />
        </Link>
        xchg
      </h3>
      Status: <span className="status">{connectionText}</span>
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
