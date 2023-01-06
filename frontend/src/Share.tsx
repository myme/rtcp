import React, { useCallback, useEffect, useState } from 'react';
import { Link, useParams } from 'react-router-dom';
import { QRCodeSVG } from 'qrcode.react';

import Header from './Header';
import ControlSocket from './ControlSocket';
import ShareForm from './ShareForm';
import { ConnectionState, Item as IItem } from './PeerConnection';
import Item from './Item';

export type Direction = 'inbound' | 'outbound';
export interface Share {
  direction: Direction,
  id: string,
  item: IItem,
}

interface Props {
  connectionState: ConnectionState,
  socket?: ControlSocket,
  shares: Share[],
  onCopyItem(id: string): void,
  onRemoveShare(id: string): void,
  onSend(item: IItem): void,
}

export default function Share(props: Props): JSX.Element {
  const { connectionState, socket, shares, onSend } = props;
  const { shareId } = useParams();
  const [fullUrl, setFullUrl] = useState(`${location}`);

  useEffect(() => {
    if (!socket) {
      return;
    }
    if (shareId) {
      socket.joinSession(shareId);
    }
    return () => { socket.leaveSession(); };
  }, [socket]);

  const onCopyItem = useCallback((id: string) => () => {
    return props.onCopyItem(id);
  }, [shares]);

  const onRemoveShare = useCallback((id: string) => () => {
    return props.onRemoveShare(id);
  }, [props.onRemoveShare]);

  const prettyShareId = shareId && shareId.replace(/^(...)(...)$/, '$1 $2');

  useEffect(() => {
    setFullUrl(`${location}`);
  }, [shareId]);

  return (
    <>
      <Header small={connectionState.status === 'connected'} />
      {(function(): JSX.Element {
        switch (connectionState.status) {
          case 'pending':
            return (
              <h1>
                <p>
                  <Link to={`/${shareId}`}>
                    {prettyShareId}
                  </Link>
                </p>
                <p>
                  <QRCodeSVG value={fullUrl} />
                </p>
                </h1>
            );
          case 'disconnected':
            return (
              <>
                <h2>Peer disconnected</h2>
                <Link className="button" to="/">Home</Link>
              </>
            );
          case 'error':
            return (
              <>
                <h2>{connectionState.error || "Something went wrong"}</h2>
                <Link className="button" to="/">Home</Link>
              </>
            );
          case 'connected':
            return (
              <>
                <Link to={`/${shareId}`} className="button">
                  {prettyShareId}
                </Link>
                {' '}
                <ShareForm onSubmit={onSend} />
                {!!shares.length && (
                  <>
                    <hr />
                    <ul className="unstyled">
                      {shares.map(({ id, item }, idx) => (
                        <li key={idx}>
                          <Item
                            item={item}
                            onCopyItem={onCopyItem(id)}
                            onRemoveItem={onRemoveShare(id)}
                          />
                        </li>
                      ))}
                    </ul>
                  </>
                )}
              </>
            );
        }
      })()}
    </>
  );
}
