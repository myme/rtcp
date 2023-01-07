import React, { useCallback } from 'react';
import { Link, useParams } from 'react-router-dom';
import { QRCodeSVG } from 'qrcode.react';

import Header from './Header';
import { Session } from './ControlSocket';
import PinForm from './PinForm';
import ShareForm from './ShareForm';
import { ConnectionState, Item as IItem } from './PeerConnection';
import Item from './Item';
import { getLogger } from './Logger';
import { useControlSocket } from './ConnectionManager';

const logger = getLogger('Share');

export type Direction = 'inbound' | 'outbound';
export interface Share {
  direction: Direction,
  id: string,
  item: IItem,
}

interface Props {
  connectionState: ConnectionState,
  session?: Session,
  shares: Share[],
  onCopyItem(id: string): void,
  onRemoveShare(id: string): void,
  onSend(item: IItem): void,
}

function prettifyShareId(shareId: string) {
  return shareId.replace(/^(...)(...)$/, '$1 $2');
}

export default function Share(props: Props): JSX.Element {
  const { connectionState, session, shares, onSend } = props;
  const controlSocket = useControlSocket();
  const { shareId } = useParams();

  const onCopyItem = useCallback((id: string) => () => {
    return props.onCopyItem(id);
  }, [shares]);

  const onRemoveShare = useCallback((id: string) => () => {
    return props.onRemoveShare(id);
  }, [props.onRemoveShare]);

  if (!shareId) {
    logger.error('Invalid app state, no share id');
    return (
      <h1>
        {'Something went wrong :('}
      </h1>
    );
  }

  const onConnectWrapper = (sharePin: string) => {
    if (!controlSocket) {
      logger.error('No control socket!');
      return;
    }
    controlSocket.joinSession({ id: shareId, pin: sharePin });
  };

  return (
    <>
      <Header small={connectionState.status === 'connected'} />
      {(function (): JSX.Element {
        switch (connectionState.status) {
          case 'pending':
            // TODO: Don't nest in H1
            return !session
              ? (
                <>
                  <h1>
                    {`Connect to share: ${prettifyShareId(shareId)}`}
                  </h1>
                  <PinForm onSubmit={onConnectWrapper} />
                </>
              ) : (
                <h1>
                  <p>
                    Share ID:
                    <Link to={`/${session.id}`}>
                      {prettifyShareId(session.id)}
                    </Link>
                  </p>
                  <p>
                    {`PIN: ${session?.pin}`}
                  </p>
                  <p>
                    <QRCodeSVG value={`${location}`} />
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
            return !session
              ? <div>spin spin</div>
              : (
                <>
                  <Link to={`/${session.id}`} className="button">
                    {prettifyShareId(session.id)}
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
