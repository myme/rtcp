import React from 'react';
import { Link } from 'react-router-dom';

import { Session } from './ControlSocket';
import ShareForm from './ShareForm';
import Item from './Item';
import { Item as IItem } from './PeerConnection';
import { Share as IShare } from './Session';
import { prettifyShareId } from './utils';
import { getLogger } from './Logger';
import { usePeerConnection } from './ConnectionManager';

const logger = getLogger('Share');

interface Props {
  session?: Session,
  shares: IShare[],
  onCopyItem(id: string): void,
  onRemoveShare(id: string): void,
  onSend(item: IItem): IShare,
}

export default function Share({ session, shares, onCopyItem, onRemoveShare, onSend }: Props) {
  const peerConnection = usePeerConnection();

  if (!session) {
    logger.error('No session for share');
    return (
      <h2>
        {'Something went wrong'}
      </h2>
    );
  }

  const onSendHandler = (item: IItem) => {
    const share = onSend(item);
    if (!peerConnection) {
      logger.error('App::onSend(): No peer connection');
      return;
    }
    peerConnection.sendShare(share);
  };

  const onRemoveShareHandler = (id: string) => {
    onRemoveShare(id);
    if (!peerConnection) {
      logger.error('App::onLocalRemoveShare(): No peer connection');
      return;
    }
    peerConnection.removeShare(id);
  };

  return (
    <>
      <Link to={`/${session.id}`} className="button">
        {prettifyShareId(session.id)}
      </Link>
      {' '}
      <ShareForm onSubmit={onSendHandler} />
      {!!shares.length && (
        <>
          <hr />
          <ul className="unstyled">
            {shares.map(({ id, item }, idx) => (
              <li key={idx}>
                <Item
                  item={item}
                  onCopyItem={() => onCopyItem(id)}
                  onRemoveItem={() => onRemoveShareHandler(id)}
                />
              </li>
            ))}
          </ul>
        </>
      )}
    </>
  );
}
