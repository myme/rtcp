import React from 'react';
import { Link } from 'react-router-dom';

import { Session } from './ControlSocket';
import ShareForm from './ShareForm';
import Item from './Item';
import { Item as IItem } from './PeerConnection';
import { Share as IShare } from './Session';
import { prettifyShareId } from './utils';
import { getLogger } from './Logger';

const logger = getLogger('Share');

interface Props {
  session?: Session,
  shares: IShare[],
  onCopyItem(id: string): void,
  onRemoveShare(id: string): void,
  onSend(item: IItem): void,
}

export default function Share({ session, shares, onCopyItem, onRemoveShare, onSend }: Props) {
  if (!session) {
    logger.error('No session for share');
    return (
      <h2>
        {'Something went wrong'}
      </h2>
    );
  }

  return (
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
                  onCopyItem={() => onCopyItem(id)}
                  onRemoveItem={() => onRemoveShare(id)}
                />
              </li>
            ))}
          </ul>
        </>
      )}
    </>
  );
}
