import React, { useState } from 'react';
import { Route, Routes } from 'react-router-dom';
import * as uuid from 'uuid';

import ConnectionManager, { usePeerConnection } from './ConnectionManager';
import Home from './Home';
import Share, { Direction, Share as IShare } from './Share';
import New from './New';
import { Session } from './ControlSocket';
import { ConnectionState, Item } from './PeerConnection';

import './style.css';

import { setLevel, getLogger } from './Logger';
setLevel('log');
const logger = getLogger('App');

export default function App() {
  const [connectionState, setConnectionState] = useState<ConnectionState>({ status: 'pending' });
  const [shares, setShares] = useState<IShare[]>([]);
  const [session, setSession] = useState<Session>();
  const peerConnection = usePeerConnection();

  const addShare = (direction: Direction, item: Item) => {
    const id = uuid.v4();
    const share = { id, direction, item };
    setShares(shares => shares.concat(share));
    return share;
  };

  const addIncomingShare = (share: IShare) => {
    setShares(shares => shares.concat(share));
  };

  const onCopyItem = (id: string) => {
    const share = shares.find(share => share.id === id);
    if (!share) return;
    navigator.clipboard.writeText(share.item.value);
  };

  const onRemoteShareRemoved = (id: string) => {
    setShares(shares => shares.filter(share => share.id !== id));
  };

  const onRemoveLocalShare = (id: string) => {
    setShares(shares => shares.filter(share => share.id !== id));
    if (!peerConnection) {
      logger.error('App::onLocalRemoveShare(): No peer connection');
      return;
    }
    peerConnection.removeShare(id);
  };

  const onSend = (item: Item) => {
    const share = addShare('outbound', item);
    if (!peerConnection) {
      logger.error('App::onSend(): No peer connection');
      return;
    }
    peerConnection.sendShare(share);
  };

  return (
    <Routes>
      <Route index element={<Home />} />
      <Route element={
        <ConnectionManager
          onAddShare={addIncomingShare}
          onShareRemoved={onRemoteShareRemoved}
          setConnectionState={setConnectionState}
          setSession={setSession}
        />
      }>
        <Route path="/new" element={<New />} />
        <Route path="/:shareId" element={
          <Share
            connectionState={connectionState}
            session={session}
            shares={shares}
            onCopyItem={onCopyItem}
            onRemoveShare={onRemoveLocalShare}
            onSend={onSend}
          />
        } />
      </Route>
    </Routes>
  );
}
