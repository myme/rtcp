import React, { useState } from 'react';
import { Route, Routes } from 'react-router-dom';
import * as uuid from 'uuid';

import ConnectionManager from './ConnectionManager';
import Home from './Home';
import Session, { Direction, Share as IShare } from './Session';
import New from './New';
import { Session as ISession } from './ControlSocket';
import { ConnectionState, Item } from './PeerConnection';

import './style.css';

import { setLevel } from './Logger';
setLevel('log');

export default function App() {
  const [connectionState, setConnectionState] = useState<ConnectionState>({ status: 'pending' });
  const [shares, setShares] = useState<IShare[]>([]);
  const [session, setSession] = useState<ISession>();

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
  };

  const onShare = (item: Item) => addShare('outbound', item);

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
          <Session
            connectionState={connectionState}
            session={session}
            shares={shares}
            onCopyItem={onCopyItem}
            onRemoveShare={onRemoveLocalShare}
            onShare={onShare}
          />
        } />
      </Route>
    </Routes>
  );
}
