import React, { useCallback, useState } from 'react';
import { Route, Routes } from 'react-router-dom';
import * as uuid from 'uuid';

import ConnectionManager from './ConnectionManager';
import Home from './Home';
import Share, { Direction, Share as IShare } from './Share';
import New from './New';
import ControlSocket from './ControlSocket';
import PeerConnection, { ConnectionState, Item } from './PeerConnection';

import './style.css';

import { setLevel, getLogger } from './Logger';
setLevel('log');
const logger = getLogger('App');

export default function App() {
  const [controlSocket, setControlSocket] = useState<ControlSocket>();
  const [peerConnection, setPeerConnection] = useState<PeerConnection>();
  const [connectionState, setConnectionState] = useState<ConnectionState>({ status: 'pending' });
  const [shares, setShares] = useState<IShare[]>([]);

  const addShare = useCallback((direction: Direction, item: Item) => {
    const id = uuid.v4();
    const share = { id, direction, item };
    setShares(shares => shares.concat(share));
    return share;
  }, [setShares]);

  const addIncomingShare = useCallback((share: IShare) => {
    setShares(shares => shares.concat(share));
  }, [setShares]);

  const onCopyItem = useCallback((id: string) => {
    const share = shares.find(share => share.id === id);
    if (!share) {
      return;
    }
    navigator.clipboard.writeText(share.item.value);
  }, [shares]);

  const onRemoveLocalShare = useCallback((id: string) => {
    setShares(shares => shares.filter(share => share.id !== id));
    if (!peerConnection) {
      logger.error('App::onLocalRemoveShare(): No peer connection');
      return;
    }
    peerConnection.removeShare(id);
  }, [peerConnection, setShares]);

  const onRemoteShareRemoved = useCallback((id: string) => {
    setShares(shares => shares.filter(share => share.id !== id));
  }, [setShares]);

  const onSend = useCallback((item: Item) => {
    const share = addShare('outbound', item);
    if (!peerConnection) {
      logger.error('App::onSend(): No peer connection');
      return;
    }
    peerConnection.sendShare(share);
  }, [addShare, peerConnection]);

  return (
    <Routes>
      <Route index element={<Home />} />
      <Route element={
        <ConnectionManager
          onAddShare={addIncomingShare}
          onShareRemoved={onRemoteShareRemoved}
          setConnectionState={setConnectionState}
          setControlSocket={setControlSocket}
          setPeerConnection={setPeerConnection}
        />
      }>
        <Route path="/new" element={<New socket={controlSocket} />} />
        <Route path="/:shareId" element={
          <Share
            connectionState={connectionState}
            shares={shares}
            socket={controlSocket}
            onCopyItem={onCopyItem}
            onRemoveShare={onRemoveLocalShare}
            onSend={onSend}
          />
        } />
      </Route>
    </Routes>
  );
}
