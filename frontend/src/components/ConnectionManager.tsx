import React, { useContext, useEffect, useMemo } from 'react';
import { Outlet } from "react-router";

import ControlSocket, { Session } from '../ControlSocket';
import PeerConnection, { ConnectionState } from '../PeerConnection';
import { Share } from '../share';

interface Props {
  onAddShare(share: Share): void,
  onShareRemoved(id: string): void,
  setConnectionState(connectionState: ConnectionState): void,
  setSession(session?: Session): void;
}

interface Context {
  controlSocket?: ControlSocket,
  peerConnection?: PeerConnection,
}

const ConnectionContext = React.createContext<Context>({});

export function useControlSocket() {
  const { controlSocket } = useContext(ConnectionContext);
  return controlSocket;
}

export function usePeerConnection() {
  const { peerConnection } = useContext(ConnectionContext);
  return peerConnection;
}

export default function ConnectionManager(props: Props) {
  const { onAddShare, onShareRemoved, setConnectionState, setSession } = props;

  const controlSocket = useMemo(() => new ControlSocket({
    onError(error: string) {
      setConnectionState({ status: 'error', error });
    },
    onIceServersUpdated(iceServers) {
      peerConnection.setIceServers(iceServers);
    },
    onPeerJoined() {
      peerConnection.sendOffer();
    },
    onBroadcast(message) {
      peerConnection.handleControlMessage(message);
    },
    setSession,
  }), []);

  const peerConnection = useMemo(() => new PeerConnection({
    onConnectionStateChange: setConnectionState,
    onIceCandidate(candidate) {
      controlSocket.broadcast('candidate', { candidate });
    },
    onSessionDescription(type, description) {
      controlSocket.broadcast(type, { description });
    },
    onShare: onAddShare,
    onRemoveShare: onShareRemoved,
  }), []);

  // Cleanup
  useEffect(() => () => {
    // Next tick to let child components send leave message
    setTimeout(() => {
      controlSocket.close();
      peerConnection.close();
      setConnectionState({ status: 'pending' });
    }, 0);
  }, []);

  return (
    <ConnectionContext.Provider value={{ controlSocket, peerConnection}}>
      <Outlet />
    </ConnectionContext.Provider>
  );
}
