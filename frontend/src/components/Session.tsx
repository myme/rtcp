import React from "react";
import { Link, useParams } from "react-router-dom";

import { Session as ISession } from "../ControlSocket";
import { ConnectionState, Item as IItem } from "../PeerConnection";
import { getLogger } from "../Logger";
import { Share as IShare } from '../share';

import Header from "./Header";
import Pending from "./Pending";
import Share from "./Share";
import Error from "./Error";

const logger = getLogger('Session');

interface Props {
  connectionState: ConnectionState;
  session?: ISession;
  shares: IShare[];
  onCopyItem(id: string): void;
  onRemoveShare(id: string): void;
  onShare(item: IItem): IShare;
}

export default function Session(props: Props): JSX.Element {
  const { connectionState, session, shares, onShare: onSend } = props;
  const { shareId } = useParams();

  if (!shareId) {
    logger.error("Invalid app state, no share id");
    return <h1>{"Something went wrong :("}</h1>;
  }

  return (
    <>
      <Header small={connectionState.status === "connected"} />
      {(() => {
        switch (connectionState.status) {
          case "pending":
            return <Pending session={session} shareId={shareId} />;
          case "disconnected":
            return (
              <>
                <h2>Peer disconnected</h2>
                <Link className="button" to="/">
                  Home
                </Link>
              </>
            );
          case "error":
            return <Error error={connectionState.error} />;
          case "connected":
            return (
              <Share
                session={session}
                shares={shares}
                onCopyItem={props.onCopyItem}
                onRemoveShare={props.onRemoveShare}
                onSend={onSend}
              />
            );
        }
      })()}
    </>
  );
}
