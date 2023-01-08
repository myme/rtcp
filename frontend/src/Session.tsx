import React from "react";
import { Link, useParams } from "react-router-dom";

import Header from "./Header";
import { Session as ISession } from "./ControlSocket";
import Pending from "./Pending";
import Share from "./Share";
import { ConnectionState, Item as IItem } from "./PeerConnection";
import { getLogger } from "./Logger";
import Error from "./Error";

const logger = getLogger('Session');

export type Direction = "inbound" | "outbound";
export interface Share {
  direction: Direction;
  id: string;
  item: IItem;
}

interface Props {
  connectionState: ConnectionState;
  session?: ISession;
  shares: Share[];
  onCopyItem(id: string): void;
  onRemoveShare(id: string): void;
  onSend(item: IItem): void;
}

export default function Session(props: Props): JSX.Element {
  const { connectionState, session, shares, onSend } = props;
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
