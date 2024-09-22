import React from "react";
import { Link } from "react-router-dom";

import { Session } from "../ControlSocket";
import { Item as IItem } from "../PeerConnection";
import { Share as IShare } from "../share";
import { prettifyShareId } from "../utils";
import { getLogger } from "../Logger";

import ShareForm from "./ShareForm";
import Item from "./Item";
import { usePeerConnection } from "./ConnectionManager";

const logger = getLogger("Share");

interface Props {
  session?: Session;
  shares: IShare[];
  onCopyItem(id: string): void;
  onRemoveShare(id: string): void;
  onSend(clientId: string, item: IItem): IShare;
}

export default function Share({
  session,
  shares,
  onCopyItem,
  onRemoveShare,
  onSend,
}: Props) {
  const peerConnection = usePeerConnection();

  if (!session) {
    logger.error("No session for share");
    return <h2>{"Something went wrong"}</h2>;
  }

  const onSendHandler = (item: IItem) => {
    if (!peerConnection) {
      logger.error("App::onSend(): No peer connection");
      return;
    }
    const clientId = peerConnection.getClientId();
    if (!clientId) {
      logger.error("App::onSend(): No client ID");
      return;
    }
    const share = onSend(clientId, item);
    peerConnection.sendShare(share);
  };

  const onRemoveShareHandler = (id: string) => {
    onRemoveShare(id);
    if (!peerConnection) {
      logger.error("App::onLocalRemoveShare(): No peer connection");
      return;
    }
    peerConnection.removeShare(id);
  };

  return (
    <>
      <div>
        <Link to={`/${session.id}`} className="button">
          {prettifyShareId(session.id)}
        </Link>{" "}
        <ShareForm onSubmit={onSendHandler} />
      </div>
      {!!shares.length && (
        <>
          <hr />
          <ul className="unstyled">
            {shares.map(({ clientId, id, item }, idx) => (
              <li key={idx}>
                <Item
                  clientId={clientId}
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
