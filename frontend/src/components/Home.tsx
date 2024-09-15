import React, { useCallback } from "react";
import { Link, useNavigate } from "react-router-dom";

import Header from "./Header";
import JoinForm from "./JoinForm";

export default function Home() {
  const navigate = useNavigate();

  const joinShare = useCallback(
    (sessionId: string) => {
      navigate(`/${sessionId}`);
    },
    [navigate],
  );

  return (
    <>
      <Header />
      <JoinForm joinShare={joinShare} />
      <p>or</p>
      <p>
        <Link to="/new" className="button">
          Start a new share
        </Link>
      </p>
    </>
  );
}
