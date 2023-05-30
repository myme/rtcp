import React from "react";
import { Route, Routes } from "react-router-dom";

import { useShare } from "../share";

import ConnectionManager from "./ConnectionManager";
import Home from "./Home";
import New from "./New";
import Session from "./Session";

import "../style.css";

import { setLevel } from "../Logger";
setLevel("log");

export default function App() {
  const {
    addIncomingShare,
    onCopyItem,
    onRemoteShareRemoved,
    onRemoveLocalShare,
    onResetLocalShares,
    onShare,
    shares,
  } = useShare();

  return (
    <Routes>
      <Route index element={<Home />} />
      <Route
        element={
          <ConnectionManager
            onAddShare={addIncomingShare}
            onShareRemoved={onRemoteShareRemoved}
            onReset={onResetLocalShares}
          />
        }
      >
        <Route path="/new" element={<New />} />
        <Route
          path="/:shareId"
          element={
            <Session
              shares={shares}
              onCopyItem={onCopyItem}
              onRemoveShare={onRemoveLocalShare}
              onShare={onShare}
            />
          }
        />
      </Route>
    </Routes>
  );
}
