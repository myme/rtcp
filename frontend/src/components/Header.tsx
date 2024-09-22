import React from "react";
import { Link } from "react-router-dom";

import logo from "../../favicon.svg";
import UserInfo from "./UserInfo";

export default function Header() {
  let link = (
    <Link to="/">
      <h2>
        <img src={logo} className="app-logo" alt="logo" /> <span>rtcp</span>
      </h2>
    </Link>
  );

  return (
    <header>
      {link}
      <UserInfo />
    </header>
  );
}
