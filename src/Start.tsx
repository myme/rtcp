import React from 'react';
import { Link } from "react-router-dom";

import logo from '../favicon.svg';

export default function Start() {
  return (
    <>
      <h1>
        <Link to="/">
          <img src={logo} height="30px" className="App-logo" alt="logo" />
        </Link>
        xchg
      </h1>
      <Link to="/share">
        {'Start a new share'}
      </Link>
    </>
  );
}
