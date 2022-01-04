import React from 'react';
import { Link } from 'react-router-dom';

import logo from '../favicon.svg';

export default function Header() {
  return (
    <Link to="/">
      <img src={logo} height="60px" className="App-logo" alt="logo" />
      <h1>xchg</h1>
    </Link>
  )
}
