import React from 'react';
import { Link, Outlet } from 'react-router-dom';

import logo from '../favicon.svg';
import './style.css';


export default function App() {
  return (
    <>
      <h1>
        <Link to="/">
          <img src={logo} height="30px" className="App-logo" alt="logo" />
        </Link>
        xchg
      </h1>
      <Outlet />
    </>
  );
}
