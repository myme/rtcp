import React from 'react';
import { Link } from 'react-router-dom';

import logo from '../favicon.svg';

interface Props {
  small?: boolean;
}

export default function Header(props: Props) {
  const { small } = props;

  if (small) {
    return (
      <Link to="/">
        <h2>
          <img src={logo} className="app-logo" alt="logo" />
          <span>xchg</span>
        </h2>
      </Link>
    );
  }

  return (
    <Link to="/">
      <h1>
        <img src={logo} className="app-logo" alt="logo" />
        <span>xchg</span>
      </h1>
    </Link>
  )
}
