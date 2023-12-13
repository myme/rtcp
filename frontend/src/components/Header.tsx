import React from 'react';
import { Link } from 'react-router-dom';

import logoBig from '../../phoop-big.png';
import logoSmall from '../../phoop-small.png';

interface Props {
  small?: boolean;
}

export default function Header(props: Props) {
  const { small } = props;

  if (small) {
    return (
      <h2>
        <Link to="/">
          <img src={logoSmall} className="app-logo-small" alt="logo" />
        </Link>
      </h2>
    );
  }

  return (
    <Link to="/">
      <img src={logoBig} className="app-logo" height="300px" alt="logo" />
    </Link>
  )
}
