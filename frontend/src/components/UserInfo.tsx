import React, { useEffect, useState } from "react";

type UserInfo = {
  loggedIn: boolean;
  email?: string;
};

export default function UserInfo() {
  const [userInfo, setUserInfo] = useState<UserInfo>();

  useEffect(() => {
    fetch("/api/oidc/userinfo")
      .then((response) => response.json())
      .then((data) => setUserInfo({ loggedIn: true, email: data.email }))
      .catch(() => setUserInfo({ loggedIn: false }));
  }, [setUserInfo]);

  return (
    <div className="user-banner">
      {!userInfo ? (
        <span></span>
      ) : !userInfo.loggedIn ? (
        <a
          href="/api/oidc/login"
          className="login"
          title="Login to access premium features."
        >
          Login
        </a>
      ) : (
        <>
          <span> {userInfo.email} </span>
          <a href="/api/oidc/logout" className="button">
            Logout
          </a>
        </>
      )}
    </div>
  );
}
