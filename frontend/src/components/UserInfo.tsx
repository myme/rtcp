import React, { useEffect, useState } from "react";

export default function UserInfo() {
  const [userInfo, setUserInfo] = useState<String>();

  useEffect(() => {
    fetch("/api/oidc/userinfo")
      .then((response) => response.json())
      .then((data) => setUserInfo(JSON.stringify(data, null, 2)));
  }, [setUserInfo]);

  return (
      <div>
        {!userInfo ? (
          <a href="/api/oidc/login" className="button">
            Login
          </a>
        ) : (
          <>
            <a href="/api/oidc/logout" className="button">
              Logout
            </a>
            <p>{userInfo}</p>
          </>
        )}
      </div>
  );
}
