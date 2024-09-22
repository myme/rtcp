# rtcp

<img src="./frontend/favicon.svg" width="50px" />

Exchange stuff between things using WebRTC.

https://rtcp.cc

## Development

### Run the development server

This runs both the frontend `vite` and backend `Haskell` development servers
which should reload on changes.

``` sh
npm run .#dev
```

### Run the frontend

It's possible to run just the frontend development server using `npm`:

``` sh
cd frontend
npm start
```

### Run the backend

It's possible to run just the backend server using `ghcid`:

``` sh
cd server

# First time only
hpack
cabal build

# Run a reloading server
ghcid -r
```

