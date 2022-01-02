#!/usr/bin/env bash

npx concurrently \
    -n frontend,backend \
    -c green,red \
    "cd frontend && npm run frontend" \
    "cd server && python3 server.py"
