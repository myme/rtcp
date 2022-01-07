#!/usr/bin/env bash

npx concurrently \
    -n FE,BE \
    -c green,red \
    "cd frontend && npm run frontend" \
    "cd server && ghcid -r Main"
