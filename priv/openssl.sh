#!/bin/sh

HOST="127.0.0.1"
PORT=11011

openssl s_client -connect $HOST:$PORT | openssl x509 -noout -modulus | openssl md5
