---
layout: post
title: "SSL on localhost with Phoenix Framework"
---

Sometimes when developing a backend, you want to use HTTPS even on localhost. I ran into this situation not long ago when I was building a mock service providing OAuth 2.0 authentication.

Here's how you can enable SSL on Elixir Phoenix in dev mode.

## 1. Get a certificate

For development purposes, you can generate a self-signed certificate with the following commands:

```bash
# generate key
$ openssl genrsa -out localhost.key 2048
# generate cert
$ openssl req -new -x509 -key localhost.key -out localhost.cert -days 3650 -subj /CN=localhost
```

You can of course also use [Letsencrypt](https://letsencrypt.org/) for a free signed certificate.

## 2. Copy the key and cert into your project

Put them in a directory called `priv/keys/`.

## 3. Set up the config

In your `config/dev.exs` file:

```elixir
config :phoenix_oauth2_mock_server, PhoenixOauth2MockServer.Endpoint,
  http: [port: 4000],
  https: [port: 4443,
          otp_app: :phoenix_oauth2_mock_server,
          keyfile: "priv/keys/localhost.key",
          certfile: "priv/keys/localhost.cert"],
  debug_errors: true,
  code_reloader: true,
  check_origin: false,
  watchers: [node: ["node_modules/brunch/bin/brunch", "watch", "--stdin"]]
```

**Done!**
