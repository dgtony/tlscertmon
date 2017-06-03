# TLS certificate check
Simple service for remote health check of TLS-certificates. Designed to be a data source for [Prometheus](https://prometheus.io) monitoring system.

Could be useful for monitoring infrastructure of numerous services with short-living TLS-certificates, e.g. obtained from LetsEncrypt.


## Build

```
cabal sandbox init
cabal install
```

## Usage

In order to monitor TLS-certificates at first one need to create config file with description of endpoints. Simple example could be found in file _sample.yaml_.

There are three sections in config file to be filled:

* general
* certificates:
* keys


#### - General
Here one defines monitoring service port for prometheus server, path to collect metrics and interval between check procedures in seconds.

#### - Certificates
In this section endpoints to monitor TLS-certificate expiration time are described.
One should define hostname and port for each service to monitor. Service labels will be constructed as `<host>:<port>`.

For each item in section _certificates_ monitor get number of days remaining to certificate expiration.


#### - Keys
Besides of certificate expiration, monitor could also check public key signature in order to detect key change.
In this section endpoints are defined in a similar way, but there is additional field `key` - public key signature.
Public key signature for each endpoint could be obtained with Openssl:

```
#!/bin/sh

HOST="127.0.0.1"
PORT=11011

openssl s_client -connect $HOST:$PORT | openssl x509 -noout -modulus | openssl md5
```



## Result interpretation
Monitoring service expose interface for Prometheus server on the defined port and path.
Results look as follows:

```
# TYPE crt_mon_exp gauge
crt_mon_exp{host="www.google.com:443"} 74.0
crt_mon_exp{host="www.yandex.ru:443"} 714.0
# TYPE crt_mon_key_valid gauge
crt_mon_key_valid{host="127.0.0.1:11011"} -1000.0
```

Here metric `crt_mon_exp` gives certificate expiration time in days and `crt_mon_key_valid` indicates if endpoint public key remains the same (value: 1) or been changed (value: 0).

All metric values could be distinguished by `host` label.


### Errors

Any possible error must be also encoded as a float number in order to be processed with Prometheus. To separate values and errors the latter are negative.

Here is mapping table for general errors:

|      Error description      | Value |
|-----------------------------|-------|
| endpoint connection error   | -1000 |
| certificate retrieval error | -2000 |
| public key retrieval error  | -3000 |


Certificate expiration time errors:

| Certificate error         | Value |
|---------------------------|-------|
| certificate not valid yet | -100  |
| certificate expired       | -200  |


### Modifications
All described mappings could be easily changed.
One just need to modify following functions in file TLSProcessing.hs and recompile the project:

 * `errToMetric`
 * `daysToMetric`
 * `keyValidToMetric`
 

