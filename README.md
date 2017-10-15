# silver
[![Build Status](https://travis-ci.org/nytopop/silver.svg?branch=master)](https://travis-ci.org/nytopop/silver)    [![Ready](https://badge.waffle.io/nytopop/silver.svg?label=ready&title=Ready)](http://waffle.io/nytopop/silver)  [![In Progress](https://badge.waffle.io/nytopop/silver.svg?label=in%20progress&title=In%20Progress)](http://waffle.io/nytopop/silver)

A bittorrent implementation in Haskell. This project aims to be a robust (-ish) implementation of the bittorrent protocol for use within other applications, and not a client in and of itself. The ultimate goal is not - as you might assume - to produce a production viable (read: your HTPC) product, but rather for me to get better at haskell. Ye who enter here be warned.

With that said, some example clients will be included in this distribution that should probably maybe kinda work correctly, sometimes.

## Support for BEPs
| BEP # | Title | Status |
| ----- | ----- | ------ |
| 0003 | The BitTorrent Protocol Specification | waiting  |
| 0005 | DHT Protocol | waiting |
| 0011 | Peer Exchange (PEX) | waiting |
| 0012 | Multitracker Metadata Extension | waiting |
| 0015 | UDP Tracker Protocol for BitTorrent | waiting |
| 0020 | Peer ID Conventions | gotta go fast |
| 0023 | Tracker Returns Compact Peer Lists | supported |
