# Roadburn Redux crawler

This is a quick Haskell project I made for getting the video URLs that were published during 15-18th April 2021 as part of Roadburn Redux.

Execute this with `cabal run roadburn-redux-crawler` and the `data` directory should be populated with a bunch of `json` files gotten from the API calls and a `txt` file containing the URLs of any post that was of type `video`, which can then be fed to the download manager of your choice or whatever.

Enjoy and support the bands you like!
