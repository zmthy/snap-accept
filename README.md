snap-accept
===========

[![Build Status](https://secure.travis-ci.org/zmthy/snap-accept.svg)](http://travis-ci.org/zmthy/snap-accept)

Complete content-type functionality as a complement to Snap's `method`
and `methods`.  Branches based on the value of the Accept header of a
request, automatically setting the Content-Type header of the response.

```haskell
route = acceptsMedia
    [ ("text/html", sendHtml)
    , ("application/json", sendJson)
    ] <|> send406Error
```
