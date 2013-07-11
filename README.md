Complete content-type functionality as a complement to Snap's `method` and
`methods`.  Branches based on the value of the Accept header of a request,
automatically setting the Content-Type header of the response.

    route = accept ("text"        // "html") sendHtml
        <|> accept ("application" // "json") sendJson
        <|> send406Error

