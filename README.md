# IMAP

This is an IMAP library for Haskell that aims to be efficient, easy to use, transparent when it comes to underlying libraries and support results streaming. To this end it employs `ListT`, so you can use it with any concurrency management library of your choosing.

It tries to implement [RFC3501](https://tools.ietf.org/html/rfc3501) as faithfully as possible, diverging from it where we noticed that servers have different ideas. If you want to understand this library, it's highly recommended to skim through that RFC first.

## Usage

Almost all of the commands available (`search` is an exception here) to you will output their results in `ListT` and `MonadIO`. Results consist of a list of `UntaggedResult`s followed by a single `TaggedResult` that describes the command result.

We've provided a helper function that simplifies the output types for the cases when you don't care about the streaming and just want a list of `UntaggedResult`s or an error message. Depending on your needs you will probably use it for all the commands that are not `FETCH`.

### Simple, no streaming

You need a connection object first, so that you can execute commands on it. It's produced by `connectServer`, which accepts parameters from [Network.Connection](https://hackage.haskell.org/package/connection-0.2.5/docs/Network-Connection.html#t:Connection). Say you want to connect to gmail:

    import Network.Connection

    let tls = TLSSettingsSimple False False False
    let params = ConnectionParams "imap.gmail.com" 993 (Just tls) Nothing
    conn <- connectServer params

From now on you can run commands on this connection. We will use the `simpleFormat` helper function to convert Let's log in:

    > simpleFormat $ login conn "mylogin" "mypass"
    Right [Capabilities [CIMAP4,CUnselect,CIdle,CNamespace,CQuota,CId,CExperimental "XLIST",CChildren,CExperimental "X-GM-EXT-1",CUIDPlus,CCompress "DEFLATE",CEnable,CMove,CCondstore,CEsearch,CUtf8 "ACCEPT",CListExtended,CListStatus,CAppendLimit 35882577]]

You can see that the server replied with a `CAPABILITIES` reply and the login was successful. Next, let's select an inbox:

    > simpleFormat $ select conn "inbox2"
    Left "[NONEXISTENT] Unknown Mailbox: inbox2 (Failure)"

Oh, let's fix that

    > simpleFormat $ select conn "inbox"
    Right [Flags [FAnswered,FFlagged,FDraft,FDeleted,FSeen,FOther "$NotPhishing",FOther "$Phishing",FOther "NonJunk"],PermanentFlags [FAnswered,FFlagged,FDraft,FDeleted,FSeen,FOther "$NotPhishing",FOther "$Phishing",FOther "NonJunk",FAny],UIDValidity 1,Exists 65,Recent 0,UIDNext 1050,HighestModSeq 251971]

Again you can use the metadata if you wish to. Let's see what messages we have ([consult the RFC](https://tools.ietf.org/html/rfc3501#section-6.4.4) if you're unsure about the parameter to `uidSearch`):

    > uidSearch conn "ALL"
    Right [105,219,411,424,425,748,763,770,774,819,824,825,826,827,828,841,842,852,857,858,909,926,946,948,955,962,965,975,984,985,989,990,991,992,993,994,999,1001,1003,1007,1008,1009,1010,1011,1014,1016,1017,1018,1020,1026,1027,1028,1029,1030,1034,1036,1037,1038,1039,1040,1042,1044,1047,1048,1049]

Search functions don't use `ListT` as they result in a bunch of numbers. If it is essential to you to have `ListT` here, open an issue. Fetching a message is straigtforward as well:

    > simpleFormat $ uidFetch conn "219"
    Right [Fetch [MessageId 2,UID 219,Body "Delivered-To: michal@monad.cat\r\nReceived: by...

If you need more control on the parameters of fetch, there is a more general function available to you:

    > simpleFormat $ uidFetchG conn "219 ALL"
    Right [Fetch [MessageId 2,UID 219,Envelope {eDate = Just "Tue, 10 Nov 2015 20:42:47 +0000", eSubject=...}, Flags [FAnswered,FSeen], Size 4880]]


Do you want multiple messages in one reply? That's easy with [UID ranges](https://tools.ietf.org/html/rfc3501#section-6.4.8)!

    > simpleFormat $ uidFetchG conn "219:9000 RFC822.SIZE"
    Right [Fetch [MessageId 2,UID 219,Size 4880],Fetch [MessageId 3,UID 411,Size 7392],...]

That's where streaming comes in handy - if these were message bodies you would probably like to do something with them before all are downloaded.

### Replies we didn't expect

### Streaming

## ToDo

We would like to see more tests. Parsing of `BODYSTRUCTURE` replies would be nice, but the output format seems to be poorly documented and a bit insane, so PRs are appreciated.
