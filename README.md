# IMAP library

### TODO

- [x] A continuation chunked parser
- [x] Way simpler output types
- [x] Output with ListT (support streaming)
- [x] A testing framework
- [ ] Support moar commands
  - [x] All the little commands
    - [x] SELECT
    - [x] EXAMINE
    - [x] CREATE
    - [x] DELETE
    - [x] RENAME
    - [x] SUBSCRIBE
    - [x] UNSUBSCRIBE
    - [x] LIST
    - [x] LSUB
    - [x] STATUS
    - [x] CHECK
    - [x] CLOSE
    - [x] EXPUNGE
  - [x] Search
  - [x] Fetch
  - [ ] Rest
    - [ ] STARTTLS
    - [ ] AUTHENTICATE
    - [ ] BYE at any time
    - [ ] APPEND
- [x] Ability to disconnect (support bye)
- [ ] Simplify output format of commands that have a single untagged reply required (search)
- [ ] Connection timeout
- [ ] Enable configuration when connecting
- [ ] Keep the connection alive
- [ ] React to exceptions
