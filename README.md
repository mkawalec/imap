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
  - [x] Enable configuration when connecting
  - [x] Simplify output format of commands that have a single untagged reply required (search)
  - [x] Connection timeout
  - [x] React to exceptions
  - [ ] Rest
    - [ ] STARTTLS
    - [ ] AUTHENTICATE
    - [x] BYE at any time
    - [ ] APPEND
    - [ ] STORE
    - [ ] COPY
- [x] Ability to disconnect (support bye)
- [ ] Comments
- [ ] Tests
