## Bech32 encoding and decoding

Bech32 is an encoding used in modern bitcoin addresses described in
[BIP-0173](https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki).

Lightning Network invoices and
[LNURLs](https://github.com/fiatjaf/lnurl-rfc) also use bech32. While
it makes sense to limit total bech32 string length to 90 characters
for bitcoin addresses, those other uses go beyond the limit, hence the
library doesn't enforce it.

## Functions:

* `(encode hrp sequence) => string`: provide bech32 string for a given
  human-readable part and a sequence of (MOD 32) elements.

* `(decode string) => (values hrp vector)`: decode bech32,
  returning the human-readable part and the vector of (MOD 32)
  elements.

* `(convert-bits sequence from to &optional pad) => vector`: rearrange
  bits from `from`-sized to `to`-sized bytes, optionally padding the
  last element with zeroes (if `pad` is true) or checking that the
  leftover *is* zero (otherwise). Supports 8-to-5 bits or 5-to-8 bits
  conversion only. Depending on the thing you encode or decode, this
  function might be useful for pre/post processing your data or its
  parts (e.g. in bitcoin addresses, witness version is an isolated
  5-bit number, while the rest of the address is really rearranged
  octets). See `example.lisp` for usage. 
