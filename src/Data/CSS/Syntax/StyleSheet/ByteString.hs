-- To determine the text encoding, attempt each text encoding to look for an initial @charset, falling back to UTF8
-- An abstraction would do the test itself.
-- text (Data.Text.Encoding) module includes:
--  decodeLatin1
--  decodeUtf8
--  decodeUtf16LE
--  decodeUtf16BE
--  decodeUtf32LE
--  decodeUtf32BE