{-# LANGUAGE OverloadedStrings #-}

module Ssl.Util where

import Control.Exception (Exception)

-- Cipher Suites ------------------------------------------------------------

{- | A small list of strong cipher suites for use with 'contextSetCiphers'
that includes only a selected subset of those based on RSA signatures over
ephemeral DH key exchanges (for perfect forward secrecy) and are thus
compatible with the RSA public key pinning implemented by the functions
'rsaFingerprint' and 'verifyRsaFingerprint'.

As in TLS 1.3 [1], only AEAD cipher suites are included, specifically only
AES-GCM and CHACHA20-POLY1305. Thereby preference is applied as follows:

 * Elliptic curve DH variants are preferred over "classic" finite
   field variants for efficiency.
 * AES variants are preferred over ChaCha20 variants for performance,
   assuming AES-NI support [2].
 * AES-256 is preferred over AES-128 "because we can" and performance
   is not significantly worse, though the comparable key sizes needed for
   RSA and DH to achieve a comparable level of security to 256 bit
   symmetric keys are typically not used (see [3]).

This list requires on both ends of a connection either a TLS 1.2
implementation that includes RFC5288 [4] (e.g. OpenSSL 1.0.1+) or a
TLS 1.3 implementation that includes at least the mandatory cipher
suites. For a list of OpenSSL cipher suites and how they map to TLS
names, see also [5].

References:

[1] https://tlswg.github.io/tls13-spec/#rfc.appendix.A.4
[2] https://calomel.org/aesni_ssl_performance.html
[3] https://www.keylength.com/en/3/
[4] https://tools.ietf.org/html/rfc5288#section-3
[5] https://www.openssl.org/docs/manmaster/apps/ciphers.html
-}
rsaCiphers :: String
rsaCiphers =
  showString "ECDHE-RSA-AES256-GCM-SHA384," -- TLS 1.3
    . showString "ECDHE-RSA-AES128-GCM-SHA256," -- TLS 1.3 (mandatory)
    . showString "ECDHE-RSA-CHACHA20-POLY1305," -- TLS 1.3
    . showString "DHE-RSA-AES256-GCM-SHA384," -- TLS 1.2 / TLS 1.3
    . showString "DHE-RSA-AES128-GCM-SHA256" -- TLS 1.2 / TLS 1.3
    . showString "DHE-RSA-CHACHA20-POLY1305" -- TLS 1.3
    $ ""

-- Public Key Pinning
--
-- Overview: https://www.owasp.org/index.php/Certificate_and_Public_Key_Pinning

-- | Exception thrown by 'verifyFingerprint'
data PinPubKeyException
  = -- | No peer certificate was found
    PinMissingCert
  | -- | A peer certificate failed validation (e.g. signature or expiry).
    PinInvalidCert
  | -- | The peer certificate does not contain a valid public key
    PinInvalidPubKey
  | -- | The public key fingerprint of the peer certificate
    -- | did not match any of the pinned fingerprints
    PinFingerprintMismatch
  deriving (Eq, Show)

instance Exception PinPubKeyException

