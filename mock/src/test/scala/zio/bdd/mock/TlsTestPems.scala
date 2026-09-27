package zio.bdd.mock

/**
 * Checked-in TLS test material (#343): an EC P-256 test CA, a `localhost`
 * server certificate it signed (SANs localhost, 127.0.0.1, ::1), a client
 * certificate it signed, and a second, unrelated CA with its own client
 * certificate. Keys are PKCS#8. Valid until 2126, so no test depends on
 * generating certificates. Regenerate with `openssl` only if a fixture must
 * change.
 */
object TlsTestPems:
  val caCert: String =
    """-----BEGIN CERTIFICATE-----
      |MIIBmjCCAUGgAwIBAgIUWCWdok8qOZA7llceeXC5YkRx/bcwCgYIKoZIzj0EAwIw
      |GjEYMBYGA1UEAwwPemlvLWJkZCB0ZXN0IENBMCAXDTI2MDkyNzE0NDMwNloYDzIx
      |MjYwOTAzMTQ0MzA2WjAaMRgwFgYDVQQDDA96aW8tYmRkIHRlc3QgQ0EwWTATBgcq
      |hkjOPQIBBggqhkjOPQMBBwNCAAReCcMnIppW1hjD4qAmBasnZVE1Y0jbusUcF09+
      |kH1tdszYM9uoM9LsVDv1VcjD9G0swDzU95YKXp6cXKpX/14lo2MwYTAdBgNVHQ4E
      |FgQUtEJ15AwYBVtOnVlTEKeYWEuRJZowHwYDVR0jBBgwFoAUtEJ15AwYBVtOnVlT
      |EKeYWEuRJZowDwYDVR0TAQH/BAUwAwEB/zAOBgNVHQ8BAf8EBAMCAQYwCgYIKoZI
      |zj0EAwIDRwAwRAIgLTtdKOlQM3XqpwgWmPC/hBLZlrnlt6HB6IHEZ3jESokCIGlI
      |vI8yZR+ph3FVkPOrarhTRM9n2WJ3+RZbdwbgFvWm
      |-----END CERTIFICATE-----""".stripMargin

  val serverCert: String =
    """-----BEGIN CERTIFICATE-----
      |MIIB1DCCAXqgAwIBAgIUC7aOErwN+s/g6/6DNAi5oSKHHBYwCgYIKoZIzj0EAwIw
      |GjEYMBYGA1UEAwwPemlvLWJkZCB0ZXN0IENBMCAXDTI2MDkyNzE0NDMwNloYDzIx
      |MjYwOTAzMTQ0MzA2WjAUMRIwEAYDVQQDDAlsb2NhbGhvc3QwWTATBgcqhkjOPQIB
      |BggqhkjOPQMBBwNCAASdRek7RdaecS77ZiqjqrNOlqqt7wqShcGuHFCGG1xyPJ+b
      |IRyjtkJsqnDr87bOYVHNDctInmBp76U1eb1scXQbo4GhMIGeMAkGA1UdEwQCMAAw
      |DgYDVR0PAQH/BAQDAgeAMBMGA1UdJQQMMAoGCCsGAQUFBwMBMCwGA1UdEQQlMCOC
      |CWxvY2FsaG9zdIcEfwAAAYcQAAAAAAAAAAAAAAAAAAAAATAdBgNVHQ4EFgQUyFLY
      |CvJ1wvPXFljILnaRHchhKnwwHwYDVR0jBBgwFoAUtEJ15AwYBVtOnVlTEKeYWEuR
      |JZowCgYIKoZIzj0EAwIDSAAwRQIgH7Q4fky1qEYqzX2e+Vc9EbHNzTANAo3bq/F+
      |Z2wd6DICIQCk7uxGzU2H/F833XPjukDf8hwi/dsxKCflF3fKRUJ77g==
      |-----END CERTIFICATE-----""".stripMargin

  val serverKey: String =
    """-----BEGIN PRIVATE KEY-----
      |MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQg770hYEnmaBXOfqGw
      |l0wheXk/uJfi4Fz9H/rZf0bunc6hRANCAASdRek7RdaecS77ZiqjqrNOlqqt7wqS
      |hcGuHFCGG1xyPJ+bIRyjtkJsqnDr87bOYVHNDctInmBp76U1eb1scXQb
      |-----END PRIVATE KEY-----""".stripMargin

  val clientCert: String =
    """-----BEGIN CERTIFICATE-----
      |MIIBrjCCAVSgAwIBAgIUC7aOErwN+s/g6/6DNAi5oSKHHBcwCgYIKoZIzj0EAwIw
      |GjEYMBYGA1UEAwwPemlvLWJkZCB0ZXN0IENBMCAXDTI2MDkyNzE0NDMwNloYDzIx
      |MjYwOTAzMTQ0MzA2WjAeMRwwGgYDVQQDDBN6aW8tYmRkIHRlc3QgY2xpZW50MFkw
      |EwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEXbsUpOZuJIPBVF0RHWy45+EZGb3/HfcP
      |9FWG5nTPcn4eZSZhkbxk5z+Ax2WnWyALBNYNzRXzJzLTD4e1fNFqJqNyMHAwCQYD
      |VR0TBAIwADAOBgNVHQ8BAf8EBAMCB4AwEwYDVR0lBAwwCgYIKwYBBQUHAwIwHQYD
      |VR0OBBYEFJ08fQiesI0JZ1J+U3TimA6iUimxMB8GA1UdIwQYMBaAFLRCdeQMGAVb
      |Tp1ZUxCnmFhLkSWaMAoGCCqGSM49BAMCA0gAMEUCIQDOHeAjVE8Rks2OcVGQ6QUR
      |w6s1JU5c4g9TwZFqKgxdCQIgW2NSE1T33qz7xIKm7HaLWj/VWeSal+6Rtu59jUW+
      |s7k=
      |-----END CERTIFICATE-----""".stripMargin

  val clientKey: String =
    """-----BEGIN PRIVATE KEY-----
      |MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQgo5sisQvNJIRqfbn6
      |OxFXisNiWG81v7cwhIw/tr6y1ImhRANCAARduxSk5m4kg8FUXREdbLjn4RkZvf8d
      |9w/0VYbmdM9yfh5lJmGRvGTnP4DHZadbIAsE1g3NFfMnMtMPh7V80Wom
      |-----END PRIVATE KEY-----""".stripMargin

  val otherCaCert: String =
    """-----BEGIN CERTIFICATE-----
      |MIIBrjCCAVWgAwIBAgIUII/Ab2H4OMIVoEFkFmGb1i9MbU8wCgYIKoZIzj0EAwIw
      |JDEiMCAGA1UEAwwZemlvLWJkZCB1bnRydXN0ZWQgdGVzdCBDQTAgFw0yNjA5Mjcx
      |NDQzMDZaGA8yMTI2MDkwMzE0NDMwNlowJDEiMCAGA1UEAwwZemlvLWJkZCB1bnRy
      |dXN0ZWQgdGVzdCBDQTBZMBMGByqGSM49AgEGCCqGSM49AwEHA0IABJRQuazBkbNB
      |i8qLskqpS6LhbzOW4lXOAmG0on+6nuyNgiwb1KTP5UVLDq51OH089dFmJsC2ouzg
      |bnv0Hat2Lr+jYzBhMB0GA1UdDgQWBBQ+CC951ag7Hf2pFZd2oHwhzMYdhzAfBgNV
      |HSMEGDAWgBQ+CC951ag7Hf2pFZd2oHwhzMYdhzAPBgNVHRMBAf8EBTADAQH/MA4G
      |A1UdDwEB/wQEAwIBBjAKBggqhkjOPQQDAgNHADBEAiAiTFUa6In/qjDJwDv3VWXJ
      |ej2Ny0DVUd1THq5taloWGAIgD2xifxAraNDC8If1jSq3CCLXXjXPCsAnmOZp14Ya
      |gYc=
      |-----END CERTIFICATE-----""".stripMargin

  val otherClientCert: String =
    """-----BEGIN CERTIFICATE-----
      |MIIBvjCCAWOgAwIBAgIUISwg9EJSXHP1KIynLInUDZnE53QwCgYIKoZIzj0EAwIw
      |JDEiMCAGA1UEAwwZemlvLWJkZCB1bnRydXN0ZWQgdGVzdCBDQTAgFw0yNjA5Mjcx
      |NDQzMDZaGA8yMTI2MDkwMzE0NDMwNlowIzEhMB8GA1UEAwwYemlvLWJkZCB1bnRy
      |dXN0ZWQgY2xpZW50MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEafFzN34/w/ic
      |6mzCPF3xr/4Ain6ECkobaI2zjC6aey1RgaelEDjQUTgNbMU22YOfBb7CqIcGkIqr
      |FSzFeP15o6NyMHAwCQYDVR0TBAIwADAOBgNVHQ8BAf8EBAMCB4AwEwYDVR0lBAww
      |CgYIKwYBBQUHAwIwHQYDVR0OBBYEFPFXfU8OdOJaUbfqIhp7gU4PTRI1MB8GA1Ud
      |IwQYMBaAFD4IL3nVqDsd/akVl3agfCHMxh2HMAoGCCqGSM49BAMCA0kAMEYCIQCJ
      |O3+NLu5nzxah5xiRJYVGZMb/HBmj0RVPNNvuY0bNBQIhAKixeA71FvYN1CMll5FA
      |S4HyooFxWtrFDItrtfo3zq63
      |-----END CERTIFICATE-----""".stripMargin

  val otherClientKey: String =
    """-----BEGIN PRIVATE KEY-----
      |MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQgnz5vBQ8DkKSWkhMt
      |kW5np8xZcIsrnK195UPgmnwGw1yhRANCAARp8XM3fj/D+JzqbMI8XfGv/gCKfoQK
      |ShtojbOMLpp7LVGBp6UQONBROA1sxTbZg58FvsKohwaQiqsVLMV4/Xmj
      |-----END PRIVATE KEY-----""".stripMargin
