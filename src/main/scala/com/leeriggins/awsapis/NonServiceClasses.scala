package com.leeriggins.awsapis

/** Manages digests of directories except 'service'.
  */
object NonServiceClasses {
  val digests = Map(
    "dynamodb/converter.d.ts"                              -> "63f8b3738b4ea489d797d144679247fa",
    "s3/managed_upload.d.ts"                               -> "0774db96ea6ab521c2c0fe5a2a94232e",
    "credentials"                                          -> "09ecdf9e3e89a3c0dea326dd75540784",
    "credentials/chainable_temporary_credentials.d.ts"     -> "625400713394777a36b62a338d7729aa",
    "credentials/cognito_identity_credentials.d.ts"        -> "7551294d419c49e38e5c8df695d35fab",
    "credentials/credential_provider_chain.d.ts"           -> "47418be991db6d116b1c264d79f01c4b",
    "credentials/ec2_metadata_credentials.d.ts"            -> "10c1f1d95c60407191f785d864d0b384",
    "credentials/ecs_credentials.d.ts"                     -> "e8e9f110846359c86b56f85d49d1737d",
    "credentials/environment_credentials.d.ts"             -> "f341528d9a3d561ec93df8330e27a77c",
    "credentials/file_system_credentials.d.ts"             -> "ae7b64d4b3643793f206f443f08725a5",
    "credentials/process_credentials.d.ts"                 -> "d162410290f8b326be0d1c008e0f9bdd",
    "credentials/remote_credentials.d.ts"                  -> "182855a8e48a2ba4425302c6d872a82c",
    "credentials/saml_credentials.d.ts"                    -> "a99c59fd6607b27b3f3a37a848d56423",
    "credentials/shared_ini_file_credentials.d.ts"         -> "b06eab7acee130014f8d53b1f712d274",
    "credentials/temporary_credentials.d.ts"               -> "d8c450ad0c53333011ee07933ed7f2a5",
    "credentials/token_file_web_identity_credentials.d.ts" -> "7803a38e7368b08b499d719f07495828",
    "credentials/web_identity_credentials.d.ts"            -> "15c734453e67888bf1f04dde1693ee53"
  )
}
