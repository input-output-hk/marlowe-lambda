# A Marlowe Runtime client for AWS Lambda

This [Marlowe Runtime](https://github.com/input-output-hk/marlowe-cardano/tree/main/marlowe-runtime) client for the AWS Lambda service enables users to create, apply inputs to, and withdraw funds from Marlowe contract instances; it also lets them list all of the Marlowe contracts on the blockchain and to examine their on-chain status and contents.  Users do not need to install any Marlowe software and the only Cardano software they need is a wallet for signing the transactions. (The example here uses several command-line tools for convenience, but any toolset for calling AWS Lambda, manipulating JSON, and signing/submitting transactions could be used.) The service may be used with any Marlowe contract. In addition to providing the convenience of zero-installation use of Marlowe, AWS Lambda also supports the orchestration of complex workflows involving multiple Marlowe contracts or a mixture of Marlowe and non-Marlowe lambdas."

See [this Jupyter notebook](examples/zcb.ipynb) or [this video](https://youtu.be/huXbRyrmW60) for a demonstration of this client's use from the command line.

See [web/](web/ReadMe.md) for a web Dapp that shows how to use Marowe Lambda from within a browser: [this video](https://youtu.be/o5m_y5l_i_g) shows the DApp in action, and [this video](https://youtu.be/nDWzEZDYsrw) shows it being used with a laptop for the Party and an android tablet for the Counterparty.


## Deployment

Here is an outline of the steps required to deploy Marlowe Lambda:

1.  Edit [deploy/bootstrap](deploy/bootstrap) to use either `mainnet`, the `preprod` testnet, or the `preview` testnet.
2.  Edit the corresponding [deploy/mainnet.config](deploy/mainnet.config), [deploy/preprod.config](deploy/preprod.config), or [deploy/preview.config](deploy/preview.config) so that the hostnames and ports match your deployment of [Marlowe Runtime](https://github.com/input-output-hk/marlowe-cardano/tree/main/marlowe-runtime).
3.  Run `./package.sh` to build the AWS Lambda ZIP file `marlowe-lambda.zip`.
4.  In the AWS Console or at the AWS Command Line, create a new lambda function and select the runtime `Provide your own bootstrap on Amazon Linux 2` and the architecture `x86_64`.
5.  The this ZIP file `marlowe-lambda.zip`.
6.  Set the handler name to `marlowe`.
7.  Set the timeout in the execution role to 45 seconds.
8.  Make sure that the execution role has `AWSLambdaBasicExecutionRole`.
9.  If you want to use the lambda in a web application, create a Cognito Identity Pool for it and make sure that it includes a policy with the actions `lambda:InvokeFunctionUrl`, `lambda:InvokeFunction`, `lambda:GetFunction`, and `lambda:InvokeAsync`.


## API

The JSON API for Marlowe Lambda contains the following requests and responses.

| Request                 | Response                  |
|-------------------------|---------------------------|
| [`list`](#list)         | [`contracts`](#contracts) |
| [`followed`](#followed) | [`contacts`](#contracts)  |
| [`follow`](#follow)     | [`result`](#result)       |
| [`unfollow`](#unfollow) | [`result`](#result)       |
| [`get`](#get)           | [`info`](#info)           |
| [`create`](#create)     | [`body`](#body)           |
| [`apply`](#apply)       | [`body`](#body)           |
| [`withdraw`](#withdraw) | [`body`](#body)           |
| [`sign`](#sign)         | [`tx`](#tx)               |
| [`submit`](#submit)     | [`txId`](#txid)           |
| [`wait`](#wait)         | [`txInfo`](#txinfo)       |


### Requests


#### List

List all Marlowe contract IDs on the blockchain.

```json
{
  "request" : "list"
}
```

The response is [Contracts](#contracts).


#### Followed

List all Marlowe contract IDs that the Marlowe Runtime backend is following.

```json
{
  "request" : "followed"
}
```

The response is [Contracts](#contracts).


#### Follow

Add a contract to the list of Marlowe contracts that the Marlowe Runtime backend is following.

```json
{
  "request" : "follow"
, "contractId" : "66e4af5ec99f09c28c6378e1d9d9508d95544e258fd8c8e7f3ff168a73e7b656#1"
}
```

The response is [Result](#result). A `true` result indicates that the contract ID was added to the list of followed contracts, whereas a `false` result indicates that the contract was already being followed.


#### Unfollow

Remove a contract to the list of Marlowe contracts that the Marlowe Runtime backend is following.

```json
{
  "request" : "unfollow"
, "contractId" : "66e4af5ec99f09c28c6378e1d9d9508d95544e258fd8c8e7f3ff168a73e7b656#1"
}
```

The response is [Result](#result). A `true` result indicates that the contract ID was removed from the list of followed contracts, whereas a `false` result indicates that the contract was not being followed.


#### Get

Fetch the history of a Marlowe contract.

```json
{
  "request" : "get"
, "contractId" : "66e4af5ec99f09c28c6378e1d9d9508d95544e258fd8c8e7f3ff168a73e7b656#1"
}
```

The response is [Info](#info).


#### Create

Build a transaction that creates a new Marlowe contract.

```json
{
  "request" : "create"
, "contract" : "/* the serialized Marlowe contract to be created */"
, "minUtxo" : "/* the number of lovelace to send to store in the contract when it is created */"
, "roles" : "/* an object that maps role names to the addresses to which the corresponding role tokens should be sent after it is minted */"
, "metadata" : "/* the transaction metadata in JSON format */"
, "addresses" : "/* the list of addresses, in addition to the change address, where UTxOs can be used as input to the transaction */"
, "change" : "/* the address to which change from the transaction will be sent */"
, "collateral" : "/* the list of UTxOs that may be used for collateral, or an empty list if any UTxO may be used for collateral */"
}
```

See `Contract` in https://github.com/input-output-hk/marlowe/blob/master/isabelle/haskell/MarloweCoreJson.lhs for definition of the JSON format of a Marlowe contract.

The response is [Body](#body).


#### Apply

Build a transaction that applies input to a Marlowe contract.

```json
{
  "request" : "apply"
, "contractId" : "/* the contract ID to which inputs will be applied */"
, "inputs" : "/* the inputs that will be applied to the contract */"
, "validityLowerBound" : "/* the POSIX time in integer milliseconds before which the transaction is not valid */"
, "validityUpperBound" : "/* the POSIX time in integer milliseconds after which the transaction is not valid */"
, "metadata" : "/* the transaction metadata in JSON format */"
, "addresses" : "/* the list of addresses, in addition to the change address, where UTxOs can be used as input to the transaction */"
, "change" : "/* the address to which change from the transaction will be sent */"
, "collateral" : "/* the list of UTxOs that may be used for collateral, or an empty list if any UTxO may be used for collateral */"
}
```

See `Input` in https://github.com/input-output-hk/marlowe/blob/master/isabelle/haskell/MarloweCoreJson.lhs for definition of the JSON format of inputs to a Marlowe contract.

The response is [Body](#body).


#### Withdraw

Build a transaction that withdraws funds paid by a Marlowe contract.

```json
{
  "request" : "withdraw"
, "contractId" : "/* the contract ID from which funds will be withdrawn */"
, "role" : "/* the name of the role making the withdrawal */"
, "addresses" : "/* the list of addresses, in addition to the change address, where UTxOs can be used as input to the transaction */"
, "change" : "/* the address to which change from the transaction will be sent */"
, "collateral" : "/* the list of UTxOs that may be used for collateral, or an empty list if any UTxO may be used for collateral */"
}
```

The response is [Body](#body).


#### Sign

Sign a transaction body with payment keys.

DO NOT USE THIS FUNCTION TO SIGN TRANSACTIONS BECAUSE IT TRANSMITS PRIVATE (SIGNING) KEYS.

```json
{
  "request" : "sign"
, "body" : "/* the transaction body in Cardano text-envelope format */"
, "paymentKeys" : "/* list of payment keys, in Cardano text-envelope format, that should sign the transaction */"
, "paymentExtendedKeys" : "/* a list of payment extended keys, in Cardano text-envelope format, that should sign the transaction */"
}
```

The response is [Tx](#tx).


#### Submit

Submit a signed transaction to the Cardano node.

```json
{
  "request" : "submit"
, "tx" : "/* the transaction in Cardano text-envelope format */"
}
```

#### Wait

Wait for the first confirmation of a transaction on the Cardano node.

```json
{
  "request" : "wait"
, "txId" : "/* the identifier of the transaction */"
, "pollingSeconds" : "/* the number of seconds to wait between pollings of the Cardano node for confirmation */"
}
```

The response is [TxInfo](#txinfo).


### Responses


#### Contracts

This response is a list of contract IDs.

```json
{
  "response" : "contracts"
, "contractIds": [
    "f06e4b760f2d9578c8088ea5289ba6276e68ae3cbaf5ad27bcfc77dc413890db#1"
   , ...
   ]
}
```


#### Result

This response is a simple `true`/`false` indication.

```json
{
  "response" : "result"
, "result" : true
}
```


#### Info

This response details the history of a Marlowe contract.

```json
{
  "response" : "info"
, "creation" : "/* information about the transaction that created the contract */"
, "steps" : [
    "/* information about each subsequent transaction involving the contract */"
  , ...
  ]
}
```

See `CreateStep` and `ContractStep` in https://github.com/input-output-hk/marlowe-cardano/blob/main/marlowe-runtime/history-api/Language/Marlowe/Runtime/History/Api.hs for details on the contents of the `creation` and `steps` fields.


#### Body

This response contains a serialized transaction body.

```json
{
  "response" : "body"
, "txId" : "66e4af5ec99f09c28c6378e1d9d9508d95544e258fd8c8e7f3ff168a73e7b656"
, "contractId" : "66e4af5ec99f09c28c6378e1d9d9508d95544e258fd8c8e7f3ff168a73e7b656#1"
, "body" : "/* the JSON-serialized transaction body in Cardano text-envelope format */"
}
```


#### Tx

This response contains a serialized transaction.

```json
{
  "response" : "tx"
, "txId" : "66e4af5ec99f09c28c6378e1d9d9508d95544e258fd8c8e7f3ff168a73e7b656"
, "tx" : "/* the JSON-serialized transaction in Cardano text-envelope format */"
}
```


#### TxId

This response contains a transaction ID.

```json
{
  "response" : "txId"
, "txId" : "66e4af5ec99f09c28c6378e1d9d9508d95544e258fd8c8e7f3ff168a73e7b656"
}
```


#### TxInfo

This response contains full information about a transaction.

```json
{
  "response" : "txInfo"
, "transaction" : "/* information about the transaction */"
}
```

See `Transaction` in https://github.com/input-output-hk/marlowe-cardano/blob/main/marlowe-runtime/src/Language/Marlowe/Runtime/Core/Api.hs for details on the contents of the `transaction` field.
