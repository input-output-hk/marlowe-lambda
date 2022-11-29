/*
 * Functions for invoking Marlowe Lambda.
 *
 * See the Haskell request and response types at
 *   https://github.com/input-output-hk/marlowe-lambda/blob/main/src/Language/Marlowe/Lambda/Types.hs
 * for definitive details on the JSON serialization used for Marlowe Lambda.
 */


'use strict'


// Use the AWS SDK.
import * as AWS from "aws-sdk"


// The Marlowe Lambda function ARN.
let FunctionName = null

// The AWS Lambda client.
let lambda = new AWS.Lambda()


/**
 * Set the credentials to be used for invocations of Marlowe Lambda.
 *
 * @param {String} functionName    The AWS Lambda function ARN for Marlowe Lambda.
 * @param {String} region          The AWS region where Marlowe Lambda resides.
 * @param {String} identityPoolId  The AWS Cognito identity pool ID used to access Marlowe Lambda.
 */
export function setCredentials(functionName, region, identityPoolId) {
  AWS.config.region = region
  AWS.config.credentials = new AWS.CognitoIdentityCredentials({
      IdentityPoolId: identityPoolId,
  })
  FunctionName = functionName
  lambda = new AWS.Lambda()
}


/**
 * Invoke Marlowe Lambda.
 *
 * @param {Object}   payload  The payload containing the request for Marlowe Lambda.
 * @param {function} f        The function that will be called with the response of the invocation,
 *                            either JSON for a successful invocation or an error String for failure.
 */
async function marloweLambda(payload, f) {

  const input = {
    FunctionName: FunctionName,
    Payload: JSON.stringify(payload),
  }

  await lambda.invoke(input, function (err, data) {
    if (err)
      console.log(err, err.stack)
    else
      f(JSON.parse(data.Payload))
  })
}


/**
 * List all Marlowe contract IDs on the blockchain.
 *
 * A successful response is of the form:
 * ```json
 * {
 *   "response" : "contracts"
 * , "contractIds": [
 *     "f06e4b760f2d9578c8088ea5289ba6276e68ae3cbaf5ad27bcfc77dc413890db#1"
 *    , ...
 *    ]
 * }
 * ```
 *
 * @param {function} f  The function that will be called with the response of the invocation,
 *                      either JSON for a successful invocation or an error String for failure.
 */
export async function allContracts(f) {
  marloweLambda({
    request: "list",
  }, function(response) {
    f(response)
  })
}


/**
 * List all Marlowe contract IDs that the Marlowe Runtime backend is following.
 *
 * A successful response is of the form:
 * ```json
 * {
 *   "response" : "contracts"
 * , "contractIds": [
 *     "f06e4b760f2d9578c8088ea5289ba6276e68ae3cbaf5ad27bcfc77dc413890db#1"
 *    , ...
 *    ]
 * }
 * ```
 *
 * @param {function} f  The function that will be called with the response of the invocation,
 *                      either JSON for a successful invocation or an error String for failure.
 */
export async function followedContracts(f) {
  marloweLambda({
    request: "followed",
  }, function(response) {
    f(response)
  })
}


/**
 * Add a contract to the list of Marlowe contracts that the Marlowe Runtime backend is following.
 *
 * A successful response is of the form:
 * ```json
 * {
 *   "response" : "result"
 * , "result" : true
 * }
 * ```
 * A `true` result indicates that the contract ID was added to the list of followed contracts,
 * whereas a `false` result indicates that the contract was already being followed.
 *
 * @param {String}   id  The contract ID.
 * @param {function} f   The function that will be called with the response of the invocation,
 *                       either JSON for a successful invocation or an error String for failure.
 */
export async function followContract(id, f) {
  marloweLambda({
    request: "follow",
    contractId: id,
  }, function(response) {
    f(response)
  })
}


/**
 * Remove a contract to the list of Marlowe contracts that the Marlowe Runtime backend is following.
 *
 * A successful response is of the form:
 * ```json
 * {
 *   "response" : "result"
 * , "result" : true
 * }
 * ```
 * A `true` result indicates that the contract ID was removed from the list of followed contracts,
 * whereas a `false` result indicates that the contract was not being followed.
 *
 * @param {String}   id  The contract ID.
 * @param {function} f   The function that will be called with the response of the invocation,
 *                       either JSON for a successful invocation or an error String for failure.
 */
export async function unfollowContract(id, f) {
  marloweLambda({
    request: "unfollow",
    contractId: id,
  }, function(response) {
    f(response)
  })
}


/**
 * Fetch the history of a Marlowe contract.
 *
 * A successful response is of the form:
 * ```json
 * {
 *   "response" : "info"
 * , "creation" : "(information about the transaction that created the contract)"
 * , "steps" : [
 *     "(information about each subsequent transaction involving the contract)"
 *   , ...
 *   ]
 * }
 * ```
 * See `CreateStep` and `ContractStep` in
 *   https://github.com/input-output-hk/marlowe-cardano/blob/main/marlowe-runtime/src/Language/Marlowe/Runtime/History/Api.hs
 * for details on the contents of the `creation` and `steps` fields.
 *
 * @param {String}   id  The contract ID.
 * @param {function} f   The function that will be called with the response of the invocation,
 *                       either JSON for a successful invocation or an error String for failure.
 */
export async function getContract(id, f) {
  marloweLambda({
    request: "get",
    contractId: id,
  }, function(response) {
    f(response)
  })
}


/**
 * Build a transaction that creates a new Marlowe contract.
 *
 * A successful response is of the form:
 * ```json
 * {
 *   "response" : "body"
 * , "txId" : "66e4af5ec99f09c28c6378e1d9d9508d95544e258fd8c8e7f3ff168a73e7b656"
 * , "contractId" : "66e4af5ec99f09c28c6378e1d9d9508d95544e258fd8c8e7f3ff168a73e7b656#1"
 * , "body" : "(the JSON-serialized transaction body in Cardano text-envelope format)"
 * }
 * ```
 *
 * See `Contract` in 
 *   https://github.com/input-output-hk/marlowe/blob/master/isabelle/haskell/MarloweCoreJson.lhs
 * for definition of the JSON format of a Marlowe contract.
 *
 * @param {Object}    contract    The serialized Marlowe contract to be created.
 * @param {Object}    roles       An object that maps role names to the addresses to which the
 *                                corresponding role tokens should be sent after it is minted.
 * @param {Number}    minUtxo     The number of lovelace to send to store in the contract when it
 *                                is created.
 * @param {[String]}  addresses   The list of addresses, in addition to the change address, where
 *                                UTxOs can be used as input to the transaction.
 * @param {String}    change      The address to which change from the transaction will be sent.
 * @param {[String]}  collateral  The list of UTxOs that may be used for collateral, or an empty
 *                                list if any UTxO may be used for collateral.
 * @param {function}  f           The function that will be called with the response of the
 *                                invocation, either JSON for a successful invocation or an error
 *                                String for failure.
 */
export async function createContract(contract, roles, minUtxo, addresses, change, collateral, f) {
  marloweLambda({
    request: "create",
    contract: contract,
    roles: roles,
    minUtxo: minUtxo,
    addresses: addresses,
    change: change,
    collateral: collateral,
  }, function(response) {
    f(response)
  })
}


/**
 * Build a transaction that applies input to a Marlowe contract.
 *
 * A successful response is of the form:
 * ```json
 * {
 *   "response" : "body"
 * , "txId" : "66e4af5ec99f09c28c6378e1d9d9508d95544e258fd8c8e7f3ff168a73e7b656"
 * , "contractId" : "66e4af5ec99f09c28c6378e1d9d9508d95544e258fd8c8e7f3ff168a73e7b656#1"
 * , "body" : "(the JSON-serialized transaction body in Cardano text-envelope format)"
 * }
 * ```
 *
 * See `Input` in 
 *   https://github.com/input-output-hk/marlowe/blob/master/isabelle/haskell/MarloweCoreJson.lhs
 * for definition of the JSON format of inputs to a Marlowe contract.
 *
 * @param {String}    id                  The contract ID to which the input will be applied.
 * @param {[Object]}  inputs              The inputs that will be applied to the contract.
 * @param {[Number]}  validityLowerBound  The POSIX time in integer milliseconds before which the
 *                                        transaction is not valid.
 * @param {[Number]}  validityUpperBound  The POSIX time in integer milliseconds after which the
 *                                        transaction is not valid.
 * @param {[String]}  addresses           The list of addresses, in addition to the change address,
 *                                        where UTxOs can be used as input to the transaction.
 * @param {String}    change              The address to which change from the transaction will be
 *                                        sent.
 * @param {[String]}  collateral          The list of UTxOs that may be used for collateral, or an
 *                                        empty list if any UTxO may be used for collateral.
 * @param {function}  f                   The function that will be called with the response of the
 *                                        invocation, either JSON for a successful invocation or an
 *                                        error String for failure.
 */
export async function applyInputs(id, inputs, validityLowerBound, validityUpperBound, addresses, change, collateral, f) {
  marloweLambda({
    request: "apply",
    contractId: id,
    inputs: inputs,
    validityLowerBound: validityLowerBound,
    validityUpperBound: validityUpperBound,
    addresses: addresses,
    change: change,
    collateral: collateral,
  }, function(response) {
    f(response)
  })
}


/**
 * Build a transaction that withdraws funds paid by a Marlowe contract.
 *
 * A successful response is of the form:
 * ```json
 * {
 *   "response" : "body"
 * , "txId" : "66e4af5ec99f09c28c6378e1d9d9508d95544e258fd8c8e7f3ff168a73e7b656"
 * , "contractId" : "66e4af5ec99f09c28c6378e1d9d9508d95544e258fd8c8e7f3ff168a73e7b656#1"
 * , "body" : "(the JSON-serialized transaction body in Cardano text-envelope format)"
 * }
 * ```
 *
 * @param {String}    id          The contract ID from which a payout will be withdrawn.
 * @param {String}    role        The name of the role making the withdrawal.
 * @param {[String]}  addresses   The list of addresses, in addition to the change address, where
 *                                UTxOs can be used as input to the transaction.
 * @param {String}    change      The address to which change from the transaction will be sent.
 * @param {[String]}  collateral  The list of UTxOs that may be used for collateral, or an empty
 *                                list if any UTxO may be used for collateral.
 * @param {function}  f           The function that will be called with the response of the
 *                                invocation, either JSON for a successful invocation or an error
 *                                String for failure.
 */
export async function withdraw(id, role, addresses, change, collateral, f) {
  marloweLambda({
    request: "withdraw",
    contractId: id,
    role: role,
    addresses: addresses,
    change: change,
    collateral: collateral,
  }, function(response) {
    f(response)
  })
}


/**
 * Sign a transaction body with payment keys.
 *
 * DO NOT USE THIS FUNCTION TO SIGN TRANSACTIONS BECAUSE IT TRANSMITS PRIVATE "(SIGNING)" KEYS.
 *
 * A successful response is of the form:
 * ```json
 * {
 *   "response" : "tx"
 * , "txId" : "66e4af5ec99f09c28c6378e1d9d9508d95544e258fd8c8e7f3ff168a73e7b656"
 * , "tx" : "(the JSON-serialized transaction in Cardano text-envelope format)"
 * }
 * ```
 *
 * @param {Object}   body                 The transaction body in Cardano text-envelope format.
 * @param {[Object]} paymentKeys          A list of payment keys, in Cardano text-envelope format,
 *                                        that should sign the transaction.
 * @param {[Object]} paymentExtendedKeys  A list of payment extended keys, in Cardano text-envelope
 *                                        format, that should sign the transaction.
 * @param {function} f                    The function that will be called with the response of the
 *                                        invocation, either JSON for a successful invocation or an
 *                                        error String for failure.
 */
export async function sign(body, paymentKeys, paymentExtendedKeys, f) {
  marloweLambda({
    request: "sign",
    body: body,
    paymentKeys: paymentKeys,
    paymentExtendedKeys: paymentExtendedKeys,
  }, function(response) {
    f(response)
  })
}


/**
 * Submit a signed transaction to the Cardano node.
 *
 * A successful response is of the form:
 * ```json
 * {
 *   "response" : "txId"
 * , "txId" : "66e4af5ec99f09c28c6378e1d9d9508d95544e258fd8c8e7f3ff168a73e7b656"
 * }
 * ```
 *
 * @param {Object}   tx  The transaction in Cardano text-envelope format.
 * @param {function} f   The function that will be called with the response of the invocation,
 *                       either JSON for a successful invocation or an error String for failure.
 */
export async function submit(tx, f) {
  marloweLambda({
    request: "submit",
    tx: tx,
  }, function(response) {
    f(response)
  })
}


/**
 * Initialize this module.
 */
export async function initialize() {
}
