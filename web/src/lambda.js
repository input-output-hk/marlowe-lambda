

'use strict'


import * as AWS from "aws-sdk"


AWS.config.region = "eu-west-3"

AWS.config.credentials = new AWS.CognitoIdentityCredentials({
    IdentityPoolId: "eu-west-3:1ad0476e-8d01-4565-b80e-6ec363a2cdef",
});

const lambda = new AWS.Lambda()

async function marloweLambda(payload, f) {

  const input = {
    FunctionName: "arn:aws:lambda:eu-west-3:454236594309:function:bbush-marlowe-runtime-2",
    Payload: JSON.stringify(payload),
  }

  await lambda.invoke(input, function (err, data) {
    if (err)
      console.log(err, err.stack)
    else
      f(JSON.parse(data.Payload))
  })
}


export async function allContracts(f) {
  marloweLambda({
    request: "list",
  }, function(response) {
    f(response)
  })
}

export async function followedContracts(f) {
  marloweLambda({
    request: "followed",
  }, function(response) {
    f(response)
  })
}

export async function followContract(id, f) {
  marloweLambda({
    request: "follow",
    contractId: id,
  }, function(response) {
    f(response)
  })
}

export async function unfollowContract(id, f) {
  marloweLambda({
    request: "unfollow",
    contractId: id,
  }, function(response) {
    f(response)
  })
}

export async function getContract(id, f) {
  marloweLambda({
    request: "get",
    contractId: id,
  }, function(response) {
    f(response)
  })
}

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

export async function submit(tx, f) {
  marloweLambda({
    request: "submit",
    tx: tx,
  }, function(response) {
    f(response)
  })
}


export async function initialize() {

}
