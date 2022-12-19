

'use strict'


import * as Lambda from "./lambda"

//FIXME: This import should work, but it doesn't.
//import * as renderjson from "renderjson"


export const lambda = Lambda


const second = 1000


let contractId = null


export async function unfollowAll() {
  Lambda.followedContracts(async function(response) {
  await Promise.all([response.contractIds.map(id => Lambda.unfollowContract(id, function() {}))])
  })
}


function makeContract(addressA, currencyA, tokenA, amountA, addressB, currencyB, tokenB, amountB, timeout = new Date().getTime() + 60 * 60 * 1000) {
  return {
    when: [
      {
        case: {
          party: { address:addressA },
          deposits: amountA,
          of_token: { token_name: tokenA, currency_symbol:currencyA },
          into_account: { address: addressA },
        },
        then: {
          when: [
            {
              case: {
                party: { address: addressB },
                deposits: amountB,
                of_token: { token_name: tokenB, currency_symbol:currencyB },
                into_account: { address: addressB },
              },
              then: {
                pay: amountB,
                token: { token_name: tokenB, currency_symbol:currencyB },
                from_account: { address: addressB },
                to: {
                  party: { address:addressA },
                },
                then: {
                  pay: amountA,
                  token: { token_name: tokenA, currency_symbol: currencyA },
                  from_account: { address:addressA },
                  to: {
                    party: { address:addressB },
                  },
                  then: "close",
                },
              },
            },
          ],
          timeout_continuation: "close",
          timeout:timeout,
        },
      }
    ],
    timeout_continuation: "close",
    timeout:timeout,
  }
}


export async function createContract(single) {
  const contract = makeContract(
    uiAddressA.value, uiCurrencyA.value, uiTokenA.value, parseInt(uiAmountA.value),
    uiAddressB.value, uiCurrencyB.value, uiTokenB.value, parseInt(uiAmountB.value),
  )
  console.log(contract)
  document.body.style.cursor = "wait"
  Lambda.createContract(
    contract,
    {},
    2000000,
    {},
    [],
    uiAddressA.value,
    [],
    function(response1) {
      transact(response1, uiKeyA.value, 0, uiTxId1, function (txId) {
        contractId = txId + "#1"
        uiContractId.innerHTML = contractId
      },
      function() {uiPartyDeposit.disabled = false},
      )
    },
  )
  uiCreate.disabled = true
}

export async function partyDeposit(single) {
  deposit(
    uiAddressA.value,
    uiKeyA.value,
    uiCurrencyA.value,
    uiTokenA.value,
    parseInt(uiAmountA.value),
    1,
    uiTxId2,
    function() {
      if (window.uiCounterpartyDeposit != null)
        uiCounterpartyDeposit.disabled = false
      else
        updateConfirmed(2, function(response) {
          showTxId(uiTxId3, response.steps[1].txId)
        })
    },
  )
  uiPartyDeposit.disabled = true
}

export async function counterpartyDeposit(single) {
  deposit(
    uiAddressB.value,
    uiKeyB.value,
    uiCurrencyB.value,
    uiTokenB.value,
    parseInt(uiAmountB.value),
    2,
    uiTxId3,
    function () {
      if (window.uiCreate != null)
        uiCreate.disabled = false
    },
  )
  if (window.uiCounterpartyDeposit != null)
    uiCounterpartyDeposit.disabled = true
}


async function deposit(address, key, currency, token, amount, steps, uiTxId, f) {
  const inputs = [{
    input_from_party: {
      address: address,
    },
    that_deposits: amount,
    of_token: {
      currency_symbol: currency,
      token_name: token,
    },
    into_account: {
      address: address,
    },
  }]
  const now = Math.round((new Date()).getTime() / second)
  const lower = (now - 1 * 60) * second
  const upper = (now + 5 * 60) * second
  document.body.style.cursor = "wait"
  Lambda.applyInputs(
    contractId,
    inputs,
    lower,
    upper,
    {},
    [],
    address,
    [],
    function(response) {
      transact(response, key, steps, uiTxId, function(txId) {}, f)
    },
  )
}

async function transact(response1, key, steps, uiTxId, f, g) {
  console.log(response1)
  Lambda.sign(response1.body, [JSON.parse(key)], [], function(response2) {
    console.log(response2)
    Lambda.submit(response2.tx, function(response3) {
      console.log(response3)
      const txId = response3.txId
      showTxId(uiTxId, txId)
      f(txId)
      updateConfirmed(steps, function(response) {
        document.body.style.cursor = "default"
        g()
      })
    })
  })
}

async function updateConfirmed(steps, f) {
  Lambda.getContract(contractId, async function(response) {
    if (response == "Contract not found." || response.steps == null || response.steps.length < steps) {
      setTimeout(function(){updateConfirmed(steps, f)}, 5000)
    } else {
      window.renderjson.set_show_to_level(3)
      delete response.response
      uiOutput.replaceChildren(window.renderjson(response))
      if (uiAddressA.readOnly && steps == 0) {
        uiAddressA.value = response.creation.output.datum.marloweContract.when[0].case.into_account.address
        uiCurrencyA.value = response.creation.output.datum.marloweContract.when[0].case.of_token.currency_symbol
        uiTokenA.value = response.creation.output.datum.marloweContract.when[0].case.of_token.token_name
        uiAmountA.value = response.creation.output.datum.marloweContract.when[0].case.deposits
        uiCurrencyB.value = response.creation.output.datum.marloweContract.when[0].then.when[0].case.of_token.currency_symbol
        uiTokenB.value = response.creation.output.datum.marloweContract.when[0].then.when[0].case.of_token.token_name
        uiAmountB.value = response.creation.output.datum.marloweContract.when[0].then.when[0].case.deposits
      }
      f(response)
    }
  })
}


async function discoverFollowed(f) {
  Lambda.followedContracts(function(response) {
    if (response.contractIds.length == 0)
      setTimeout(function(){discoverFollowed(f)}, 10000)
    else
      f(response.contractIds[0])
  })
}


export async function initialize(single) {

  Lambda.setCredentials(
    window.secrets.functionName,
    window.secrets.region,
    window.secrets.identityPoolId,
  )

  if (window.uiKeyA != null) {
    uiAddressA.value = window.secrets.party.address
    if (window.secrets.party.key != null)
      uiKeyA.value = window.secrets.party.key
    uiCurrencyA.value = window.secrets.party.currency
    uiTokenA.value = window.secrets.party.token
    uiAmountA.value = window.secrets.party.amount
    uiCurrencyB.value = window.secrets.counterparty.currency
    uiTokenB.value = window.secrets.counterparty.token
    uiAmountB.value = window.secrets.counterparty.amount
  } else {
    uiAddressA.readOnly = true
    uiCurrencyA.readOnly = true
    uiTokenA.readOnly = true
    uiAmountA.readOnly = true
    uiCurrencyB.readOnly = true
    uiTokenB.readOnly = true
    uiAmountB.readOnly = true
  }

  uiAddressB.value = window.secrets.counterparty.address
  if (window.uiKeyB != null) {
    if (window.secrets.counterparty.key != null)
      uiKeyB.value = window.secrets.counterparty.key
  }

  reset(single)

}


function showTxId(uiTxId, txId) {
  uiTxId.innerHTML = '<a href="' + window.secrets.cardanoscanUrl + '/transaction/' + txId + '?tab=utxo" target="marloweSwap">' + txId + '</a>'
}


export async function reset(single) {

  uiNetwork.innerHTML = window.secrets.network
  uiOutput.innerHTML = ""

  contractId = ""
  uiContractId.innerHTML = ""
  uiTxId1.innerHTML = ""
  uiTxId2.innerHTML = ""
  uiTxId3.innerHTML = ""

  if (window.uiCreate != null) {
    uiCreate.disabled = false
    uiPartyDeposit.disabled = true
  }
  if (window.uiCounterpartyDeposit != null) {
    if (single) {
      uiAddressA.value = ""
      uiCurrencyA.value = ""
      uiTokenA.value = ""
      uiAmountA.value = ""
      uiCurrencyB.value = ""
      uiTokenB.value = ""
      uiAmountB.value = ""
    }
    uiCounterpartyDeposit.disabled = true
  }

  document.body.style.cursor = "default"

  if (single) {
    if (window.uiCreate != null) {
      await unfollowAll()
    } else {
      discoverFollowed(async function(id) {
        contractId = id
        uiContractId.innerHTML = contractId
        showTxId(uiTxId1, id.slice(0, -2))
        updateConfirmed(0, async function(response1) {
          updateConfirmed(1, async function(response2) {
            showTxId(uiTxId2, response2.steps[0].txId)
            uiCounterpartyDeposit.disabled = false
          })
        })
      })
    }
  }
}
