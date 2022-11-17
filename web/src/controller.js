

'use strict'


import * as Lambda from "./lambda"

//FIXME: This import should work, but it doesn't.
//import * as renderjson from "renderjson"


const second = 1000


var contractId = "26e8839a5674df7059bf3427eb670d6985632df99859e3c14b012d39bba989e5#1"


function makeContract(addressA, currencyA, tokenA, amountA, addressB, currencyB, tokenB, amountB, timeout = 1669918020000) {
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


export async function createContract() {
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

export async function partyDeposit() {
  deposit(
    uiAddressA.value,
    uiKeyA.value,
    uiCurrencyA.value,
    uiTokenA.value,
    parseInt(uiAmountA.value),
    1,
    uiTxId2,
    function() {uiCounterpartyDeposit.disabled = false},
  )
  uiPartyDeposit.disabled = true
}

export async function counterpartyDeposit() {
  deposit(
    uiAddressB.value,
    uiKeyB.value,
    uiCurrencyB.value,
    uiTokenB.value,
    parseInt(uiAmountB.value),
    2,
    uiTxId3,
    function () {uiCreate.disabled = false},
  )
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
      uiTxId.innerHTML = '<a href="' + window.secrets.cardanoscalUrl + '/transaction/' + txId + '?tab=utxo" target="marloweSwap">' + txId + '</a>'
      f(txId)
      updateConfirmed(steps, function() {
        document.body.style.cursor = "default"
	g()
      })
    })
  })
}

async function updateConfirmed(steps, f) {
  Lambda.getContract(contractId, async function(response) {
    if (response == "Contract not found." || response.steps.length < steps) {
      setTimeout(function(){updateConfirmed(steps, f)}, 5000)
    } else {
      window.renderjson.set_show_to_level(3)
      delete response.response
      uiOutput.replaceChildren(window.renderjson(response))
      f()
    }
  })
}


export async function initialize() {

  uiAddressA.value = window.secrets.party.address
  uiKeyA.value = window.secrets.party.key
  uiCurrencyA.value = window.secrets.party.currency
  uiTokenA.value = window.secrets.party.token
  uiAmountA.value = window.secrets.party.amount

  uiAddressB.value = window.secrets.counterparty.address
  uiKeyB.value = window.secrets.counterparty.key
  uiCurrencyB.value = window.secrets.counterparty.currency
  uiTokenB.value = window.secrets.counterparty.token
  uiAmountB.value = window.secrets.counterparty.amount

  reset()

}


export async function reset() {
  uiNetwork.innerHTML = window.secrets.network
  uiOutput.innerHTML = ""
  uiCreate.disabled = false
  uiPartyDeposit.disabled = true
  uiCounterpartyDeposit.disabled = true
  document.body.style.cursor = "default"
}
