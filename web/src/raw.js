

'use strict'


import * as Lambda from "./lambda"

//FIXME: This import should work, but it doesn't.
//import * as renderjson from "renderjson"


export async function allContracts() {
  Lambda.allContracts(function(response) {
    window.renderjson.set_show_to_level(2)
    delete response.response
    uiResponse.replaceChildren(window.renderjson(response))
  })
}

export async function followedContracts() {
  Lambda.followedContracts(function(response) {
    window.renderjson.set_show_to_level(2)
    delete response.response
    uiResponse.replaceChildren(window.renderjson(response))
  })
}

export async function followContract() {
  Lambda.followContract(uiContractId.value, function(response) {
    window.renderjson.set_show_to_level(1)
    delete response.response
    uiResponse.replaceChildren(window.renderjson(response))
  })
}

export async function unfollowContract() {
  Lambda.unfollowContract(uiContractId.value, function(response) {
    window.renderjson.set_show_to_level(1)
    delete response.response
    uiResponse.replaceChildren(window.renderjson(response))
  })
}

export async function getContract() {
  Lambda.getContract(uiContractId.value, function(response) {
    window.renderjson.set_show_to_level(5)
    delete response.response
    uiResponse.replaceChildren(window.renderjson(response))
  })
}
