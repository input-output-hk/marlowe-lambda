# Simple Token-Swap Dapp for Marlowe Lambda

This distributed web application implements a simple token swap, where two parties can swap Cardano native tokens. It provides two web pages, [party.html](party.html) and [counterparty.html](counterparty.html), that can be run on separate machines or a single web page, [index.html](index.html) that acts for both parties.


## Deployment

The script [deploy.sh](deploy.sh) runs `npx webpack` to package the JavaScript libraries, and then upload these to Google Cloud Storage. Because the Dapp runs purely in the browser, it can be deployed on any cloud service (or on IPFS) and does not require a backend other than the AWS Lambda function.


## Videos

[This video](https://youtu.be/o5m_y5l_i_g) shows the DApp in action, and [this video](https://youtu.be/nDWzEZDYsrw) shows it being used with a laptop for the Party and an android tablet for the Counterparty.
