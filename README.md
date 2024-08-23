# Revision history for simple Term Savings Schema

## 0.1.0.0 -- 2022-11-27

* First version.
* Intended as final project to understand Cardano Smart Contract using Plutus

# Feature:
* Register: create saving smart contract with first deposit 2000000 Lovelace
* Deposit: deposit coin to the smart contract
* Withdraw: withdraw all coin in smart contract to the wallet

# Running Project
* Open Plutus Playground (online https://playground.plutus.iohkdev.io/ | ofline)
* Before anything, please do register to make saving smart contract address.
  - [Input] Beneficiary --> target savings address (target withdraw address)
  - [Input] Deadline --> Savings can only be withdrawn if the deadline has passed
  - [Input] Pin --> use this pin to withdraw
* Deposit must have same target beneficiary, no pin required and any wallet can make deposit into the target beneficiary.
  - [Input] Beneficiary --> target savings address (target withdraw address).
  - [Validation] Beneficiary address must be the same
* Withdrawal must have same address with beneficiary, same pin and already passed the deadline.
  - [Input] Pin --> must be the same Pin.
  - [Validation] Address must same as beneficiary, Withdrawal must have passed deadline and the PIN must match.

# Demo
* Register: Wallet 1
  - Beneficiary: bf342ddd3b1a6191d4ce936c92d29834d6879edf2849eaea84c827f8 (Wallet 5)
  - Deadline: 1596059131000 (+40 slot)
  - Pin: 12345
* Wait: 10 (Slot)
* Deposit: Wallet 2
  - Beneficiary: bf342ddd3b1a6191d4ce936c92d29834d6879edf2849eaea84c827f8 (Wallet 5)
  - Amount: 10000000 (Lovelace)
* Wait: 10 (Slot)
* Deposit: Wallet 3
  - Beneficiary: bf342ddd3b1a6191d4ce936c92d29834d6879edf2849eaea84c827f8 (Wallet 5)
  - Amount: 20000000 (Lovelace)
* Wait: 10 (Slot)
* Deposit: Wallet 4
  - Beneficiary: bf342ddd3b1a6191d4ce936c92d29834d6879edf2849eaea84c827f8 (Wallet 5)
  - Amount: 30000000 (Lovelace)
* Wait: 10 (Slot)
* Withdraw: Wallet 5
  - Pin: 12345
* Wait: 10 (Slot)
