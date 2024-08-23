{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module SavingsSchema where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)

firstDeposit = 2000000

data RegisterDatum = RegisterDatum
    { rdBeneficiary  :: PaymentPubKeyHash
    , rdDeadline     :: POSIXTime
    , rdPin          :: Integer
    } deriving Show

PlutusTx.unstableMakeIsData ''RegisterDatum

data DepositDatum = DepositDatum
    { ddBeneficiary  :: PaymentPubKeyHash
    } deriving Show

PlutusTx.unstableMakeIsData ''DepositDatum

data WithdrawDatum = WithdrawDatum
    { wdPin          :: Integer
    } deriving Show

PlutusTx.unstableMakeIsData ''WithdrawDatum

data SavingDatum = SavingDatum
    { adRegister     :: RegisterDatum
    , adSave         :: (Maybe DepositDatum)
    } deriving Show

PlutusTx.unstableMakeIsData ''SavingDatum

{-# INLINABLE mkValidator #-}
mkValidator :: SavingDatum -> WithdrawDatum -> ScriptContext -> Bool
mkValidator SavingDatum{..} withdraw ctx = traceIfFalse "Can't withdraw, beneficiary address not match" signedByBeneficiary &&
                    traceIfFalse "Can't withdraw, deadline not reached" deadlineValidation &&
                    traceIfFalse "Can't withdraw, incorrect PIN" (pinValidation withdraw)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    
    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ rdBeneficiary adRegister

    deadlineValidation :: Bool
    deadlineValidation = contains (from $ rdDeadline adRegister) $ txInfoValidRange info

    pinValidation :: WithdrawDatum -> Bool
    pinValidation wd = rdPin adRegister == wdPin wd

data Savings
instance Scripts.ValidatorTypes Savings where
    type instance DatumType Savings = SavingDatum
    type instance RedeemerType Savings = WithdrawDatum

typedValidator :: Scripts.TypedValidator Savings
typedValidator = Scripts.mkTypedValidator @Savings
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @SavingDatum @WithdrawDatum

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

data RegisterParams = RegisterParams
    { rpBeneficiary :: !PaymentPubKeyHash
    , rpDeadline    :: !POSIXTime
    , rpPin         :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data DepositParams = DepositParams
    { dpBeneficiary :: !PaymentPubKeyHash
    , dpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data WithdrawParams = WithdrawParams
    { wpPin         :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type SavingsSchema =
            Endpoint "1.Register" RegisterParams
        .\/ Endpoint "2.Deposit" DepositParams
        .\/ Endpoint "3.Withdraw" WithdrawParams

-- Before anything, please do register to make saving address
-- Beneficiary => target savings address (target withdraw address)
-- Deadline => Savings can only be withdrawn if the deadline has passed
-- Pin => use this pin to withdraw
register :: AsContractError e => RegisterParams -> Contract w s e ()
register rp = do
    let rd  = RegisterDatum
                { rdBeneficiary = rpBeneficiary rp
                , rdDeadline    = rpDeadline rp
                , rdPin         = rpPin rp
                }
        dat = SavingDatum
                { adRegister    = rd
                , adSave        = Nothing
                }
        tx  = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ firstDeposit
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "Register saving address for %s with deadline %s and pin %s"
        (show $ rpBeneficiary rp)
        (show $ rpDeadline rp)
        (show $ rpPin rp)

-- Deposit must have same target beneficiary, No pin required
-- Any wallet can make deposit into the target beneficiary
-- Beneficiary => target savings address (target withdraw address)
deposit :: forall w s e. DepositParams -> Contract w s Text ()
deposit dp = do
    (oref, o, sd@SavingDatum{..}) <- validateSaving (dpBeneficiary dp)
    logInfo @String $ printf "Found auction utxo with datum %s" (show sd)

    let dd      = DepositDatum
                { ddBeneficiary   = dpBeneficiary dp
                }
        dat     = sd
                { adSave          = Just dd
                }
        amount  = Ada.lovelaceValueOf $ dpAmount dp
        lookups = Constraints.typedValidatorLookups typedValidator      <>
                  Constraints.otherScript validator
        tx  = Constraints.mustPayToTheScript dat amount
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "Made saving of %d lovelace" $ dpAmount dp

validateSaving :: PaymentPubKeyHash -> Contract w s Text (TxOutRef, ChainIndexTxOut, SavingDatum)
validateSaving beneficiary = do
    utxos <- utxosAt scrAddress
    let xs = head $ Map.toList utxos
    case xs of
        (oref, o) -> case _ciTxOutDatum o of
            Left _          -> throwError "[Deposit] Saving Datum missing"
            Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> throwError "[Deposit] Saving Datum has wrong type"
                Just sd@SavingDatum{..}
                    | rdBeneficiary adRegister == beneficiary -> return (oref, o, sd)
                    | otherwise                               -> throwError "[Deposit] Saving Address missmatch"
        _           -> throwError "[Deposit] Saving utxo not found"

-- Withdrawal must have same address with beneficiary, same pin and already passed the deadline
-- Validation Beneficiary => must be the same Addrees
-- Deadline => must have passed
-- Pin => must be the same Pin
withdraw :: forall w s e. AsContractError e => WithdrawParams -> Contract w s e ()
withdraw wp = do
    now   <- currentTime
    pkh   <- ownPaymentPubKeyHash
    utxos <- Map.filter (isSuitable pkh now) <$> utxosAt scrAddress
    if Map.null utxos
        then logInfo @String $ "[Withdraw] Validation Saving Address / PIN / Deadline missmatch"
        else do
            let dat     = WithdrawDatum
                        { wdPin         = wpPin wp
                        }
                r       = Redeemer $ PlutusTx.toBuiltinData dat
                orefs   = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos  <>
                          Constraints.otherScript validator
                tx :: TxConstraints Void Void
                tx      = mconcat [Constraints.mustSpendScriptOutput oref r | oref <- orefs] <>
                          Constraints.mustValidateIn (from now)
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "Withdrawal success"
  where
    isSuitable :: PaymentPubKeyHash -> POSIXTime -> ChainIndexTxOut -> Bool
    isSuitable pkh now o = case _ciTxOutDatum o of
        Left _          -> False
        Right (Datum e) -> case PlutusTx.fromBuiltinData e of
            Nothing -> False
            Just sd@SavingDatum{..}  -> rdBeneficiary adRegister == pkh && rdDeadline adRegister <= now && rdPin adRegister == wpPin wp

-- Call endpoint
endpoints :: Contract () SavingsSchema Text ()
endpoints = awaitPromise (register' `select` deposit' `select` withdraw') >> endpoints
  where
    register' = endpoint @"1.Register" register
    deposit'  = endpoint @"2.Deposit" deposit
    withdraw' = endpoint @"3.Withdraw" withdraw

-- Call schema
mkSchemaDefinitions ''SavingsSchema 
