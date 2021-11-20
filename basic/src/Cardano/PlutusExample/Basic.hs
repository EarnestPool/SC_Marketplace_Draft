{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}

module Cardano.PlutusExample.Basic
  ( basicSerialised
  , basicSBS
  ) where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS

import Ledger hiding (singleton)
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Ada as Ada

import qualified Plutus.V1.Ledger.Scripts as Plutus
import PlutusTx (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude as P hiding (Semigroup (..), unless)
import Prelude (IO, Semigroup (..), String, Show, toInteger, fromIntegral, floor)


data ContractParams = ContractParams
     {
      owner :: !PubKeyHash,
      commission :: !Integer,
      minPrice :: !Integer,
      minCommission :: !Integer
     } deriving Show

contractParams = ContractParams
     {
       owner = "082becc381b7c9dd384554159e4a3ff1f59efbac262a67e156972264",
       commission = 25,
       minPrice = 10000000,
       minCommission = 2500000
     }


data ListingDatum = ListingDatum
    { seller :: !PubKeyHash,
      price :: !Integer
    } deriving Show

PlutusTx.makeIsDataIndexed ''ListingDatum [('ListingDatum,0)]
PlutusTx.makeLift ''ListingDatum

PlutusTx.makeIsDataIndexed ''ContractParams [('ContractParams,0)]
PlutusTx.makeLift ''ContractParams

{-
    validator script
-}

{-# INLINABLE mkValidator #-}
mkValidator :: ContractParams -> ListingDatum -> Integer -> ScriptContext -> Bool
mkValidator contractParams@ContractParams{..} dat r ctx = if r == 0 then cancelledBySeller else if r == 1 then boughtByBuyer else False
      where 
          info :: TxInfo
          info = scriptContextTxInfo ctx

          cancelledBySeller :: Bool
          cancelledBySeller = txSignedBy info $ seller dat

          boughtByBuyer :: Bool
          boughtByBuyer = let paidFee = (Ada.fromValue (valuePaidTo info owner)) 
                              in 
                                 (Ada.fromValue (valuePaidTo info $ seller dat)) + paidFee >= (Ada.lovelaceOf $ price dat) &&
                                 paidFee >= (Ada.lovelaceOf minCommission) && 
                                 paidFee >= (Ada.lovelaceOf ((P.divide (price dat) 1000) * commission)) &&
                                 paidFee <= (Ada.lovelaceOf minCommission) + (Ada.lovelaceOf ((P.divide (price dat) 1000) * commission)) 
        
data Typed
instance Scripts.ValidatorTypes Typed where
         type instance DatumType Typed = ListingDatum
         type instance RedeemerType Typed = Integer

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
     ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractParams)
     $$(PlutusTx.compile [|| wrap ||])
   where
     wrap = Scripts.wrapValidator @ListingDatum @Integer

{-
    As a Validator
-}

validator :: Validator
validator = Scripts.validatorScript typedValidator

{-
    As a Script
-}

validatorScript :: Plutus.Script
validatorScript = Plutus.unValidatorScript validator

{-
    As a Short Byte String
-}

basicSBS :: SBS.ShortByteString
basicSBS =  SBS.toShort . LBS.toStrict $ serialise validatorScript

{-
    As a Serialised Script
-}

basicSerialised :: PlutusScript PlutusScriptV1
basicSerialised = PlutusScriptSerialised basicSBS
