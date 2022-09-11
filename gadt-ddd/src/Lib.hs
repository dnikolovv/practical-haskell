{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Lib where

import Data.List.NonEmpty (NonEmpty)

-- For a more real-world example of this "pattern"
-- check `aws-lambda-haskell-runtime`
-- https://github.com/theam/aws-lambda-haskell-runtime/blob/master/src/Aws/Lambda/Runtime/Common.hs

-- Link to the article presenting this pattern
-- https://dnikolovv.github.io/practical-haskell-ddd-gadt

data OrderId

data CreatedAt

data OrderItem

data TrackingNumber

data ShippedAt

data ShipmentInfo
  = AwaitingShipment
  | Shipped TrackingNumber ShippedAt

data OrderData = OrderData
  { id :: OrderId,
    createdAt :: CreatedAt,
    items :: NonEmpty OrderItem
  }

data OrderStatus
  = Outstanding
  | PaidFor

data Order (status :: OrderStatus) where
  OutstandingOrder :: OrderData -> Order 'Outstanding
  PaidOrder :: OrderData -> ShipmentInfo -> Order 'PaidFor

-- We can now specify a status where others don't make sense
markAsPaid :: Order 'Outstanding -> m ()
markAsPaid = undefined

refundOrder :: Order 'PaidFor -> m ()
refundOrder order = do
  -- We don't need to validate the status as
  -- it cannot be anything different than `PaidFor`
  undefined

markAsShipped :: Order 'PaidFor -> m ()
markAsShipped (PaidOrder orderData shipmentInfo) = do
  -- We've stated in the type signature that
  -- `markAsShipped` works with orders that are PaidFor.
  -- Since `ShipmentInfo` on `PaidOrder` is no longer a `Maybe`,
  -- we don't need to validate anything.
  undefined

-- If you don't care about the order type, just don't specify a status
queryOrders :: m [Order any]
queryOrders = undefined