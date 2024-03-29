{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.TPCC where

import UCap.TPCC.Data

import Control.Arrow
import Control.Monad (foldM)
import Control.Monad.Except
import Data.Foldable (for_)
import Data.Map (Map)
import qualified Data.Map as Map
import UCap

-- | Transactions that can abort, using the ExceptT monad transformer.
type Opx c m a b = Op c (ExceptT () m) a b

-- | Record a new order, add its cost to the customer's balance, and
-- reduce warehouse stocks to supply it.
newOrder
  :: (Monad m)
  => ReplicaId
  -> CustomerId
  -> [OrderLine]
  -> Opx TpccC m a OrderId
newOrder rid cid ols =
  -- Reduce item warehouse stock counts (for warehouse "w1"),
  (stockLf ^# takeItems "w1" ols)
  -- and then calculate order cost for customer,
  *> (itemsLf ^# calcCost ols)
     -- feeding that cost into a transaction that applies it to the
     -- customer's balance,
     >>> (customersLf >: keyLf cid >: balanceLf ^# addOp')
  -- and finally, on the table of orders,
  *> ordersLf
     -- create an unused order ID,
     ^# (newOid rid
         -- pair it with the new order (for date "today"),
         >>> mapOp (\oid -> (oid, mkOrder cid "today" ols))
         -- and insert this as a new table entry.  The "insertOp"
         -- transaction, provided by the ucap library, inserts a pair
         -- as key and value into a Map structure.
         >>> insertOp)

-- | Accept payment from a customer against their balance.
acceptPayment :: (Monad m) => CustomerId -> Int -> Opx TpccC m a Int
acceptPayment cid amt =
  -- Zoom in on customer's balance,
  customersLf >: keyLf cid >: balanceLf
  -- subtract the payed amount from the balance,
  ^# (subGuard 0 amt
      -- and read the resulting balance as the transaction's return
      -- value.
      *> query uniC)

-- | Calculate the total cost of an OrderLine list, by reading the
-- item prices in the application state.
calcCost :: (Monad m) => [OrderLine] -> Opx ItemsC m a Int
calcCost ols =
  let cost ol s = case s ^. at (ol^.olItemId) of
                    Just item -> return $ ol^.olQuantity * item^.iPrice
                    Nothing -> throwError ()
      total s = foldM (\a ol -> (+ a) <$> cost ol s) 0 ols
  in query uniC >>> actionOp total

-- | Generate a fresh order ID that has not been used in the current
-- order table.  This transaction takes a complete (idC) read of the
-- table, blocking all insertions and deletions, which is stronger
-- than strictly necessary, for now.
newOid :: (Monad m) => ReplicaId -> Opx OrdersC m a OrderId
newOid rid =
  -- Take a complete read of the table.
  query idC
  -- Using that snapshot, create a new ID that increments on any
  -- existing ID.
  >>^ \s ->
    let nums = map snd . filter ((== rid) . fst) $ Map.keys s
    in case nums of
         [] -> (rid,0)
         ns -> (rid, maximum ns + 1)

-- | Decrement item stocks according to OrderLines, or abort
-- transaction if the stocks are not sufficient.
takeItems
  :: (Monad m)
  => WarehouseId
  -> [OrderLine]
  -> Opx StocksC m a ()
takeItems w ols =
  -- For each item+quantity listed in the OrderLine list, run a
  -- transaction that removes quantity from the stock listing of the
  -- item...
  for_ (itemReqs w ols) $ \(item, quantity) ->
    -- by zooming in on the quantity field of item's stock listing...
    (keyLf item >: quantityLf)
    -- and subtracting quantity from that field, as long as result is
    -- not below 0.  The transaction "subGuard lim amt", provided by
    -- the ucap library, aborts when the result of subtracting amt
    -- would be less than lim.
    ^# (subGuard 0 quantity)

-- | Create item+quantity pairs from an OrderLine list.
itemReqs :: WarehouseId -> [OrderLine] -> [((WarehouseId, ItemId), Int)]
itemReqs w ols = Map.toList $ foldl
  (\m ol -> Map.alter (g ol) (w, ol^.olItemId) m)
  Map.empty
  ols
  where g ol a = Just $ ol^.olQuantity + (case a of
                                            Just n -> n
                                            Nothing -> 0)
