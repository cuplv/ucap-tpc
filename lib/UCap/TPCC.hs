{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.TPCC where

import UCap.TPCC.Data

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (traverse_)
import Data.UCap
import Data.UCap.Lens
import Data.UCap.Op

type TpccOp a m b = Op TpccC a (ExceptT () m) b

newOrder
  :: (Monad m)
  => ReplicaId
  -> CustomerId
  -> [OrderLine]
  -> TpccOp a m OrderId
newOrder rid cid ols =
  takeItems "w1" ols
  *> chargeCustomer cid ols
  *> insertOrder rid (mkOrder cid "today" ols)

acceptPayment :: (Monad m) => CustomerId -> Int -> TpccOp a m Int
acceptPayment cid amt = edLift
  (tpccCustomersEd *: keyEd cid *: cBalanceEd)
  (subGuard 0 amt *> queryOp uniC)

chargeCustomer :: (Monad m) => CustomerId -> [OrderLine] -> TpccOp a m Int
chargeCustomer cid ols =
  let read = queryOp uniC
      calc = mapOp (\s -> olTotal s ols)
      add = edLift (tpccCustomersEd *: keyEd cid *: cBalanceEd) addOp'
  in read *>= calc *>= add

insertOrder :: (Monad m) => ReplicaId -> Order -> TpccOp a m OrderId
insertOrder rid o = edLift tpccOrdersEd $
  queryOp idC
  *>= mapOp (\s -> (newOid s rid, o))
  *>= insertOp

newOid :: Map OrderId Order -> ReplicaId -> OrderId
newOid s rid =
  let nums = map snd . filter ((== rid) . fst) $ Map.keys s
  in case nums of
       [] -> (rid,0)
       ns -> (rid, maximum ns + 1)

takeItems :: (Monad m) => WarehouseId -> [OrderLine] -> TpccOp a m ()
takeItems w ols = traverse_ itemOp (itemReqs w ols)

itemReqs :: WarehouseId -> [OrderLine] -> [((WarehouseId, ItemId), Int)]
itemReqs w ols = Map.toList $ foldl
  (\m ol -> Map.alter (g ol) (w, ol^.olItemId) m)
  Map.empty
  ols
  where g ol a = Just $ ol^.olQuantity + (case a of
                                            Just n -> n
                                            Nothing -> 0)

itemOp :: (Monad m) => ((WarehouseId, ItemId), Int) -> TpccOp a m Int
itemOp (i,n) = edLift
  (tpccStockEd *: keyEd i *: sQuantityEd)
  (subGuard 0 n)

olTotal :: Tpcc -> [OrderLine] -> Int
olTotal s = foldl
  (\a ol -> case olCost s ol of
              Just c -> a + c)
  0

olCost :: Tpcc -> OrderLine -> Maybe Int
olCost s ol = do
  item <- s ^. tpccItems . at (ol^.olItemId)
  return $ ol^.olQuantity * item^.iPrice
